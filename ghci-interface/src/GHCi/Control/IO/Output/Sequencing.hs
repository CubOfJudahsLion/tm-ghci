{- |
    Module      : GHCi.Control.IO.Output.Sequencing
    Description : Coordinated output capture and processing of /GHCi/'s outputs
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    /GHCi/, along with any executing programs within, is capable of switching between @STDOUT@ and
    @STDERR@ without notice. The input prompt is also produced as the last @STDOUT@ line, but it
    might be succeeded by @STDERR@ output. The routines in this module deal with /GHCi/'s multiple-
    output nature.
-}

{-# LANGUAGE PatternSynonyms #-}

module GHCi.Control.IO.Output.Sequencing ( captureOutputs, extractPrompt, joinEqualOutputs ) where


import Control.Arrow ( first, (>>>) )
import Control.DeepSeq ( ($!!), (<$!!>) )
import Control.Monad ( when )
import Control.Concurrent ( threadDelay )
import Data.Bits ( (.&.) )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty, toList )
import GHCi.Control.IO.Types  ( OutputTag(Out)
                              , Tagged
                              , pattern (:@)
                              , TaggedLine
                              , TaggedLines
                              , TaggedLines1
                              )
import GHCi.Control.IO.Output.Reading ( readAndTagAvailable )
import System.IO ( Handle, hReady )


--  Maximum dead (consecutive) time in __milliseconds__ before the function exits
maxDeadTime :: Int
maxDeadTime = 200

--  Allowed maximum wait between characters of a single stream
charWait :: Int
charWait = 1

--  Waiting time after each cycle.
cycleWait :: Int
cycleWait = 5


-- |  Takes two readable 'Handle's and returns the lines read, all tagged by
--    their origin stream (this helps in later formatting and directing output.)
--    The function will wait and read the first 'Handle'; afterwards, it will
--    read from either as it becomes available, within a maximum inactivity limit.
captureOutputs  :: (Tagged Handle, Tagged Handle) -- ^  The streams to be read
                -> IO TaggedLines1                -- ^  A non-empty list of lines, each tagged with its origin output stream
captureOutputs (hot@(tag :@ handle), cold@(_ :@ otherHandle)) = do
  (x :| xs) <- readAndTagAvailable tag charWait handle otherHandle  --  First mandatory read
  let accum = foldl' (.) (x :|) $ map (:) xs                        --  Turn lines read into a /difference list/
  capture' accum 0 0 (cold, hot)
  where
    --  Worker function. Swaps streams on recursion so each gets its turn.
    capture'  :: (TaggedLines -> TaggedLines1)  --  Output accumulator (as a difference list)
              -> Int                            --  Total dead time after last output
              -> Int                            --  Number of 'Handle' readiness test failed in a row
              -> (Tagged Handle, Tagged Handle) --  Switching stream pair
              -> IO TaggedLines1                --  Returns all tagged lines gathered
    capture' !accum !deadTime !failedTests (hotStream@(tag :@ handle), coldStream@(_ :@ otherHandle)) = do
      readable <- hReady handle
      if not readable && failedTests .&. 1 == 1 && deadTime >= maxDeadTime then
        --  If there are no ready streams and we're over the idle limit, stop. Note that
        --  'failedTests' is one failure short now, i.e., if the actual failure count is
        --  even (both streams failed several times), then 'failedTests' is odd.
        pure $ accum []
      else do
        (accum', deadTime', delayBeforeRecursion, failedTests') <-
          if readable then do
            --  If a stream is readable, we read what we can with minimal pause
            taggedTexts <- readAndTagAvailable tag charWait handle otherHandle
            let extendedAccum = foldl' (.) accum $ (:) <$> taggedTexts
            --  A successful read resets both dead time and failure count
            pure (extendedAccum, 0, 0, 0)
          else
            --  Every second failure (i.e., after both streams fail to be ready again) we
            --  set a delay before the iteration and add it to the cumulative dead time
            let addedWait = (failedTests .&. 1) * cycleWait
            in  pure (accum, deadTime + addedWait, addedWait, failedTests + 1)
        --  Don't hog the CPU
        when (delayBeforeRecursion > 0) $
          threadDelay (delayBeforeRecursion * 1_000)   -- 'threadDelay' counts in __nanoseconds__.
        --  Swap stream on recursion regardless of result, for fairer chances.
        capture' accum' deadTime' failedTests' (coldStream, hotStream)


-- |  Extracts the last line tagged as 'Out' (/GHCi/
--    produces the prompt as its last @STDOUT@ line.)
extractPrompt :: TaggedLines1 -> (Maybe TaggedLines1, Maybe String)
extractPrompt =   toList
              >>> foldr (\taggedLine@(tag :@ line) !(lines, maybeLast) ->
                          case (tag, maybeLast) of
                            (Out, Nothing)  -> (lines,              Just line)
                            (_  , _      )  -> (taggedLine : lines, maybeLast))
                        ([], Nothing)
              >>> first nonEmpty


-- |  Takes a list of 'TaggedLines1' and joins successive lines with the
--    same 'OutputTag' into a single line with @newline@s interspersed.
joinEqualOutputs :: Maybe TaggedLines1 -> Maybe TaggedLines1
joinEqualOutputs = fmap (foldr joinTagged []) >>> (>>= nonEmpty)
  where
    joinTagged :: TaggedLine -> TaggedLines -> TaggedLines
    joinTagged line [] = [line]
    joinTagged line@(tag :@ msg) !accum@(prevTag :@ prevMsg : rest)
      | prevTag == tag  = tag :@ (msg ++ '\n' : prevMsg) : rest
      | otherwise       = line:accum

