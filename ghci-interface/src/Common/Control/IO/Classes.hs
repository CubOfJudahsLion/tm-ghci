{- |
    Module      : Common.Control.IO.Classes
    Description : Behaviors for common I/O classes
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Typeclasses in this module make the types in 'Common.Control.IO.Types'
    actually useful, structuring the requirement of the providers according
    to their types.
-}


{-# LANGUAGE TypeFamilies #-}

module Common.Control.IO.Classes where


import Common.Control.IO.Types
import Data.Kind ( Type )


-- -- |  Makes a stream type initializable, with a @noop@ as default.
-- class StreamInitializer application (direction :: DirectionType) where
--   init :: TaggedStream application direction -> IO (Either String (TaggedStream application direction))
--   init _ = pure ()
-- 
-- -- |  Provides a data transformer, to be applied to strings after reading or
-- --  before writing, depending on the 'StreamDirectionType'.
-- class StreamTransformer application (direction :: DirectionType) where
--   data InType application direction :: Type
--   data OutType application direction :: Type
--   transform :: proxy application direction -> InType -> OutType
--   transform _ = id
-- 
-- 
-- -- |  Provides a set of standard streams ( to communicate 
-- class ( StreamInitializer application InputDirection
--       , StreamInitializer application (OutputDirection RegularOutput)
--       , StreamInitializer application (OutputDirection ErrorOutput)
--       )
--       => StandardHandleProvider application where
--   type SourceType application
--   provideInput, provideOutput, provideError :: SourceType application -> Handle
--   provide :: SourceType application -> StandardStreams application
--   provide source = StandardStreams
--     { input  = Tagged $ init $ provideInput  source
--     , output = Tagged $ init $ provideOutput source
--     , error  = Tagged $ init $ provideError  source
--     }
--   {-# MINIMAL provideInput, provideOutput, provideError #-}
-- 
-- 
-- class OutputTaggedStreamer a d where
--   sendToStream :: TaggedStream ('StreamType p ('OutputDirection t)) -> String -> IO ()
--   flushStream :: TaggedStream ('StreamType p ('OutputDirection t))
-- 
-- 
-- class InputStreamer p where
--   getStream :: TaggedStream ('StreamType p 'InputDirection) -> IO String
-- 
