{-|
Module      : Launch
Description : Utilities for launching various various Haskell REPLs.
              Testing the proper conditions for launch is done
              automatically and is not part of the interface.
Copyright   : (c) 2022,2023 Alexander Feterman Naranjo
License     : MIT
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX
-}

module Launch ( Package, Argument, LaunchRepl(..), launch ) where


import Data.Either.Combinators ( maybeToRight )
import System.Directory ( {-doesDirectoryExist,-} doesFileExist
                        , findExecutable )
import System.IO ( hPutStrLn, stderr )
--import System.Process ( proc, withCreateProcess, CreateProcess(..)
--                      , StdStream(CreatePipe) )


type Package = String
type Argument = String
type ExecutableName = String


--  Verifies that the executable and project path (if present) exist.
--  Returns either an error message or a tuple with the project path and full executable location.
verifyLaunchConditions  :: Maybe FilePath -- project file
                        -> ExecutableName -- REPL executable name
                        -> IO (Either String (Maybe FilePath, FilePath))
verifyLaunchConditions maybePath replExeName = do
  Right maybePath'  <-  case maybePath of
                          Just path   -> do
                            pathExists <- doesFileExist path
                            pure $ if pathExists
                              then Right (Just path)
                              else Left ("Path \"" ++ path ++ "\" does not exist")
                          Nothing     ->
                            pure $ Right Nothing
  Right replExePath <-  maybeToRight ("Executable for \"" ++ replExeName ++ "\" not found")
                    <$> findExecutable replExeName
  pure $ Right (maybePath', replExePath)


--  Actual launcher of REPL processes
launchHelper  :: ( Maybe FilePath -- project file
                 , ExecutableName -- REPL executable name
                 , [Argument]     -- argument list
                 )
              -> IO ()
launchHelper (maybePrj, replExeName, arguments) = do
  maybePrjAndExe <- verifyLaunchConditions maybePrj replExeName
  case maybePrjAndExe of
    Left errorStr                   ->
      hPutStrLn stderr errorStr
    Right (maybePath', replExePath) ->
      putStrLn  (   "REPL executable found. Would run: "
                ++  replExePath
                ++  case unwords arguments of
                      ""                ->  ""
                      argumentsAsString ->  ' ' : argumentsAsString
                ++  case maybePath' of
                      Nothing   ->  ""
                      Just path ->  " (with project file: "
                                ++  path
                                ++  " as pwd)")


-- |  Types of REPL. They all run GHCi at the end, but some
-- |  do have a project context (i.e., a project file.)
data LaunchRepl = CabalRepl FilePath
                | StackRepl FilePath
                | GHCiRepl            -- runs ghci solo (using only distribution libs)


--  Converts a LaunchRepl into the concrete strings it represents.
replStrings :: LaunchRepl -> [Argument] -> (Maybe FilePath, ExecutableName, [Argument])
replStrings GHCiRepl arguments                = (Nothing,          "ghci",           arguments)
replStrings (CabalRepl projectPath) arguments = (Just projectPath, "cabal", "repl" : arguments)
replStrings (StackRepl projectPath) arguments = (Just projectPath, "stack", "repl" : arguments)


-- |  Launcher function
launch :: LaunchRepl -> [Argument] -> IO ()
launch = (launchHelper .) . replStrings

