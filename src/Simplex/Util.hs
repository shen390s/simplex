{-# LANGUAGE Haskell2010 #-}

module Simplex.Util (
    when', ifElse, tail', tail'',
    skipOneSpace, (~=), (~~), (^=), units,
    removeIfExists, isLeft, isRight,
    getModificationTime', exec) where

import Text.Regex
import Data.Maybe

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (rnf)
import Control.Exception
import Control.Monad
import Data.Time
import System.Directory
import System.Exit
import System.IO
import System.Process
import GHC.IO.Encoding (mkTextEncoding)

--  common tex units

units :: [String]
units = ["pt", "mm", "cm", "in", "ex", "em",
         "bp", "pc", "dd", "cc", "sp"]


--  regex operators

(~=) :: String -> String -> Bool
s ~= rx = isJust (matchRegex (mkRegex rx) s)

(~~) :: String -> String -> Maybe [String]
s ~~ rx = matchRegex (mkRegex rx) s

(^=) :: String -> String -> Bool
s ^= [] = True
(s:ss) ^= (x:xs)
    | s == x = ss ^= xs
_ ^= _ = False

endsWith s xs = take (length s') (reverse xs) == s'
    where s' = reverse s

--  control flow

when' :: Bool -> [a] -> [a]
when' True x = x
when' False _ = []

ifElse :: Bool -> a -> a -> a
ifElse True x _ = x
ifElse _    _ y = y

--  convenience functions for list processing

-- | like @tail@, but returns the empty list for the empty list.
tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

-- | same as `tail' . tail'`
tail'' :: [a] -> [a]
tail'' [] = []
tail'' xs = tail' $ tail' xs

skipOneSpace :: String -> String
skipOneSpace (' ':xs) = xs
skipOneSpace s = s

--  file handling

removeIfExists :: String -> IO ()
removeIfExists file = do
    exists <- doesFileExist file
    when exists (removeFile file)

getModificationTime' :: FilePath -> IO UTCTime
getModificationTime' file = do
    exists <- doesFileExist file
    if exists then getModificationTime file
              else return $ UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

--  process execution

exec :: Bool -> String -> [String]
     -> IO (Either (Int, String) String)

exec True cmd args = do
    r <- try $ rawSystem cmd args
    return $ either (\e -> Left (127, show (e :: IOException))) handle r
        where
            handle (ExitFailure 127) = Left  (127, cmd ++ ": command not found")
            handle (ExitFailure exc) = Left  (exc, "")
            handle ExitSuccess       = Right ""

exec _ cmd args = do
    r <- try $ readProcessRobust cmd args
    return $ either (\e -> Left (127, show (e :: IOException))) handle r
        where
            handle (ExitFailure 127, out, err) = Left  (127, cmd ++ ": command not found")
            handle (ExitFailure exc, out, err) = Left  (exc, out ++ err)
            handle (ExitSuccess,     out, err) = Right $ out ++ err

-- | Like @readProcessWithExitCode@, but decodes the child's stdout/stderr with
-- a UTF-8 decoder that ignores (rather than crashes on) invalid byte
-- sequences.  External tools such as @pdflatex@/@xelatex@ and the diagram
-- renderers may emit log output that is not valid UTF-8 (especially with
-- @\@cjk@ documents); the default lazy @hGetContents@ used by
-- @readProcessWithExitCode@ throws
-- "hGetContents: invalid argument (cannot decode byte sequence ...)" on such
-- output.  Reading the pipes with a non-failing encoding avoids that.
readProcessRobust :: String -> [String] -> IO (ExitCode, String, String)
readProcessRobust cmd args = do
    -- "UTF-8//IGNORE" drops undecodable bytes instead of raising an error.
    enc <- mkTextEncoding "UTF-8//IGNORE"
    (_, Just hOut, Just hErr, ph) <- createProcess (proc cmd args)
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    hSetEncoding hOut enc
    hSetEncoding hErr enc
    outVar <- newEmptyMVar
    errVar <- newEmptyMVar
    let slurp h var = forkIO $ do
            s <- hGetContents h
            _ <- evaluate (rnf s)
            putMVar var s
    _ <- slurp hOut outVar
    _ <- slurp hErr errVar
    out <- takeMVar outVar
    err <- takeMVar errVar
    ec  <- waitForProcess ph
    return (ec, out, err)

--  either utilities

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

