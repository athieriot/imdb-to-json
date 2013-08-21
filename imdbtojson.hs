{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import System.IO.UTF8
import Prelude hiding (readFile, writeFile, print, putStrLn)
import System.FilePath
import System.Directory
import System.Console.CmdArgs
import Data.List
import Data.Typeable
import Data.Data
import Data.Maybe
import Data.Text (unpack, strip, pack)
import Control.Monad
import Text.Regex.TDFA
import Text.JSON
import Text.JSON.Generic

data ImdbToJson = ImdbToJson
    { directory :: FilePath }
    deriving (Data, Typeable, Show, Eq)

mode = cmdArgsMode (ImdbToJson
    { directory = def &= help "Help"}
    &= verbosity)

main = do
    ImdbToJson{..} <- cmdArgsRun mode
    files <- listFiles directory
    strings <- mapM transformFiles files

    print strings

listFiles :: FilePath -> IO [FilePath]
listFiles dir = do
    contents <- getDirectoryContents dir
    return $ map (dir </>) contents

transformFiles :: FilePath -> IO String
transformFiles path
    | "actors" `isInfixOf` path = transformActors path
    | otherwise                 = return ""

transformActors :: FilePath -> IO String
transformActors path = do
    contents <- readFile path
    let actors = map matchActors $ lines contents

    writeFile (path <.> "json") (encode $ toJSON $ catMaybes actors)

    return path

data Actor = Actor {name        :: String, 
                    surname     :: String,
                    title       :: String,
                    year        :: String,
                    series      :: String,
                    number      :: String,
                    role        :: String }
                    deriving (Show, Eq, Typeable, Data)

matchActors :: String -> Maybe Actor
matchActors string =
    let actorPattern = "^([^, \t]*)(, ([^ \t]*))?[ \t]*([^\\(]*)\\(([0-9]*)\\)([^\\{]\\{([^\\(]*)\\((.*)\\)\\})?([^\\[]*\\[(.*)\\])?"
    in case string =~ actorPattern :: (String,String,String,[String])
        of (_, _, _, (name : _ : surname : title : year : _ : series : number :  _ : role : [])) -> Just $ Actor name surname title year series number role
           otherwise -> Nothing
