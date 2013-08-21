{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.IO.UTF8
import Prelude hiding (readFile, writeFile, print, putStrLn)
import System.FilePath
import System.Directory
import System.Console.CmdArgs
import Data.List
import Control.Monad
import Text.Regex.TDFA
import Data.Text (unpack, strip, pack)

data ImdbToJson = ImdbToJson
    {directory :: FilePath
    }
    deriving (Data, Typeable, Show, Eq)

imdbToJson = ImdbToJson
    {directory = def &= help "Help"
    } &= verbosity

mode = cmdArgsMode imdbToJson

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
    let list = lines contents
    let splited = map catchPatterns list
    let jsons = filter (not.null) (map jsonise splited)
    mapM (\l -> print l) jsons

    return path

catchPatterns :: String -> [String]
catchPatterns string =
    let pattern = "^([^, \t]*)(, ([^ \t]*))?[ \t]*([^\\(]*)\\(([0-9]*)\\)([^\\{]\\{([^\\(]*)\\((.*)\\)\\})?([^\\[]*\\[(.*)\\])?"
    in case string =~ pattern :: (String,String,String,[String])
        of (_, _, _, groups) -> groups

jsonise :: [String] -> String
jsonise groups = 
    let trimedGroups = map (\t -> unpack $ strip $ pack t) groups
    in case trimedGroups
        of (name : _ : surname : title : year : _ : series : number :  _ : role : []) -> "{ \"name\":'" ++ name ++ "', \"surname\":'" ++ surname ++ "', \"title\":'" ++ title ++ "', \"year\":'" ++ year ++ "', \"role\":'" ++ role ++ "', series:'" ++ series ++  "', number:'" ++ number ++ "'}\n"
           x -> ""
