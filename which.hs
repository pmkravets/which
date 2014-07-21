import Data.String
import System.Environment (getEnv, getArgs)
import Control.Monad
import System.Posix.Files
import System.Directory
import Data.List
import Data.Bool
import Data.Maybe

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM f = foldr check (return Nothing)
    where
      check c r = do
          b <- f c
          if b then return (Just c) else r

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                     "" -> []
                     s' -> w : wordsWhen p s''
                           where (w, s'') = break p s'

splitByColon = wordsWhen (==':')

checkExecutable :: FilePath -> IO Bool
checkExecutable file = fileAccess file False False True

checkFile :: FilePath -> IO Bool
checkFile path = do
    fileExist <- doesFileExist path
    if fileExist
        then checkExecutable path
        else return False


which' :: String -> String -> IO (Maybe String)
which' path s = findM checkFile $ map (\x-> x ++ "/" ++ s) $ splitByColon path

which :: String -> IO (Maybe String)
which s = do
   path <- getEnv "PATH"
   which' path s

main :: IO ()
main = do
    (program:_) <- getArgs
    fullPath <- which program
    putStrLn $ fromMaybe "No such path" fullPath
