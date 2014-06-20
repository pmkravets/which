import Data.String
import System.Environment (getEnv, getArgs)
import Control.Monad
import System.Posix.Files
import System.Directory
import Data.List
import Data.Bool

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
    if fileExist == True
        then checkExecutable path
        else do
            return False


which' :: String -> String -> IO String
which' path s = do
    p <- filterM checkFile $ map (\x-> x ++ "/" ++ s) $ nub $ splitByColon path
    return $ head p

which :: String -> IO String
which s = getEnv "PATH" >>= flip which' s

main :: IO ()
main = do
    (program:_) <- getArgs
    which program >>= putStrLn
