module InputOutput
where


import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S 

inputOutputFunc = putStrLn "InputOutput"

-- copyFile ".LICENSE.txt" ".LICENSE1.txt"
copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do 
    contents <- B.readFile source
    B.writeFile dest contents
