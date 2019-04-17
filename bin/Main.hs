module Main
    ( main
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson (eitherDecodeFileStrict')


-- base ----------------------------------------------------------------------
import           System.Environment (getArgs)
import           System.IO (hPutStrLn, stderr)


-- devtools-api-generate -----------------------------------------------------
import           DevTools.Meta.Generate (write)


-- text -------------------------------------------------------------------
import qualified Data.Text as T


------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case args of
        [protocol, revision] -> go protocol revision dir
          where
            dir = "."
        [protocol, revision, dir] -> go protocol revision dir
        _ -> usage
  where
    usage = hPutStrLn stderr
        "Usage: devtools-api-generate PROTOCOL REVISION [DIR]"
    go path revision dir = do
        protocol <- eitherDecodeFileStrict' path >>= either fail pure
        write dir protocol $ T.pack revision
