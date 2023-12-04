module Utils where

import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec ( ParsecT, runParserT )
import Control.Monad.IO.Class (MonadIO, liftIO)

parseFile :: (MonadIO m) => ParsecT Void Text m a -> FilePath -> m a
parseFile parser filepath = do
  input <- pack <$> liftIO (readFile filepath)
  result <- runParserT parser "Utils.hs" input
  case result of
    Left e -> error $ "Failed to parse: " ++ show e
    Right x -> return x
