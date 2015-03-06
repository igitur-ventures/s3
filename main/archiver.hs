{-# LANGUAGE OverloadedStrings #-}

import System.Environment(getArgs)
import System.Posix
import qualified Aws
import qualified Aws.S3 as S3
import Control.Applicative ((<$>))
import           Control.Monad.Trans
import Data.Conduit.Binary(sourceFile)
import           Network.HTTP.Conduit (withManager, requestBodySource)

main :: IO ()
main = do
  [file] <- getArgs
  {- Set up AWS credentials and the default configuration. -}
  cfg <- Aws.dbgConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery
  sz <- fileSize <$> getFileStatus file
  {- Set up a ResourceT region with an available HTTP manager. -}
  withManager $ \mgr -> do
    
    {- Create a request object with S3.getObject and run the request with pureAws. -}
    S3.PutObjectResponse { S3.porVersionId = vers } <-
      Aws.pureAws cfg s3cfg mgr $
      -- need to read file fully and know its size: https://forums.aws.amazon.com/message.jspa?messageID=554788
      S3.putObject "capitalmatch" "test.txt" (requestBodySource (fromIntegral sz) $ sourceFile file)

    liftIO $ putStrLn $ "put file with version " ++ show vers
