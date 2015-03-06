{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.S3 as S3
import Control.Monad.Trans
import           Data.Conduit (($$+-))
import           Data.Conduit.Binary (sourceFile)
import           Network.HTTP.Conduit (withManager, requestBodySourceChunked)

main :: IO ()
main = do
  {- Set up AWS credentials and the default configuration. -}
  cfg <- Aws.baseConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  {- Set up a ResourceT region with an available HTTP manager. -}
  withManager $ \mgr -> do
    {- Create a request object with S3.getObject and run the request with pureAws. -}
    S3.PutObjectResponse { S3.porVersionId = vers } <-
      Aws.pureAws cfg s3cfg mgr $
        S3.putObject "capitalmatch" "test.pdf" (requestBodySourceChunked $ sourceFile "test.pdf")

    liftIO $ putStrLn $ "put file with version " ++ show vers
