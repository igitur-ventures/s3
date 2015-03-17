{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           System.Console.GetOpt
import           System.Environment    (getArgs)

import           System.Posix

import qualified Aws
import qualified Aws.S3                as S3
import           Control.Applicative   ((<$>))
import           Control.Monad         (forM_)
import           Control.Monad.Trans   (liftIO)
import           Data.Conduit.Binary   (sourceFile)
import           Data.Default
import           Data.Monoid           ((<>))
import           Data.Text             (pack)
import           Network.HTTP.Conduit  (requestBodySource, withManager)

type BucketName = String
type Files = [String]

data AWSOp = Put  { bucket :: BucketName
                  , files  :: Files
                  }
           | Get { bucket :: BucketName
                 , files  :: Files
                 }
           | NoOp
           deriving (Show)

data AWSConfig = AWSConfig { awsOps :: [AWSOp] } deriving (Show)

instance Default AWSConfig where
  def = AWSConfig []

putToBucket :: BucketName -> AWSConfig -> AWSConfig
putToBucket bname   config = config { awsOps  = Put bname [] : awsOps config }

getFromBucket :: BucketName -> AWSConfig -> AWSConfig
getFromBucket bname config = config { awsOps  = Get bname [] : awsOps config }

appendToLastOp :: String -> AWSConfig -> AWSConfig
appendToLastOp f (AWSConfig [])                       = error $ "bad argument: " ++ f ++ ", cannot process files without an operation"
appendToLastOp f (AWSConfig ((Get bucket files):ops)) = AWSConfig (Get bucket (f:files):ops)
appendToLastOp f (AWSConfig ((Put bucket files):ops)) = AWSConfig (Put bucket (f:files):ops)
appendToLastOp _ c                                    = c

options :: [OptDescr (AWSConfig -> AWSConfig)]
options = [ Option ['p'] ["put"]
            (ReqArg ( \ f -> putToBucket f) "BUCKET NAME")
            "execute a put operation to given bucket"
          , Option ['g'] ["get"]
            (ReqArg ( \ f -> getFromBucket f) "BUCKET NAME")
            "execute a get operation to given bucket"
          ]

parseOptions :: [String] -> IO AWSConfig
parseOptions args = case getOpt (ReturnInOrder appendToLastOp) options args of
   (c,_,[])   -> return (foldl (flip id) def c)
   (_,_,errs) -> ioError (userError (concat errs  ++ usageInfo header options))
     where
       header = "Usage: archiver [OPTION...]"

main :: IO ()
main = do
  args <- getArgs
  config <- parseOptions args
  let ops = reverse $ awsOps config

  {- Set up AWS credentials and the default configuration. -}
  cfg <- Aws.dbgConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  mapM_ (runOp cfg s3cfg)  ops

runOp :: Aws.Configuration -> S3.S3Configuration Aws.NormalQuery -> AWSOp -> IO ()
runOp cfg s3cfg Put{..}  = forM_ files (putFile cfg s3cfg bucket)
runOp _   _     Get{..}  = undefined
runOp _   _     NoOp{..} = return ()

putFile :: Aws.Configuration -> S3.S3Configuration Aws.NormalQuery -> FilePath -> String -> IO ()
putFile cfg s3cfg bucket file = do
  sz <- fileSize <$> getFileStatus file
  {- Set up a ResourceT region with an available HTTP manager. -}
  withManager $ \mgr -> do
    {- Create a request object with S3.getObject and run the request with pureAws. -}
    S3.PutObjectResponse { S3.porVersionId = vers } <-
      Aws.pureAws cfg s3cfg mgr $
      -- need to read file fully and know its size: https://forums.aws.amazon.com/message.jspa?messageID=554788
      S3.putObject (pack bucket) (pack file) (requestBodySource (fromIntegral sz) $ sourceFile file)
    liftIO $ putStrLn $ "put file " <> file <> ":  " <> show vers
