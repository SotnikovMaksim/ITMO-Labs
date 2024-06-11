{-# LANGUAGE DerivingVia #-}

module HW5.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Control.Exception (Exception, throwIO)
import Data.Set (Set, member)
import qualified Data.ByteString as DB
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import System.Directory
import HW5.Base
import Data.Text.Encoding (decodeUtf8')
import Control.Monad.Reader (ReaderT(..))
import Data.Time (getCurrentTime)
import System.Random (randomRIO)
  
data HiPermission 
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving stock (Show, Eq, Ord, Enum, Bounded)

newtype PermissionException
  = PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)

instance HiMonad HIO where
  runAction action = HIO $ \permissions -> checkPermissions permissions action

-- Helper function to check permissions and execute actions
checkPermissions :: Set HiPermission -> HiAction -> IO HiValue
checkPermissions permissions action =
  case action of
    HiActionRead filePath -> 
      requirePermission AllowRead permissions $ handleReadAction filePath
    HiActionWrite filePath content -> 
      requirePermission AllowWrite permissions $ handleWriteAction filePath content
    HiActionMkDir filePath -> 
      requirePermission AllowWrite permissions $ createDirectory filePath >> return HiValueNull
    HiActionChDir filePath -> 
      requirePermission AllowRead permissions $ setCurrentDirectory filePath >> return HiValueNull
    HiActionCwd -> 
      requirePermission AllowRead permissions $ HiValueString . DT.pack <$> getCurrentDirectory
    HiActionNow -> 
      requirePermission AllowTime permissions $ HiValueTime <$> getCurrentTime     
    HiActionRand x y ->
      handleRandAction x y 
    HiActionEcho text ->
      requirePermission AllowWrite permissions $ handleEchoAction text

-- Functions to handle specific actions
handleReadAction :: FilePath -> IO HiValue
handleReadAction filePath = do
  exists <- doesFileExist filePath
  if exists then readContent filePath else listDirectoryContents filePath

handleWriteAction :: FilePath -> DB.ByteString -> IO HiValue
handleWriteAction filePath content = DB.writeFile filePath content >> return HiValueNull

handleRandAction :: Int -> Int -> IO HiValue
handleRandAction lowerBound upperBound = do
  randomNumber <- randomRIO (lowerBound, upperBound)
  return $ HiValueNumber $ toRational randomNumber
  
handleEchoAction :: DT.Text -> IO HiValue
handleEchoAction text = do
  putStrLn $ DT.unpack text
  return HiValueNull

-- Utility functions
readContent :: FilePath -> IO HiValue
readContent filePath = do
  content <- DB.readFile filePath
  return $ either (const $ HiValueBytes content) HiValueString (decodeUtf8' content)

listDirectoryContents :: FilePath -> IO HiValue
listDirectoryContents filePath =
  HiValueList . DS.fromList . fmap (HiValueString . DT.pack) <$> listDirectory filePath

requirePermission :: HiPermission -> Set HiPermission -> IO HiValue -> IO HiValue
requirePermission permission permissions action =
  if permission `member` permissions then action else throwIO $ PermissionRequired permission
