{-# LANGUAGE DerivingVia #-}

module HW5.Action
  ( HIO(..)
  , HiPermission(..)
  , PermissionException(..)
  ) where

import HW5.Base
import HW5.Evaluator(decodeUtf8OrNull)
import qualified Data.Set as Set
import Data.Maybe
import Data.Time.Clock
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import System.Directory
import System.Random
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Data.ByteString as B

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

newtype PermissionException =
  PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set.Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT (Set.Set HiPermission) IO)

instance MonadIO HIO where
  liftIO = HIO . const

instance HiMonad HIO where
  runAction (HiActionRead fp) = HIO $ \perms -> do
    requireHiPermission AllowRead perms
    isFile <- doesFileExist fp
    if isFile
      then do
        bytes <- B.readFile fp
        return $ fromMaybe (HiValueBytes bytes) $ decodeUtf8OrNull bytes
      else do
        entries <- listDirectory fp
        return $ HiValueList $ S.fromList $ map (HiValueString . T.pack) entries
  runAction (HiActionWrite fp bs) = HIO $ \perms -> do
    requireHiPermission AllowWrite perms
    liftIO $ B.writeFile fp bs
    return HiValueNull
  runAction (HiActionMkDir fp) = HIO $ \perms -> do
    requireHiPermission AllowWrite perms
    liftIO $ createDirectory fp
    return HiValueNull
  runAction (HiActionChDir fp) = HIO $ \perms -> do
    requireHiPermission AllowRead perms
    liftIO $ setCurrentDirectory fp
    return HiValueNull
  runAction HiActionCwd = HIO $ \perms -> do
    requireHiPermission AllowRead perms
    HiValueString . T.pack <$> getCurrentDirectory
  runAction HiActionNow = HIO $ \perms -> do
    requireHiPermission AllowTime perms
    HiValueTime <$> getCurrentTime
  runAction (HiActionRand x y) = HIO $ const $ do
    val <- getStdRandom $ uniformR (x, y)
    return $ HiValueNumber $ toRational val
  runAction (HiActionEcho t) = HIO $ \perms -> do
    requireHiPermission AllowWrite perms
    liftIO $ putStrLn (T.unpack t)
    return HiValueNull

requireHiPermission :: HiPermission -> Set.Set HiPermission -> IO ()
requireHiPermission p perms = when (Set.notMember p perms) (throwIO $ PermissionRequired p)
