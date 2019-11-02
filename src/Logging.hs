{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{- |
Module      : Logging
Description : Utility methods
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3
Maintainer  : gbwey9@gmail.com

Mainly has various logging functions and timing of commands.
Allows you to log to a file or the screen or both
-}
module Logging where
import Data.Time
import Data.Time.Format (FormatTime)
import System.Clock
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Control.Monad.Reader
import qualified Control.Exception as E
import Control.Monad.Logger
import System.Log.FastLogger
import Data.Time.ISO8601
import qualified Data.ByteString.Char8 as B
import Text.Shakespeare.Text
import Formatting
import qualified Formatting.Time as F
import System.IO
import Control.Lens
import Data.Time.Lens
import System.Time.Extra (showDuration)
import Network.Mail.SMTP
import Network.Mail.Mime hiding (simpleMail)
import System.Environment
import qualified UnliftIO as U
import qualified UnliftIO.Exception as UE
import Data.String
import qualified GHC.Generics as G
import Data.Maybe
import Dhall hiding (string,auto,map)
import qualified Dhall as D
import qualified Control.Monad.State.Strict as S
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Info as SI
import Data.Char
import Data.List (foldl', dropWhileEnd)
import System.Directory
import Numeric (showHex)
import Data.Aeson (ToJSON(..))
import Data.Functor.Contravariant ((>$<))
import Data.Text.Lazy.Builder (fromText)

newtype GBException = GBException { gbMessage :: Text } deriving (Show,Eq)
instance E.Exception GBException

newtype Secret = Secret { unSecret :: Text } deriving (TH.Lift, Generic, Eq, Read)

instance IsString Secret where
  fromString = Secret . T.pack

instance ToDhall Secret where
  injectWith i = unSecret >$< injectWith @Text i

instance FromDhall Secret where
  autoWith i = Secret <$> autoWith @Text i

instance ToJSON Secret where
  toJSON (Secret s) = toJSON s

instance Show Secret where
  show _ = "Secret *******"

-- | 'ML' has the minimum set of constraints for running sql commands in sqlhandler-odbc
-- use MonadReader e to customise the environment
type ML e m = (MLog m, MonadLogger m, MonadLoggerIO m, MonadReader e m)
type MLog m = U.MonadUnliftIO m
-- | 'RL' defines the outer two layers of 'ML'
type RL e m a = ReaderT e (LoggingT m) a

data LLog = Debug | Info | Warn | Error deriving (TH.Lift, G.Generic, Show, Eq, Enum, Bounded, Ord)
makePrisms ''LLog
instance FromDhall LLog

-- | log to the screen
data ScreenType = StdOut | StdErr deriving (TH.Lift, Show, Eq, G.Generic, Enum, Bounded, Ord)
makePrisms ''ScreenType
instance FromDhall ScreenType

data Screen = Screen {
      _sScreenType :: !ScreenType
    , _sLevel :: !LLog
    } deriving (TH.Lift, Show, Eq, G.Generic)

makeLenses ''Screen

-- | log to a file
data File = File {
      _fPrefix :: FilePath -- ^ basename of log file
    , _fLongName :: !Bool -- ^ whether to use a unique name based on datetime or the 'lfName' as is
    , _fLevel :: !LLog
    , _fDir :: !FilePath
    } deriving (TH.Lift, Show, Eq, G.Generic)

makeLenses ''File

data Email = Email
       { _eSmtpServer :: !Text
       , _eSmtpTo :: !Text
       , _eSmtpFrom :: !Text
       } deriving (TH.Lift, Generic, Show)

data LogOpts = LogOpts
       { _lFile :: !(Maybe File)
       , _lScreen :: !(Maybe Screen)
       , _lEmail :: !(Maybe Email)
       } deriving (TH.Lift, Generic, Show)

makeLenses ''LogOpts
makeLenses ''Email

instance ToText LogOpts where
  toText = fromText . T.pack . show

genericAutoZ :: (Generic a, GenericFromDhall (G.Rep a)) => InterpretOptions -> Type a
genericAutoZ i = fmap G.to (S.evalState (genericAutoWith i) 1)

instance FromDhall Email where
  autoWith i = genericAutoZ i { fieldModifier = T.drop 2 }

instance FromDhall File where
  autoWith i = genericAutoZ i { fieldModifier = T.drop 2 }

-- dont do it with screen cos is a tuple and you will lose _1 and _2
instance FromDhall LogOpts where
  autoWith i = genericAutoZ i { fieldModifier = T.drop 2 }

instance FromDhall Screen where
  autoWith i = genericAutoZ i { fieldModifier = T.drop 2 }

toLogLevel :: LLog -> LogLevel
toLogLevel = \case
               Debug -> LevelDebug
               Info -> LevelInfo
               Warn -> LevelWarn
               Error -> LevelError

isWindows :: Bool
isWindows =  SI.os == "mingw32"

joinPaths :: [FilePath] -> FilePath
joinPaths = foldl' (\a (trim -> b) -> if null a then b
                                      else case unsnoc b of
                                             Nothing -> a
                                             Just (_,x) | x == pathSeparator -> a <> b
                                                        | otherwise -> a <> (pathSeparator : b)
                   ) ""

pathSeparator :: Char
pathSeparator = if isWindows then '\\' else '/'

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

fixfn :: FilePath -> FilePath -> FilePath
fixfn (trim -> dir) fn =
  case fn of
    "." -> fn
    ""  -> fn
    _   -> let msep = if anyOf _last (==pathSeparator) dir then "" else [pathSeparator]
           in dir <> msep <> fn

chklogdir :: FilePath -> IO ()
chklogdir dir = do
  b <- doesDirectoryExist dir
  unless b $ do
   let msg = [st|Logging.hs: directory [#{dir}] does not exist for logging to a file: see key File/Some/Dir|]
   T.putStrLn msg
   UE.throwIO $ GBException msg

-- skip sending an email if not logging to a file
-- | log using the LogOpts and pass in the reader value
logWith :: MLog m => e -> LogOpts -> RL e m a -> m a
logWith e opts mra = do
  let ma = runReaderT mra e
  let sendemail txt ltxt = \case
          Nothing -> return ()
          Just email -> do
            es <- liftIO loadEnvs
            liftIO $ emailMessage email [st|failure: #{txt}|] [TL.pack (show ltxt), es]
  case opts of
    LogOpts (Just logfn) mscreen memail -> do
      tm <- liftIO getZonedTime
      let dir = _fDir logfn
      liftIO $ chklogdir dir
      let fn = fixfn dir $ fileNameDate tm (if _fLongName logfn then fmtLong else fmtShort) (_fPrefix logfn) ".log"
      runMyFileLoggingT (_fLevel logfn,mscreen) fn $ UE.catchAny ma $ \x -> do
        $logError [st|outermost error: #{show x}|]
        sendemail (T.pack fn) x memail
        UE.throwIO x
    LogOpts Nothing o _memail ->
      ma & case o of
                  Nothing -> flip runLoggingT (\_ _ _ _  -> return ()) -- skip logging entirely
                  Just (Screen StdOut p) -> runStdoutLoggingT . filterLogger (\_ lvl -> toLogLevel p <= lvl)
                  Just (Screen StdErr p) -> runStderrLoggingT . filterLogger (\_ lvl -> toLogLevel p <= lvl)


-- | custom logger for writing to a file
runMyFileLoggingT :: U.MonadUnliftIO m => (LLog, Maybe Screen) -> FilePath -> LoggingT m b -> m b
runMyFileLoggingT p fn logt =
  UE.bracket (liftIO $ newFileLoggerSet defaultBufSize fn)
     (liftIO . rmLoggerSet)
     (runLoggingT logt . loggerSetOutput p)

loggerSetOutput :: (LLog, Maybe Screen)
              -> LoggerSet
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
loggerSetOutput (pfile, mstdout) logt l s level msg = do
  case mstdout of
    Nothing -> return ()
    Just (Screen x pscreen) ->
      when (toLogLevel pscreen <= level) $ B.hPutStrLn (case x of StdOut -> stdout; StdErr -> stderr) $ dispLevel level <> B.take 8000 (fromLogStr msg) -- to avoid overflow to stdout
  when (toLogLevel pfile <= level) $ do
    utcTime <- localUTC <$> getZonedTime
    let timestampStr = formatISO8601Millis utcTime
    pushLogStr logt $ defaultLogStr l s level (toLogStr (timestampStr <> " ") <> msg)

fileNameDate :: tm -> Format (String -> String) (tm -> String -> String) -> String -> String -> FilePath
fileNameDate tm fmt pref = formatToString (string % "_" % fmt % string) pref tm

fileNameDateQualified :: FormatTime a => a -> String -> String -> String
fileNameDateQualified tm pref = formatToString (string % "_" % fmtLong % string) pref tm

fmtShort, fmtLong, fmtLongCrazy :: FormatTime a => Format r (a -> r)
fmtShort = F.year <> F.month <> F.dayOfMonth
fmtLong = fmtShort <> "_" % F.hour24 <> F.minute <> F.second
fmtLongCrazy = fmtLong <> "." % F.pico

loadEnvs :: IO TL.Text
loadEnvs = TL.pack . unlines . map (\(x,y) -> x <> " = " <> y) <$> getEnvironment

-- | send an email using 'myemail' which pulls the settings from log.dhall
emailMessage :: Email -> Text -> [TL.Text] -> IO ()
emailMessage email subj bodys =
  sendMail (T.unpack (_eSmtpServer email))
          $ simpleMail (fromString (T.unpack (_eSmtpFrom email))) [fromString (T.unpack (_eSmtpTo email))] [] [] subj [plainPart $ TL.intercalate "\n\n" bodys]

-- | used for logging start and end time of a job
timeCommand :: ML e m => Text -> m a -> m a
timeCommand = timeCommand' (\_ _ -> return ())

timeCommand' :: ML e m => (Text -> (ZonedTime, ZonedTime) -> m ()) -> Text -> m a -> m a
timeCommand' callback txt cmd = do
  (c,a) <- do
    c <- liftIO getZonedTime
    let msg = [st|Start TimeCommand #{fmtZt c} #{txt}|]
    $logInfo msg
    a <- liftIO $ getTime Monotonic
    return (c,a)
  (ret :: Either E.SomeException a) <- UE.try $ cmd >>= \x -> return $! x
  do
    b <- liftIO $ getTime Monotonic
    d <- liftIO getZonedTime
    let xs = [st|#{difftimes a b} started=#{fmtZt c} ended=#{fmtZt d}|]
    case ret of
      Left e -> do
                  let msg = [st|FAILURE!!!! TimeCommand #{xs} #{txt} [#{show e}]|]
                  --liftIO $ T.putStrLn msg
                  $logError msg
                  UE.throwIO e
      Right x -> do
                   --liftIO $ T.putStrLn $ "OK TimeCommand " <> xs
                   $logInfo [st|OK TimeCommand #{xs} #{txt}|]
                   callback txt (c,d)
                   return x


difftimes :: TimeSpec -> TimeSpec -> Text
difftimes a b = T.pack $ showDuration (fromIntegral (sec (b - a)))

fmtZt :: ZonedTime -> String
fmtZt =  formatTime defaultTimeLocale "%T"

localUTC :: ZonedTime -> UTCTime
localUTC = roundSeconds . localTimeToUTC utc . zonedTimeToLocalTime

roundSeconds :: Timeable t => t -> t
roundSeconds = over seconds (fromIntegral @Integer . round)

zeroDate :: Timeable t => t -> t
zeroDate = over time (const (TimeOfDay 0 0 0))

dispLevel :: LogLevel -> B.ByteString
dispLevel LevelDebug = mempty
dispLevel LevelInfo = mempty
dispLevel LevelWarn = "WARN: "
dispLevel LevelError = "ERROR: "
dispLevel (LevelOther txt) = T.encodeUtf8 txt <> ": "

-- | MyLogger is a manual logger when you dont have access to MonadLogger
type MyLogger = LogLevel -> Text -> IO ()

getLogger :: MonadLoggerIO m => Loc -> m MyLogger
getLogger loc = do
  x <- askLoggerIO
--  return (\lvl msg -> x (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) "" lvl (toLogStr msg))
  return (\lvl msg -> x loc "" lvl (toLogStr msg))

lgDebug, lgInfo, lgWarn, lgError :: MonadIO m => MyLogger -> Text -> m ()
lgDebug lg = liftIO . lg LevelDebug
lgInfo lg = liftIO . lg LevelInfo
lgWarn lg = liftIO . lg LevelWarn
lgError lg = liftIO . lg LevelError

loadDhallTH :: forall a . (TH.Lift a, FromDhall a) => Text -> TH.Q TH.Exp
loadDhallTH txt = do
  c <- TH.runIO $ input (D.auto @a) txt
  TH.lift c

loadFromLogConfig :: Text -> IO LogOpts
loadFromLogConfig expr = do
  config <- input D.auto expr :: IO LogOpts
  T.putStrLn $[st|configuration [#{expr}] found:#{show config}|]
  return config

leWith :: MLog m => Text -> e -> (LogOpts -> LogOpts) -> RL e m a -> m a
leWith expr e g stuff = do
  logcfg <- liftIO $ loadFromLogConfig expr
  logWith e (g logcfg) stuff

fbeWith :: MLog m => e -> (LogOpts -> LogOpts) -> RL e m a -> m a
fbeWith = leWith "./log.dhall" -- batch stuff

fb :: MLog m => RL () m a -> m a
fb = fbeWith () id -- batch

fs :: MLog m => RL () m a -> m a
fs = fse ()

fse :: MLog m => e -> RL e m a -> m a
fse e = logWith e logs

-- basic ones that i use all the time
-- no need to read the dhall files for these
fd, fi, fw :: MLog m => RL () m a -> m a
fd = fde ()
fi = fie ()
fw = fwe ()

fde, fie, fwe :: MLog m => e -> RL e m a -> m a
fde e = logWith e logd
fie e = logWith e (logx Info)
fwe e = logWith e (logx Warn)

logs :: LogOpts
logs = LogOpts Nothing
               (Just (Screen StdOut Debug))
               Nothing

logd :: LogOpts
logd = LogOpts (Just (File "def" False Debug "."))
               (Just (Screen StdOut Debug))
               Nothing

logx :: LLog -> LogOpts
logx lvl = logd & lScreen . _Just . sLevel .~ lvl

chk :: LLog -> Maybe LLog -> LogOpts -> LogOpts
chk deflvl mlvl lo =
  lo & lScreen ?~ Screen StdOut (fromMaybe deflvl mlvl)

hoistEitherM :: MonadIO m => Text -> Either Text a -> m a
hoistEitherM = hoistEitherMFunc id

hoistEitherMFunc :: MonadIO m => (e -> Text) -> Text -> Either e a -> m a
hoistEitherMFunc f txt = either (\e -> UE.throwIO $ GBException (txt <> f e)) return


newline :: Text
newline = "\n"

hexChar :: Char -> String
hexChar c =
  let (a,b) = quotRem (ord c) 16
  in showHex a (showHex b "")

dumpDecHex :: B.ByteString -> IO ()
dumpDecHex bs = do
  B.putStrLn bs
  putStrLn $ "hex=" ++ unwords (map hexChar (B.unpack bs))
  putStrLn $ "dec=" ++ unwords (map (\c -> [c,'_']) (B.unpack bs))

