{-# OPTIONS -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
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
import qualified Data.UnixTime as UT
import Data.Time
import System.Clock
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Control.Monad.Reader
import qualified Control.Exception as E
import Control.Monad.Logger
import System.Log.FastLogger
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
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Info as SI
import Data.Char
import Data.List (foldl', dropWhileEnd, isInfixOf)
import System.Directory
import Numeric (showHex)
import qualified Data.ByteString.Builder as BB
import qualified Chronos as C
#ifdef mingw32_HOST_OS
import qualified System.Win32.Time as W32
import System.Win32.Time (SYSTEMTIME(..))
import Text.Printf
#endif
import qualified DateTA
import Data.Time.Clock.POSIX

newtype GBException = GBException { gbMessage :: Text } deriving (Show,Eq)
instance E.Exception GBException

-- | 'ML' has the minimum set of constraints for running sql commands in sqlhandler-odbc
-- use MonadReader e to customise the environment
type ML e m = (U.MonadUnliftIO m, MonadLogger m, MonadLoggerIO m, MonadReader e m)
-- | 'RL' defines the outer two layers of 'ML'
type RL e m a = ReaderT e (LoggingT m) a

data LLog = Debug | Info | Warn | Error deriving (TH.Lift, G.Generic, Show, Eq, Enum, Bounded, Ord)
makePrisms ''LLog
instance FromDhall LLog
instance ToDhall LLog

-- | log to the screen
data ScreenType = StdOut | StdErr deriving (TH.Lift, Show, Eq, G.Generic, Enum, Bounded, Ord)
makePrisms ''ScreenType
instance FromDhall ScreenType
instance ToDhall ScreenType

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
       , _lDebug :: !Bool
       } deriving (TH.Lift, Generic, Show)

makeLenses ''LogOpts
makeLenses ''Email

instance ToText LogOpts where
  toText = TLB.fromText . T.pack . show

instance FromDhall Email where
  autoWith _i = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop 2 }

instance ToDhall Email where
  injectWith _o = genericToDhallWith defaultInterpretOptions { fieldModifier = T.drop 2 }

instance FromDhall File where
  autoWith _i = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop 2 }

instance ToDhall File where
  injectWith _o = genericToDhallWith defaultInterpretOptions { fieldModifier = T.drop 2 }

-- dont do it with screen cos is a tuple and you will lose _1 and _2
instance FromDhall LogOpts where
  autoWith _i = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop 2 }

logopts :: Decoder LogOpts
logopts =  genericAutoWith defaultInterpretOptions { fieldModifier = T.drop 2 }

instance ToDhall LogOpts where
  injectWith _o = genericToDhallWith defaultInterpretOptions { fieldModifier = T.drop 2 }


instance FromDhall Screen where
  autoWith _i = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop 2 }

instance ToDhall Screen where
  injectWith _o = genericToDhallWith defaultInterpretOptions { fieldModifier = T.drop 2 }

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

logWith :: U.MonadUnliftIO m => e -> LogOpts -> RL e m a -> m a
logWith = logWith' Nothing

logWith' :: U.MonadUnliftIO m => Maybe UT.UnixTime -> e -> LogOpts -> RL e m a -> m a
logWith' = logWithImpl $ fmap toLogStr
#ifdef mingw32_HOST_OS
  ioDateWin32Katip
#else
  ioDateU
#endif

-- | log using the LogOpts and pass in the reader value
--   only sends emails when there is an exception ie logError does not send an email
logWithImpl :: U.MonadUnliftIO m => IO LogStr -> Maybe UT.UnixTime -> e -> LogOpts -> RL e m a -> m a
logWithImpl mdt mut e opts mra = do
  when (opts ^. lDebug) $ liftIO $ T.putStrLn [st|starting in debug mode: #{show opts}|]
  let ma = runReaderT mra e
  case opts ^. lFile of
    Just logfn -> do
      let dir = _fDir logfn
      liftIO $ chklogdir dir
--      fmt1 <- liftIO $ T.unpack . (if _fLongName logfn then formatAsFileNameLong else formatAsFileNameShort) <$> W32.getLocalTime
      fmt1 <- liftIO $ fmap B.unpack (UT.formatUnixTime (if _fLongName logfn then fmtLong1 else fmtShort1) =<< Data.Maybe.maybe UT.getUnixTime pure mut)
      let fn = fixfn dir (_fPrefix logfn <> "_" <> fmt1 <> ".log")
      runMyFileLoggingT mdt (_fLevel logfn, opts ^. lScreen) fn $ emailOnError (T.pack fn) opts ma
    Nothing ->
      emailOnError "no file" opts ma & case opts ^. lScreen of
                  Nothing -> flip runLoggingT (\_ _ _ _  -> return ()) -- skip logging entirely
                  Just (Screen ss p) ->
                    toLoggingT ss . filterLogger (\_ lvl -> toLogLevel p <= lvl)

-- | custom logger for writing to a file
runMyFileLoggingT :: U.MonadUnliftIO m => IO LogStr -> (LLog, Maybe Screen) -> FilePath -> LoggingT m b -> m b
runMyFileLoggingT mdt p fn logt =
  UE.bracket (liftIO $ newFileLoggerSet defaultBufSize fn)
     (liftIO . rmLoggerSet)
     (runLoggingT logt . loggerSetOutput mdt p)

loggerSetOutput :: IO LogStr
              -> (LLog, Maybe Screen)
              -> LoggerSet
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
loggerSetOutput mdt (pfile, mstdout) logt l s level msg = do
  case mstdout of
    Nothing -> return ()
    Just (Screen x pscreen) ->
      when (toLogLevel pscreen <= level) $ B.hPutStrLn (toSout x) (dispLevel level <> B.take 8000 (fromLogStr msg)) -- to avoid overflow to stdout
  when (toLogLevel pfile <= level) $ do
    timestampStr <- mdt
    pushLogStr logt $ defaultLogStr l s level (timestampStr <> " " <> msg)

toLoggingT :: MonadIO m => ScreenType -> (LoggingT m a -> m a)
toLoggingT = \case
  StdOut -> runStdoutLoggingT
  StdErr -> runStderrLoggingT

emailOnError :: (MonadLogger m, U.MonadUnliftIO m) => Text -> LogOpts -> m a -> m a
emailOnError txt opts ma =
  UE.catchAny ma $ \x -> do
    $logError [st|outermost error: #{show x}|]
    liftIO $ sendemail opts txt x
    UE.throwIO x

sendemail :: (ToText a1, Show a2) => LogOpts -> a1 -> a2 -> IO ()
sendemail opts txt ltxt =
  case opts ^. lEmail of
     Nothing -> when (opts ^. lDebug) $ T.putStrLn [st|debug: no email specified: txt=[#{txt}] ltxt=[#{show ltxt}]|]
     Just email -> do
       when (opts ^. lDebug) $ T.putStrLn [st|debug: sending email: txt=[#{txt}] ltxt=[#{show ltxt}]|]
       es <- loadEnvs
       emailMessage email [st|failure: #{txt}|] [TL.pack (show ltxt), es]

ioDateC :: TimeZone -> IO BB.Builder
ioDateC tz = do
  tm <- C.now
  return $ C.builderUtf8_YmdHMSz C.OffsetFormatColonOff C.SubsecondPrecisionAuto C.w3c (C.timeToOffsetDatetime (C.Offset (timeZoneMinutes tz)) tm)
{-# INLINEABLE ioDateC #-}

ioDateU :: IO B.ByteString
ioDateU = do
  tm <- UT.getUnixTime
  UT.formatUnixTime "%FT%T%z" tm
{-# INLINEABLE ioDateU #-}

ioDateD :: IO String
ioDateD = do
  tm <- getZonedTime
  return $ formatTime defaultTimeLocale "%FT%T.%3q%z" tm
{-# INLINEABLE ioDateD #-}

#ifdef mingw32_HOST_OS
ioDateWin32Katip :: IO Text
ioDateWin32Katip = do
  tm <- W32.getLocalTime
  return $ DateTA.formatAsTimeStampST tm
{-# INLINEABLE ioDateWin32Katip #-}

ioDateWin32Printf :: IO String
ioDateWin32Printf = do
  SYSTEMTIME{..} <- W32.getLocalTime
  return $ printf "%d-%02d-%02dT%02d:%02d:%02d.%03d" wYear wMonth wDay wHour wMinute wSecond wMilliseconds
{-# INLINEABLE ioDateWin32Printf #-}

#endif

ioDateP :: TimeZone -> IO Text
ioDateP tz = do
  tm <- getPOSIXTime
  let y = utcToZonedTime tz (posixSecondsToUTCTime tm)
  return $ DateTA.formatAsTimeStampP y
{-# INLINEABLE ioDateP #-}

toSout :: ScreenType -> Handle
toSout =
  \case
    StdOut -> stdout
    StdErr -> stderr

fileNameDate :: tm -> Format (String -> String) (tm -> String -> String) -> String -> String -> FilePath
fileNameDate tm fmt pref = formatToString (string % "_" % fmt % string) pref tm
{-
fileNameDateQualified :: FormatTime a => a -> String -> String -> String
fileNameDateQualified tm pref = formatToString (string % "_" % fmtLong % string) pref tm
-}
fmtShort1, fmtLong1 :: B.ByteString
fmtShort1 = "%Y%m%d"
fmtLong1 = fmtShort1 <> "_%H%M%S"

fmtShort, fmtLong, fmtLongCrazy :: FormatTime a => Format r (a -> r)
fmtShort = F.year <> F.month <> F.dayOfMonth
fmtLong = fmtShort <> "_" % F.hour24 <> F.minute <> F.second
fmtLongCrazy = fmtLong <> "." % F.pico

loadEnvs :: IO TL.Text
loadEnvs =
    TL.pack
  . unlines
  . map (\(x,y) -> x <> " = " <> y)
  . filter (\(x,_) -> let z = map toLower x
                      in not (isInfixOf "pwd" z || isInfixOf "pass" z)
           )
  <$> getEnvironment

-- | send an email using 'myemail' which pulls the settings from log.dhall
emailMessage :: Email -> Text -> [TL.Text] -> IO ()
emailMessage email subj bodys =
  sendMail (T.unpack (_eSmtpServer email))
          $ simpleMail (fromString (T.unpack (_eSmtpFrom email))) [fromString (T.unpack (_eSmtpTo email))] [] [] subj [plainPart $ TL.intercalate "\n\n" bodys]

-- | used for logging start and end time of a job
timeCommand :: ML e m => Text -> m a -> m a
timeCommand = timeCommand' const

timeCommand' :: ML e m => (a -> (UT.UnixTime, UT.UnixTime) -> b) -> Text -> m a -> m b
timeCommand' callback txt cmd = do
  (c,c1,a) <- do
    c <- liftIO UT.getUnixTime
    c1 <- liftIO $ T.decodeUtf8 <$> fmtZt1 c
    let msg = [st|Start TimeCommand #{c1} #{txt}|]
    $logInfo msg
    a <- liftIO $ getTime Monotonic
    return (c,c1,a)
  (ret :: Either E.SomeException a) <- UE.try $ cmd >>= \x -> return $! x
  do
    b <- liftIO $ getTime Monotonic
    d <- liftIO UT.getUnixTime
    d1 <- liftIO $ T.decodeUtf8 <$> fmtZt1 d
    let xs = [st|#{difftimes a b} started=#{c1} ended=#{d1}|]
    case ret of
      Left e -> do
                  let msg = [st|FAILURE!!!! TimeCommand #{xs} #{txt} [#{show e}]|]
                  $logError msg
                  UE.throwIO e
      Right x -> do
                   $logInfo [st|OK TimeCommand #{xs} #{txt}|]
                   return $ callback x (c,d)

difftimes :: TimeSpec -> TimeSpec -> Text
difftimes a b = T.pack $ showDuration (fromIntegral (sec (b - a)))

fmtZt :: ZonedTime -> String
fmtZt =  formatTime defaultTimeLocale "%T"

fmtZt1 :: UT.UnixTime -> IO B.ByteString
fmtZt1 =  UT.formatUnixTime "%T"

roundSeconds :: Timeable t => t -> t
roundSeconds = over seconds (fromIntegral @Integer . floor)

localUTC :: ZonedTime -> UTCTime
localUTC = roundSeconds . localTimeToUTC utc . zonedTimeToLocalTime

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
  T.putStrLn [st|configuration [#{expr}] found:#{show config}|]
  return config

leWith :: U.MonadUnliftIO m => Text -> e -> (LogOpts -> LogOpts) -> RL e m a -> m a
leWith expr e g stuff = do
  logcfg <- liftIO $ loadFromLogConfig expr
  logWith e (g logcfg) stuff

fbeWith :: U.MonadUnliftIO m => e -> (LogOpts -> LogOpts) -> RL e m a -> m a
fbeWith = leWith "./log.dhall" -- batch stuff

fb :: U.MonadUnliftIO m => RL () m a -> m a
fb = fbeWith () id -- batch

fs :: U.MonadUnliftIO m => RL () m a -> m a
fs = fse ()

fse :: U.MonadUnliftIO m => e -> RL e m a -> m a
fse e = logWith e logs

fnone :: U.MonadUnliftIO m => RL () m a -> m a
fnone = logWith () lognone

-- basic ones that i use all the time
-- no need to read the dhall files for these
fd, fi, fw :: U.MonadUnliftIO m => RL () m a -> m a
fd = fde ()
fi = fie ()
fw = fwe ()

fde, fie, fwe :: U.MonadUnliftIO m => e -> RL e m a -> m a
fde e = logWith e logd
fie e = logWith e (logx Info)
fwe e = logWith e (logx Warn)

lognone :: LogOpts
lognone = LogOpts Nothing
               Nothing
               Nothing
               False

logs :: LogOpts
logs = LogOpts Nothing
               (Just (Screen StdOut Debug))
               Nothing
               False

logd :: LogOpts
logd = LogOpts (Just (File "def" False Debug "."))
               (Just (Screen StdOut Debug))
               Nothing
               False

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

