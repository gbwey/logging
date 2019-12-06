-- staticDhallExpression checks for syntax only! doesnt load into haskell type
{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Logging
import Test.Tasty
import Test.Tasty.HUnit
import Control.Lens
import Data.Maybe
import System.IO

lg :: LogOpts
lg = $(loadDhallTH @LogOpts "./test_log.dhall")

lgempty :: LogOpts
lgempty = $(loadDhallTH @LogOpts "./test_log_empty.dhall")

lgdef :: IO LogOpts
lgdef = loadFromLogConfig "(./corelog.dhall)::{=}"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  lg1 <- lgdef
  defaultMain $ testGroup "Logging"
    [
        testCase "dir in file" $ (@?=) (Just "testdir") (lg ^? lFile . _Just . fDir)
      , testCase "smtpfrom in email" $ (@?=) (Just "testsmtpfrom") (lg ^? lEmail . _Just . eSmtpFrom)
      , testCase "stdout in screen" $ (@?=) (Just StdOut) (lg ^? lScreen . _Just . sScreenType)
      , testCase "empty file" $ assertBool "a1" $ isNothing $ lgempty ^. lFile
      , testCase "empty email" $ assertBool "a2" $ isNothing $ lgempty ^. lEmail
      , testCase "empty screen" $ assertBool "a3" $ isNothing $ lgempty ^. lScreen
      , testCase "defPrefix" $ (@?=) (Just "def") (lg1 ^? lFile . _Just . fPrefix)
      , testCase "defDir" $ (@?=) (Just ".") (lg1 ^? lFile . _Just . fDir)
     ]

{-
>loadFromLogConfig "let x = ./corelog.dhall in x::{=}"
configuration [let x = ./corelog.dhall in x::{=}] found:LogOpts {_lFile = Just (File {_fPrefix = "def", _fLongName = True, _fLevel = Debug, _fDir = "."}), _lScreen = Just (Screen {_sScreenType = StdOut, _sLevel = Info}), _lEmail = Nothing}
LogOpts {_lFile = Just (File {_fPrefix = "def", _fLongName = True, _fLevel = Debug, _fDir = "."}), _lScreen = Just (Screen {_sScreenType = StdOut, _sLevel = Info}), _lEmail = Nothing}
it :: LogOpts

>loadFromLogConfig "(./corelog.dhall)::{=}"
configuration [(./corelog.dhall)::{=}] found:LogOpts {_lFile = Just (File {_fPrefix = "def", _fLongName = True, _fLevel = Debug, _fDir = "."}), _lScreen = Just (Screen {_sScreenType = StdOut, _sLevel = Info}), _lEmail = Nothing}
LogOpts {_lFile = Just (File {_fPrefix = "def", _fLongName = True, _fLevel = Debug, _fDir = "."}), _lScreen = Just (Screen {_sScreenType = StdOut, _sLevel = Info}), _lEmail = Nothing}
it :: LogOpts
-}