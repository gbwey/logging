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

lg :: LogOpts
lg = $(loadDhallTH @LogOpts "./test_log.dhall")

lgempty :: LogOpts
lgempty = $(loadDhallTH @LogOpts "./test_log_empty.dhall")

main :: IO ()
main = defaultMain $ testGroup "Logging"
    [
        testCase "dir in file" $ (@?=) (Just "testdir") (lg ^? lFile . _Just . fDir)
      , testCase "smtpfrom in email" $ (@?=) (Just "testsmtpfrom") (lg ^? lEmail . _Just . eSmtpFrom)
      , testCase "stdout in screen" $ (@?=) (Just StdOut) (lg ^? lScreen . _Just . sScreenType)
      , testCase "empty file" $ assertBool "a1" $ isNothing $ lgempty ^. lFile
      , testCase "empty email" $ assertBool "a2" $ isNothing $ lgempty ^. lEmail
      , testCase "empty screen" $ assertBool "a3" $ isNothing $ lgempty ^. lScreen
     ]
