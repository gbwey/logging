-- staticDhallExpression checks for syntax only! doesnt load into haskell type
{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Logging
import EasyTest
import Control.Lens
import Data.Maybe

lg :: LogOpts
lg = $(loadDhallTH @LogOpts "./test_log.dhall")

lgempty :: LogOpts
lgempty = $(loadDhallTH @LogOpts "./test_log_empty.dhall")

main :: IO ()
main =
  run $ tests [
        scope "dir in file" $ expectEq (Just "testdir") (lg ^? lFile . _Just . fDir)
      , scope "smtpfrom in email" $ expectEq (Just "testsmtpfrom") (lg ^? lEmail . _Just . eSmtpFrom)
      , scope "stdout in screen" $ expectEq (Just StdOut) (lg ^? lScreen . _Just . sScreenType)
      , scope "empty file" $ expect $ isNothing $ lgempty ^. lFile
      , scope "empty email" $ expect $ isNothing $ lgempty ^. lEmail
      , scope "empty screen" $ expect $ isNothing $ lgempty ^. lScreen
     ]
