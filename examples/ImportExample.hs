#!/usr/bin/env runhaskell

{-# LANGUAGE TemplateHaskell #-}

import HFlags
import X.B

main = do _ <- $initHFlags "Importing example v0.1"
          print $ "foobar"
