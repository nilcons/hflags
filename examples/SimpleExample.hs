#!/usr/bin/env runhaskell

{-# LANGUAGE TemplateHaskell #-}

import HFlags

defineFlag "name" "Indiana Jones" "Who to greet."
defineFlag "repeat" (3 + 4 :: Int) "Number of times to repeat the message."

main = do s <- $initHFlags "Simple program v0.1"
          sequence_ $ replicate flags_repeat greet
          putStrLn $ "Your additional arguments were: " ++ show s
          putStrLn $ "Which is the same as: " ++ show HFlags.arguments
  where
    greet = putStrLn $ "Hello " ++ flags_name ++ ", very nice to meet you!"
