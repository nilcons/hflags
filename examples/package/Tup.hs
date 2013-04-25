{-# LANGUAGE TemplateHaskell #-}

module Tup (get) where

import HFlags

defineFlag "which" (1 :: Int) "which"

get :: (a, a) -> a
get x = case flags_which of
  1 -> fst x
  2 -> snd x
