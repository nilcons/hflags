{-# LANGUAGE TemplateHaskell #-}
-- remove when hflags 0.5 is released with ghc 7.8
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tup (get) where

import HFlags

defineFlag "which" (1 :: Int) "which"

-- This is unfortunately neccessary for make flags visible through
-- compilation boundaries (e.g. from packages).  The correct solution
-- is already being worked on inside GHC:
-- http://ghc.haskell.org/trac/ghc/ticket/7867

-- remove when hflags 0.5 is released with GHC 7.8
{-# RULES "make_this_orphan" id = id :: MakeThisOrphan -> MakeThisOrphan #-}

get :: (a, a) -> a
get x = case flags_which of
  1 -> fst x
  _ -> snd x
