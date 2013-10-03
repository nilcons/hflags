{-# LANGUAGE TemplateHaskell #-}

import HFlags
import TupMain

main = do
  $initHFlags "foobar"
  print $ get (1,2)
