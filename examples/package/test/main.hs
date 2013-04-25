{-# LANGUAGE TemplateHaskell #-}

import HFlags
import Tup

main = do
  $(initHFlags "foobar")
  print $ get (1,2)
