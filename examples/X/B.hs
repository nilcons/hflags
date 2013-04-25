-- to test collision detection
-- {-# LANGUAGE TemplateHaskell #-}

module X.B (b) where

import qualified X.Y_Y.A as A

-- to test collision detection
-- import HFlags
-- defineFlag "verbose" True "Whether debug output should be printed."

b = A.flags_verbose
