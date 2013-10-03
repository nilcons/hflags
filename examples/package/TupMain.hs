module TupMain (get) where
import qualified Tup

get :: (a, a) -> a
get = Tup.get
