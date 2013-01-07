-- Copyright 2012 Google Inc. All Rights Reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--
-- Authors: Mihaly Barasz <klao@google.com>, Gergely Risko <errge@google.com>

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module: HFlags
-- License: Apache 2.0
--
-- The @HFlags@ library supports easy definition of command line flags,
-- reimplementing the ideas from Google's @gflags@
-- (<http://code.google.com/p/gflags>).
--
-- Command line flags can be declared in any file at the toplevel,
-- using 'defineFlag'.  At runtime, the actual values are assigned to
-- the toplevel @flags_name@ constants.  Those can be used purely
-- throughout the program.
--
-- At the beginning of the @main@ function, @$(initHFlags "program
-- description")@ has to be called to initialize the flags.  All flags
-- will be initialized that are transitively reachable via imports
-- from @main@.  This means, that any Haskell package can easily
-- define command line flags with @HFlags@.  This feature is
-- demonstrated by
-- <http://github.com/errge/hflags/blob/master/examples/ImportExample.hs>.
--
-- A simple example (more in the
-- <http://github.com/errge/hflags/tree/master/examples> directory):
--
-- > #!/usr/bin/env runhaskell
-- >
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import HFlags
-- >
-- > defineFlag "name" "Indiana Jones" "Who to greet."
-- > defineFlag "r:repeat" (3 + 4 :: Int) "Number of times to repeat the message."
-- >
-- > main = do s <- $(initHFlags "Simple program v0.1")
-- >           sequence_ $ replicate flags_repeat greet
-- >   where
-- >     greet = putStrLn $ "Hello " ++ flags_name ++ ", very nice to meet you!"
--
-- At @initHFlags@ time, the library also tries to gather flags out of
-- environment variables.  @HFLAGS_verbose=True@ is equivalent to
-- specify --verbose=True.  This environment feature only works with
-- long options and the user has to specify a value even for Bools.

module HFlags (
  -- * Definition of flags
  defineCustomFlag,
  defineEQFlag,
  FlagType(..),
  -- * Initialization of flags at runtime
  initHFlags) where

-- TODOs:
-- ?--no* for bools?
-- --help should show the current value if it's different than the default value, so user can test command line args

import Control.Exception
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.Text
import Language.Haskell.TH
import System.Console.GetOpt
import System.Environment
import System.IO
import System.IO.Unsafe
import System.Exit

import Prelude

-- | Data type for storing every property of a flag.
data FlagData = FlagData
            { fName :: String
            , fShort :: Maybe Char
            , fDefValue :: String
            , fArgHelp :: String
            , fDescription :: String
            , fModuleName :: String
            , fCheck :: IO () -- ^ function to evaluate in 'initFlags'
                              -- to force syntax check of the argument.
            }

-- | Every flag the program supports has to be defined through a new
-- phantom datatype and the Flag instance of that datatype.
--
-- But users of the library shouldn't worry about this class or the
-- implementation details behind these functions, just use the
-- @defineFlag@ Template Haskell function for defining new flags.
class Flag a where
  getFlagData :: a -> FlagData

-- | The most flexible way of defining a flag.  For an example see
-- <http://github.com/errge/hflags/blob/master/examples/ComplexExample.hs>.
-- For most things 'defineFlag' should be enough instead.
--
-- The parameters:
--
--   * name of the flag (@l:long@ syntax if you want to have the short option @l@ for this flag),
--
--   * expression quoted and type signed default value,
--
--   * help string for the argument,
--
--   * read function, expression quoted,
--
--   * show function, expression quoted,
--
--   * help string for the flag.
defineCustomFlag :: String -> ExpQ -> String -> ExpQ -> ExpQ -> String -> Q [Dec]
defineCustomFlag name' defQ argHelp readQ showQ description =
  do (name, short) <- case () of
       () | length name' == 0 -> fail "Flag's without names are not supported."
          | length name' == 1 -> return (name', Just $ head name')
          | length name' == 2 -> return (name', Nothing)
          | name' !! 1 == ':' -> return (drop 2 name', Just $ head name')
          | otherwise -> return (name', Nothing)
     defE <- defQ
     case defE of
       SigE _ _ -> return ()
       _ -> fail "Default value for defineCustomFlag has to be an explicitly typed expression, like (12 :: Int)"
     let SigE _ flagType = defE
     moduleName <- fmap loc_module location
     let accessorName = mkName $ "flags_" ++ name
     let dataName = mkName $ "HFlag_" ++ name
     dataDec <- return $ DataD [] dataName [] [] []
     instanceDec <- instanceD
                    (return [])
                    (appT (conT ''Flag) (conT dataName))
                      [funD 'getFlagData [clause [wildP]
                                          (normalB
                                           [| FlagData
                                              name
                                              short
                                              $(appE showQ defQ)
                                              argHelp
                                              description
                                              moduleName
                                              (evaluate $(varE accessorName) >> return ())
                                           |]) []]]
     flagPragmaDec <- return $ PragmaD $ InlineP accessorName NoInline FunLike AllPhases
     let flagSig = SigD accessorName flagType
     flagDec <- funD accessorName [clause [] (normalB [| case True of
                                                           True -> $(appE readQ [| lookupFlag name moduleName |])
                                                           False -> $(defQ) |]) []]
     return [dataDec, instanceDec, flagPragmaDec, flagSig, flagDec]

-- | This just forwards to 'defineCustomFlag' with @[| read |]@ and
-- @[| show |]@.  Useful for flags where the type is not an instance
-- of 'FlagType'.  For examples, see
-- <http://github.com/errge/hflags/blob/master/examples/ComplexExample.hs>.
--
-- The parameters:
--
--   * name of the flag (@l:long@ syntax if you want to have the short option @l@ for this flag),
--
--   * expression quoted and type signed default value,
--
--   * help string for the argument,
--
--   * help string for the flag.
defineEQFlag :: String -> ExpQ -> String -> String -> Q [Dec]
defineEQFlag name defQ argHelp description =
 defineCustomFlag name defQ argHelp [| read |] [| show |] description

-- | Class of types for which the easy 'defineFlag' syntax is supported.
class FlagType t where
  -- | The @defineFlag@ function defines a new flag.
  --
  -- The parameters:
  --
  --   * name of the flag (@l:long@ syntax if you want to have the short option @l@ for this flag),,
  --
  --   * default value,
  --
  --   * help string for the flag.
  defineFlag :: String -> t -> String -> Q [Dec]

boolShow :: Bool -> String
boolShow True = "true"
boolShow False = "false"

boolRead :: String -> Bool
boolRead = boolRead' . map toLower
  where
    boolRead' ('y':_) = True
    boolRead' ('t':_) = True
    boolRead' ('1':_) = True
    boolRead' ('n':_) = False
    boolRead' ('f':_) = False
    boolRead' ('0':_) = False
    boolRead' s = error $ "Unable to parse string as boolean: " ++ s

instance FlagType Bool where
  defineFlag n v = defineCustomFlag n [| v :: Bool |] "BOOL" [| boolRead |] [| boolShow |]

instance FlagType Int where
  defineFlag n v = defineEQFlag n [| v :: Int |] "INT"

instance FlagType Integer where
  defineFlag n v = defineEQFlag n [| v :: Integer |] "INTEGER"

instance FlagType String where
  defineFlag n v = defineCustomFlag n [| v :: String |] "STRING" [| id |] [| id |]

instance FlagType Double where
  defineFlag n v = defineEQFlag n (sigE (litE (RationalL (toRational v))) [t| Double |] ) "DOUBLE"

instance FlagType Data.Text.Text where
  defineFlag n v =
    let s = Data.Text.unpack v
    in defineCustomFlag n [| Data.Text.pack s :: Data.Text.Text |] "TEXT" [| Data.Text.pack |] [| Data.Text.unpack |]

-- | A global "IORef" for the communication between @initHFlags@ and
-- @flag_*@.  This is a map between flag name and current value.
{-# NOINLINE globalHFlags #-}
globalHFlags :: IORef (Maybe (Map String String))
globalHFlags = unsafePerformIO $ newIORef Nothing

lookupFlag :: String -> String -> String
lookupFlag fName fModuleName = unsafePerformIO $ do
  flags <- readIORef globalHFlags
  case flags of
    Just flagmap -> return $ flagmap ! fName
    Nothing -> error $ "Flag " ++ fName ++ " (from module: " ++ fModuleName ++ ") used before calling initHFlags."

-- | Initializes @globalHFlags@ and returns the non-option arguments.
initFlags :: String -> [FlagData] -> [String] -> IO [String]
initFlags progDescription flags args = do
  doHelp
  let (opts, nonopts, errs) | doUndefok = (\(a,b,_,c) -> (a,b,c)) $ getOpt' Permute getOptFlags args
                            | otherwise = getOpt Permute getOptFlags args
  when (not $ null errs) $ do
    mapM_ (hPutStrLn stderr) errs
    exitFailure
  let defaults = map (\FlagData { fName, fDefValue } -> (fName, fDefValue)) flags
  env <- getEnvironment
  let envDefaults = map (mapFst (fromJust . stripPrefix "HFLAGS_")) $ filter ((isPrefixOf "HFLAGS_") . fst) env
  writeIORef globalHFlags $ Just $ Map.fromList $ defaults ++ envDefaults ++ opts
  mapM_ forceFlag flags
  return nonopts
    where
      mapFst f (a, b) = (f a, b)
      helpOption = Option "h" ["help", "usage", "version"] (NoArg ("", "")) "Display help and version information."
      doHelp = case getOpt Permute [helpOption] args of
        ([], _, _) -> return ()
        _ -> do putStrLn $ usageInfo (progDescription ++ "\n") (helpOption:undefokOption:getOptFlags)
                exitFailure

      undefokOption = Option "" ["undefok"] (NoArg ("", "")) "Whether to fail on unrecognized command line options."
      doUndefok = case getOpt Permute [undefokOption] args of
        ([], _, _) -> False
        _ -> True

      flagToGetOptArgDescr FlagData { fName, fArgHelp }
        | fArgHelp == "BOOL" = OptArg (\a -> (fName, maybe "True" id a)) fArgHelp
        | otherwise = ReqArg (\a -> (fName, a)) fArgHelp

      -- compute GetOpt compatible [Option] structure from flags ([FlagData])
      getOptFlags = flip map flags $ \flagData@(FlagData { fName, fShort, fDefValue, fDescription, fModuleName }) ->
        Option (maybeToList fShort) [fName] (flagToGetOptArgDescr flagData)
               (fDescription ++ " (default: " ++ fDefValue ++ ", from module: " ++ fModuleName ++ ")")

      forceFlag FlagData { fName, fModuleName, fCheck } =
        fCheck `catch`
        (\e -> error $
               "Error while parsing argument for flag: " ++ fName ++
               ", value: " ++ lookupFlag fName fModuleName ++
               ", error: " ++ show (e :: ErrorCall))

-- | Has to be called from the main before doing anything else:
--
-- @
-- main = do args <- $(initHFlags "Simple program v0.1")
--           ...
-- @
--
-- Internally, it uses Template Haskell trickery to gather all the
-- instances of the Flag class and then generates a call to
-- @initFlags@ with the appropriate data gathered together from those
-- instances to a list.
--
-- Type after splicing is @IO [String]@.
initHFlags :: String -> ExpQ -- IO [String]
initHFlags progDescription = do
  ClassI _ instances <- reify ''Flag
  case dupes instances of
    [] -> return ()
    (dupe:_) -> fail ("Multiple definition of flag " ++ (snd $ head dupe) ++
                       ", modules: " ++ (show $ map fst dupe))
  [| getArgs >>= initFlags progDescription $(listE $ map instanceToOptTuple instances ) |]
    where
      instanceToOptTuple (InstanceD _ (AppT _ inst) _) = [| getFlagData (undefined :: $(return inst)) |]
      instanceToOptTuple _ = error "Shouldn't happen"
      instanceToModuleNamePair (InstanceD _ (AppT _ (ConT inst)) _) =
        let (flagrev, modrev) = span (/= '.') $ reverse $ show inst
            modName = reverse $ drop 1 modrev
            flag = drop 1 $ dropWhile (/= '_') $ reverse $ flagrev
        in (modName, flag)
      instanceToModuleNamePair _ = error "Shouldn't happen"
      dupes instances = filter ((>1) . length) $
                        groupBy ((==) `on` snd) $
                        sortBy (compare `on` snd) $
                        map instanceToModuleNamePair instances
