-- Copyright 2012 Google Inc. All Rights Reserved.
-- Copyright 2013 and onwards, Gergely Risko
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
-- Authors: Mihaly Barasz <klao@google.com>, Gergely Risko <gergely@risko.hu>
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}

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
-- At the beginning of the @main@ function, @$'initHFlags' \"program
-- description\"@ has to be called to initialize the flags.  All flags
-- will be initialized that are transitively reachable via imports
-- from @main@.  This means, that any Haskell package can easily
-- define command line flags with @HFlags@.  This feature is
-- demonstrated by
-- <http://github.com/errge/hflags/blob/master/examples/ImportExample.hs>
-- and <http://github.com/errge/hflags/tree/master/examples/package>.
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
-- > main = do s <- $initHFlags "Simple program v0.1"
-- >           sequence_ $ replicate flags_repeat greet
-- >           putStrLn $ "Your additional arguments were: " ++ show s
-- >           putStrLn $ "Which is the same as: " ++ show HFlags.arguments
-- >   where
-- >     greet = putStrLn $ "Hello " ++ flags_name ++ ", very nice to meet you!"
--
-- At 'initHFlags' time, the library also tries to gather flags out of
-- environment variables.  @HFLAGS_verbose=True@ is equivalent to
-- specifying @--verbose=True@ on the command line.  This environment
-- feature only works with long options and the user has to specify a
-- value even for @Bool@s.
--
-- /Since version 0.2, you mustn't put the initHFlags in a parentheses with the program description.  Just/ @$initHFlags@, /it's cleaner./


module HFlags (
  -- * Definition of flags
  defineCustomFlag,
  defineEQFlag,
  FlagType(..),
  -- * Initialization of flags at runtime
  initHFlags,
  initHFlagsDependentDefaults,
  -- * For easy access to arguments, after initHFlags has been called
  arguments,
  undefinedOptions,
  -- * For debugging, shouldn't be used in production code
  Flag(..),
  MakeThisOrphan(..),
  globalHFlags,
  globalArguments,
  globalUndefinedOptions
  ) where

-- TODOs:
-- duplicate checking for short options: it's tricky, we need to encode info in the HFlag_... data name
-- ?--no* for bools?
-- --help should show the current value if it's different than the default value, so user can test command line args

import Control.Exception
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.IORef
import Data.Maybe
-- This is intentionally lazy, so dependent defaults are only
-- evaluated if they are needed.  (The user hasn't specified the
-- option in the command line or via environment variables.)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import qualified Data.Text
import Language.Haskell.TH
import System.Console.GetOpt
import System.Environment
import System.IO
import System.IO.Unsafe
import System.Exit

import Prelude

-- | This is a temporary hack to force visibility of flags inside
-- libraries, by making the module that defines the flag orphan.  For
-- usage example, check out
-- <http://github.com/errge/hflags/blob/master/examples/package/Tup.hs>.
-- A proper fix is already proposed for GHC 7.8, see
-- <http://ghc.haskell.org/trac/ghc/ticket/7867>.
data MakeThisOrphan = MakeThisOrphan

-- | Data type for storing every property of a flag.
data FlagData = FlagData
            { fName :: String
            , fShort :: Maybe Char
            , fDefValue :: String
            , fArgType :: String
            , fDescription :: String
            , fModuleName :: String
            , fCheck :: IO () -- ^ function to evaluate in @initFlags@
                              -- to force syntax check of the argument.
            }

instance Show FlagData where
  show fd = show (fName fd, fShort fd, fDefValue fd, fArgType fd, fDescription fd, fModuleName fd)

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
--   * help string identifying the type of the argument (e.g. INTLIST),
--
--   * read function, expression quoted,
--
--   * show function, expression quoted,
--
--   * help string for the flag.
defineCustomFlag :: String -> ExpQ -> String -> ExpQ -> ExpQ -> String -> Q [Dec]
defineCustomFlag name' defQ argHelp readQ showQ description =
  do (name, short) <- if | length name' == 0 -> fail "Flag's without names are not supported."
                         | length name' == 1 -> return (name', Just $ head name')
                         | length name' == 2 -> return (name', Nothing)
                         | name' !! 1 == ':' -> return (drop 2 name', Just $ head name')
                         | otherwise -> return (name', Nothing)
     defE <- defQ
     flagType <- case defE of
       SigE _ flagType -> return $ return flagType
       _ -> fail "Default value for defineCustomFlag has to be an explicitly typed expression, like (12 :: Int)"
     moduleName <- fmap loc_module location
     let accessorName = mkName $ "flags_" ++ name
     -- attention: formatting of the dataName matters here, initHFlags
     -- parses the name, so the generation here and the parsing in
     -- initHFlags has to be consistent.
     let dataName = mkName $ "HFlag_" ++ name
     let dataConstrName = mkName $ "HFlagC_" ++ name
     -- Note: support for splicing inside [d| |] would make all this a lot nicer
#if MIN_VERSION_template_haskell(2,11,0)
     dataDec <- dataD (cxt []) dataName [] Nothing [normalC dataConstrName []] (cxt [])
#else
     dataDec <- dataD (cxt []) dataName []         [normalC dataConstrName []]      []
#endif
     instanceDec <- instanceD
                    (cxt [])
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
                                              -- seq'ng the constructor name, so it's not unused in the generated code
                                              ($(conE dataConstrName) `seq` evaluate $(varE accessorName) >> return ())
                                           |]) []]]
     flagPragmaDec <- pragInlD accessorName NoInline FunLike AllPhases
     flagSig <- sigD accessorName flagType
     flagDec <- funD accessorName [clause [] (normalB $ appE readQ [| lookupFlag name moduleName |]) []]
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
--   * help string identifying the type of the argument (e.g. INTLIST),
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

charShow :: Char -> String
charShow x = x:[]

charRead :: String -> Char
charRead [x] = x
charRead s = error $ "Unable to parse string as char: " ++ s

instance FlagType Char where
  defineFlag n v = defineCustomFlag n [| v :: Char |] "CHAR" [| charRead |] [| charShow |]

instance FlagType Int where
  defineFlag n v = defineEQFlag n [| v :: Int |] "INT"

instance FlagType Integer where
  defineFlag n v = defineEQFlag n [| v :: Integer |] "INTEGER"

instance FlagType String where
  defineFlag n v = defineCustomFlag n [| v :: String |] "STRING" [| id |] [| id |]

instance FlagType Double where
  defineFlag n v = defineEQFlag n (sigE (litE (RationalL (toRational v))) [t| Double |] ) "DOUBLE"

-- TODO(errge): hflags-instances cabal package, so the base hflags
-- doesn't depend on text, which is not in GHC.
instance FlagType Data.Text.Text where
  defineFlag n v =
    -- defer lifting of Data.Text.Text to String lifting
    let s = Data.Text.unpack v
    in defineCustomFlag n [| Data.Text.pack s :: Data.Text.Text |] "TEXT" [| Data.Text.pack |] [| Data.Text.unpack |]

-- | A global 'IORef' for the communication between 'initHFlags' and
-- @flags_*@.  This is a map between flag name and current value.
{-# NOINLINE globalHFlags #-}
globalHFlags :: IORef (Maybe (Map String String))
globalHFlags = unsafePerformIO $ newIORef Nothing

-- | A global 'IORef' for the easy access to the arguments.
{-# NOINLINE globalArguments #-}
globalArguments :: IORef (Maybe [String])
globalArguments = unsafePerformIO $ newIORef Nothing

-- | Contains the non-parsed, non-option parts of the command line,
-- the arguments.  Can only be used after 'initHFlags' has been called.
{-# NOINLINE arguments #-}
arguments :: [String]
arguments = unsafePerformIO $ do
  margs <- readIORef globalArguments
  case margs of
    Just args -> return $ args
    Nothing -> error $ "HFlags.arguments used before calling initHFlags."

-- | A global 'IORef' for the easy access to the undefined options, if
-- @--undefok@ is used.  Useful, if you have to pass these options to
-- another library, e.g. @criterion@ or @GTK@.
{-# NOINLINE globalUndefinedOptions #-}
globalUndefinedOptions :: IORef (Maybe [String])
globalUndefinedOptions = unsafePerformIO $ newIORef Nothing

-- | Contains the non-parsed, option parts of the command line, if
-- @--undefok@ is in use.  This can be useful, when you have to pass
-- these options to other libraries, e.g. @criterion@ or @GTK@.  Can
-- only be used after 'initHFlags' has been called.
{-# NOINLINE undefinedOptions #-}
undefinedOptions :: [String]
undefinedOptions = unsafePerformIO $ do
  margs <- readIORef globalUndefinedOptions
  case margs of
    Just args -> return $ args
    Nothing -> error $ "HFlags.globalUndefOpts used before calling initHFlags."

lookupFlag :: String -> String -> String
lookupFlag fName fModuleName = unsafePerformIO $ do
  flags <- readIORef globalHFlags
  case flags of
    Just flagmap -> case Map.lookup fName flagmap of
      Just v -> return v
      Nothing -> error $ "Flag " ++ fName ++ " not found at runtime, look into HFlags-source/examples/package/Tup.hs.  Sorry."
    Nothing -> error $ "Flag " ++ fName ++ " (from module: " ++ fModuleName ++ ") used before calling initHFlags."

-- | Lisp like alist, key -> value pairs.
type AList = [(String, String)]

-- | A function that gets three alists and returns a new one.
type DependentDefaults = AList -> AList -> AList -> AList

-- | Initializes 'globalHFlags' and returns the non-option arguments.
initFlags :: DependentDefaults -> String -> [FlagData] -> [String] -> IO [String]
initFlags dependentDefaults progDescription flags args = do
  doHelp
  let (opts, nonopts, undefopts, errs)
        | doUndefok = getOpt' Permute getOptFlags args
        | otherwise = (\(a,b,c) -> (a,b,[],c)) $ getOpt Permute getOptFlags args
  when (not $ null errs) $ do
    mapM_ (hPutStrLn stderr) errs
    exitFailure
  let defaults = map (\FlagData { fName, fDefValue } -> (fName, fDefValue)) flags
  env <- getEnvironment
  let envDefaults = map (mapFst (fromJust . stripPrefix "HFLAGS_")) $ filter ((isPrefixOf "HFLAGS_") . fst) env
  let depdef = dependentDefaults defaults envDefaults opts
  writeIORef globalHFlags $ Just $ Map.fromList $ defaults ++ depdef ++ envDefaults ++ opts
  writeIORef globalArguments $ Just nonopts
  writeIORef globalUndefinedOptions $ Just undefopts
  mapM_ forceFlag flags
  return nonopts
    where
      mapFst f (a, b) = (f a, b)
      helpOption = Option "h" ["help", "usage", "version"] (NoArg ("", "")) "Display help and version information."
      doHelp = case getOpt Permute [helpOption] args of
        ([], _, _) -> return ()
        _ -> do putStrLn $ usageInfo (progDescription ++ "\n") (helpOption:getOptFlags)
                exitFailure

      undefokOption = Option "" ["undefok"] (NoArg ("", "")) "Whether to fail on unrecognized command line options."
      doUndefok = case getOpt Permute [undefokOption] args of
        ([], _, _) -> False
        _ -> True

      flagToGetOptArgDescr FlagData { fName, fArgType }
        | fArgType == "BOOL" = OptArg (\a -> (fName, maybe "True" id a)) fArgType
        | otherwise = ReqArg (\a -> (fName, a)) fArgType

      -- compute GetOpt compatible [Option] structure from flags ([FlagData])
      getOptFlags = undefokOption:
        (flip map flags $ \flagData@(FlagData { fName, fShort, fDefValue, fDescription, fModuleName }) ->
         Option (maybeToList fShort) [fName]
                (flagToGetOptArgDescr flagData)
                (fDescription ++ " (default: " ++ fDefValue ++ ", from module: " ++ fModuleName ++ ")"))

      forceFlag FlagData { fName, fModuleName, fCheck } =
        fCheck `catch`
        (\e -> error $
               "Error while parsing argument for flag: " ++ fName ++
               ", value: " ++ lookupFlag fName fModuleName ++
               ", error: " ++ show (e :: ErrorCall))

-- | Gathers all the flag data from every module that is in (transitive) scope.
-- Type after splicing: @[FlagData]@.
getFlagsData :: ExpQ -- [FlagData]
getFlagsData = do
  ClassI _ instances <- reify ''Flag
  case dupes instances of
    [] -> return ()
    (dupe:_) -> fail ("Multiple definition of flag " ++ (snd $ head dupe) ++
                       ", modules: " ++ (show $ map fst dupe))
  listE $ map instanceToFlagData instances
    where
      instanceToFlagData (getInstanceType -> (AppT _ inst)) = [| getFlagData (undefined :: $(return inst)) |]
      instanceToFlagData _ = error "Shouldn't happen"
      -- Duplicate checking is based on the generated `data HFlag_...'
      -- names, and not on FlagData, because we want to do the checks
      -- at compile time.  It's not possible in TH, to run getFlagData
      -- on the just reified instances.
      instanceToModuleNamePair (getInstanceType -> AppT _ (ConT inst)) =
        let (flagrev, modrev) = span (/= '.') $ reverse $ show inst
            modName = reverse $ drop 1 modrev
            flag = drop 1 $ dropWhile (/= '_') $ reverse $ flagrev
        in (modName, flag)
      instanceToModuleNamePair _ = error "Shouldn't happen"
      dupes instances = filter ((>1) . length) $
                        groupBy ((==) `on` snd) $
                        sortBy (compare `on` snd) $
                        map instanceToModuleNamePair instances

getInstanceType :: Dec -> Type
#if MIN_VERSION_template_haskell(2,11,0)
getInstanceType (InstanceD _ _ ty _) = ty
#else
getInstanceType (InstanceD   _ ty _) = ty
#endif
getInstanceType _                  = error "Shouldn't happen"

-- | Same as initHFlags, but makes it possible to introduce
-- programmatic defaults based on user supplied flag values.
--
-- The second parameter has to be a function that gets the following
-- alists:
--
--   * defaults,
--
--   * values from HFLAGS_* environment variables,
--
--   * command line options.
--
-- Has to return an alist that contains the additional defaults that
-- will override the default flag values (but not the user supplied
-- values: environment or command line).
--
-- Type after splicing is @String -> DependentDefaults -> IO [String]@.
-- Where:
--
--   * @type AList = [(String, String)]@
--
--   * @type DependentDefaults = AList -> AList -> AList -> AList@
initHFlagsDependentDefaults :: ExpQ -- (String -> DependentDefaults -> IO [String])
initHFlagsDependentDefaults = do
  [| \progDescription depDefaults ->
        getArgs >>= initFlags depDefaults progDescription $getFlagsData |]

-- | Has to be called from the main before doing anything else:
--
-- > main = do args <- $initHFlags "Simple program v0.1"
-- >           ...
--
-- /Since version 0.2, you mustn't put the initHFlags in a parentheses with the program description.  Just/ @$initHFlags@, /it's cleaner./
--
-- Internally, it uses Template Haskell trickery to gather all the
-- instances of the Flag class and then generates a call to
-- @initFlags@ with the appropriate data gathered together from those
-- instances to a list.
--
-- Type after splicing is @String -> IO [String]@.
initHFlags :: ExpQ -- (String -> IO [String])
initHFlags = do
  [| \progDescription ->
        getArgs >>= initFlags (const $ const $ const []) progDescription $getFlagsData |]
