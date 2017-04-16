#!/usr/bin/env runhaskell

{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import Control.Monad
import System.Exit

import HFlags

-- For flags with a simple type (technically speaking, for instances
-- of FlagType, see `:info FlagType' in GHCi), defining a new flag is
-- very straightforward:
defineFlag "name" ("Indiana Jones"::String) "Who to greet."

-- There is a special syntax to support short options, like `d' here.
-- Also, for the Bool options the argument is not mandatory, if the
-- user just runs the program with -d or --dry_run, flags_dru_run will
-- be set to True.
defineFlag "d:dry_run" False "Don't print anything, just exit."
-- For integral types you have to explicitly define Int or Integer.
defineFlag "repeat" (3 + 4 :: Int) "Number of times to repeat the message."

-- For flags with a more complex type, where the existing Read and
-- Show is suitable for parsing of the argument value, you can use the
-- defineEQFlag syntax.  Here, the EQ is mnemonic for expression
-- quotes, because you have to expression quote the default value, and
-- type sign it inside.  Also, you have to provide the name of the
-- type for --help generation.
defineEQFlag "numbers_to_sum" [| [1,2,3,4,5] :: [Int] |] "INT_LIST" "Print the sum of these numbers."

-- This works quite well for simple enums too!
data Color = Red | Yellow | Green deriving (Show, Read)
defineEQFlag "favorite_color" [| Yellow :: Color |] "COLOR" "Your favorite color."

-- Sometimes the default read or show instance is not good enough for
-- your needs.  In that case you can use defineCustomFlag to override
-- those functions.
data Language = English | Hungarian
defineCustomFlag "language" [| English :: Language |] "en|hu"
  [| \s -> case s of
      "en" -> English
      "hu" -> Hungarian
      _ -> error $ "Unknown language: " ++ s
   |]
  [| \l -> case l of
      English -> "en"
      Hungarian -> "hu"
   |]
  "Language of the greeting."

-- You can also do simple range checks with this.
defineCustomFlag "percent" [| 100 :: Double |] "PERCENTAGE"
  [| \s -> let p = read s
           in if 0.0 <= p && p <= 100.0
              then p
              else error "Percentage value has to be between 0 and 100."
   |]
  [| show |]
  "Print first percent percentage of the message."

-- You can use combinators syntax and pass enviornment option.
defineFlag (flag "etest" `env` "APP_ENVTEST") ("TEST"::String) "pass value from environment"
$(return [])

main = do
  _ <- $initHFlags "HFlags example program v0.1"
  when (flags_dry_run) $ exitSuccess
  forM_ [1..flags_repeat] (const greet)
  putStrLn $ "IIRC, your favorite color is " ++ show flags_favorite_color ++ "."
  putStrLn $ "And enviroment says " ++ flags_etest ++ "."
  where
    s = show (sum flags_numbers_to_sum)
    percentPutStrLn 0 _ = return ()
    percentPutStrLn p str = let n = ceiling $ fromIntegral (length str) * p / 100.0
                            in putStrLn $ take n str
    greet = percentPutStrLn flags_percent $ case flags_language of
      English -> "Hello " ++ flags_name ++ ", very nice to meet you, the sum is " ++ s ++ "!"
      Hungarian -> "Szia " ++ flags_name ++ ", orvendek a talalkozasnak, a szamok osszege " ++ s ++ "!"
