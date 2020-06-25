module Main where

import Reflex.Dom
import Data.Text

main :: IO ()
main = mainWidget $ text $ pack "hello"
