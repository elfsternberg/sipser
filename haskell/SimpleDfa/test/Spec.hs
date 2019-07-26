{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, it, shouldBe, describe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import SimpleDfa (amatch, amatch2, Dfa(..))

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "Amatch" $ for_ cases test
  where
    test Case{..} = it description assertion
      where
        assertion = matcher dfa input `shouldBe` expected

data Case = Case {
  dfa :: Dfa Char Char,
  matcher :: Dfa Char Char -> [Char] -> Bool,
  description :: String,
  input :: String,
  expected :: Bool
}

createCase dfa matcher (description, input, expected) =
  Case {
    dfa = dfa,
    matcher = matcher,
    description = description,
    input = input,
    expected = expected
  }

dfa14 = Dfa {
  nodes = [('a', [('0', 'a'), ('1', 'b')]),
           ('b', [('1', 'b'), ('0', 'c')]),
           ('c', [('1', 'b'), ('0', 'b')])],
  starts = 'a',
  accepts = ['b']
}

cD14a = createCase dfa14 amatch

cD14b = createCase dfa14 amatch2

cases :: [Case]  
cases = [
    cD14a ("accept 0001", "0001", True),
    cD14a ("accept 0011", "0011", True),
    cD14a ("reject 0110", "0110", False), 
    cD14b ("accept 0001", "0001", True),
    cD14b ("accept 0011", "0011", True),
    cD14b ("reject 0110", "0110", False)
  ]
  
