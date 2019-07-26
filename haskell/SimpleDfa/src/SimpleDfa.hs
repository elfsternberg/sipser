module SimpleDfa (amatch, amatch2, Dfa (..)) where

-- A DFA is a finite automata consisting of a five-tuple
-- (Q, Σ, δ, q0, F), where Q is the set of all possible
-- states, Σ is the set of all possible symbols, δ is
-- the set of transition rules Q ⨯ Σ → Q, q0 is the start
-- rule, and F is the set of accept states.

-- A node here contains two things: A state, and a list of the
-- transition rules from that state to other states.

import Control.Monad (foldM)

data Dfa state symbol = Dfa {
  nodes :: [(state, [(symbol, state)])],
  starts :: state,
  accepts :: [state]
}

trans :: (Eq sym, Eq sta) => Dfa sta sym -> sta -> sym -> Maybe sta
trans dfa cur sym =
  case lookup cur (nodes dfa) of
    Just rules -> lookup sym rules
    Nothing -> Nothing

amatch :: (Eq symbol, Eq state) => Dfa state symbol -> [symbol] -> Bool
amatch dfa str =
  case lookup (starts dfa) (nodes dfa)  of
    Just _  -> aamatch (starts dfa) str
    Nothing -> False         
  where
    aamatch cur []     = cur `elem` (accepts dfa)
    aamatch cur (s:ss) =
      case trans dfa cur s of
        Just c -> aamatch c ss
        Nothing -> False

amatch2 :: (Eq symbol, Eq state) => Dfa state symbol -> [symbol] -> Bool  
amatch2 dfa str = case foldM anext (starts dfa) str of
  Just c  -> c `elem` (accepts dfa)
  Nothing -> False
  where
    anext c s = case lookup c (nodes dfa) of
      Just rules -> lookup s rules
      Nothing    -> Nothing


