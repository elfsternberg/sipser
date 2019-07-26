module SimpleDfa (amatch2, Dfa (..), Node(..)) where

-- A DFA is a finite automata consisting of a five-tuple
-- (Q, Σ, δ, q0, F), where Q is the set of all possible
-- states, Σ is the set of all possible symbols, δ is
-- the set of transition rules Q ⨯ Σ → Q, q0 is the start
-- rule, and F is the set of accept states.

-- A node here contains two things: A state, and a list of the
-- transition rules from that state to other states.

import Control.Monad (foldM)

data Node sym = Node [(sym, Node sym)] Bool
  
data Dfa sym = Dfa {
  nodes :: [Node sym],
  starts :: (Node sym)
}

nextn :: (Eq sym) => Node sym -> sym -> Maybe (Node sym)
nextn node s =
  let (Node rules _) = node
  in lookup s rules

amatch2 :: (Eq sym) => Dfa sym -> [sym] -> Bool  
amatch2 dfa str =
  case foldM nextn (starts dfa) str of
      Just c  ->
        accepts
        where (Node _ accepts) = c
      Nothing -> False
