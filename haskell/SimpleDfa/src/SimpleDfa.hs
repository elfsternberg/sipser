module SimpleDfa (amatch2, Dfa (..), Node(..)) where

-- A DFA is a finite automata consisting of a five-tuple (Q, Σ, δ, q0,
-- F), where Q is the set of all possible states, Σ is the set of all
-- possible symbols, δ is the set of transition rules Q ⨯ Σ → Q, q0 is
-- the start rule, and F is the set of accept states.

-- Some things don't need to be "on the page".  Here, our DFA is a
-- collection of references to Nodes in the graph; each Node consists
-- entirely of the transition function rules and a boolean indicating
-- whether or not it is the "accept" state.

-- This does not exacty resemble the five-tuple, but it *does*
-- represent the graphical representation of the DFA as seen in
-- section 1.4 of the book.

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

-- I'm especially proud of the use of `foldM` as a way of turning what
-- Sipser describes as a recursive problem (in fact, it is a
-- *primitive* recursive problem) into one that can be handled
-- by a fold.  Using the `Maybe` monad here allows me to prepare
-- for future implementations where I just won't bother with
-- transitions that lead permanently to the reject state.

amatch2 :: (Eq sym) => Dfa sym -> [sym] -> Bool  
amatch2 dfa str =
  case foldM nextn (starts dfa) str of
      Just c  ->
        accepts
        where (Node _ accepts) = c
      Nothing -> False
