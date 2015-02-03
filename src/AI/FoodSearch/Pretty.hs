-- | Pretty print
module AI.FoodSearch.Pretty where

import AI.FoodSearch.Base

import Text.PrettyPrint

-- | Pretty print a world
pprWorld :: World -> Doc
pprWorld w = pprWorld' w Nothing

-- | Pretty print a world and a position
pprPosition :: World -> Position -> Doc
pprPosition w p = pprWorld' w $ Just p

-- | Pretty print a world with an optional position
pprWorld' :: World -> Maybe Position -> Doc
pprWorld' wrld mp = let
  (w,h) = wSize wrld
  hdr=text $ replicate (w + 2) '#' 
  ftr=hdr
  ls = vcat $ map (ln w) $ take h [0..]
  in vcat [hdr,ls,ftr]
  where
    ln w y = let
      cnts = hcat $ map (char . cell y) $ take w [0..]
      in  hcat [char '#',cnts,char '#']
    cell y x 
      | mp == Just (x,y) = '@' 
      | foundFood wrld (x,y) = 'X'
      | otherwise= '.'

-- | Show a progression of positions      
pprPath :: World -> [Position] -> Doc
pprPath w = vcat . zipWith pprPathIdx [1 ..]
  where
    pprPathIdx idx pos = vcat [text "Iteration:" <> int idx,pprPosition w pos]
