{-

    A module that helps with many-to-many joins.
    
    Originally written by Pat Brisbin <me@pbrisbin.com>.

    https://github.com/pbrisbin/renters-reality/blob/master/Helpers/Model.hs

-}

module ManyToMany (
    joinTables,
    joinTables3
) where

import Prelude
import Data.Maybe                      ( catMaybes )
import qualified Data.Map as M         ( fromList, lookup )

import Yesod

joinTables :: (a -> Key b)
           -> [Entity a]
           -> [Entity b]
           -> [(Entity a, Entity b)]
joinTables f as bs =
    catMaybes . for as $ \a -> fmap (\b -> (a,b)) $ lookupRelation f a bs
    
joinTables3 :: (a -> Key b)
            -> (a -> Key c)
            -> [Entity a]
            -> [Entity b]
            -> [Entity c]
            -> [(Entity a, Entity b, Entity c)]
joinTables3 f g as bs cs = catMaybes . for as $ \a ->
    case (lookupRelation f a bs, lookupRelation g a cs) of
        (Just b, Just c) -> Just (a,b,c)
        _ -> Nothing

lookupRelation :: (a -> Key b) -> Entity a -> [Entity b] -> Maybe (Entity b)
lookupRelation f a bs = let k = f $ entityVal a
                            vs = M.fromList $ map (\(Entity k' v) -> (k',v)) bs
                        in fmap (Entity k) $ M.lookup k vs
                        
for :: [a] -> (a -> b) -> [b]
for xs f = map f xs
