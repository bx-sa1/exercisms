{-# LANGUAGE RankNTypes #-}
module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Functor.Const
import Data.Functor.Identity

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens g s f x = s x <$> f (g x)

street :: Lens Address String
street = lens _street $ \ a x -> a { _street = x }

address :: Lens Person Address
address = lens _address $ \ p x -> p { _address = x}

bornAt :: Lens Born Address
bornAt = lens _bornAt $ \ b x -> b { _bornAt = x }

bornOn :: Lens Born Day
bornOn = lens _bornOn $ \ b x -> b { _bornOn = x }

born :: Lens Person Born
born = lens _born $ \ p x -> p { _born = x }

view :: Lens s a -> s -> a
view l x = getConst (l Const x)

set :: Lens s a -> a -> s -> s
set l x = runIdentity . l (\_ -> Identity x)

over :: Lens s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set (address . street)

setBirthMonth :: Int -> Person -> Person
setBirthMonth month = over (born . bornOn) $ \x -> let (y,m,d) = toGregorian x in fromGregorian y month d

renameStreets :: (String -> String) -> Person -> Person
renameStreets f = over (address . street) f . over (born . bornAt . street) f
