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

type Lens s a = forall f. (s -> f a) -> s -> f s

type Getter r s a = (a -> Const r a) -> s -> Const r s
type Setter s a = (a -> Identity a) -> s -> Identity s

view :: Getter a s a -> s -> a
view g r = getConst (g Const r)

to :: (s -> a) -> Getter r s a
to k f = Const . getConst . f . k

sets :: ((a -> a) -> s -> s) -> Setter s a
sets f g = Identity . f (runIdentity . g)

over :: Setter s a -> (a -> a) -> s -> s
over s f r = runIdentity (s (Identity . f) r)

bornStreet :: Born -> String
bornStreet = view (to _street) . view (to _bornAt)

streetL f addr = (\street' -> addr {_street = street'}) <$> f (_street addr)
addrL f p = (\addr -> p { _address = addr }) <$> f (_address p)
dayL f day = (\(y,m,d) -> fromGregorian y m d) <$> f (toGregorian day)
bornAtL f born = (\bornAt -> born {_bornAt = bornAt}) <$> f (_bornAt born)
bornOnL f born = (\bornOn -> born {_bornOn = bornOn}) <$> f (_bornOn born)
bornL f p = (\born -> p { _born = born }) <$> f (_born p)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person = over (addrL . streetL) (const street) person

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = over (bornL . bornOnL . dayL) (\(y,_,d) -> (y, month, d)) person

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = over (addrL . streetL) f . over (bornL . bornAtL . streetL) f $ person
