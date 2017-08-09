module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy, null)
import Data.Maybe (Maybe)


type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


findByStreet :: String -> AddressBook -> Maybe Entry
findByStreet street = head <<< filter byStreet
  where
  byStreet :: Entry -> Boolean
  byStreet entry = entry.address.street == street


nameExists :: String -> String -> AddressBook -> Boolean
nameExists firstName lastName = null <<< filter filterEntry 
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy namesEqual
  where
    namesEqual :: Entry -> Entry -> Boolean
    namesEqual e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
