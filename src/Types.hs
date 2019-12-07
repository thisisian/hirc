module Types where

import Data.Text.Encoding
import Data.Text

newtype Nick = Nick Text
  deriving (Eq, Ord)

instance Show Nick where
  show (Nick s) = unpack s

newtype Room = Room Text
  deriving (Eq, Ord)

instance Show Room where
  show (Room s) = '#': (unpack s)

data Message
  = FromServer Text
  | FromClient Nick Text
  | FromRoom Room Nick Text

instance Show Message where
  show (FromServer msg) = "**" ++ unpack msg ++ "\n"
  show (FromClient nick msg) = show nick ++ ": " ++ unpack msg ++ "\n"
  show (FromRoom room nick msg) = show room ++ ", " ++ show nick ++ ": " ++ unpack msg ++ "\n"

data Target
  = TgtRoom Room
  | TgtNick Nick
 deriving Show

data Command
  = TakeNick Nick
  | Quit
  | Join Room
  | Part Room
  | Names Room
  | List
  | PrivMsg [Target] Text
 deriving Show
