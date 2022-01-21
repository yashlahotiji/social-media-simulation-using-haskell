{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Types
Description : Create User data type which includes their username and a message type to keep track of messages between users.
License     : GPL-3
-}

module Types
    ( 
        User (..)
    ) where

import Control.Concurrent ( MVar )
-- | create user data type and have user name and message.
data User = User {
    name :: String,
    messages :: MVar Integer,
    msg :: String
} deriving (Eq)