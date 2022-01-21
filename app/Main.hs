{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Store Data in Multiple Threads, creation of a common box and 10 userlist.
License     : GPL-3
-}

module Main where

import Types
import Data.List
import Data.Maybe
import System.Random
import Control.Concurrent
import Control.Concurrent (takeMVar)
import Types (User(messages))
import System.IO.Unsafe  
import SendMessage
-- |Create 10 users and store them in an array and then create threads for each user and call SendMessage function from the SendMessage.hs file and stop the execution once we have an emtpy box.
main :: IO ()
main = do
    -- msg'xx' is used for counting purpose
    msg1 <- newMVar 0
    msg2 <- newMVar 0
    msg3 <- newMVar 0
    msg4 <- newMVar 0
    msg5 <- newMVar 0
    msg6 <- newMVar 0
    msg7 <- newMVar 0
    msg8 <- newMVar 0
    msg9 <- newMVar 0
    msg10 <- newMVar 0
    --us'x' are the users along with their message and userlist stores all users name inside it.
    let us1 = User "Yash" msg1 "This is Yash"
    let us2 = User "Khushi" msg2 "This is Khushi"
    let us3 = User "Mahek" msg3 "This is Mahek"
    let us4 = User "Purvi" msg4 "This is Purvi"
    let us5 = User "Anita" msg5 "This is Anita"
    let us6 = User "Raj" msg6 "This is Raj"
    let us7 = User "Raghunath" msg7 "This is Raghunath"
    let us8 = User "Amitabh" msg8 "This is Amitabh"
    let us9 = User "Abhishek" msg9 "This is Abhishek"
    let us10 = User "Shahid" msg10 "This is Shahid"
    let userlist = [us1, us2, us3, us4, us5, us6, us7, us8, us9, us10]
    totalMessages <- newMVar 0
    finished <- newEmptyMVar
    --create threads for each user for concurrent use.sadasd
    forkIO $ sendMessage totalMessages us1 userlist finished
    forkIO $ sendMessage totalMessages us2 userlist finished
    forkIO $ sendMessage totalMessages us3 userlist finished
    forkIO $ sendMessage totalMessages us4 userlist finished
    forkIO $ sendMessage totalMessages us5 userlist finished
    forkIO $ sendMessage totalMessages us6 userlist finished
    forkIO $ sendMessage totalMessages us7 userlist finished
    forkIO $ sendMessage totalMessages us8 userlist finished
    forkIO $ sendMessage totalMessages us9 userlist finished
    forkIO $ sendMessage totalMessages us10 userlist finished
    --w1 has the 'completed' status and then we can display all user details and counts as per our requirement.
    w1 <- takeMVar finished
    print "A total of 100 messages has been sent across all the users."
    print $ "---------------------------------------------------------"
    --userxcount is used for counting total number of messages received by the respective user.
    user1count <- readMVar (messages us1)
    print $ " --> " ++ name us1 ++ " has received " ++ show (user1count) ++ " messages."
    user2count <- takeMVar (messages us2)
    print $ " --> " ++ name us2 ++ " has received " ++ show (user2count) ++ " messages."
    user3count <- takeMVar (messages us3)
    print $ " --> " ++ name us3 ++ " has received " ++ show (user3count) ++ " messages."
    user4count <- takeMVar (messages us4)
    print $ " --> " ++ name us4 ++ " has received " ++ show (user4count) ++ " messages."
    user5count <- takeMVar (messages us5)
    print $ " --> " ++ name us5 ++ " has received " ++ show (user5count) ++ " messages."
    user6count <- takeMVar (messages us6)
    print $ " --> " ++ name us6 ++ " has received " ++ show (user6count) ++ " messages."
    user7count <- takeMVar (messages us7)
    print $ " --> " ++ name us7 ++ " has received " ++ show (user7count) ++ " messages."
    user8count <- takeMVar (messages us8)
    print $ " --> " ++ name us8 ++ " has received " ++ show (user8count) ++ " messages."
    user9count <- takeMVar (messages us9)
    print $ " --> " ++ name us9 ++ " has received " ++ show (user9count) ++ " messages."    
    user10count <- takeMVar (messages us10)
    print $ " --> " ++ name us10 ++ " has received " ++ show (user10count) ++ " messages."
    {-let tuser1count = round user1count :: Int
    --print $ tuser1count (integer)
    let tuser2count = round user2count :: Int
    --print $ tuser2count (integer)
    let tuser3count = round user3count :: Int
    --print $ tuser3count (integer)
    let tuser4count = round user4count :: Int
    --print $ tuser4count (integer)
    let tuser5count = round user5count :: Int
    --print $ tuser5count (integer)
    let tuser6count = round user6count :: Int
    --print $ tuser6count (integer)
    let tuser7count = round user7count :: Int
    --print $ tuser7count (integer)
    let tuser8count = round user8count :: Int
    --print $ tuser8count (integer)
    let tuser9count = round user9count :: Int
    --print $ tuser9count (integer)
    let tuser10count = round user10count :: Int
    --print $ tuser10count (integer) -}
    let usercountfinal = [user1count, user2count, user3count, user4count, user5count, user6count, user7count, user8count, user9count, user10count]
    --we just converted type(double) to type(int) and put them all in a list -> usercountfinal
    {-
    $ ":::: TRYING TO PRINT IN INTEGER FORMAT ::::"
    print $ usercountfinal
    print $ " --> " ++ name us1 ++ " has received " ++ show (tuser1count) ++ " messages."
    print $ " --> " ++ name us2 ++ " has received " ++ show (tuser2count) ++ " messages."
    print $ " --> " ++ name us3 ++ " has received " ++ show (tuser3count) ++ " messages."
    print $ " --> " ++ name us4 ++ " has received " ++ show (tuser4count) ++ " messages."
    print $ " --> " ++ name us5 ++ " has received " ++ show (tuser5count) ++ " messages."
    print $ " --> " ++ name us6 ++ " has received " ++ show (tuser6count) ++ " messages."
    print $ " --> " ++ name us7 ++ " has received " ++ show (tuser7count) ++ " messages."
    print $ " --> " ++ name us8 ++ " has received " ++ show (tuser8count) ++ " messages."
    print $ " --> " ++ name us9 ++ " has received " ++ show (tuser9count) ++ " messages."
    print $ " --> " ++ name us10 ++ " has received " ++ show (tuser10count) ++ " messages." 
    -}
    let y = maximum usercountfinal
    tre y usercountfinal userlist
-- | To find user details of the user who has received the most number of messages.
tre y usercountfinal userlist= do
    let u = fromJust $ elemIndex y usercountfinal
    print $ name (userlist !! u) ++ " has received the most number of messages (" ++ show (y) ++")."
