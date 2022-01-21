{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : SendMessage
Description : Count total number of messages and update status of box to terminate execution.
License     : GPL-3
-}

module SendMessage
    ( 
        sendMessage
    ) where

import Types
import System.Random
import Control.Concurrent
import Control.Concurrent (takeMVar)
import Types (User(messages))
import System.IO.Unsafe  
-- | sendMessage function is called to count the total number of messages and to complete the function once the box contains 'Completed'. 
sendMessage totalMessages sender userlist finished = do
    totalMsgs <- takeMVar totalMessages
    -- Check if less than 100 Messages or no
    if totalMsgs < 100 then do
        -- Thats an IO, it gives an IO Int, so to get Int we get unsafePerformIO
        let n = (unsafePerformIO (getStdRandom (randomR (0, 9)))) :: Int
        let user = userlist !! n
        --if both user, sender are same, redo.
        if user == sender then do
            putMVar totalMessages totalMsgs
            sendMessage totalMessages sender userlist finished
        else do
            -- For clarity purpose : to see who is the sender, receiver, and what the message is.
            print $ " -- Message sender --> " ++ (name sender) ++ " <-- Message receiver --> " ++ (name user) ++ " <-- Message : " ++ (msg sender)
            -- For counting the user specific message count.
            count <- takeMVar (messages user)
            -- To increment the user specific message count.
            let ncount = count+1
            -- We put the new count inside the MVar
            putMVar (messages user) ncount
            let msgcount = totalMsgs+1
            -- Closing the MVar
            putMVar totalMessages msgcount 
            -- Generate random number to delay for
            delayTime <- randomRIO (10, 100)
            -- Delay in microseconds
            threadDelay delayTime
            -- Call te function recursively
            sendMessage totalMessages sender userlist finished
    else do
        -- Close the MVar
        putMVar totalMessages totalMsgs
        -- Make the 'finshed' MVar non-empty
        putMVar finished "Completed"
