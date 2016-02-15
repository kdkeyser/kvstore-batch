{-# LANGUAGE GADTs #-}
module KVOperation where

-- | Basic key/value store API
-- | Get retrieve the value associated with a key
-- | Put sets the value associated with a key
-- | Key/values are strings
data KVOperation a where
    Get :: String -> KVOperation String
    Put :: String -> String -> KVOperation ()

