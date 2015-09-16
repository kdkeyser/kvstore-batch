{-# LANGUAGE GADTs #-}
module SimpleAction where

data SimpleAction a where
    None :: SimpleAction ()
    Get :: String -> SimpleAction String
    Put :: String -> String -> SimpleAction ()

