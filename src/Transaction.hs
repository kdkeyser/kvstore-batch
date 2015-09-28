module Transaction where
    
data Asserts = Asserts [(String, String)] deriving (Show)
data Puts = Puts [(String, String)] deriving (Show)
data Transaction = Transaction Asserts Puts deriving (Show)

emptyTransaction :: Transaction
emptyTransaction = Transaction (Asserts []) (Puts [])