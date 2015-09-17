{-# LANGUAGE GADTs #-}
module Backend where
    
data BackendAction = GetAction String | PutAction String String
data BackendResult = GetResult String | PutResult ()

data Backend = Backend ([BackendAction] -> IO [BackendResult])

{-get :: Backend -> String -> IO String 
get (Backend batch) key = do
    [GetResult value] <- batch [GetAction key]
    return value
    -}