{-# LANGUAGE GADTs #-}
module Backend where
    
data BackendAction = GetAction String | PutAction String String
data BackendResult = GetResult String | PutResult ()

data Backend = Backend ([BackendAction] -> IO [BackendResult])

