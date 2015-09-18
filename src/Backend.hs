{-# LANGUAGE GADTs #-}
module Backend where
    
data BackendAction = GetAction String | PutAction String String
data BackendResult = GetResult String | PutResult ()

data Backend = Backend ([BackendAction] -> IO [BackendResult]) -- function to batch a list of requests on the backend
