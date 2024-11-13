module Worker where

import Foreign.C.String (CString, peekCString, newCString)
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Sql2er.Parser

foreign export ccall "hs_runWorker" runWorker :: CString -> IO CString

runWorker :: CString -> IO CString
runWorker cstr = do 
    inputStr <- peekCString cstr
    let res = parseScript inputStr
    case res of
        Left e -> newCString e
        Right r -> newCString r
    -- newCString res

foreign export ccall "callocBuffer" callocBuffer :: Int -> IO (Ptr a)

callocBuffer :: Int -> IO (Ptr a)
callocBuffer = callocBytes

foreign export ccall "freeBuffer" freeBuffer :: Ptr a -> IO ()

freeBuffer :: Ptr a -> IO ()
freeBuffer = free
