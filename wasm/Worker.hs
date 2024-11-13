module Worker where

foreign export ccall "hs_runWorker" runWorker :: IO ()

runWorker :: IO ()
runWorker = mempty