{-# LANGUAGE ForeignFunctionInterface #-}

module PutEnv
( putEnv
) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "stdlib.h putenv"
    c_putEnv :: CString -> IO ()

putEnv :: String -> IO ()
putEnv str = do
    c_str <- newCString str
    c_putEnv c_str
