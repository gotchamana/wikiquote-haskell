{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module Notify (show, defautlConfig, Config (..)) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Foreign.C.String (CString, newCString)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Prelude hiding (init, show)
import Foreign.C.Types (CInt (..))

data Config = Config {
    title :: String,
    summary :: String,
    content :: String,
    icon :: String,
    timeout :: Int
} deriving (Show)

data Notification

foreign import capi unsafe "libnotify/notify.h notify_init" initN :: CString -> IO Bool

foreign import capi unsafe "libnotify/notify.h notify_uninit" uninitN :: IO ()

foreign import capi unsafe "libnotify/notify.h notify_notification_new" newN :: CString -> CString -> CString -> IO (Ptr Notification)

foreign import capi unsafe "libnotify/notify.h notify_notification_set_timeout" setTimeoutN :: Ptr Notification -> CInt -> IO ()

foreign import capi unsafe "libnotify/notify.h notify_notification_show" showN :: Ptr Notification -> Ptr () -> IO Bool

foreign import capi unsafe "libnotify/notify.h g_object_unref" unrefN :: Ptr Notification -> IO ()

defautlConfig :: Config
defautlConfig = Config { title = "Notify", summary = "", content = "", icon = "", timeout = 5000 }

show :: Config -> IO ()
show Config { title = "" } = error "title must not be empty"
show Config { timeout = t } | t <= 0 = error "timeout must be great than zero"
show Config { title = title, summary = summary, content = content, icon = icon, timeout = timeout } = do
    [title, summary, content, icon] <- mapM newCString [title, summary, content, icon]

    initN title

    bracket
        (newN summary content icon)
        unrefN
        (\ptr -> do
            setTimeoutN ptr (fromIntegral timeout)
            showN ptr nullPtr)

    uninitN

    forM_ [title, summary, content, icon] free