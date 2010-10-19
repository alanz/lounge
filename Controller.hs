{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
	( withLounge
	) where

import Lounge
import Settings
import Storage

import Yesod.Helpers.Static


-- Import all relevant handler modules here.
import Handler.Entry
import Handler.Account
import Handler.Page
import Handler.Feed

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Lounge.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Lounge" resourcesLounge

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent "User-agent: *"

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withLounge :: (Application -> IO a) -> IO a
withLounge f = do
	let h = Lounge s
	runLSM Settings.connStr initialize
	toWaiApp h >>= f
	where
		s = fileLookupDir Settings.staticdir typeByExt

