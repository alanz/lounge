module Common where

import Data.Time
import System.Locale
import Data.Hash.MD5

import Model.User

import Storage
import Settings
import Lounge

title :: String
title = "fortytools"

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%B %d, %Y"

checkAuth :: Handler (Maybe User)
checkAuth = do
		s <- reqSession `fmap` getRequest
		maybe (return Nothing) findUser (lookup "user" s)
		where
			findUser n = do
				liftIO $ runLSM Settings.connStr $ loadUser n	

requireAuth :: Handler User
requireAuth = checkAuth >>= (maybe (permissionDenied "Access denied") return)

tryLogin :: String -> String -> Handler ()
tryLogin n p = do
		mu <- liftIO $ runLSM Settings.connStr $ loadUser n
		maybe deny checkPwd mu
		where
			checkPwd u = do
				if uHash u == (md5s $ Str p)
					then setSession "user" n
					else deny
			deny = do
				deleteSession "user"
				permissionDenied "Access denied"

 
