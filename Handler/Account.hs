{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Account where

import Lounge
import Common

import Control.Applicative ((<$>), (<*>))

getLoginR :: Handler RepHtml
getLoginR = do
	defaultLayout $ do
		setTitle $ string $ title ++ " - Login"
		addHamlet [$hamlet|
			.login
				%form!method=post
					Benutzer:
					%input!type=text!name=user
					%br
					Passwort:
					%input!type=password!name=pwd
					%br
					%input!type=submit!value=Login
		|]

postLoginR :: Handler ()
postLoginR = do
	(name, password) <- runFormPost' $ (,) <$> stringInput "user" <*> stringInput "pwd"
	tryLogin name password
	redirect RedirectTemporary EntriesR

getLogoutR :: Handler ()
getLogoutR = do
	deleteSession "user"
	redirect RedirectTemporary EntriesR
