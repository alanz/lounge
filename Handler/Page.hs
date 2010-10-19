{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Page where

import Lounge
import Storage
import Settings
import Common

import Model.Page

getPageR :: PageId -> Handler RepHtml
getPageR pid = do
	page <- (liftIO $ runLSM Settings.connStr $ loadPage pid) >>= maybe notFound return
	defaultLayout $ do
		setTitle $ string $ (pTitle page ++ " - " ++ title)
		addBody $(hamletFile "page")
		addStyle $(cassiusFile "page")

