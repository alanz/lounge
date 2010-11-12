{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Entry where

import Lounge
import Storage
import Settings
import Common

import Model.User
import Model.Entry

import Control.Applicative ((<$>), (<*>))

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Lounge.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getEntriesR :: Handler RepHtml
getEntriesR = do
	user <- checkAuth
	entries <- liftIO $ runLSM Settings.connStr $ loadEntriesByDate
	defaultLayout $ do
		setTitle $ string title
		addHamlet $(hamletFile "entries")
		addCassius $(cassiusFile "entries")

postPostR :: Handler ()
postPostR = do
	user <- requireAuth
	(subject, body, tagstr) <- runFormPost' $ (,,) <$> 
		stringInput "subject" <*> stringInput "body" <*> stringInput "tags"
	_ <- liftIO $ runLSM Settings.connStr $ 
		storeEntry (uName user) subject body (tags tagstr)
	redirect RedirectTemporary EntriesR

getEntryR :: EntryId -> Handler RepHtml
getEntryR eid = do
	entry <- (liftIO $ runLSM Settings.connStr $ loadEntry eid) >>= maybe notFound return
	defaultLayout $ do
		setTitle $ string $ (eTitle entry ++ " - " ++ title)
		addHamlet $(hamletFile "entry")
		addCassius $(cassiusFile "entry")

getTagR :: Tag -> Handler RepHtml
getTagR tag = do
	user <- checkAuth
	entries <- (liftIO $ runLSM Settings.connStr $ loadEntriesByTag tag)
	defaultLayout $ do
		setTitle $ string title
		addHamlet $(hamletFile "entries")
		addCassius $(cassiusFile "entries")

tags :: String -> [String]
tags "" = []
tags s = let (l,s') = break (','==) s
	in l : case s' of 
		[] -> []
		(_:s'') -> tags s''


