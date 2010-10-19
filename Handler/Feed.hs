{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Feed where

import Lounge
import Storage
import Settings
import Common

import Model.Entry

import Yesod.Helpers.AtomFeed

getFeedR :: Handler RepAtom
getFeedR = do
	entries <- liftIO $ runLSM Settings.connStr $ loadEntriesByDate
	let entries' = take 5 entries
	atomFeed $ AtomFeed
		{ atomTitle = title
		, atomLinkSelf = FeedR
		, atomLinkHome = EntriesR
		, atomUpdated = eTimestamp $ snd $ head entries'
		, atomEntries = map go entries'
		}
	where
		go e = AtomFeedEntry
			{ atomEntryLink = EntryR $ fst e
			, atomEntryUpdated = eTimestamp $ snd e
			, atomEntryTitle = eTitle $ snd e
			, atomEntryContent = preEscapedString $ eText $ snd $ e
			}

