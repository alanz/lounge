
module Model.Entry where

import Data.Time

import Model.Common

type Tag = String

type Author = String
type EntryId = String

data Entry = Entry
	{ eTimestamp :: UTCTime
	, eAuthor :: Author 
	, eTags :: [Tag]
	, eTitle ::Title
	, eText :: Text
	}
	deriving (Eq,Show)
