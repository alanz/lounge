
module Model.Page where

import Data.Time

import Model.Common

type PageId = String

data Page = Page
	{ pTimestamp :: UTCTime
	, pTitle :: Title
	, pText :: Text
	}
	deriving (Eq,Show)
