{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Storage 
	( initialize
	, loadEntry
	, storeEntry
	, loadPage
	, storePage
	, loadEntriesByTag
	, loadEntriesByDate
	, loadUser
	, LSM
	, runLSM
	, makeIdentifier
	) where

import Data.Char

import Data.Time
import Data.Maybe

import Database.CouchDB
import Database.CouchDB.JSON

import Text.JSON

import Control.Exception

import "mtl" Control.Monad.Trans (MonadIO (..))

import Model.Common
import Model.Entry
import Model.Page
import Model.User

instance JSON Entry where
	showJSON e = JSObject $ toJSObject
		[ ("timestamp", showJSON $ show $ eTimestamp e)
		, ("author", showJSON $ eAuthor e)
		, ("title", showJSON $ eTitle e)
		, ("text", showJSON $ eText e)
		, ("tags", showJSON $ eTags e)
		, ("type", showJSON $ entryType)
		, ("_id", showJSON $ makeIdentifier $ eTitle e)
		]
	readJSON v = do
		o <- jsonObject v
		ts <- jsonField "timestamp" o
		au <- jsonField "author" o
		ti <- jsonField "title" o
		te <- jsonField "text" o
		ta <- jsonField "tags" o
		return $ Entry (read ts) au ta ti te

instance JSON Page where
	showJSON p = JSObject $ toJSObject
		[ ("timestamp", showJSON $ show $ pTimestamp p)
		, ("title", showJSON $ pTitle p)
		, ("text", showJSON $ pText p)
		, ("type", showJSON $ pageType)
		, ("_id", showJSON $ makeIdentifier $ pTitle p)
		]
	readJSON v = do
		o <- jsonObject v
		ts <- jsonField "timestamp" o
		ti <- jsonField "title" o
		te <- jsonField "text" o
		return $ Page (read ts) ti te

instance JSON User where
	showJSON u = JSObject $ toJSObject
		[ ("hash", showJSON $ uHash u)
		, ("name", showJSON $ uName u)
		, ("_id", showJSON $ makeIdentifier $ uName u)
		]
	readJSON v = do
		o <- jsonObject v
		h <- jsonField "hash" o
		n <- jsonField "name" o
		return $ User n h

-- | The Lounge Storage Monad, encapsulates the CouchDB monad and the database name.
data LSM a = LSM (String -> CouchMonad (a, String))

instance Monad LSM where
	return a = LSM $ \s -> return (a, s)

	(LSM m) >>= k = LSM $ \s -> do
		(a, s') <- m s
		let (LSM m') = k a
		m' s'

	fail msg = LSM $ \_ -> do
		fail $ "internal error: " ++ msg

instance MonadIO LSM where
	liftIO m = LSM $ \s -> liftIO $ m >>= \a -> return (a, s)

lift :: CouchMonad a -> LSM a
lift m = LSM $ \s -> m >>= \a -> return (a, s)

-- | The name of the document containing the view definitions.
application :: String
application = "lounge"

-- | Identifiers for named views.
entries, pages, users, tags, date, cloud :: String
entries = "entries"
pages = "pages"
tags = "tags"
date = "date"
cloud = "cloud"
users = "users"

-- | Type names to differentiate between entities in the database.
entryType, pageType, userType :: String
entryType = "entry"
pageType = "page"
userType = "user"

views :: [ CouchView ]
views = 
	[ ViewMap tags $ "function(doc) { if (doc.type && doc.type == '" ++ entryType ++ "') { for (i in doc.tags) emit(doc.tags[i], doc) } }" 
	, ViewMap date $ "function(doc) { if (doc.type && doc.type == '" ++ entryType ++ "') { emit(doc.timestamp, doc) } }"
	, ViewMap entries $ "function(doc) { if (doc.type && doc.type == '" ++ entryType ++ "') { emit(doc._id, doc) } }"
	, ViewMap pages $ "function(doc) { if(doc.type && doc.type == '" ++ pageType ++ "') { emit(doc._id, doc) } }"
	, ViewMap users $ "function(doc) { if(doc.type && doc.type == '" ++ userType ++ "') { emit(doc._id, doc) } }"
	, ViewMapReduce cloud 
		("function(doc) { if (doc.type && doc.type == '" ++ entryType ++ "') { for (i in doc.tags) emit(doc.tags[i], 1) } }")
		("function(key, values, rereduce) { return sum(values) }")
	]

getStorage :: LSM String
getStorage = LSM $ \s -> do
	return $ (s, s)

-- | Run the actual storage operation.
runLSM :: String -> LSM a -> IO a
runLSM s (LSM m) = do
	(a, _) <- runCouchDB' $ (m s)
	return a

-- | Checks for existing database and otherwise creates the database and views.
initialize :: LSM ()
initialize = do
	s <- getStorage
	liftIO $ handle ignore  $ runCouchDB' $ do
	createDB s
	newView s application views
	liftIO $ putStrLn $ "Database '" ++ s ++ "' not found, initializing"
		where
		ignore :: (Monad m) => SomeException -> m ()
		ignore _ = return ()

-- | Load a blog entry by its unique identifier
loadEntry :: EntryId -> LSM (Maybe Entry)
loadEntry e = do
	r <- query (doc entries) [("key", showJSON e)]
	return $ listToMaybe $ map snd r

-- | Store a blog entry
storeEntry :: Author -> Title -> Text -> [Tag]  -> LSM EntryId
storeEntry au ti te ta = do
	s <- getStorage
	ts <- liftIO $ getCurrentTime
	(d, _) <- lift $ newDoc (db s) (Entry ts au ta ti te)
	return $ show d

-- | Load all blog entries matching the given tag.
loadEntriesByTag :: Tag -> LSM [(EntryId, Entry)]
loadEntriesByTag t = query (doc tags) [("key", showJSON t)]

-- | Load all blog entries, sorted by timestamp.
loadEntriesByDate :: LSM [(EntryId, Entry)]
loadEntriesByDate = query (doc date) [("descending", showJSON True)]

-- | Load a page by its unique identifier.
loadPage :: PageId -> LSM (Maybe Page)
loadPage p = do
	r <- query (doc pages) [("key", showJSON p)]
	return $ listToMaybe $ map snd r

-- | Store a new page, returning its identifier.
storePage :: Title -> Text -> LSM PageId
storePage ti te = do
	s <- getStorage
	ts <- liftIO $ getCurrentTime
	(d, _) <- lift $ newDoc (db s) (Page ts ti te)
	return $ show d

-- | Load a user by its name.
loadUser :: UserId -> LSM (Maybe User)
loadUser u = do
	r <- query (doc users) [("key", showJSON u)]
	return $ listToMaybe $ map snd r

{-
tagCloud :: LSM [(Tag, Int)]
tagCloud = do
	s <- getStorage
	queryView (db s) (doc application) (doc cloud) [("group", showJSON True)]
	return $ map (\(d, e) -> 
-}

-- | Convenience function for querying views.
query :: (JSON a) => Doc -> [(String, JSValue)] -> LSM [(String, a)]
query v p = do
	s <- getStorage
	es <- lift $ queryView (db s) (doc application) v p
	return $ map (\(d, e) -> (show d, e)) es

-- | Make a readable identifier from a page title
makeIdentifier :: String -> String
makeIdentifier = (mapMaybe replace)
	where
	replace ' ' = Just '-'
{-	replace 'Ü' = Just 'U'
	replace 'ü' = Just 'u'
	replace 'Ö' = Just 'O'
	replace 'ö' = Just 'o'
	replace 'Ä' = Just 'A'
	replace 'ä' = Just 'a'
	replace 'ß' = Just 'a'-}
	replace c | isAlphaNum c = Just c
						| otherwise    = Nothing
