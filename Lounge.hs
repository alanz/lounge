{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module Lounge
	( Lounge (..)
	, LoungeRoute (..)
	, resourcesLounge
	, Handler
	, module Yesod
	, module Settings
	, StaticRoute (..)
	) where

import Yesod
import Yesod.Helpers.Static
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Web.Routes
import Settings (hamletFile, cassiusFile, juliusFile)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Lounge = Lounge
    { getStatic :: Static -- ^ Settings for static file serving.
    }

-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler Lounge Lounge

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype LoungeRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Lounge = LoungeRoute
-- * Creates the value resourcesLounge which contains information on the
--   resources declared below. This is used in Controller.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Lounge. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the LoungeRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Lounge" [$parseRoutes|
/static StaticR Static getStatic

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/ EntriesR GET
/entry/#String EntryR GET
/tag/#String TagR GET
/post PostR POST
/feed FeedR GET
/page/#String PageR GET
/login LoginR GET POST
/logout LogoutR GET
|]

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Lounge where
	approot _ = Settings.approot

	defaultLayout widget = do
		mmsg <- getMessage
		about <- return $ PageR "Über"
		imprint <- return $ PageR "Impressum"
		pc <- widgetToPageContent $ do
			widget
			addStylesheet $ StaticR $ StaticRoute ["reset-min.css"] []
			addScript $ StaticR $ StaticRoute ["jquery-1.4.2.min.js"] []
			addScript $ StaticR $ StaticRoute ["jquery.tweet.js"] []
			addCassius $(Settings.cassiusFile "default-layout")
		hamletToRepHtml $(Settings.hamletFile "default-layout")

	-- This is done to provide an optimization for serving static files from
	-- a separate domain. Please see the staticroot setting in Settings.hs
	urlRenderOverride a (StaticR s) =
		Just $ uncurry (joinPath a Settings.staticroot) $ format s
			where
			format = formatPathSegments ss
			ss :: Site StaticRoute (String -> Maybe (GHandler Static Lounge ChooseRep))
			ss = getSubSite
	urlRenderOverride _ _ = Nothing

	-- This function creates static content files in the static folder
	-- and names them based on a hash of their content. This allows
	-- expiration dates to be set far in the future without worry of
	-- users receiving stale content.
	addStaticContent ext' _ content = do
		let fn = base64md5 content ++ '.' : ext'
		let statictmp = Settings.staticdir ++ "/tmp/"
		liftIO $ createDirectoryIfMissing True statictmp
		liftIO $ L.writeFile (statictmp ++ fn) content
		return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

