{-# LANGUAGE CPP #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Lounge.hs file.
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , connStr
    , approot
    , staticroot
    , staticdir
    ) where

import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import qualified Text.Julius as H
import Language.Haskell.TH.Syntax

-- | The base URL for your application. This will usually be different for
-- development and production. Yesod automatically constructs URLs for you,
-- so this value must be accurate to create valid links.
approot :: String
#ifdef PRODUCTION
-- You probably want to change this. If your domain name was "yesod.com",
-- you would probably want it to be:
-- > approot = "http://www.yesod.com"
-- Please note that there is no trailing slash.
approot = "http://www.fortytools.com"
#else
approot = "http://localhost:3000"
#endif

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticdir :: FilePath
staticdir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Lounge.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Lounge.hs
staticroot :: String
staticroot = approot ++ "/static"

-- | The database connection string. The meaning of this string is backend-
-- specific.
connStr :: String
#ifdef PRODUCTION
connStr = "lounge"
#else
connStr = "lounge"
#endif

-- The rest of this file contains settings which rarely need changing by a
-- user.

-- The following three functions are used for calling HTML, CSS and
-- Javascript templates from your Haskell code. During development,
-- the "Debug" versions of these functions are used so that changes to
-- the templates are immediately reflected in an already running
-- application. When making a production compile, the non-debug version
-- is used for increased performance.
--
-- You can see an example of how to call these functions in Handler/Root.hs

hamletFile :: FilePath -> Q Exp
#ifdef PRODUCTION
hamletFile x = H.hamletFile $ "hamlet/" ++ x ++ ".hamlet"
#else
hamletFile x = H.hamletFileDebug $ "hamlet/" ++ x ++ ".hamlet"
#endif

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile x = H.cassiusFile $ "cassius/" ++ x ++ ".cassius"
#else
cassiusFile x = H.cassiusFileDebug $ "cassius/" ++ x ++ ".cassius"
#endif

juliusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
juliusFile x = H.juliusFile $ "julius/" ++ x ++ ".julius"
#else
juliusFile x = H.juliusFileDebug $ "julius/" ++ x ++ ".julius"
#endif

