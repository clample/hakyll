--------------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Check
    ( Check (..)
    , check
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                (forM_, foldM)
import           Control.Monad.Reader         (ask, ReaderT, runReaderT)
import           Control.Monad.Trans          (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.ByteString.Char8        (unpack)
import           Data.List                    (isPrefixOf)
import qualified Data.Map.Lazy                as M
import           Network.URI                  (unEscapeString)
import           System.Directory             (doesDirectoryExist,
                                               doesFileExist)
import           System.Exit                  (ExitCode (..))
import           System.FilePath              (takeDirectory, takeExtension,
                                               (</>))
import qualified Text.HTML.TagSoup            as TS
import Control.Concurrent.MVar                (MVar, newEmptyMVar, putMVar, readMVar)


--------------------------------------------------------------------------------
#ifdef CHECK_EXTERNAL
import           Control.Exception            (SomeAsyncException (..),
                                               SomeException (..), try, throw)
import           Control.Monad.State          (get, modify, StateT, runStateT)
import           Data.List                    (intercalate)
import           Data.Typeable                (cast)
import           Data.Version                 (versionBranch)
import           GHC.Exts                     (fromString)
import qualified Network.HTTP.Conduit         as Http
import qualified Network.HTTP.Types           as Http
import qualified Paths_hakyll                 as Paths_hakyll
#endif


--------------------------------------------------------------------------------
import           Hakyll.Core.Configuration
import           Hakyll.Core.Logger           (Logger)
import qualified Hakyll.Core.Logger           as Logger
import           Hakyll.Core.Util.File
import           Hakyll.Web.Html


--------------------------------------------------------------------------------
data Check = All | InternalLinks
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
check :: Configuration -> Logger -> Check -> IO ExitCode
check config logger check' = do
    ((), state) <- runChecker checkDestination config logger check'
    failed <- failedLinks state
    if failed > 0 then return ( ExitFailure 1) else return ExitSuccess


--------------------------------------------------------------------------------
data CheckerRead = CheckerRead
    { checkerConfig :: Configuration
    , checkerLogger :: Logger
    , checkerCheck  :: Check
    }


--------------------------------------------------------------------------------
data CheckerWrite = CheckerWrite
    { checkerFaulty :: Int
    , checkerOk     :: Int
    } deriving (Show)


--------------------------------------------------------------------------------
instance Monoid CheckerWrite where
    mempty                                            = CheckerWrite 0 0
    mappend (CheckerWrite f1 o1) (CheckerWrite f2 o2) =
        CheckerWrite (f1 + f2) (o1 + o2)


--------------------------------------------------------------------------------
type CheckerState = M.Map String (MVar CheckerWrite)


--------------------------------------------------------------------------------
type Checker a = ReaderT CheckerRead (StateT CheckerState IO) a


--------------------------------------------------------------------------------
runChecker :: Checker a -> Configuration -> Logger -> Check
           -> IO (a, CheckerState)
runChecker checker config logger check' = do
    let read' = CheckerRead
                    { checkerConfig = config
                    , checkerLogger = logger
                    , checkerCheck  = check'
                    }
    runStateT (runReaderT checker read') M.empty


--------------------------------------------------------------------------------
checkDestination :: Checker ()
checkDestination = do
    config <- checkerConfig <$> ask
    files  <- liftIO $ getRecursiveContents
        (const $ return False) (destinationDirectory config)

    let htmls =
            [ destinationDirectory config </> file
            | file <- files
            , takeExtension file == ".html"
            ]

    forM_ htmls checkFile


--------------------------------------------------------------------------------
checkFile :: FilePath -> Checker ()
checkFile filePath = do
    logger   <- checkerLogger <$> ask
    contents <- liftIO $ readFile filePath
    Logger.header logger $ "Checking file " ++ filePath

    let urls = getUrls $ TS.parseTags contents
    forM_ urls $ \url -> do
        Logger.debug logger $ "Checking link " ++ url
        m <- liftIO newEmptyMVar
        checkUrl filePath url m


--------------------------------------------------------------------------------
checkUrl :: FilePath -> String -> MVar CheckerWrite -> Checker ()
checkUrl filePath url m
    | isExternal url  = checkExternalUrl url m
    | hasProtocol url = skip "Unknown protocol, skipping" url m
    | otherwise       = checkInternalUrl filePath url m
  where
    validProtoChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+-."
    hasProtocol str = case break (== ':') str of
        (proto, ':' : _) -> all (`elem` validProtoChars) proto
        _                -> False


--------------------------------------------------------------------------------
ok :: String -> MVar CheckerWrite -> Checker ()
ok url m = liftIO $ putMVar m mempty {checkerOk = 1}


--------------------------------------------------------------------------------
skip :: String -> String -> MVar CheckerWrite -> Checker ()
skip reason url m = do
    logger <- checkerLogger <$> ask
    Logger.debug logger $ reason
    liftIO $ putMVar m mempty {checkerOk = 1}

--------------------------------------------------------------------------------
faulty :: String -> Maybe String -> MVar CheckerWrite -> Checker ()
faulty url reason m = do
    logger <- checkerLogger <$> ask
    Logger.error logger $ "Broken link to " ++ show url ++ explanation
    liftIO $ putMVar m mempty {checkerFaulty = 1}
  where
    formatExplanation = (" (" ++) . (++ ")")
    explanation = maybe "" formatExplanation reason


--------------------------------------------------------------------------------
checkInternalUrl :: FilePath -> String -> MVar CheckerWrite -> Checker ()
checkInternalUrl base url m = case url' of
    "" -> ok url m
    _  -> do
        config <- checkerConfig <$> ask
        let dest = destinationDirectory config
            dir  = takeDirectory base
            filePath
                | "/" `isPrefixOf` url' = dest ++ url'
                | otherwise             = dir </> url'

        exists <- checkFileExists filePath
        if exists then ok url m else faulty url Nothing m
  where
    url' = stripFragments $ unEscapeString url


--------------------------------------------------------------------------------
checkExternalUrl :: String -> MVar CheckerWrite -> Checker ()
#ifdef CHECK_EXTERNAL
checkExternalUrl url m = do
    logger     <- checkerLogger           <$> ask
    needsCheck <- (== All) . checkerCheck <$> ask
    checked    <- (urlToCheck `M.member`) <$> get
    if not needsCheck || checked
        then Logger.debug logger "Already checked, skipping"
        else do
            result <- requestExternalUrl urlToCheck
            modify $ M.insert urlToCheck m
            case result of
                Left (SomeException e) ->
                    case (cast e :: Maybe SomeAsyncException) of
                        Just ae -> throw ae
                        _ -> faulty url (Just $ showException e) m
                Right _ -> ok url m
  where
    -- Check scheme-relative links
    schemeRelative = isPrefixOf "//"
    urlToCheck     = if schemeRelative url then "http:" ++ url else url

    -- Convert exception to a concise form
    showException e = case cast e of
        Just (Http.HttpExceptionRequest _ e') -> show e'
        _ -> head $ words $ show e
#else
checkExternalUrl _ = return ()
#endif


--------------------------------------------------------------------------------

requestExternalUrl :: String -> Checker (Either SomeException Bool)
requestExternalUrl urlToCheck = liftIO $ try $ do
  mgr <- Http.newManager Http.tlsManagerSettings
  runResourceT $ do
    request  <- Http.parseRequest urlToCheck
    response <- Http.http (settings request) mgr
    let code = Http.statusCode (Http.responseStatus response)
    return $ code >= 200 && code < 300
  where
    -- Add additional request info
    settings r = r
        { Http.method         = "HEAD"
        , Http.redirectCount  = 10
        , Http.requestHeaders = ("User-Agent", ua) : Http.requestHeaders r
        }

    -- Nice user agent info
    ua = fromString $ "hakyll-check/" ++
        (intercalate "." $ map show $ versionBranch $ Paths_hakyll.version)

--------------------------------------------------------------------------------
-- | Wraps doesFileExist, also checks for index.html
checkFileExists :: FilePath -> Checker Bool
checkFileExists filePath = liftIO $ do
    file <- doesFileExist filePath
    dir  <- doesDirectoryExist filePath
    case (file, dir) of
        (True, _) -> return True
        (_, True) -> doesFileExist $ filePath </> "index.html"
        _         -> return False


--------------------------------------------------------------------------------
stripFragments :: String -> String
stripFragments = takeWhile (not . flip elem ['?', '#'])


--------------------------------------------------------------------------------
failedLinks :: CheckerState -> IO Int
failedLinks state = foldM addIfFailure 0 (M.elems state)
  where addIfFailure f mvar = do
          cwrite <- readMVar mvar
          return $ f + (checkerFaulty cwrite)
