module Language.PureScript.Web
  ( compileTextToJS
  , printErrors
  ) where

import Language.PureScript
import Language.PureScript.CodeGen.JS
import Language.PureScript.CodeGen.JS.Printer
import Language.PureScript.CoreFn.Module
import Language.PureScript.CST

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bifunctor
import Data.IORef
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

inputPath :: FilePath
inputPath = "<input>"

compileTextToJS
  :: Text -> IO (MultipleErrors, Either MultipleErrors Text)
compileTextToJS input = do
  let (ws, m') = first warningsToMultipleErrors $ parseFromFile inputPath input
  case m' of
    Left es -> pure (ws, Left $ parserErrorsToMultipleErrors es)
    Right m -> do
      (r, ws') <- runMake webOptions $ do
        wma <- webMakeActions
        rebuildModule (wmaMakeActions wma) [] m
        maybe (fail "compileTextToJS: empty wmaOutputRef") pure =<<
          liftIO (readIORef $ wmaOutputRef wma)
      pure (ws <> ws', r)

webOptions :: Options
webOptions = Options False False (S.singleton JS)

data WebMakeActions m = WebMakeActions{
    wmaOutputRef   :: IORef (Maybe Text)
  , wmaMakeActions :: MakeActions m
  }

webMakeActions :: Make (WebMakeActions Make)
webMakeActions = do
  outputRef <- liftIO $ newIORef Nothing
  pure WebMakeActions{
      wmaOutputRef = outputRef
    , wmaMakeActions = MakeActions{
          getInputTimestampsAndHashes = fail "getInputTimestampsAndHashes: undefined"
        , getOutputTimestamp = fail "getOutputTimestamp: undefined"
        , readExterns = fail "readExterns: undefined"
        , codegen = \m _ _ -> do
            targets <- lift $ asks optionsCodegenTargets
            when (targets /= S.singleton JS) $
              fail "codegen: only JS target is supported"
            when (not $ null $ moduleForeign m) $
              fail "codegen: FFI is not supported"
            resJS <- prettyPrintJS <$> moduleToJs m Nothing
            lift $ liftIO $ writeIORef outputRef $ Just resJS
        , ffiCodegen = \_ -> pure ()
        , progress = liftIO . putStrLn . renderProgressMessage
        , readCacheDb = fail "readCacheDb: undefined"
        , writeCacheDb = fail "writeCacheDb: undefined"
        , outputPrimDocs = fail "outputPrimDocs: undefined"
        }
    }

warningsToMultipleErrors :: [ParserWarning] -> MultipleErrors
warningsToMultipleErrors =
  MultipleErrors . fmap (ErrorMessage [] . WarningParsingCSTModule)

parserErrorsToMultipleErrors :: NonEmpty ParserError -> MultipleErrors
parserErrorsToMultipleErrors =
  MultipleErrors . toList  . fmap (ErrorMessage [] . ErrorParsingCSTModule)

-- TODO: distinguish warnings
printErrors :: MultipleErrors -> IO ()
printErrors = putStrLn . prettyPrintMultipleErrors
  defaultPPEOptions{ ppeRelativeDirectory = "/" }

renderProgressMessage :: ProgressMessage -> String
renderProgressMessage (CompilingModule mn) =
  "Compiling " ++ T.unpack (runModuleName mn)
