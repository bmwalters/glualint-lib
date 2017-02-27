{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import           "glualint-lib" GLua.AG.Token (Region(..))
import qualified "glualint-lib" GLuaFixer.LintSettings as Settings
import qualified "glualint-lib" GLuaFixer.AG.ASTLint as Lint
import           "glualint-lib" GLuaFixer.LintMessage (LintMessage (..), sortLintMessages)
import qualified "glualint-lib" GLuaFixer.Util as Util
import qualified "glualint-lib" GLua.Parser as P
import qualified "glualint-lib" GLua.AG.PrettyPrint as PP
import GHCJS.Marshal.Internal(toJSVal, toJSValListOf)
import GHCJS.Marshal.Pure(pToJSVal)
import GHCJS.Marshal(fromJSVal, toJSVal)
import GHCJS.Foreign.Callback (Callback, syncCallback1, syncCallback1', OnBlocked(ContinueAsync))
import Data.JSString (JSString, pack)
import GHCJS.Types (JSVal, jsval)
import JavaScript.Object (create, setProp)
import System.IO.Unsafe(unsafePerformIO)
import Text.ParserCombinators.UU.BasicInstances(LineColPos(..))

data LintStatus =
    Good -- Good code, no problems!
  | Warnings [LintMessage] -- Shit compiles, but you should worry about some things
  | SyntaxErrors [LintMessage] -- Syntax errors!

defConfig :: Settings.LintSettings
defConfig = Settings.defaultLintSettings

lintString :: String -> LintStatus
lintString str = do
  let parsed = Util.parseFile defConfig "input" str

  case parsed of
    Right ([], ast) -> case Lint.astWarnings defConfig ast of
      [] -> Good
      xs -> Warnings $ map ($"input") xs
    Right (warnings, _) -> Warnings warnings
    Left errs -> SyntaxErrors errs

prettyPrint :: String -> String
prettyPrint lua =
  let
    parsed = P.parseGLuaFromString lua
    ast = fst parsed
    ppconf = Settings.lint2ppSetting defConfig
    pretty = PP.prettyprintConf ppconf $ ast
  in
    pretty

jsObjectFromLintMessage :: LintMessage -> IO JSVal
jsObjectFromLintMessage (LintError (Region (LineColPos sline spos sabs) (LineColPos eline epos eabs)) msg _) = do
  o <- create
  setProp "type" (jsval $ pack $ "error") o
  setProp "msg" (jsval $ pack msg) o
  setProp "startLine" (pToJSVal sline) o
  setProp "startPos" (pToJSVal spos) o
  setProp "startAbs" (pToJSVal sabs) o
  setProp "endLine" (pToJSVal eline) o
  setProp "endPos" (pToJSVal epos) o
  setProp "endAbs" (pToJSVal eabs) o
  return $ jsval o

jsObjectFromLintMessage (LintWarning (Region (LineColPos sline spos sabs) (LineColPos eline epos eabs)) msg _) = do
  o <- create
  setProp "type" (jsval $ pack $ "warning") o
  setProp "msg" (jsval $ pack msg) o
  setProp "startLine" (pToJSVal sline) o
  setProp "startPos" (pToJSVal spos) o
  setProp "startAbs" (pToJSVal sabs) o
  setProp "endLine" (pToJSVal eline) o
  setProp "endPos" (pToJSVal epos) o
  setProp "endAbs" (pToJSVal eabs) o
  return $ jsval o

foreign import javascript unsafe "gluaLintString = $1" set_gluaLintString :: Callback a -> IO ()

exposeLintFunction = do
    let lintJSString inputJSStr = do
            Just str <- fromJSVal inputJSStr
            let !linted = lintString str
            case linted of
              Good -> return (jsval $ pack $ "good")
              Warnings msgs -> return $ unsafePerformIO $ toJSValListOf $ map (unsafePerformIO . jsObjectFromLintMessage) msgs
              SyntaxErrors msgs -> return $ unsafePerformIO $ toJSValListOf $ map (unsafePerformIO . jsObjectFromLintMessage) msgs

    gluaLintStringCallback <- syncCallback1' lintJSString
    set_gluaLintString gluaLintStringCallback

foreign import javascript unsafe "gluaPrettyPrintString = $1" set_gluaPrettyPrintString :: Callback a -> IO ()

exposePrettyPrintFunction = do
  let prettyPrintJSString inputJSStr = do
            Just str <- fromJSVal inputJSStr
            return $ jsval $ pack $ prettyPrint $ str

  gluaPrettyPrintStringCallback <- syncCallback1' prettyPrintJSString
  set_gluaPrettyPrintString gluaPrettyPrintStringCallback

main = do
    exposeLintFunction
    exposePrettyPrintFunction
