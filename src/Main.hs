{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import           "glualint-lib" GLua.AG.Token (Region(..))
import qualified "glualint-lib" GLuaFixer.LintSettings as Settings
import qualified "glualint-lib" GLuaFixer.AG.ASTLint as Lint
import           "glualint-lib" GLuaFixer.LintMessage (LintMessage (..), sortLintMessages)
import qualified "glualint-lib" GLuaFixer.Util as Util
import qualified "glualint-lib" GLua.Parser as P
import qualified "glualint-lib" GLua.AG.PrettyPrint as PP
import GHCJS.Marshal.Internal(toJSValListOf)
import GHCJS.Marshal(fromJSVal)
import GHCJS.Foreign.Callback (Callback, syncCallback1, syncCallback1', OnBlocked(ContinueAsync))
import Data.JSString (JSString, pack)
import GHCJS.Types (JSVal, jsval)
import JavaScript.Object (create, setProp)
import System.IO.Unsafe(unsafePerformIO)

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

-- "getHello" test:
-- Assigns a haskell callback, getHello,  to a javascript function.
-- The getHello function constructs a javascript object and
-- returns it to the javascript caller.  The "js_getHello" function
-- is callable from javascript.

foreign import javascript unsafe "js_getHello = $1"
    set_getHelloCallback :: Callback a -> IO ()

getHelloTest = do
    let getHello inputJSStr = do
            Just str <- fromJSVal inputJSStr
            o <- create
            let !linted = lintString str
            case linted of
              Good -> setProp "helloworld" (jsval $ pack $ "good") o
              Warnings msgs -> setProp "helloworld" (unsafePerformIO (toJSValListOf (map (warn_msg) msgs))) o
              SyntaxErrors msgs -> setProp "helloworld" (jsval $ pack $ "errors") o
            --setProp "helloworld" (jsval $ pack $ "(get): hello, " ++ str) o
            return $ jsval o -- accessible from javascript caller.

    getHelloCallback <- syncCallback1' getHello
    set_getHelloCallback getHelloCallback

main = do
    getHelloTest
