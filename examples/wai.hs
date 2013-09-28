-- This example also depends on wai-digestive-functors
module Main (main) where

import Control.Applicative
import Data.String (IsString, fromString)
import Data.Text (Text)

import Network.Wai (Application, Response(ResponseBuilder))
import Network.Wai.Util (stringHeaders')
import Network.Wai.Handler.Warp (run)
import Network.Wai.Digestive (bodyFormEnv_)
import Network.HTTP.Types (ok200)

import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Blaze.XHtml5
import Text.Blaze.XHtml5.Attributes hiding (form, name)

import SimpleForm.Combined hiding (text)
import SimpleForm.Digestive.Combined
import SimpleForm.Render.XHTML5

import Debug.Trace

s :: (IsString s) => String -> s
s = fromString

data Category = Web | Text | Math deriving (Bounded, Enum, Eq, Show, Read)

data Package = Package {
		name :: Text,
		category :: Category
	} deriving (Show)

app :: Application
app req = ResponseBuilder ok200 headers . renderHtmlBuilder <$> do
	(html, pkg) <- postSimpleForm render (bodyFormEnv_ req) $ do
		name' <- input_ (s"name") (Just . name)
		category' <- input_ (s"category") (Just . SelectEnum . category)
		return $ Package <$> name' <*> fmap unSelectEnum category'

	return $ do
		toHtml $ show $ pkg
		form ! action (s "/") ! method (s"POST") $ html
	where
	headers = stringHeaders' [("Content-Type", "text/html; charset=utf-8")]

main :: IO ()
main = run 3000 app
