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

data User = User {
		name :: Text,
		age :: Int
	} deriving (Show)

data Package = Package {
		user :: User,
		category :: Category
	} deriving (Show)

app :: Application
app req = ResponseBuilder ok200 headers . renderHtmlBuilder <$> do
	(html, pkg) <- postSimpleForm render (requestFormEnv_ req) $ do
		user' <- fieldset (s"user") user $ do
			name' <- input_ (s"name") (Just . name)
			age' <- input_ (s"age") (Just . age)
			return $ User <$> name' <*> age'
		category' <- input_ (s"category") (Just . SelectEnum . category)
		return $ Package <$> user' <*> fmap unSelectEnum category'

	return $ do
		toHtml $ show $ pkg
		form ! action (s"/") ! method (s"GET") $ html
	where
	headers = stringHeaders' [("Content-Type", "text/html; charset=utf-8")]

main :: IO ()
main = run 3000 app
