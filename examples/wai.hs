-- This example also depends on wai-digestive-functors
module Main (main) where

import Control.Applicative
import Data.Monoid
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

s :: (IsString s) => String -> s
s = fromString

-- | Enumerable type for package categories
data Category = Web | Text | Math deriving (Bounded, Enum, Eq, Show, Read)

-- | The type of a Package (this is what our form will produce)
data Package = Package {
		name :: Text,
		category :: Category
	} deriving (Show)

app :: Application
app req = do
	-- Run the given form in the presence of params from the HTTP request body
	-- Produces the 'Html' of the form, and also 'Maybe Package'
	(html, pkg) <- postSimpleForm render (bodyFormEnv_ req) $ do
		-- Add the input for name, and get the parser for the associated field
		name' <- input_ (s"name") (Just . name)
		-- Add the input for category, and get the parser for the associated field
		-- The field will use 'SelectEnum' to generate the options automatically
		-- from the enumerated type.
		category' <- input_ (s"category") (Just . SelectEnum . category)
		-- Build up the parser for 'Package' from the parsers for each field
		return $ Package <$> name' <*> fmap unSelectEnum category'

	-- Create a WAI 'Response' that shows the parse result and the form
	return $ ResponseBuilder ok200 headers $ renderHtmlBuilder $ mconcat [
			toHtml $ show $ pkg, -- Show parse result
			form ! action (s "/") ! method (s"POST") $ html -- and form
		]
	where
	headers = stringHeaders' [("Content-Type", "text/html; charset=utf-8")]

main :: IO ()
main = run 3000 app
