import Data.Monoid
import Control.Applicative
import Data.Text (Text)
import Data.String

import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.XHtml5
import Text.Blaze.XHtml5.Attributes hiding (form, name)

import Text.Digestive

import SimpleForm hiding (text, name)
import SimpleForm.Digestive
import SimpleForm.Render.XHTML5

s :: (IsString s) => String -> s
s = fromString

-- Setup a digestive-functors form

data Category = Web | Text | Math deriving (Bounded, Enum, Eq, Show)

data Package = Package {
		name :: Text,
		category :: Category
	} deriving (Show)

packageForm :: Monad m => Form Text m Package
packageForm = Package
	<$> s"name"     .: text Nothing
	<*> s"category" .: choice categories Nothing
	where
	categories = fmap (\x -> (x, s (show x))) [minBound .. maxBound]

main = do
	view <- getForm mempty packageForm
	putStrLn $ renderHtml $
		form ! action (s "/post/here") ! method (s"POST") $
			simpleForm render (view, Nothing) $ do
				input_ (s"name") (Just . name)
				choiceInput_ (s"category") (Just . s . show . category)
