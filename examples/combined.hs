import Data.Monoid
import Control.Applicative
import Data.Text (Text)
import Data.String

import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.XHtml5
import Text.Blaze.XHtml5.Attributes hiding (form, name)

import SimpleForm.Combined hiding (text, name)
import SimpleForm.Digestive.Combined
import SimpleForm.Render.XHTML5

s :: (IsString s) => String -> s
s = fromString

data Category = Web | Text | Math deriving (Bounded, Enum, Eq, Show, Read)

data Package = Package {
		name :: Text,
		category :: Category
	} deriving (Show)

main = putStrLn =<< renderHtml <$>
	form ! action (s "/post/here") ! method (s"POST") <$>
		getSimpleForm render Nothing (do
			name' <- input_ (s"name") (Just . name)
			category' <- input_ (s"category") (Just . SelectEnum . category)
			return $ Package <$> name' <*> fmap unSelectEnum category'
		)
