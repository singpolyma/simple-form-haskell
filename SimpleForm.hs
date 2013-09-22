-- | Forms that configure themseles based on type
module SimpleForm (
	Widget,
	DefaultWidget(..),
	-- * Widgets
	text,
	checkbox,
	-- * Options
	InputOptions(..),
	Label(..),
	-- * Rendering
	Renderer,
	RenderOptions(..),
	renderOptions,
	-- * Helpers
	input_tag,
	humanize,
	applyAttrs
) where

import Data.Maybe
import Data.Monoid
import Data.Function (on)
import Data.Foldable (foldl')
import Data.List (nubBy)
import Control.Applicative ((<|>))
import Text.Blaze.XHtml5 (Html, (!), toValue)
import qualified Text.Blaze.XHtml5 as HTML
import qualified Text.Blaze.XHtml5.Attributes as HTML hiding (label, span)
import Data.Text (Text)
import qualified Data.Text as T
import Data.String

-- | A block label, inline label, or implied value label
data Label = Label Text | InlineLabel Text | DefaultLabel
	deriving (Show, Eq)

instance IsString Label where
	fromString = Label . fromString

type Renderer = (RenderOptions -> Html)

-- | 'InputOptions' that have been prepped for rendering
data RenderOptions = RenderOptions {
		name :: Text,
		widgetHtml :: Html,
		errors :: [Html],
		options :: InputOptions
	}

-- | Prep 'InputOptions' for rendering
renderOptions ::
	Maybe a          -- ^ The parsed value for this input (if available)
	-> Maybe Text    -- ^ The unparsed value for this input (if available)
	-> Text          -- ^ The name of this input
	-> Widget a      -- ^ Widget to render with
	-> [Html]        -- ^ Any error messages for this input
	-> InputOptions
	-> RenderOptions
renderOptions v u n w errors opt = RenderOptions {
		name = n,
		widgetHtml = w v u n opt,
		errors = errors,
		options = opt
	}

-- | The setup for rendering an input. Blank is 'mempty', default is 'def'.
data InputOptions = InputOptions {
		label :: Maybe Label,
		hint :: Maybe Text,
		required :: Bool,
		disabled :: Bool,
		input_html :: [(Text,Text)],
		label_html :: [(Text,Text)],
		error_html :: [(Text,Text)],
		hint_html :: [(Text,Text)],
		wrapper_html :: [(Text,Text)]
	} deriving (Show, Eq)

instance Monoid InputOptions where
	mempty = InputOptions {
		label = Just DefaultLabel,
		hint = Nothing,
		required = True,
		disabled = False,
		input_html = [],
		label_html = [],
		error_html = [],
		hint_html = [],
		wrapper_html = []
	}

	mappend a b = InputOptions {
		label = if label b == Just DefaultLabel then label a else label b,
		hint = monoidOr (hint b) (hint a),
		required = if required b then required a else required b,
		disabled = if not (disabled b) then disabled a else disabled b,
		input_html = input_html a ++ input_html b,
		label_html = label_html a ++ label_html b,
		error_html = error_html a ++ error_html b,
		hint_html = hint_html a ++ hint_html b,
		wrapper_html = wrapper_html a ++ wrapper_html b
	}

monoidOr :: (Monoid a, Eq a) => a -> a -> a
monoidOr a b
	| a == mempty = b
	| otherwise = a

-- | Format identifiers nicely for humans to read
humanize :: Text -> Text
humanize = id -- TODO

-- | Infer a widget based on type
class DefaultWidget a where
	wdef :: Widget a

instance DefaultWidget Bool where
	wdef = checkbox

instance DefaultWidget Text where
	wdef = text

type Widget a = (Maybe a -> Maybe Text -> Text -> InputOptions -> Html)

text :: Widget Text
text v u n = input_tag n (v <|> u) (T.pack "text") []

checkbox :: Widget Bool
checkbox v u n = input_tag n Nothing (T.pack "checkbox") [
		[(T.pack "checked", T.pack "checked") | isChecked]
	]
	where
	isChecked = fromMaybe (maybe False (/=mempty) u) v

-- | <input />
input_tag ::
	Text               -- ^ name
	-> Maybe Text      -- ^ textual value
	-> Text            -- ^ type
	-> [[(Text,Text)]] -- ^ Extra default attributes
	-> InputOptions    -- ^ Attributes from options override defaults
	-> Html
input_tag n v t dattr (InputOptions {disabled = d, required = r, input_html = iattrs}) =
	applyAttrs [
		[(T.pack "disabled", T.pack "disabled") | d],
		[(T.pack "required", T.pack "required") | r],
		[(T.pack "type", t)],
		concat dattr
	] iattrs $ maybe id (\v' h -> h ! HTML.value (toValue v')) v (
		HTML.input !
			HTML.name (toValue n)
	)

mkAttribute :: (Text,Text) -> HTML.Attribute
mkAttribute (k,v) = HTML.customAttribute (HTML.textTag k) (toValue v)

-- | Apply a list of default attributes and user overrides to some 'Html'
applyAttrs ::
	[[(Text,Text)]]  -- ^ Defaults
	-> [(Text,Text)] -- ^ User overrides
	-> Html          -- ^ Apply attributes to this 'Html'
	-> Html
applyAttrs dattr cattr html = foldl' (!) html (map mkAttribute attrs)
	where
	attrs = nubBy ((==) `on` fst) attrsWithClass
	attrsWithClass
		| null classes = attrs'
		| otherwise = (T.pack "class", T.unwords classes):attrs'
	classes = concatMap (T.words . snd) $ filter ((== T.pack "class") . fst) attrs'
	attrs' = cattr ++ concat dattr
