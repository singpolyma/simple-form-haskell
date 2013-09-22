-- | Forms that configure themseles based on type
module SimpleForm (
	Widget,
	DefaultWidget(..),
	-- * Widgets
	hidden,
	checkbox,
	file,
	-- ** Text-like
	text,
	textarea,
	password,
	search,
	email,
	url,
	tel,
	-- ** Numbers
	number,
	integral,
	boundedNumber,
	boundedIntegral,
	-- ** Dates and times
	date,
	time,
	datetime,
	datetime_local,
	-- ** Collections
	select,
	-- ** Wrappers
	ShowRead(..),
	SelectEnum(..),
	-- * Options
	InputOptions(..),
	Label(..),
	-- * Rendering
	Renderer,
	RenderOptions(..),
	renderOptions,
	-- * Helpers
	input_tag,
	selectEnum,
	humanize,
	applyAttrs
) where

import Data.Maybe
import Data.Monoid
import Data.Ratio
import Data.Function (on)
import Data.Foldable (foldl', forM_)
import Data.List (nubBy)
import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Time (UTCTime, LocalTime, ZonedTime, Day, TimeOfDay, formatTime, FormatTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
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

instance DefaultWidget Char where
	wdef = text . fmap T.singleton

instance DefaultWidget Integer where
	wdef = integral

instance DefaultWidget Int where
	wdef = boundedIntegral

instance DefaultWidget Float where
	wdef = number

instance DefaultWidget Double where
	wdef = number

instance DefaultWidget UTCTime where
	wdef = datetime

instance DefaultWidget ZonedTime where
	wdef = datetime

instance DefaultWidget LocalTime where
	wdef = datetime_local

instance DefaultWidget Day where
	wdef = date

instance DefaultWidget TimeOfDay where
	wdef = time

instance (Integral a, Show a) => DefaultWidget (Ratio a) where
	wdef = number

instance (DefaultWidget a, DefaultWidget b) => DefaultWidget (a, b) where
	wdef v u n opt = wdef (fmap fst v) u n opt `mappend` wdef (fmap snd v) u n opt

instance (DefaultWidget a) => DefaultWidget (Maybe a) where
	wdef = wdef . join

-- | Wrapper for types that should be rendered using 'show'
newtype ShowRead a = ShowRead a

instance (Show a, Read a) => DefaultWidget (ShowRead a) where
	wdef = text . fmap (T.pack . show . unShowRead)
		where
		unShowRead (ShowRead x) = x

-- | Wrapper for select boxes on enumerable types
newtype SelectEnum a = SelectEnum a

-- | Derive a collection from an enumerable type
selectEnum :: (Show a, Read a, Bounded a, Enum a) => a -> [(Text, Text)]
selectEnum v = map (\x -> let x' = T.pack $ show x in (x',x')) [min..max]
	where
	min = minBound `asTypeOf` v
	max = maxBound `asTypeOf` v

instance (Show a, Read a, Bounded a, Enum a) => DefaultWidget (SelectEnum a) where
	wdef v = select (selectEnum $ fromJust v') (fmap (T.pack . show) v')
		where
		v' = fmap (\(SelectEnum x) -> x) v

type Widget a = (Maybe a -> Maybe Text -> Text -> InputOptions -> Html)

text :: Widget Text
text v u n = input_tag n (v <|> u) (T.pack "text") []

password :: Widget Text
password v u n = input_tag n (v <|> u) (T.pack "password") []

search :: Widget Text
search v u n = input_tag n (v <|> u) (T.pack "search") []

email :: Widget Text
email v u n = input_tag n (v <|> u) (T.pack "email") []

url :: Widget Text
url v u n = input_tag n (v <|> u) (T.pack "url") []

tel :: Widget Text
tel v u n = input_tag n (v <|> u) (T.pack "tel") []

number :: (Num a, Show a) => Widget a
number v u n = input_tag n (fmap (T.pack . show) v <|> u) (T.pack "number") [
		[(T.pack "step", T.pack "any")]
	]

integral :: (Integral a, Show a) => Widget a
integral v u n = input_tag n (fmap (T.pack . show) v <|> u) (T.pack "number") [
		[(T.pack "step", T.pack "1")]
	]

boundedNumber :: (Bounded a, Num a, Show a) => Widget a
boundedNumber v u n = input_tag n (fmap (T.pack . show) v <|> u) (T.pack "number") [
		[(T.pack "step", T.pack "any")],
		[(T.pack "min", T.pack $ show (minBound `asTypeOf` fromJust v))],
		[(T.pack "max", T.pack $ show (maxBound `asTypeOf` fromJust v))]
	]

boundedIntegral :: (Bounded a, Integral a, Show a) => Widget a
boundedIntegral v u n = input_tag n (fmap (T.pack . show) v <|> u) (T.pack "number") [
		[(T.pack "step", T.pack "1")],
		[(T.pack "min", T.pack $ show (minBound `asTypeOf` fromJust v))],
		[(T.pack "max", T.pack $ show (maxBound `asTypeOf` fromJust v))]
	]

textarea :: Widget Text
textarea v u n (InputOptions {disabled = d, required = r, input_html =    iattrs}) =
	applyAttrs [
		[(T.pack "disabled", T.pack "disabled") | d],
		[(T.pack "required", T.pack "required") | r],
		[(T.pack "rows", T.pack "10")],
		[(T.pack "cols", T.pack "55")]
	] iattrs (
		HTML.textarea ! HTML.name (toValue n) $
			maybe mempty HTML.toHtml (v <|> u)
	)

hidden :: Widget Text
hidden v u n = input_tag n (v <|> u) (T.pack "hidden") []

file :: Widget Text
file v u n = input_tag n (v <|> u) (T.pack "file") []

checkbox :: Widget Bool
checkbox v u n = input_tag n Nothing (T.pack "checkbox") [
		[(T.pack "checked", T.pack "checked") | isChecked]
	]
	where
	isChecked = fromMaybe (maybe False (/=mempty) u) v

date :: (FormatTime a) => Widget a
date v u n = input_tag n (fmap fmt v <|> u) (T.pack "date") []
	where
	fmt = T.pack . formatTime defaultTimeLocale format
	format = iso8601DateFormat Nothing

time :: (FormatTime a) => Widget a
time v u n = input_tag n (fmap fmt v <|> u) (T.pack "time") []
	where
	fmt = T.pack . formatTime defaultTimeLocale format
	format = "%H:%M:%S%Q"

datetime :: (FormatTime a) => Widget a
datetime v u n = input_tag n (fmap fmt v <|> u) (T.pack "datetime") []
	where
	fmt = T.pack . formatTime defaultTimeLocale format
	format = iso8601DateFormat $ Just "%H:%M:%S%Q%z"

datetime_local :: (FormatTime a) => Widget a
datetime_local v u n = input_tag n (fmap fmt v <|> u) (T.pack "datetime-local") []
	where
	fmt = T.pack . formatTime defaultTimeLocale format
	format = iso8601DateFormat $ Just "%H:%M:%S%Q"

select :: [(Text, Text)] -> Widget Text
select collection v u n (InputOptions {disabled = d, required = r, input_html = iattrs}) =
	applyAttrs [
		[(T.pack "disabled", T.pack "disabled") | d],
		[(T.pack "required", T.pack "required") | r]
	] iattrs (
		HTML.select ! HTML.name (toValue n) $
			forM_ collection $ \(value, label) ->
				mkSelected (Just value == v) $
				HTML.option ! HTML.value (toValue value) $
					HTML.toHtml label
	)

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

mkSelected :: Bool -> Html -> Html
mkSelected True = (! HTML.selected (toValue "selected"))
mkSelected False = id

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
