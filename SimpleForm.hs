-- | Forms that configure themselves based on type
module SimpleForm (
	Widget,
	DefaultWidget(..),
	Input(..),
	-- * Options
	InputOptions(..),
	Label(..),
	-- * Wrappers
	ShowRead(..),
	SelectEnum(..),
	-- * Widgets
	button,
	hidden,
	checkbox,
	file,
	-- ** Text-like
	text,
	textarea,
	password,
	search,
	email,
	uri,
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
	GroupedCollection,
	Collection,
	select,
	multi_select,
	radio_buttons,
	checkboxes,
	-- * Helpers
	input_tag,
	selectEnum,
	enum,
	group_,
	multiEnum,
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
import qualified Text.Blaze.XHtml5.Attributes as HTMLA
import Data.Text (Text)
import qualified Data.Text as T
import Data.String

data Input = Input Html | MultiInput [Html] | SelfLabelInput Html

instance Monoid Input where
	mempty = Input mempty
	(Input x) `mappend` (Input y) = MultiInput [x,y]
	(Input x) `mappend` (MultiInput y) = MultiInput (x:y)
	(MultiInput x) `mappend` (Input y) = MultiInput (x ++ [y])
	(MultiInput x) `mappend` (MultiInput y) = MultiInput (x ++ y)
	(SelfLabelInput x) `mappend` y = (Input x) `mappend` y
	x `mappend` (SelfLabelInput y) = x `mappend` (Input y)

-- | A block label, inline label, or implied value label
data Label = Label Text | InlineLabel Text | DefaultLabel
	deriving (Show, Eq)

instance IsString Label where
	fromString = Label . fromString

-- | The setup for rendering an input. Blank is 'Data.Monoid.mempty'
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

-- | Infer a 'Widget' based on type
class DefaultWidget a where
	wdef :: Widget a
	wdefList :: Widget [a]
	wdefList _ _ _ _ =
		-- Some things just can't be multi-selected (like Text)
		Input $ HTML.p $ HTML.toHtml "No useful multi-select box for this type."

instance (DefaultWidget a) => DefaultWidget [a] where
	wdef = wdefList

instance DefaultWidget Bool where
	wdef = checkbox
	wdefList = wdefList . fmap (map SelectEnum)

instance DefaultWidget Text where
	wdef = text

instance DefaultWidget Char where
	wdef = text . fmap T.singleton
	wdefList = text . fmap T.pack -- Heh, hack for 'String'

instance DefaultWidget Integer where
	wdef = integral

instance DefaultWidget Int where
	wdef = boundedIntegral
	wdefList = wdefList . fmap (map SelectEnum)

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

instance (Show a, Read a) => Show (ShowRead a) where
	show (ShowRead x) = show x

instance (Read a) => Read (ShowRead a) where
	readsPrec n s = map (\(v,s') -> (ShowRead v, s')) (readsPrec n s)

instance (Show a, Read a) => DefaultWidget (ShowRead a) where
	wdef = text . fmap (T.pack . show)

-- | Wrapper for select boxes on enumerable types
newtype SelectEnum a = SelectEnum a

instance (Show a, Read a) => Show (SelectEnum a) where
	show (SelectEnum x) = show x

instance (Read a) => Read (SelectEnum a) where
	readsPrec n s = map (\(v,s') -> (SelectEnum v, s')) (readsPrec n s)

instance (Bounded a) => Bounded (SelectEnum a) where
	minBound = SelectEnum minBound
	maxBound = SelectEnum maxBound

instance (Enum a) => Enum (SelectEnum a) where
	toEnum = SelectEnum . toEnum
	fromEnum (SelectEnum x) = fromEnum x

-- | Collection of items for the user to choose from, with optional grouping
--
-- A trivial 'GroupedCollection' (with just one, blankly-named group)
-- should be treated by 'Widget's as if it were just a 'Collection'
type GroupedCollection = [(Text, [(Text, Text)])]

-- | Collection of items for the user to choose from
type Collection = [(Text, Text)]

-- | Derive a collection from an enumerable type
selectEnum :: (Show a, Read a, Bounded a, Enum a) => a -> Collection
selectEnum v = map (\x -> let x' = T.pack $ show x in (x', humanize x')) opts
	where
	opts = [minBound `asTypeOf` v .. maxBound `asTypeOf` v]

-- | Feed a collection 'Widget' from an enumerable type
enum :: (Show a, Read a, Bounded a, Enum a) => (GroupedCollection -> Widget Text) -> Widget a
enum w v = w (group_ $ selectEnum $ fromJust v) (fmap (T.pack . show) v)

-- | Feed a multi-select collection 'Widget' from an enumerable type
multiEnum :: (Show a, Read a, Bounded a, Enum a) => (GroupedCollection -> Widget [Text]) -> Widget [a]
multiEnum w v = w (group_ $ selectEnum $ head $ fromJust v) (fmap (fmap (T.pack . show)) v)

-- | Push any 'Collection' to a trivial 'GroupedCollection'
group_ :: Collection -> GroupedCollection
group_ c = [(mempty, c)]

instance (Show a, Read a, Bounded a, Enum a) => DefaultWidget (SelectEnum a) where
	wdef = enum select
	wdefList = multiEnum multi_select

-- | The type of a widget renderer
type Widget a = (Maybe a -> Maybe Text -> Text -> InputOptions -> Input)

text :: Widget Text
text v u n = Input . input_tag n (v <|> u) (T.pack "text") []

password :: Widget Text
password v u n = Input . input_tag n (v <|> u) (T.pack "password") []

search :: Widget Text
search v u n = Input . input_tag n (v <|> u) (T.pack "search") []

email :: Widget Text
email v u n = Input . input_tag n (v <|> u) (T.pack "email") []

uri :: Widget Text
uri v u n = Input . input_tag n (v <|> u) (T.pack "url") []

tel :: Widget Text
tel v u n = Input . input_tag n (v <|> u) (T.pack "tel") []

number :: (Num a, Show a) => Widget a
number v u n =
	Input . input_tag n (fmap (T.pack . show) v <|> u) (T.pack "number") [
		[(T.pack "step", T.pack "any")]
	]

integral :: (Integral a, Show a) => Widget a
integral v u n =
	Input . input_tag n (fmap (T.pack . show) v <|> u) (T.pack "number") [
		[(T.pack "step", T.pack "1")]
	]

boundedNumber :: (Bounded a, Num a, Show a) => Widget a
boundedNumber v u n =
	Input . input_tag n (fmap (T.pack . show) v <|> u) (T.pack "number") [
		[(T.pack "step", T.pack "any")],
		[(T.pack "min", T.pack $ show (minBound `asTypeOf` fromJust v))],
		[(T.pack "max", T.pack $ show (maxBound `asTypeOf` fromJust v))]
	]

boundedIntegral :: (Bounded a, Integral a, Show a) => Widget a
boundedIntegral v u n =
	Input . input_tag n (fmap (T.pack . show) v <|> u) (T.pack "number") [
		[(T.pack "step", T.pack "1")],
		[(T.pack "min", T.pack $ show (minBound `asTypeOf` fromJust v))],
		[(T.pack "max", T.pack $ show (maxBound `asTypeOf` fromJust v))]
	]

textarea :: Widget Text
textarea v u n (InputOptions {disabled = d, required = r, input_html =    iattrs}) = Input $
	applyAttrs [
		[(T.pack "disabled", T.pack "disabled") | d],
		[(T.pack "required", T.pack "required") | r],
		[(T.pack "rows", T.pack "10")],
		[(T.pack "cols", T.pack "55")]
	] iattrs (
		HTML.textarea ! HTML.name (toValue n) $
			maybe mempty HTML.toHtml (v <|> u)
	)

button :: Widget Text
button v u n (InputOptions {label = l, disabled = d, input_html = iattrs}) = SelfLabelInput $
	applyAttrs [
		[(T.pack "disabled", T.pack "disabled") | d],
		[(T.pack "type", T.pack "submit")]
	] iattrs $ maybe id (\v' h -> h ! HTML.value (toValue v')) v (
		HTML.button ! HTML.name (toValue n) $
			maybe mempty (HTML.toHtml . getLabel) l
	)
	where
	getLabel (Label s) = s
	getLabel (InlineLabel s) = s
	getLabel DefaultLabel = humanize n

hidden :: Widget Text
hidden v u n = SelfLabelInput . input_tag n (v <|> u) (T.pack "hidden") []

file :: Widget Text
file v u n = Input . input_tag n (v <|> u) (T.pack "file") []

checkbox :: Widget Bool
checkbox v u n = Input . input_tag n Nothing (T.pack "checkbox") [
		[(T.pack "checked", T.pack "checked") | isChecked]
	]
	where
	isChecked = fromMaybe (maybe False (/=mempty) u) v

date :: (FormatTime a) => Widget a
date v u n = Input . input_tag n (fmap fmt v <|> u) (T.pack "date") []
	where
	fmt = T.pack . formatTime defaultTimeLocale format
	format = iso8601DateFormat Nothing

time :: (FormatTime a) => Widget a
time v u n = Input . input_tag n (fmap fmt v <|> u) (T.pack "time") []
	where
	fmt = T.pack . formatTime defaultTimeLocale format
	format = "%H:%M:%S%Q"

datetime :: (FormatTime a) => Widget a
datetime v u n = Input . input_tag n (fmap fmt v <|> u) (T.pack "datetime") []
	where
	fmt = T.pack . formatTime defaultTimeLocale format
	format = iso8601DateFormat $ Just "%H:%M:%S%Q%z"

datetime_local :: (FormatTime a) => Widget a
datetime_local v u n =
	Input . input_tag n (fmap fmt v <|> u) (T.pack "datetime-local") []
	where
	fmt = T.pack . formatTime defaultTimeLocale format
	format = iso8601DateFormat $ Just "%H:%M:%S%Q"

select :: GroupedCollection -> Widget Text
select collection v u n (InputOptions {disabled = d, required = r, input_html = iattrs}) = Input $
	applyAttrs [
		[(T.pack "disabled", T.pack "disabled") | d],
		[(T.pack "required", T.pack "required") | r]
	] iattrs (
		HTML.select ! HTML.name (toValue n) $
			formatCollection $ \subCollection ->
				forM_ subCollection $ \(value, label) ->
					mkSelected (Just value == v) $
					HTML.option ! HTML.value (toValue value) $
						HTML.toHtml label
	)
	where
	formatCollection f
		| length collection == 1 && fst (head collection) == mempty =
			f (snd $ head collection)
		| otherwise =
			forM_ collection $ \(group, subCollection) ->
				HTML.optgroup ! HTMLA.label (toValue group) $
					f subCollection

multi_select :: GroupedCollection -> Widget [Text]
multi_select collection v u n (InputOptions {disabled = d, required = r, input_html = iattrs}) = Input $
	applyAttrs [
		[(T.pack "disabled", T.pack "disabled") | d],
		[(T.pack "required", T.pack "required") | r]
	] iattrs (
		HTML.select ! HTML.name (toValue n) ! HTML.multiple (toValue "multiple") $
			formatCollection $ \subCollection ->
				forM_ subCollection $ \(value, label) ->
					mkSelected (value `elem` items) $
					HTML.option ! HTML.value (toValue value) $
						HTML.toHtml label
	)
	where
	items = fromMaybe [] v
	formatCollection f
		| length collection == 1 && fst (head collection) == mempty =
			f (snd $ head collection)
		| otherwise =
			forM_ collection $ \(group, subCollection) ->
				HTML.optgroup ! HTMLA.label (toValue group) $
					f subCollection

radio_buttons :: GroupedCollection -> Widget Text
radio_buttons collection v u n opt =
	MultiInput $ formatCollection $ map radio
	where
	radio (value, label) = HTML.label $ do
		mkChecked (Just value == v) $
			input_tag n (Just value) (T.pack "radio") [] opt
		HTML.toHtml label
	formatCollection f
		| length collection == 1 && fst (head collection) == mempty =
			f (snd $ head collection)
		| otherwise =
			(`map` collection) $ \(group, subCollection) ->
				HTML.fieldset $ do
					HTML.legend $ HTML.toHtml group
					mconcat (f subCollection)

checkboxes :: GroupedCollection -> Widget [Text]
checkboxes collection v u n opt =
	MultiInput $ formatCollection $ map check
	where
	items = fromMaybe [] v
	check (value, label) = HTML.label $ do
		mkChecked (value `elem` items) $
			input_tag n (Just value) (T.pack "checkbox") [] opt
		HTML.toHtml label
	formatCollection f
		| length collection == 1 && fst (head collection) == mempty =
			f (snd $ head collection)
		| otherwise =
			(`map` collection) $ \(group, subCollection) ->
				HTML.fieldset $ do
					HTML.legend $ HTML.toHtml group
					mconcat (f subCollection)

-- | \<input /\>
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

mkChecked :: Bool -> Html -> Html
mkChecked True = (! HTML.checked (toValue "checked"))
mkChecked False = id

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
