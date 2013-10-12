-- | Forms that configure themselves based on type
--
-- The Combined module both renders to 'Html' and also parses input.
module SimpleForm.Combined (
	Widget,
	DefaultWidget(..),
	Validation(..),
	DefaultValidation(..),
	-- * Options
	InputOptions(..),
	Label(..),
	-- * Wrappers
	ShowRead(..),
	unShowRead,
	SelectEnum(..),
	unSelectEnum,
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
	GroupedCollection',
	Collection',
	select,
	multi_select,
	radio_buttons,
	checkboxes,
	-- * Helpers
	selectEnum,
	enum,
	group_,
	multiEnum,
	humanize
) where

import Data.Time (FormatTime, ParseTime)
import Text.Email.Validate (EmailAddress)
import Network.URI (URI)

import Data.Text (Text)
import qualified Data.Text as T

import SimpleForm (DefaultWidget, Widget, InputOptions(..), Label(..), ShowRead(..), unShowRead, SelectEnum(..), unSelectEnum, humanize)
import SimpleForm.Validation (DefaultValidation, Validation(..), selectEnum, GroupedCollection', Collection', group_)
import qualified SimpleForm
import qualified SimpleForm.Validation as Validation

-- Orphan instances, but for our own classes

instance DefaultWidget EmailAddress where
	wdef = SimpleForm.email . fmap (T.pack . show)

instance DefaultWidget URI where
	wdef = SimpleForm.email . fmap (T.pack . show)

-- | Feed a collection 'Widget' and 'Validation' from an enumerable type
enum :: (Show a, Read a, Bounded a, Enum a) => (GroupedCollection' a -> (Widget Text, Validation a)) -> (Widget a, Validation a)
enum f = (w . fmap (T.pack . show), v)
	where
	(w,v) = f (group_ selectEnum)

-- | Feed a multi-select collection 'Widget' and 'Validation' from an enumerable type
multiEnum :: (Show a, Read a, Bounded a, Enum a) => (GroupedCollection' a -> (Widget [Text], Validation [a])) -> (Widget [a], Validation [a])
multiEnum f = (w . fmap (fmap (T.pack . show)), v)
	where
	(w,v) = f (group_ selectEnum)

text :: (Widget Text, Validation Text)
text = (SimpleForm.text, Validation.text)

password :: (Widget Text, Validation Text)
password = (SimpleForm.password, Validation.text)

search :: (Widget Text, Validation Text)
search = (SimpleForm.search, Validation.text)

email :: (Widget EmailAddress, Validation EmailAddress)
email = (SimpleForm.email . fmap (T.pack . show), Validation.email)

uri :: (Widget URI, Validation URI)
uri = (SimpleForm.uri . fmap (T.pack . show), Validation.uri)

tel :: (Widget Text, Validation Text)
tel = (SimpleForm.tel, Validation.text)

number :: (Num a, Show a, Read a) => (Widget a, Validation a)
number = (SimpleForm.number, Validation.read)

integral :: (Integral a, Show a, Read a) => (Widget a, Validation a)
integral = (SimpleForm.integral, Validation.read)

boundedNumber :: (Bounded a, Num a, Show a, Read a) => (Widget a, Validation a)
boundedNumber = (SimpleForm.boundedNumber, Validation.read)

boundedIntegral :: (Bounded a, Integral a, Show a, Read a) => (Widget a, Validation a)
boundedIntegral = (SimpleForm.boundedIntegral, Validation.read)

textarea :: (Widget Text, Validation Text)
textarea = (SimpleForm.textarea, Validation.text)

button :: (Widget Text, Validation Text)
button = (SimpleForm.textarea, Validation.text)

hidden :: (Widget Text, Validation Text)
hidden = (SimpleForm.hidden, Validation.text)

file :: (Widget Text, Validation Text)
file = (SimpleForm.file, Validation.text)

checkbox :: (Widget Bool, Validation Bool)
checkbox = (SimpleForm.checkbox, Validation.bool)

date :: (FormatTime a, ParseTime a) => (Widget a, Validation a)
date = (SimpleForm.date, Validation.date)

time :: (FormatTime a, ParseTime a) => (Widget a, Validation a)
time = (SimpleForm.time, Validation.time)

datetime :: (FormatTime a, ParseTime a) => (Widget a, Validation a)
datetime = (SimpleForm.datetime, Validation.datetime)

datetime_local :: (FormatTime a, ParseTime a) => (Widget a, Validation a)
datetime_local = (SimpleForm.datetime_local, Validation.datetime_local)

select :: GroupedCollection' a -> (Widget Text, Validation a)
select collection = (
		SimpleForm.select (Validation.viewGroupedCollection collection),
		Validation.includes collection
	)

multi_select :: GroupedCollection' a -> (Widget [Text], Validation [a])
multi_select collection = (
		SimpleForm.multi_select (Validation.viewGroupedCollection collection),
		Validation.multi_includes collection
	)

radio_buttons :: GroupedCollection' a -> (Widget Text, Validation a)
radio_buttons collection = (
		SimpleForm.radio_buttons (Validation.viewGroupedCollection collection),
		Validation.includes collection
	)

checkboxes :: GroupedCollection' a -> (Widget [Text], Validation [a])
checkboxes collection = (
		SimpleForm.checkboxes (Validation.viewGroupedCollection collection),
		Validation.multi_includes collection
	)
