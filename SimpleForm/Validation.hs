module SimpleForm.Validation (
	Validation(..),
	DefaultValidation(..),
	-- * Wrappers
	ShowRead(..),
	unShowRead,
	SelectEnum(..),
	unSelectEnum,
	-- * Validations
	bool,
	-- ** Text-like
	text,
	textLength,
	read,
	email,
	uri,
	absoluteUri,
	-- ** Dates and times
	dateFormat,
	date,
	time,
	datetime,
	datetime_local,
	-- ** Collections
	GroupedCollection',
	Collection',
	includes,
	multi_includes,
	-- * Helpers
	pmap,
	selectEnum,
	selectEnumIdx,
	enumIdx,
	multiEnum,
	multiEnumIdx,
	group_,
	viewGroupedCollection
) where

import Prelude hiding (read)
import Data.Fixed (Fixed, HasResolution)
import Control.Arrow (first, second)
import Control.Monad
import Data.Monoid
import Data.Ratio (Ratio)
import Data.Time (UTCTime, LocalTime, ZonedTime, Day, TimeOfDay, parseTime, ParseTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as EmailAddress

import Network.URI (URI)
import qualified Network.URI as URI

import SimpleForm (GroupedCollection, humanize, SelectEnum(..), unSelectEnum, ShowRead(..), unShowRead)

-- | 'GroupedCollection' including the parsed value
type GroupedCollection' a = [(Text, [(a, (Text, Text))])]

-- | 'Collection' including the parsed value
type Collection' a = [(a, (Text, Text))]

-- | Either try to parse the submitted values, or have a list of allowed values
data Validation a = Check ([Text] -> Maybe a) | Includes (GroupedCollection' a)

instance Functor Validation where
	fmap f (Includes xs) = Includes $ map (second $ map (first f)) xs
	fmap f (Check chk) = Check (chk >=> Just . f)

-- | Map over a 'Validation' with a partial function
pmap :: (a -> Maybe b) -> Validation a -> Validation b
pmap f (Includes xs) = Includes $ (`map` xs) $ second $
	foldr (\(x,s) b -> maybe b (\x' -> (x',s):b) (f x)) []
pmap f (Check chk) = Check (chk >=> f)

-- | Convert a 'GroupedCollection'' to a 'GroupedCollection' for use in a view
viewGroupedCollection :: GroupedCollection' a -> GroupedCollection
viewGroupedCollection = map (second $ map snd)

shw :: (Show a) => a -> Text
shw = T.pack . show

-- Infer a 'Validation' based on type
class DefaultValidation a where
	vdef :: Validation a
	vdefList :: Validation [a]
	vdefList = case vdef of
		Check f -> Check (mapM f . return)
		Includes c -> multi_includes c

instance (DefaultValidation a) => DefaultValidation [a] where
	vdef = vdefList

instance DefaultValidation Bool where
	vdef = bool
	vdefList = fmap (map (\(SelectEnum x) -> x)) vdefList

instance DefaultValidation Text where
	vdef = text

instance DefaultValidation Char where
	vdef = pmap (fmap fst . T.uncons) text
	vdefList = fmap T.unpack text -- Heh, hack for 'String'

instance DefaultValidation Integer where
	vdef = read

instance DefaultValidation Int where
	vdef = read
	vdefList = fmap (map (\(SelectEnum x) -> x)) vdefList

instance DefaultValidation Float where
	vdef = read

instance DefaultValidation Double where
	vdef = read

instance (HasResolution a) => DefaultValidation (Fixed a) where
	vdef = read

instance DefaultValidation UTCTime where
	vdef = datetime

instance DefaultValidation ZonedTime where
	vdef = datetime

instance DefaultValidation LocalTime where
	vdef = datetime_local

instance DefaultValidation Day where
	vdef = date

instance DefaultValidation TimeOfDay where
	vdef = time

instance (Integral a) => DefaultValidation (Ratio a) where
	vdef = fmap realToFrac (read :: Validation Double)

instance (DefaultValidation a) => DefaultValidation (Maybe a) where
	vdef = optional vdef

instance DefaultValidation EmailAddress where
	vdef = email

instance DefaultValidation URI where
	vdef = uri

instance (Read a) => DefaultValidation (ShowRead a) where
	vdef = read

instance (Show a, Read a, Bounded a, Enum a) => DefaultValidation (SelectEnum a) where
	vdef = enum includes
	vdefList = multiEnum multi_includes

-- | Derive a collection from an enumerable type
selectEnum :: (Show a, Read a, Bounded a, Enum a) => Collection' a
selectEnum = map (\x -> let x' = shw x in (x, (x', humanize x'))) opts
	where
	opts = [minBound..maxBound]

-- | Derive an indexed collection from an enumerable type
selectEnumIdx :: (Show a, Bounded a, Enum a) => Collection' a
selectEnumIdx = map (\(i,x) -> (x, (shw i, humanize $ shw x))) opts
	where
	opts = zip [(0::Int)..] [minBound..maxBound]

-- | Feed a collection 'Validation' from an enumerable type
enum :: (Show a, Read a, Bounded a, Enum a) => (GroupedCollection' a -> Validation a) -> Validation a
enum w = w (group_ selectEnum)

-- | Feed a multi-select collection 'Validation' from an enumerable type
multiEnum :: (Show a, Read a, Bounded a, Enum a) => (GroupedCollection' a -> Validation [a]) -> Validation [a]
multiEnum w = w (group_ selectEnum)

-- | Feed a collection 'Validation' from an enumerable type
enumIdx :: (Show a, Bounded a, Enum a) => (GroupedCollection' a -> Validation a) -> Validation a
enumIdx w = w (group_ selectEnumIdx)

-- | Feed a multi-select collection 'Validation' from an enumerable type
multiEnumIdx :: (Show a, Bounded a, Enum a) => (GroupedCollection' a -> Validation [a]) -> Validation [a]
multiEnumIdx w = w (group_ selectEnumIdx)

-- | Push any 'Collection'' to a trivial 'GroupedCollection''
group_ :: Collection' a -> GroupedCollection' a
group_ c = [(mempty, c)]

optional :: Validation a -> Validation (Maybe a)
optional (Check chk) = Check go
	where
	go t | null t || T.null (head t)  = Just Nothing
	     | otherwise = fmap Just (chk t)
optional (Includes _) =
	error "You cannot both validate against a list and be optional."

text :: Validation Text
text = Check go
	where
	go [x] = Just x
	go _ = Nothing

textLength :: Int -> Validation Text
textLength len = pmap go text
	where
	go t | T.length t <= len = Just t
	go _ = Nothing

read :: (Read a) => Validation a
read = pmap (go . reads . T.unpack) text
	where
	go [(x, "")] = Just x
	go _ = Nothing

email :: Validation EmailAddress
email = pmap (go . EmailAddress.validate . T.encodeUtf8) text
	where
	go (Left _) = Nothing
	go (Right email) = Just email

uri :: Validation URI
uri = pmap (URI.parseURIReference . T.unpack) text

absoluteUri :: Validation URI
absoluteUri = pmap (URI.parseAbsoluteURI . T.unpack) text

bool :: Validation Bool
bool = Check (Just . go)
	where
	go [x] | x /= mempty = True
	go _ = False

dateFormat :: (ParseTime a) => String -> Validation a
dateFormat fmt = pmap (parseTime defaultTimeLocale fmt . T.unpack) text

date :: (ParseTime a) => Validation a
date = dateFormat $ iso8601DateFormat Nothing

time :: (ParseTime a) => Validation a
time = dateFormat "%H:%M:%S%Q"

datetime :: (ParseTime a) => Validation a
datetime = dateFormat $ iso8601DateFormat $ Just "%H:%M:%S%Q%z"

datetime_local :: (ParseTime a) => Validation a
datetime_local = dateFormat $ iso8601DateFormat $ Just "%H:%M:%S%Q"

includes :: GroupedCollection' a -> Validation a
includes = Includes

-- TODO: This needs work
multi_includes :: GroupedCollection' a -> Validation [a]
multi_includes = Includes . map (second $ map (first return))
