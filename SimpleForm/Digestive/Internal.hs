{-# LANGUAGE GADTs #-}
-- | Torn from the internals of digestive-functors
module SimpleForm.Digestive.Internal (
	SimpleForm(..),
	SimpleFormEnv,
	input',
	getField,
	subView',
	fieldInputChoiceGroup',
	underRef
) where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Text.Blaze.Html (Html)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.List (isPrefixOf)
import Data.Functor.Identity (Identity)
import Control.Arrow (second)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Digestive.Form.Internal
import Text.Digestive.Form.Internal.Field
import Text.Digestive.Types
import Text.Digestive.View

import SimpleForm
import SimpleForm.Render

type SimpleFormEnv r = (Maybe r, View Html, (RenderOptions -> Html))

-- | A form for producing something of type r
newtype SimpleForm r a = SimpleForm (ReaderT (SimpleFormEnv r) (Writer Html) a)

instance Functor (SimpleForm r) where
	fmap = liftM

instance Applicative (SimpleForm r) where
	pure = return
	(<*>) = ap

instance Monad (SimpleForm r) where
	return = SimpleForm . return
	(SimpleForm x) >>= f = SimpleForm (x >>= (\v -> let SimpleForm r = f v in r))
	fail = SimpleForm . fail

instance MonadFix (SimpleForm r) where
	mfix f = SimpleForm (mfix $ unSimpleForm . f)
		where
		unSimpleForm (SimpleForm form) = form

instance (Monoid a) => Monoid (SimpleForm r a) where
	mempty = SimpleForm $ ReaderT (\_ -> tell mempty >> return mempty)
	(SimpleForm a) `mappend` (SimpleForm b) = SimpleForm $ ReaderT $ \env -> do
		a' <- runReaderT a env
		b' <- runReaderT b env
		return (a' `mappend` b')

input' ::
	Text              -- ^ Form element name
	-> (r -> Maybe a) -- ^ Get value from parsed data
	-> Widget a       -- ^ Widget to use (such as 'SimpleForm.wdef')
	-> InputOptions   -- ^ Other options
	-> SimpleFormEnv r
	-> Html
input' n sel w opt (env, view@(View {viewForm = form}), render) =
	render $ renderOptions
		(maybe Nothing sel env) unparsed (pathToText apth) w errors $
			opt {
				label = defaultLabel (label opt),
				disabled = disabled opt || Disabled `elem` metadata
			}
	where
	defaultLabel (Just DefaultLabel) = Just $ Label $ humanize n
	defaultLabel x = x
	apth = case absolutePath n view of
		(p:ps)
			| T.null p -> ps
			| otherwise -> p:ps
		_ -> []
	metadata = concatMap snd $ lookupFormMetadata [n] form
	errors = map snd $ filter ((==[n]) . fst) $ viewErrors view
	unparsed = getField [n] view

-- | Format form paths just like PHP/Rails
pathToText :: [Text] -> Text
pathToText [] = mempty
pathToText [p] = p
pathToText (p:ps) = mconcat (p : concatMap fragment ps)
	where
	fragment n = [
			T.singleton '[',
			n,
			T.singleton ']'
		]

getField :: Path -> View v -> Maybe Text
getField pth (View _ _ form input _ method) =
	queryField pth form (getField' method givenInput)
	where
	givenInput = lookupInput pth input

lookupInput :: Path -> [(Path, FormInput)] -> [FormInput]
lookupInput path = map snd . filter ((== path) . fst)

-- This function is why we need GADTs turned on :(
getField' ::
	Method		-- ^ Get/Post
	-> [FormInput]  -- ^ Given input
	-> Field v a	-- ^ Field
	-> Maybe Text   -- ^ Result
getField' _	_				 (Singleton _) = Nothing
getField' _	(TextInput x : _) (Text _)	  = Just x
getField' _	_				 (Text x)	  = Just x
getField' _	(TextInput x : _) (Choice _ _) = Just x
getField' _	_				 (Choice ls' x) =
	Just $ fst (concatMap snd ls' !! x)
getField' Get  _				 (Bool x)
	| x		 = Just (T.pack "on")
	| otherwise = Nothing
getField' Post (TextInput x : _) (Bool _)	  = Just x
getField' Post _				 (Bool _)	  = Nothing
getField' Post (FileInput x : _) File		  = Just (T.pack x)
getField' _	_				 File		  = Nothing

fieldInputChoiceGroup' ::
	Path
	-> View v
	-> [(Text, [(Text, v)])]
fieldInputChoiceGroup' path (View _ _ form input _ method) =
	map (second $ map (\(v,l,_) -> (v,l))) (queryField path form eval')
	where
	givenInput = lookupInput path input

	eval' :: Field v b -> [(Text, [(Text, v, Bool)])]
	eval' field = case field of
		Choice xs didx ->
			let idx = snd $ evalField method givenInput (Choice xs didx) in
				merge idx xs [0..]
		f -> error $ show path ++ ": expected (Choice _ _), " ++
			"but got: (" ++ show f ++ ")"

merge ::
	Int
	-> [(Text, [(Text, (a, v))])]
	-> [Int]
	-> [(Text, [(Text, v, Bool)])]
merge _ [] _ = []
merge idx (g:gs) is = cur : merge idx gs b
	where
	(a,b) = splitAt (length $ snd g) is
	cur = (fst g, map (\(i, (k, (_, v))) -> (k, v, i == idx)) $ zip a (snd g))

subView' :: Path -> View v -> View v
subView' path (View name ctx form input errs method) =
	case lookupForm path form of
		[] -> View name (ctx ++ path) notFound (strip input) (strip errs) method
		(SomeForm f : _) -> View name (ctx ++ path) f (strip input) (strip errs) method
	where
	lpath	= length path

	strip :: [(Path, a)] -> [(Path, a)]
	strip xs = [(drop lpath p, x) | (p, x) <- xs, path `isPrefixOf` p]

	notFound :: FormTree Identity v Identity a
	notFound = error $ "Text.Digestive.View.subView: " ++
		"No such subView: " ++ show path

underRef :: (Form v m a -> Form v m b) -> Form v m a -> Form v m b
underRef f (Ref r x) = Ref r (f x)
underRef f form = f form
