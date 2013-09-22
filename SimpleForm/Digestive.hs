-- | SimpleForm implementation that works along with digestive-functors
module SimpleForm.Digestive (
	SimpleForm,
	simpleForm,
	-- * Create forms
	input,
	input_,
	toForm,
	-- * Subforms
	withFields,
	wrap,
	fieldset
) where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Html (Html, ToMarkup, toHtml)
import qualified Text.Blaze.XHtml5 as HTML

import Text.Digestive.View
import Text.Digestive.Form.Internal (Metadata(Disabled), lookupFormMetadata)
import SimpleForm
import SimpleForm.Digestive.Internal

type SimpleFormEnv r = (Maybe r, View Html, (RenderOptions -> Html))

-- | The type of a form
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

-- | Render a 'SimpleForm' to 'Html'
simpleForm :: (ToMarkup v) =>
	(RenderOptions -> Html) -- ^ Renderer
	-> (View v, Maybe a)    -- ^ Results of running a digestive-functors 'Form'
	-> SimpleForm a ()      -- ^ The simple form to render
	-> Html
simpleForm render (view, val) (SimpleForm form) =
	execWriter $ runReaderT form (val, fmap toHtml view, render)

-- | Add some raw markup to a 'SimpleForm'
toForm :: (ToMarkup h) => h -> SimpleForm a ()
toForm = SimpleForm . lift . tell . toHtml

-- | Create an input element for a 'SimpleForm'
input ::
	Text                        -- ^ Form element name
	-> (r -> Maybe a)           -- ^ Get value from parsed data
	-> InputOptions a           -- ^ Other options (base off 'SimpleForm.def')
	-> SimpleForm r ()
input n sel opt = SimpleForm $ ReaderT $ tell . input' n sel opt

-- | Same as 'input', but just use the default options
input_ :: (DefaultInputOptions a) =>
	Text                        -- ^ Form element name
	-> (r -> Maybe a)           -- ^ Get value from parsed data
	-> SimpleForm r ()
input_ n sel = input n sel def

input' ::
	Text                        -- ^ Form element name
	-> (r -> Maybe a)           -- ^ Get value from parsed data
	-> InputOptions a           -- ^ Other options (base off 'def')
	-> SimpleFormEnv r
	-> Html
input' n sel opt (env, view@(View {viewForm = form}), render) =
	render $ renderInputOptions unparsed errors $
		opt {
			name = pathToText apth,
			value = maybe Nothing sel env,
			disabled = disabled opt || Disabled `elem` metadata
		}
	where
	apth = case absolutePath n view of
		(p:ps)
			| T.null p -> ps
			| otherwise -> (p:ps)
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

-- | Project out some part of the parsed data
withFields ::
	Maybe Text     -- ^ Optional subview name
	-> (r' -> r)   -- ^ Projection function
	-> SimpleForm r a
	-> SimpleForm r' a
withFields n f (SimpleForm reader) = SimpleForm $
	withReaderT (\(r, view, render) ->
		(fmap f r, maybe view (`subView'` view) (fmap (:[]) n), render)
	) reader

-- | Wrap a 'SimpleForm' in an 'Html' tag
wrap :: (Html -> Html) -> SimpleForm r a -> SimpleForm r a
wrap f (SimpleForm reader) = SimpleForm $ ReaderT $ \env ->
	let (a, w) = runWriter (runReaderT reader env) in
	tell (f w) >> return a

-- | Like 'withFields', but also wrap in fieldset tag
fieldset :: Maybe Text -> (r' -> r) -> SimpleForm r a -> SimpleForm r' a
fieldset n f = wrap HTML.fieldset . withFields n f
