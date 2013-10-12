-- | SimpleForm implementation that works along with digestive-functors
--
-- This module is for constructing forms that only output to 'Html'.
-- For forms that also parse input, see SimpleForm.Digestive.Combined
module SimpleForm.Digestive (
	SimpleForm,
	simpleForm,
	simpleForm',
	-- * Create forms
	input,
	input_,
	choiceInput,
	choiceInput_,
	toForm,
	-- * Subforms
	withFields,
	wrap,
	fieldset
) where

import Data.Monoid
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class

import Data.Text (Text)
import qualified Data.Text.Lazy as TL

import Text.Blaze.Html (Html, ToMarkup, toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.XHtml5 as HTML

import Text.Digestive.View
import SimpleForm
import SimpleForm.Render
import SimpleForm.Digestive.Internal

-- | Render a 'SimpleForm' to 'Html'
--
-- This produces the contents of the form, but you must still wrap it in
-- the actual \<form\> element.
simpleForm :: (ToMarkup v) =>
	Renderer
	-> (View v, Maybe a)    -- ^ Results of running a digestive-functors 'Form'
	-> SimpleForm a ()      -- ^ The simple form to render
	-> Html
simpleForm render viewVal = snd . simpleForm' render viewVal

-- | Render a 'SimpleForm' to 'Html' and get the return value
--
-- This produces the contents of the form, but you must still wrap it in
-- the actual \<form\> element.
simpleForm' :: (ToMarkup v) =>
	Renderer
	-> (View v, Maybe a)    -- ^ Results of running a digestive-functors 'Form'
	-> SimpleForm a r      -- ^ The simple form to render
	-> (r, Html)
simpleForm' render (view, val) (SimpleForm form) =
	runWriter $ runReaderT form (val, fmap toHtml view, render)

-- | Add some raw markup to a 'SimpleForm'
toForm :: (ToMarkup h) => h -> SimpleForm a ()
toForm = SimpleForm . lift . tell . toHtml

-- | Like 'input', but grabs a collection out of the 'View'
choiceInput ::
	Text                               -- ^ Form element name
	-> (r -> Maybe a)                  -- ^ Get value from parsed data
	-> (GroupedCollection -> Widget a) -- ^ Widget to use
	-> InputOptions                    -- ^ Other options
	-> SimpleForm r ()
choiceInput n sel w opt = SimpleForm $ ReaderT $ \(env, view, render) ->
	let
		textView = fmap (TL.toStrict . renderHtml) view -- TODO: this is wrong
		collection = fieldInputChoiceGroup' [n] textView
	in
	tell $ input' n sel (w collection) opt (env, view, render)

-- | Like 'choiceInput', but chooses defaults for 'Widget' and 'InputOptions'
choiceInput_ ::
	Text                 -- ^ Form element name
	-> (r -> Maybe Text) -- ^ Get value from parsed data
	-> SimpleForm r ()
choiceInput_ n sel = choiceInput n sel select mempty

-- | Create an input element for a 'SimpleForm'
--
-- > input "username" (Just . username) wdef mempty
input ::
	Text                        -- ^ Form element name
	-> (r -> Maybe a)           -- ^ Get value from parsed data
	-> Widget a                 -- ^ Widget to use (such as 'SimpleForm.wdef')
	-> InputOptions             -- ^ Other options
	-> SimpleForm r ()
input n sel w opt = SimpleForm $ ReaderT $ tell . input' n sel w opt

-- | Same as 'input', but just use the default options
input_ :: (DefaultWidget a) =>
	Text                        -- ^ Form element name
	-> (r -> Maybe a)           -- ^ Get value from parsed data
	-> SimpleForm r ()
input_ n sel = input n sel wdef mempty

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
fieldset n f form = wrap HTML.fieldset $ do
	maybe (return ()) (toForm . HTML.legend . toHtml . humanize) n
	withFields n f form
