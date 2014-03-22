{-# LANGUAGE CPP #-}
-- | SimpleForm implementation that works along with digestive-functors
--
-- The Combined module both renders to 'Html' and also parses input.
module SimpleForm.Digestive.Combined (
	SimpleForm,
	SimpleForm',
	postSimpleForm,
	getSimpleForm,
	simpleForm',
	-- * Create forms
	input,
	input_,
	toForm,
	-- * Subforms
	withFields,
	withFields',
	wrap,
	fieldset
) where

import Data.Monoid
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Html (Html)

import Text.Digestive.View
import Text.Digestive.Form
import Text.Digestive.Types (Env)
import SimpleForm.Digestive (toForm, wrap, simpleForm')
import qualified SimpleForm.Digestive (withFields, fieldset)
import SimpleForm.Combined
import SimpleForm.Digestive.Internal
import SimpleForm.Digestive.Validation

import SimpleForm.Render

-- | Convenience type synonym for combined forms
type SimpleForm' m a = SimpleForm a (Form Html m a)

-- | Render a 'SimpleForm' to 'Html'
--
-- This produces the contents of the form, but you must still wrap it in
-- the actual \<form\> element.
getSimpleForm :: (Monad m) =>
	Renderer
	-> Maybe a         -- ^ Default values for the form
	-> SimpleForm' m a -- ^ The simple form to render
	-> m Html
getSimpleForm render val form = do
		view <- getForm T.empty initialForm
		return $ snd $ simpleForm' render (view, val) form
	where
	(initialForm, _) = simpleForm' render (noView, val) form
	noView :: View Html
	noView = error "SimpleForm.Digestive.Combined: cannot use View in generating Form"

-- | Render a 'SimpleForm' to 'Html' in the presence of input
--
-- This also parses the input to the correct datatype.
--
-- The 'Html' is the contents of the form, but you must still wrap it in
-- the actual \<form\> element.
postSimpleForm :: (Monad m) =>
	Renderer
	-> m (Env m)       -- ^ The digestive-functors input environment
	-> SimpleForm' m a -- ^ The simple form to render
	-> m (Html, Maybe a)
postSimpleForm render env form = do
#if MIN_VERSION_digestive_functors(0,7,0)
		(view, val) <- postForm T.empty initialForm (const env)
#else
		env' <- env
		(view, val) <- postForm T.empty initialForm env'
#endif
		let html = snd $ simpleForm' render (view, val) form
		return (html, val)
	where
	(initialForm, _) = simpleForm' render (noView, Nothing) form
	noView :: View Html
	noView = error "SimpleForm.Digestive.Combined: cannot use View in generating Form"

-- | Create an input element for a 'SimpleForm'
--
-- > input "username" (Just . username) (wdef,vdef) mempty
input :: (Eq a, Monad m) =>
	Text                        -- ^ Form element name
	-> (r -> Maybe a)           -- ^ Get value from parsed data
	-> (Widget a, Validation a) -- ^ Widget and validation to use
	-> InputOptions             -- ^ Other options
	-> SimpleForm r (Form Html m a)
input n sel (w,v) opt = SimpleForm $ ReaderT $ \env -> do
	tell $ input' n sel w opt env
	return $ validationToForm n v

-- | Same as 'input', but just use the default options
input_ :: (DefaultWidget a, DefaultValidation a, Eq a, Monad m) =>
	Text                        -- ^ Form element name
	-> (r -> Maybe a)           -- ^ Get value from parsed data
	-> SimpleForm r (Form Html m a)
input_ n sel = input n sel (wdef,vdef) mempty

-- | Project out some part of the parsed data (does not add name to subview)
withFields' ::
	Maybe Text     -- ^ Optional subview name
	-> (r' -> r)   -- ^ Projection function
	-> SimpleForm r a
	-> SimpleForm r' a
withFields' = SimpleForm.Digestive.withFields

-- | Project out some part of the parsed data and name the subview
withFields :: (Monad m) =>
	Text           -- ^ Subview name
	-> (r' -> r)   -- ^ Projection function
	-> SimpleForm r (Form Html m a)
	-> SimpleForm r' (Form Html m a)
withFields n f = fmap (n .:) . withFields' (Just n) f

-- | Like 'withFields'', but also wrap in fieldset tag
fieldset' :: Maybe Text -> (r' -> r) -> SimpleForm r a -> SimpleForm r' a
fieldset' = SimpleForm.Digestive.fieldset

-- | Like 'withFields', but also wrap in fieldset tag
fieldset :: (Monad m) => Text -> (r' -> r) -> SimpleForm r (Form Html m a) -> SimpleForm r' (Form Html m a)
fieldset n f = fmap (n .:) . fieldset' (Just n) f
