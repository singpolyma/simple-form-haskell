-- | SimpleForm implementation that works along with digestive-functors
module SimpleForm.Digestive.Combined (
	SimpleForm,
	getSimpleForm,
	postSimpleForm,
	simpleForm,
	simpleForm',
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
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Html (Html)

import Text.Digestive.View
import Text.Digestive.Form
import Text.Digestive.Types (Env)
import SimpleForm.Digestive (toForm, withFields, wrap, fieldset, simpleForm, simpleForm')
import SimpleForm.Combined
import SimpleForm.Digestive.Internal
import SimpleForm.Digestive.Validation

import SimpleForm.Render

-- | Render a 'SimpleForm' to 'Html'
--
-- This produces the contents of the form, but you must still wrap it in
-- the actual \<form\> element.
getSimpleForm :: (Monad m) =>
	Renderer
	-> Maybe a                      -- ^ Default values for the form
	-> SimpleForm a (Form Html m a) -- ^ The simple form to render
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
-- This produces the contents of the form, but you must still wrap it in
-- the actual \<form\> element.
postSimpleForm :: (Monad m) =>
	Renderer
	-> m (Env m)
	-> SimpleForm a (Form Html m a) -- ^ The simple form to render
	-> m (Html, Maybe a)
postSimpleForm render env form = do
		env' <- env
		(view, val) <- postForm T.empty initialForm env'
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
