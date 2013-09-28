-- | Simple XHTML5 form renderer
module SimpleForm.Render.XHTML5 (render) where

import Data.Monoid
import Data.Foldable (forM_)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.XHtml5 (Html, toHtml)
import qualified Text.Blaze.XHtml5 as HTML

import SimpleForm
import SimpleForm.Render

render :: Renderer
render opt@(RenderOptions {
		name = n,
		widgetHtml = Input whtml,
		options = InputOptions {
			label = lbl,
			disabled = d,
			required = r,
			wrapper_html = wattr,
			label_html = lattr
		}
	}) =
		applyAttrs [
			[(T.pack "class", T.pack "disabled") | d],
			[(T.pack "class", T.pack "required") | r]
		] wattr $ HTML.label $ do
			forM_ lbl $ applyAttrs [] lattr . label_value (humanize n)
			whtml
			hintAndError opt

render opt@(RenderOptions {
		widgetHtml = SelfLabelInput whtml,
		errors = errors,
		options = InputOptions {
			hint = hint,
			disabled = d,
			required = r,
			wrapper_html = wattr
		}
	}) =
		applyAttrs [
			[(T.pack "class", T.pack "disabled") | d],
			[(T.pack "class", T.pack "required") | r]
		] wattr $ (if errorsOrHint then HTML.div else id) $ do
			whtml
			hintAndError opt
	where
	errorsOrHint = not (null errors && hint == mempty)

render opt@(RenderOptions {
		name = n,
		widgetHtml = MultiInput whtml,
		options = InputOptions {
			label = lbl,
			disabled = d,
			required = r,
			wrapper_html = wattr,
			label_html = lattr
		}
	}) =
		applyAttrs [
			[(T.pack "disabled", T.pack "disabled") | d],
			[(T.pack "class", T.pack "disabled") | d],
			[(T.pack "class", T.pack "required") | r]
		] wattr $ HTML.fieldset $ do
			forM_ lbl $ applyAttrs [] lattr . legend_value (humanize n)
			HTML.ul $ mconcat $ map HTML.li whtml
			hintAndError opt

hintAndError :: RenderOptions -> Html
hintAndError (RenderOptions {
		errors = errors,
		options = InputOptions {
			hint = hint,
			hint_html = hattr,
			error_html = eattr
		}
	}) = do
		forM_ errors $ applyAttrs [[(T.pack "class", T.pack "error")]] eattr . HTML.span
		forM_ hint $ applyAttrs [[(T.pack "class", T.pack "hint")]] hattr . HTML.span . toHtml

label_value :: Text -> Label -> Html
label_value _ (Label s) = HTML.span (toHtml s) `mappend` toHtml " "
label_value _ (InlineLabel s) = toHtml s `mappend` toHtml " "
label_value d (DefaultLabel) = label_value d (Label d)

legend_value :: Text -> Label -> Html
legend_value _ (Label s) = HTML.legend $ toHtml s
legend_value d (InlineLabel s) = legend_value d (Label s)
legend_value d (DefaultLabel) = legend_value d (Label d)
