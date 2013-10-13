-- | Bootstrap3 form renderer
module SimpleForm.Render.Bootstrap3 (render) where

import Data.Monoid
import Data.Char
import Data.Foldable (forM_)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.XHtml5 (Html, toHtml)
import qualified Text.Blaze.XHtml5 as HTML
import qualified Text.Blaze.XHtml5.Attributes as HTML hiding (span, label)

import SimpleForm
import SimpleForm.Render

render :: Renderer
render opt@(RenderOptions {
		name = n,
		widgetHtml = Input whtml,
		errors = errors,
		options = InputOptions {
			label = lbl,
			disabled = d,
			required = r,
			wrapper_html = wattr,
			label_html = lattr
		}
	}) =
		applyAttrs (
			maybeCons (not $ null errors) (T.pack "class", T.pack "has-error") $
			maybeCons d (T.pack "class", T.pack "disabled") $
			maybeCons r (T.pack "class", T.pack "required")
			[(T.pack "class", wrapClass)]
		) wattr $ HTML.div $ do
			whtml'
			hintAndError opt
		where
		(wrapClass, whtml') = doLabel lbl
		doLabel Nothing =(T.pack "form-group",
			whtml HTML.! HTML.class_ (HTML.toValue "form-control"))
		doLabel (Just (Label s)) = (T.pack "form-group", do
			applyAttrs [(T.pack "class", T.pack "control-label")] lattr $
				HTML.label HTML.! HTML.for (HTML.toValue $ idize n) $ toHtml s
			whtml HTML.! HTML.class_ (HTML.toValue "form-control") HTML.!
				HTML.id (HTML.toValue $ idize n))
		doLabel (Just (InlineLabel s)) = (T.pack "checkbox",
			applyAttrs [] lattr $ HTML.label $ whtml >> toHtml s)
		doLabel (Just DefaultLabel) = doLabel (Just $ Label $ humanize n)

render opt@(RenderOptions {
		widgetHtml = SelfLabelInput whtml,
		errors = errors,
		options = InputOptions {
			disabled = d,
			required = r,
			wrapper_html = wattr
		}
	}) =
		applyAttrs (
			maybeCons (not $ null errors) (T.pack "class", T.pack "has-error") $
			maybeCons d (T.pack "class", T.pack "disabled") $
			maybeCons r (T.pack "class", T.pack "required")
			[(T.pack "class", T.pack "form-group")]
		) wattr $ HTML.div $ do
			whtml HTML.! HTML.class_ (HTML.toValue "form-control")
			hintAndError opt

render opt@(RenderOptions {
		name = n,
		widgetHtml = MultiInput whtml,
		errors = errors,
		options = InputOptions {
			label = lbl,
			disabled = d,
			required = r,
			wrapper_html = wattr,
			label_html = lattr
		}
	}) =
		applyAttrs (
			maybeCons (not $ null errors) (T.pack "class", T.pack "has-error") $
			maybeCons d (T.pack "disabled", T.pack "disabled") $
			maybeCons d (T.pack "class", T.pack "disabled") $
			maybeCons r (T.pack "class", T.pack "required")
			[]
		) wattr $ HTML.fieldset $ do
			forM_ lbl $ applyAttrs [] lattr . legend_value (humanize n)
			mconcat $ map (HTML.div HTML.! HTML.class_ (HTML.toValue "checkbox")) whtml
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
		forM_ errors $ applyAttrs [(T.pack "class", T.pack "help-block has-error")] eattr . HTML.span
		forM_ hint $ applyAttrs [(T.pack "class", T.pack "help-block")] hattr . HTML.span . toHtml

legend_value :: Text -> Label -> Html
legend_value _ (Label s) = HTML.legend $ toHtml s
legend_value d (InlineLabel s) = legend_value d (Label s)
legend_value d (DefaultLabel) = legend_value d (Label d)

idize :: Text -> Text
idize = T.append (T.pack "input-") . T.toLower . T.concatMap go
	where
	go c
		| isUpper c = T.pack ['-', c]
		| not (isAlphaNum c) = T.singleton '-'
		| otherwise = T.singleton c
