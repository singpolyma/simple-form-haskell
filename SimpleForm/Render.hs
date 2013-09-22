-- | Simple XHTML5 form renderer
module SimpleForm.Render (render) where

import Data.Maybe
import Data.Monoid
import Data.Function (on)
import Data.Foldable (forM_, foldl')
import Data.List (nubBy)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.XHtml5 (Html, Attribute, (!), toValue, toHtml)
import qualified Text.Blaze.XHtml5 as HTML

import SimpleForm

render :: RenderOptions -> Html
render opt@(RenderOptions {
		render_name = n,
		render_label = lbl,
		render_hint = hint,
		render_disabled = d,
		render_required = r,
		render_wrapper_html = wattr,
		render_label_html = lattr,
		render_hint_html = hattr,
		render_error_html = eattr,
		render_errors = errors
	}) =
		applyAttrs [
			[(T.pack "class", T.pack "disabled") | d],
			[(T.pack "class", T.pack "required") | r]
		] wattr $ HTML.label $ do
			forM_ lbl $ applyAttrs [] lattr . label_value (humanize n)
			input_field opt
			forM_ errors $ applyAttrs [[(T.pack "class", T.pack "error")]] eattr . HTML.span
			forM_ hint $ applyAttrs [[(T.pack "class", T.pack "hint")]] hattr . HTML.span . toHtml

label_value :: Text -> Label -> Html
label_value _ (Label s) = HTML.span $ toHtml s
label_value _ (InlineLabel s) = toHtml s
label_value d (DefaultLabel) = label_value d (Label d)

input_field :: RenderOptions -> Html
input_field opt = applyAttrs [] (render_input_html opt) $
	fromMaybe mempty (render_as opt)

mkAttribute :: (Text,Text) -> Attribute
mkAttribute (k,v) = HTML.customAttribute (HTML.textTag k) (toValue v)

applyAttrs :: [[(Text,Text)]] -> [(Text,Text)] -> Html -> Html
applyAttrs dattr cattr html = foldl' (!) html (map mkAttribute attrs)
	where
	attrs = nubBy ((==) `on` fst) attrsWithClass
	attrsWithClass
		| null classes = attrs'
		| otherwise = (T.pack "class", T.unwords classes):attrs'
	classes = concatMap (T.words . snd) $ filter ((== T.pack "class") . fst) attrs'
	attrs' = cattr ++ concat dattr
