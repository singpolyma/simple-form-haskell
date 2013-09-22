-- | Forms that configure themseles based on type
module SimpleForm (
	InputOptions(..),
	text,
	checkbox,
	Label(..),
	humanize,
	RenderOptions,
	renderInputOptions,
) where

import Data.Maybe
import Data.Monoid
import Text.Blaze.XHtml5 (Html, (!), toValue)
import qualified Text.Blaze.XHtml5 as HTML
import qualified Text.Blaze.XHtml5.Attributes as HTML hiding (label, span)
import Data.Text (Text)
import Data.String

-- | A block label, inline label, or implied value label
data Label = Label Text | InlineLabel Text | DefaultLabel
	deriving (Show, Eq)

instance IsString Label where
	fromString = Label . fromString

-- | 'InputOptions' that have been prepped for rendering
data RenderOptions = RenderOptions {
		render_name :: Text,
		render_label :: Maybe Label,
		render_hint :: Maybe Text,
		render_required :: Bool,
		render_disabled :: Bool,
		render_as :: Maybe Html,
		render_input_html :: [(Text,Text)],
		render_label_html :: [(Text,Text)],
		render_error_html :: [(Text,Text)],
		render_hint_html :: [(Text,Text)],
		render_wrapper_html :: [(Text,Text)],
		render_errors :: [Html]
	}

-- | Prep 'InputOptions' for rendering
renderInputOptions ::
	Maybe Text    -- ^ The unparsed value for this input (if available)
	-> [Html]     -- ^ Any error messages for this input
	-> InputOptions a
	-> RenderOptions
renderInputOptions unparsed errors opt = RenderOptions {
		render_name = name opt,
		render_label = label opt,
		render_hint = hint opt,
		render_required = required opt,
		render_disabled = disabled opt,
		render_as = fmap (\f -> f unparsed opt) (as opt),
		render_input_html = input_html opt,
		render_label_html = label_html opt,
		render_error_html = error_html opt,
		render_hint_html = hint_html opt,
		render_wrapper_html = wrapper_html opt,
		render_errors = errors
	}

-- | The setup for rendering an input. Blank is 'mempty', default is 'def'.
data InputOptions a = InputOptions {
		name :: Text,
		value :: Maybe a,
		label :: Maybe Label,
		hint :: Maybe Text,
		required :: Bool,
		disabled :: Bool,
		as :: Maybe (Maybe Text -> InputOptions a -> Html),
		input_html :: [(Text,Text)],
		label_html :: [(Text,Text)],
		error_html :: [(Text,Text)],
		hint_html :: [(Text,Text)],
		wrapper_html :: [(Text,Text)]
	}

instance (Show a) => Show (InputOptions a) where
	show (InputOptions {name = n, value = v}) =
		"InputOptions { name = " ++ show n ++ ", value = " ++ show v ++ " }"

instance Monoid (InputOptions a) where
	mempty = InputOptions {
		name = mempty,
		value = Nothing,
		label = Just DefaultLabel,
		hint = Nothing,
		required = True,
		disabled = False,
		as = Nothing,
		input_html = [],
		label_html = [],
		error_html = [],
		hint_html = [],
		wrapper_html = []
	}

	mappend a b = InputOptions {
		name = monoidOr (name b) (name a),
		value = case value b of { Nothing -> value a; _ -> value b},
		label = if label b == Just DefaultLabel then label a else label b,
		hint = monoidOr (hint b) (hint a),
		required = if required b then required a else required b,
		disabled = if not (disabled b) then disabled a else disabled b,
		as = case as b of { Nothing -> as a; _ -> as b },
		input_html = input_html a ++ input_html b,
		label_html = label_html a ++ label_html b,
		error_html = error_html a ++ error_html b,
		hint_html = hint_html a ++ hint_html b,
		wrapper_html = wrapper_html a ++ wrapper_html b
	}

monoidOr :: (Monoid a, Eq a) => a -> a -> a
monoidOr a b
	| a == mempty = b
	| otherwise = a

humanize :: Text -> Text
humanize = id -- TODO

-- | Infer sane defaults for an input based on its type
class DefaultInputOptions a where
	def :: InputOptions a

instance DefaultInputOptions Bool where
	def = mempty { as = Just checkbox }

instance DefaultInputOptions Text where
	def = mempty { as = Just text }

text :: Maybe Text -> InputOptions Text -> Html
text u (InputOptions {name = n, value = v, disabled = d, required = r}) =
	mkDisabled d $ mkRequired r (
		HTML.input !
			HTML.type_ (toValue "text") !
			HTML.name (toValue n) !
			HTML.value (toValue $ fromMaybe (fromMaybe mempty u) v)
	)

checkbox :: Maybe Text -> InputOptions Bool -> Html
checkbox u (InputOptions {name = n, value = v, disabled = d, required = r}) =
	mkChecked isChecked $ mkDisabled d $ mkRequired r (
		HTML.input !
			HTML.type_ (toValue "checkbox") !
			HTML.name (toValue n)
	)
	where
	isChecked = fromMaybe (maybe False (/=mempty) u) v

mkChecked :: Bool -> Html -> Html
mkChecked True = (! HTML.checked (toValue "checked"))
mkChecked False = id

mkDisabled :: Bool -> Html -> Html
mkDisabled True = (! HTML.disabled (toValue "disabled"))
mkDisabled False = id

mkRequired :: Bool -> Html -> Html
mkRequired True = (! HTML.required (toValue "required"))
mkRequired False = id
