-- | These utilities are for writing 'Renderer's
module SimpleForm.Render (
	Renderer,
	Input(..),
	RenderOptions(..),
	renderOptions
) where

import Text.Blaze.Html (Html)
import Data.Text (Text)
import SimpleForm

-- | The type of a final form-renderer
type Renderer = (RenderOptions -> Html)

-- | 'InputOptions' that have been prepped for rendering
data RenderOptions = RenderOptions {
		name :: Text,
		widgetHtml :: Input,
		errors :: [Html],
		options :: InputOptions
	}

-- | Prep 'InputOptions' for rendering
renderOptions ::
	Maybe a          -- ^ The parsed value for this input (if available)
	-> Maybe Text    -- ^ The unparsed value for this input (if available)
	-> Text          -- ^ The name of this input
	-> Widget a      -- ^ Widget to render with
	-> [Html]        -- ^ Any error messages for this input
	-> InputOptions
	-> RenderOptions
renderOptions v u n w errors opt = RenderOptions {
		name = n,
		widgetHtml = w v u n opt,
		errors = errors,
		options = opt
	}
