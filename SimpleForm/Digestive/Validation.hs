module SimpleForm.Digestive.Validation (validationToForm, underRef) where

import Data.Monoid
import Control.Arrow (second)
import Text.Blaze.Html (Html, toHtml)
import Text.Digestive.Form (Form, text, validate, groupedChoiceWith, (.:))
import Text.Digestive.Types (Result(..))
import Data.Text (Text)
import SimpleForm.Validation (Validation(..))
import SimpleForm.Digestive.Internal (underRef)

validationToForm :: (Eq a, Monad m) => Text -> Validation a -> Form Html m a
validationToForm n (Check chk) = n .: validate (maybeErr . chk . (:[])) (text Nothing)
	where
	maybeErr Nothing = Error (toHtml n `mappend` toHtml " is invalid")
	maybeErr (Just x) = Success x
validationToForm n (Includes xs) = n .: groupedChoiceWith xs' Nothing
	where
	xs' = map (second $ map (\(x, (v,l)) -> (v, (x, toHtml l)))) xs
