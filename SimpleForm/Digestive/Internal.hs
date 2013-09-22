{-# LANGUAGE GADTs #-}
-- | Torn from the internals of digestive-functors
module SimpleForm.Digestive.Internal (getField, subView') where

import Data.List (isPrefixOf)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Digestive.Form.Internal
import Text.Digestive.Form.Internal.Field
import Text.Digestive.Types
import Text.Digestive.View

getField :: Path -> View v -> Maybe Text
getField pth (View _ _ form input _ method) =
	queryField pth form (getField' method givenInput)
	where
	givenInput = lookupInput pth input

lookupInput :: Path -> [(Path, FormInput)] -> [FormInput]
lookupInput path = map snd . filter ((== path) . fst)

-- This function is why we need GADTs turned on :(
getField' ::
	Method		-- ^ Get/Post
	-> [FormInput]  -- ^ Given input
	-> Field v a	-- ^ Field
	-> Maybe Text   -- ^ Result
getField' _	_				 (Singleton _) = Nothing
getField' _	(TextInput x : _) (Text _)	  = Just x
getField' _	_				 (Text x)	  = Just x
getField' _	(TextInput x : _) (Choice _ _) = Just x
getField' _	_				 (Choice ls' x) =
	Just $ fst (concatMap snd ls' !! x)
getField' Get  _				 (Bool x)
	| x		 = Just (T.pack "on")
	| otherwise = Nothing
getField' Post (TextInput x : _) (Bool _)	  = Just x
getField' Post _				 (Bool _)	  = Nothing
getField' Post (FileInput x : _) File		  = Just (T.pack x)
getField' _	_				 File		  = Nothing

subView' :: Path -> View v -> View v
subView' path (View name ctx form input errs method) =
	case lookupForm path form of
		[] -> View name (ctx ++ path) notFound (strip input) (strip errs) method
		(SomeForm f : _) -> View name (ctx ++ path) f (strip input) (strip errs) method
	where
	lpath	= length path

	strip :: [(Path, a)] -> [(Path, a)]
	strip xs = [(drop lpath p, x) | (p, x) <- xs, path `isPrefixOf` p]

	notFound :: FormTree Identity v Identity a
	notFound = error $ "Text.Digestive.View.subView: " ++
		"No such subView: " ++ show path
