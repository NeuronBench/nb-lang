{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

-- | This module implements the main interpretation function
module Lang.Interpret
    ( -- * Interpret
      Input(..)
    , interpret
    , interpretWith
      -- * Errors related to interpretation
    , InterpretError(..)
    ) where

import Control.Exception.Safe (Exception(..), Handler(..))
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Lang.HTTP (Manager)
import Lang.Input (Input(..))
import Lang.Location (Location(..))
import Lang.Syntax (Syntax(..))
import Lang.Type (Type)
import Lang.Value (Value)

import qualified Control.Exception.Safe as Exception
import qualified Control.Lens as Lens
import qualified Control.Monad.Except as Except
import qualified Lang.Context as Context
import qualified Lang.HTTP as HTTP
import qualified Lang.Import as Import
import qualified Lang.Infer as Infer
import qualified Lang.Normalize as Normalize
import qualified Lang.Parser as Parser
import qualified Lang.Syntax as Syntax
import qualified Text.URI as URI

{-| Interpret Lang source code, return the inferred type and the evaluated
    result

    This is the top-level function for the Lang interpreter
-}
interpret
    :: (MonadError InterpretError m, MonadIO m)
    => Input -> m (Type Location, Value)
interpret input = do
    manager <- liftIO HTTP.newManager

    interpretWith [] Nothing manager input

-- | Like `interpret`, but accepts a custom list of bindings
interpretWith
    :: (MonadError InterpretError m, MonadIO m)
    => [(Text, Type Location, Value)]
    -- ^ @(name, type, value)@ for each custom binding
    -> Maybe (Type Location)
    -- ^ Optional expected type for the input
    -> Manager
    -> Input
    -> m (Type Location, Value)
interpretWith bindings maybeAnnotation manager input = do
    eitherPartiallyResolved <- do
        liftIO
            (Exception.catches
                (fmap Right (Import.resolve manager input))
                [ Handler (\e -> return (Left (ParseError e)))
                , Handler (\e -> return (Left (ImportError e)))
                ]
            )

    partiallyResolved <- case eitherPartiallyResolved of
        Left  interpretError    -> throwError interpretError
        Right partiallyResolved -> return partiallyResolved

    let process (maybeAnnotation', child) = do
            referentiallySane input (input <> child)

            interpretWith bindings maybeAnnotation' manager (input <> child)

    resolvedExpression <- traverse process (annotate partiallyResolved)

    let annotatedExpression =
            case maybeAnnotation of
                Nothing         -> resolvedExpression
                Just annotation ->
                    Annotation
                        { location = Syntax.location resolvedExpression
                        , annotated = resolvedExpression
                        , ..
                        }

    let typeContext = do
            (variable, type_, _) <- bindings

            return (Context.Annotation variable type_)

    case Infer.typeWith typeContext annotatedExpression of
        Left message -> do
            Except.throwError (TypeInferenceError message)

        Right inferred -> do
            let evaluationContext = do
                    (variable, _, value) <- bindings

                    return (variable, value)

            return (inferred, Normalize.evaluate evaluationContext resolvedExpression)

remote :: Input -> Bool
remote (URI uri) = any (`elem` schemes) (URI.uriScheme uri)
  where
    schemes = map (fromJust . URI.mkScheme) [ "https", "http" ]
remote _ = False

referentiallySane :: MonadError InterpretError m => Input -> Input -> m ()
referentiallySane parent child
    | remote parent && not (remote child) = do
        Except.throwError
            (ImportError
                (Import.ImportError parent (Import.ReferentiallyInsane child))
            )
    | otherwise = do
        return ()

{-| We use this utility so that when we resolve an import of the form:

    > ./someImport.ffg : SomeType

    … then the type-annotation is used when type-checking the import.  This
    allows the user to supply an expected type to fix imports that would
    otherwise not type-check in isolation.  You can think of this function as
    \"pushing\" the type annotation into the imported expression.

    This is particularly useful when importing JSON.  For example, suppose
    that we had the following JSON expression:

    > [ 1, true ]

    We can't interpret that directly because it is a type error, and we also
    can't import that without a type annotation for the same reason.  However,
    we can import the JSON like this:

    > ./example.json : List (exists (a : Type) . a)

    … and the expression will succeed since the type annotation is used when
    type-checking @./example.json@.  We wouldn't be able to add that same type
    annotation directly to @./example.json@ because then it would no longer be
    valid JSON.
-}
annotate :: Syntax s a -> Syntax s (Maybe (Type s), a)
annotate = Lens.transform transformSyntax . fmap ((,) Nothing)
  where
    transformSyntax Annotation{ annotated = Embed{ embedded = (_, a) }, .. } =
        Embed{ embedded = (Just annotation, a), .. }
    transformSyntax syntax =
        syntax

-- | Errors related to interpretation of an expression
data InterpretError
    = ImportError Import.ImportError
    | ParseError Parser.ParseError
    | TypeInferenceError Infer.TypeInferenceError
    deriving stock (Show)

instance Exception InterpretError where
    displayException (ImportError e) = displayException e
    displayException (ParseError e) = displayException e
    displayException (TypeInferenceError e) = displayException e
