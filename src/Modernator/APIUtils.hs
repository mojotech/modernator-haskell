{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Modernator.APIUtils where

import Modernator.Types
import Servant
import Control.Monad.Error.Class (MonadError, throwError)

-- | A helper to translate between application errors and servant errors
withError :: MonadError ServantErr m => Either AppError a -> m a
withError (Right a) = return a
withError (Left QuestionNotFound) = throwError $ err404 { errBody = "Question not found" }
withError (Left AnswererNotFound) = throwError $ err404 { errBody = "Answerer not found" }
withError (Left QuestionerNotFound) = throwError $ err404 { errBody = "Questioner not found" }
withError (Left SessionNotFound) = throwError $ err404 { errBody = "Session not found" }
withError (Left NotAuthorizedForSession) = throwError $ err401 { errBody = "Not authorized for session" }
withError (Left MustBeAnswerer) = throwError $ err401 { errBody = "Requires answerer" }
withError (Left MustBeQuestioner) = throwError $ err401 { errBody = "Requires questioner" }
withError (Left SessionAlreadyLocked) = throwError $ err400 { errBody = "Cannot lock session. Already locked." }

unitToNoContent :: () -> NoContent
unitToNoContent () = NoContent

withError' :: AppError -> ServantErr
withError' QuestionNotFound = err404 { errBody = "Question not found" }
withError' AnswererNotFound = err404 { errBody = "Answerer not found" }
withError' QuestionerNotFound = err404 { errBody = "Questioner not found" }
withError' SessionNotFound = err404 { errBody = "Session not found" }
withError' NotAuthorizedForSession = err401 { errBody = "Not authorized for session" }
withError' MustBeAnswerer = err401 { errBody = "Requires answerer" }
withError' MustBeQuestioner = err401 { errBody = "Requires questioner" }
withError' SessionAlreadyLocked = err400 { errBody = "Cannot lock session. Already locked." }
