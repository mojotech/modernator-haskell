{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Modernator.APIUtils where

import Modernator.Types
import Servant
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Trans.Except (ExceptT)

unitToNoContent :: () -> NoContent
unitToNoContent () = NoContent

withError :: AppError -> ServantErr
withError QuestionNotFound = err404 { errBody = "Question not found" }
withError AnswererNotFound = err404 { errBody = "Answerer not found" }
withError QuestionerNotFound = err404 { errBody = "Questioner not found" }
withError SessionNotFound = err404 { errBody = "Session not found" }
withError NotAuthorizedForSession = err401 { errBody = "Not authorized for session" }
withError MustBeAnswerer = err401 { errBody = "Requires answerer" }
withError MustBeQuestioner = err401 { errBody = "Requires questioner" }
withError SessionAlreadyLocked = err400 { errBody = "Cannot lock session. Already locked." }

withAppError :: MonadError AppError m => Either AppError a -> m a
withAppError (Right a) = return a
withAppError (Left e) = throwError e
