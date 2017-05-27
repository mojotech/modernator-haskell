{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Modernator.APIUtils where

import Modernator.Types
import Servant
import Control.Monad.Error.Class (MonadError)

-- | A helper to translate between application errors and servant errors
withError :: MonadError ServantErr m => Either AppError a -> m a
withError (Right a) = return a
withError (Left QuestionNotFound) = throwError $ err404 { errBody = "Question not found" }
withError (Left SessionNotFound) = throwError $ err404 { errBody = "Session not found" }
withError (Left NotAuthorizedForSession) = throwError $ err401 { errBody = "Not authorized for session" }
withError (Left MustBeAnswerer) = throwError $ err401 { errBody = "Requires answerer" }
withError (Left MustBeQuestioner) = throwError $ err401 { errBody = "Requires questioner" }
withError (Left SessionAlreadyLocked) = throwError $ err400 { errBody = "Cannot lock session. Already locked." }
withError (Left QuestionAlreadyUpvoted) = throwError $ err400 { errBody = "Cannot upvote question. You've already upvoted it." }
withError (Left UserNotFound) = throwError $ err404 { errBody = "User not found" }
withError (Left InvalidCredentials) = throwError $ err401 { errBody = "Invalid Credentials" }

unitToNoContent :: () -> NoContent
unitToNoContent () = NoContent
