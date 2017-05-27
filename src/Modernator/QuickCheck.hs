module Modernator.QuickCheck where

import Test.QuickCheck
import Test.QuickCheck.Instances()
import Modernator.Types
import Modernator.RequestBodies
import Modernator.Cookies
import Servant.Server.Experimental.Auth.Cookie (EncryptedSession(..))

instance Arbitrary SessionReq where
    arbitrary = SessionReq <$> arbitrary <*> arbitrary

instance Arbitrary QuestionReq where
    arbitrary = QuestionReq <$> arbitrary

instance Arbitrary JoinReq where
    arbitrary = JoinReq <$> arbitrary

instance Arbitrary UserReq where
    arbitrary = UserReq <$> arbitrary <*> arbitrary

instance Arbitrary LoginReq where
    arbitrary = LoginReq <$> arbitrary <*> arbitrary

instance Arbitrary SessionId where
    arbitrary = SessionId <$> arbitrary

instance Arbitrary Question where
    arbitrary = Question <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary QuestionId where
    arbitrary = QuestionId <$> arbitrary

instance Arbitrary Votes where
    arbitrary = Votes <$> arbitrary

instance Arbitrary Answered where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary User where
    arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AnswererSessions where
    arbitrary = AnswererSessions <$> arbitrary

instance Arbitrary QuestionerSessions where
    arbitrary = QuestionerSessions <$> arbitrary

instance Arbitrary UserId where
    arbitrary = UserId <$> arbitrary

constArb a = fmap (const a) (arbitrary :: Gen ())

-- template haskell this
instance Arbitrary SessionMessage where
    arbitrary = oneof
        [ constArb SessionLocked
        , constArb SessionClosed
        , constArb SessionExpired
        , fmap SessionExceptionMessage arbitrary
        , fmap SessionState arbitrary
        , fmap QuestionAsked arbitrary
        , fmap QuestionUpvoted arbitrary
        , fmap QuestionAnswered arbitrary
        , fmap QuestionerJoined arbitrary
        ]

instance Arbitrary AppError where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary FullSession where
    arbitrary = FullSession <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Session where
    arbitrary = Session <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LockedStatus where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary ModernatorCookie where
    arbitrary = ModernatorCookie <$> arbitrary

-- for session cookies
instance Arbitrary EncryptedSession where
    arbitrary = EncryptedSession <$> arbitrary
