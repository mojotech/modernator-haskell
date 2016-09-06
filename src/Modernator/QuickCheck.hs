module Modernator.QuickCheck where

import Test.QuickCheck
import Test.QuickCheck.Instances
import Modernator.Types
import Modernator.RequestBodies

instance Arbitrary SessionReq where
    arbitrary = SessionReq <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary QuestionReq where
    arbitrary = QuestionReq <$> arbitrary

instance Arbitrary JoinReq where
    arbitrary = JoinReq <$> arbitrary

instance Arbitrary Questioner where
    arbitrary = Questioner <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary QuestionerId where
    arbitrary = QuestionerId <$> arbitrary

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

instance Arbitrary Answerer where
    arbitrary = Answerer <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AnswererId where
    arbitrary = AnswererId <$> arbitrary

constArb a = fmap (const a) (arbitrary :: Gen ())

-- template haskell this
instance Arbitrary SessionMessage where
    arbitrary = oneof
        [ constArb SessionLocked
        , constArb SessionClosed
        , constArb SessionExpired
        , fmap SessionExceptionMessage arbitrary
        , fmap SessionState arbitrary
        ]

instance Arbitrary AppError where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary FullSession where
    arbitrary = FullSession <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Session where
    arbitrary = Session <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LockedStatus where
    arbitrary = arbitraryBoundedEnum
