module Main where

import Servant.Swagger.Test
import Test.Hspec
import Modernator.API
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Modernator.RequestBodies
import Modernator.Types

spec :: Spec
spec = describe "Swagger" $ do
  context "ToJSON matches ToSchema" $ validateEveryToJSON basicAPI

main = hspec spec

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
