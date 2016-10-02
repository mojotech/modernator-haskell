Feature: A Question is asked in a Session of an Answerer. Questions have votes,
  and can be upvoted to raise their votes. A Question's votes indicate to the
  Answerer how important it is for the Question to be answered. Once a Question
  is answered, it can be marked as such so both Answerers and Questioners know
  it is answered.

  See the following files for more information:

  - [Answering a Question](AnsweringQuestions.feature)
  - [Asking a question](AskingQuestions.feature)
  - [Upvoting a question](UpvotingQuestions.feature)
  - [Questioners](Questioners.feature)
  - [Sessions](Sessions.feature)

  Scenario: A Question can be asked
    * See AskingQuestions.feature

  Scenario: A Question can be upvoted
    * See UpvotingQuestions.feature

  Scenario: A Question can be answered
    * See AnsweringQuestions.feature

  Scenario: Question properties
    * A Question has some text describing the Question
    * A Question has a number of votes
    * A Question is either answered or unanswered
    * A Question belongs to a Session
    * A Question belongs to a Questioner

  Scenario: Question defaults
    * The number of votes on a Question defaults to 0
