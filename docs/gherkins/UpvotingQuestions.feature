Feature: When a Session has many Questions it can be difficult for the Answerer
  to determine which Questions are most valuable. Questioners indicate that
  they value a Question by upvoting it. This increases the number of votes on
  the question.

  Answerers cannot upvote Questions. This is so that they cannot abuse the
  system to only answer Questions they want to answer. However, there's nothing
  stopping the same person from logging on in two different sessions as both
  Answerer and Questioner.

  See the following files for more information:

  - [Answerers](Answerers.feature)
  - [Questioners](Questioners.feature)
  - [Questions](Questions.feature)
  - [Sessions](Sessions.feature)

  Scenario: Authorization
    * A Questioner in a Session can upvote a Question in that Session
    * Questioners in different Sessions cannot upvote Questions in that Session
    * Answerers cannot upvote Questions

  Scenario: Prerequisites for upvoting a Question
    * The Session in which a Question is being upvoted must be unlocked
    * The Questioner upvoting a question cannot have upvoted it previously
    * The Question being upvoted must not be answered

  Scenario: Upvoting a Question
    Given a Session
    And a Question in that session
    And a Questioner in that session
    When the Question is upvoted by the Questioner
    Then the Question has one more vote than it had before it was upvoted
