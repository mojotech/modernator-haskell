Feature: A user takes on the role of Questioner when they join a Session.
  Questioners may ask and upvote Questions in a Session. Questioners are free
  to provide a name, but can remain anonymous if desired.

  See the following files for more information:

  - [Asking a Question](AskingQuestions.feature)
  - [Joining a Session](JoiningSessions.feature)
  - [Upvoting a Question](UpvotingQuestions.feature)
  - [Sessions](Sessions.feature)
  - [Users](Users.feature)

  Scenario: A Questioner may ask a Question
    * See AskingQuestions.feature

  Scenario: A Questioner may upvote a Question
    * See UpvotingQuestions.feature

  Scenario: A Questioner may join a Session
    * See JoiningSessions.feature

  Scenario: Properties of Questioners
    * A Questioner belongs to a Session
    * A Questioner may have a name

  Scenario: Questioner defaults
    * A Questioner defaults to no name
