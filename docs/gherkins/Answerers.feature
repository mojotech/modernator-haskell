Feature: A user takes on the role of Answerer when they create a Session.
  Answerers can lock Sessions, create Sessions, delete Sessions, and answer
  Questions. Answerers must provide a name so Questioners know who they are
  asking Questions of.

  See the following files for more information:

  - [Answering a Question](AnsweringQuestions.feature)
  - [Deleting a Session](DeletingSessions.feature)
  - [Locking a Session](LockingSessions.feature)
  - [Session](Sessions.feature)
  - [Users](Users.feature)

  Scenario: An Answerer may lock a Session
    * See LockingSessions.feature

  Scenario: An Answerer may delete a Session
    * See DeletingSessions.feature

  Scenario: An Answerer may answer a Question
    * See AnsweringQuestions.feature

  Scenario: Answerer properties
    * An Answerer belongs to a Session
    * An Answerer has a name
