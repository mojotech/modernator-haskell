Feature: A Question and answer Session is how a group of users (Questioners)
  can ask Questions with a specific user (Answerer) designated to answer them.
  Sessions provide a means of collecting these Questions and users into a
  single place. Sessions can be either unlocked or locked, with locked Sessions
  prohibiting modification to the Questions within. Sessions can also be
  deleted entirely.

  See the following files for more information:

  - [Creating Sessions](CreatingSessions.feature)
  - [Deleting Sessions](DeletingSessions.feature)
  - [Joining Sessions](JoiningSessions.feature)
  - [Listing all Sessions](ListingSessions.feature)
  - [Locking Sessions](LockingSessions.feature)
  - [Viewing Sessions](ViewingSessions.feature)
  - [Answerers](Answerers.feature)
  - [Questioners](Questioners.feature)
  - [Questions](Questions.feature)

  Scenario: A Session can be created
    * See CreatingSessions.feature

  Scenario: A Session can be locked
    * See LockingSessions.feature

  Scenario: A Session can be deleted
    * See DeletingSessions.feature

  Scenario: A Session can be joined
    * See JoiningSessions.feature

  Scenario: Sessions can be viewed
    * See ViewingSessions.feature

  Scenario: Session properties
    * A Session has a single Answerer
    * A Session can have many Questioners
    * A Session can have many Questions
    * A Session can be either locked or unlocked
    * A Session has a name

  Scenario: Session defaults
    * A Session defaults to having no Questioners
    * A Session defaults to having no Questions
    * A Session defaults to being unlocked
