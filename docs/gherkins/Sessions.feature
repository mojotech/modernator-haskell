Feature: A Session is how a group of users (Proposers) can ask Proposals of a
  specific user (Moderator) designated to moderate them.  Sessions provide a
  means of collecting these Proposals and users into a single place. Sessions
  can be either unlocked or locked, with locked Sessions prohibiting
  modification to the Proposals within. Sessions can also be deleted entirely.

  See the following files for more information:

  - [Creating Sessions](CreatingSessions.feature)
  - [Deleting Sessions](DeletingSessions.feature)
  - [Joining Sessions](JoiningSessions.feature)
  - [Listing all Sessions](ListingSessions.feature)
  - [Locking Sessions](LockingSessions.feature)
  - [Viewing Sessions](ViewingSessions.feature)
  - [Moderators](Moderators.feature)
  - [Proposers](Proposers.feature)
  - [Proposals](Proposals.feature)

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
    * A Session has a single Moderator
    * A Session can have many Proposers
    * A Session can have many Proposals
    * A Session can be either locked or unlocked
    * A Session has a name

  Scenario: Session defaults
    * A Session defaults to having no Proposers
    * A Session defaults to having no Proposals
    * A Session defaults to being unlocked
