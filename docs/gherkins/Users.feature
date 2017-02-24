Feature: The users of Modernator are free to take on one of two defined roles.
  A user may take on the role of a Moderator in order to moderate Proposals posed
  by Proposers. A user may also take on the role of a Proposer to pose
  Proposals to Moderators. A user may be many Moderators and many Proposers at
  the same time, but a user may not be both a Moderator and a Proposer in the
  same Session.

  See the following files for more information:

  - [Creating a Session](CreatingSessions.feature)
  - [Listing all Sessions](ListingSessions.feature)
  - [Viewing a Session](ViewingSessions.feature)
  - [Moderators](Moderators.feature)
  - [Proposers](Proposers.feature)
  - [Sessions](Sessions.feature)

  Scenario: A user may take on the role of a Moderator
    * See Moderators.feature

  Scenario: A user may take on the role of a Proposer
    * See Proposers.feature

  Scenario: A user may list all Sessions
    * See ListingSessions.feature

  Scenario: A user may view a Session
    * See ViewingSessions.feature

  Scenario: A user may create a Session
    * See CreatingSessions.feature

  Scenario: Restrictions
    * A user may not be both a Moderator and a Proposer in the same Session

  Scenario: Multiplicity
    * A user may be a Proposer in many Sessions
    * A user may be a Moderator in many Sessions
