Feature: A user takes on the role of Moderator when they create a Session.
  Moderators can lock Sessions, create Sessions, delete Sessions, and moderate
  Proposals. Moderators must provide a name so Proposers know who they are
  making Proposals to.

  See the following files for more information:

  - [Moderating a Proposal](ModeratingProposals.feature)
  - [Deleting a Session](DeletingSessions.feature)
  - [Locking a Session](LockingSessions.feature)
  - [Session](Sessions.feature)
  - [Users](Users.feature)

  Scenario: A Moderator may lock a Session
    * See LockingSessions.feature

  Scenario: A Moderator may delete a Session
    * See DeletingSessions.feature

  Scenario: A Moderator may moderate a Proposal
    * See ModeratingProposals.feature

  Scenario: Moderator properties
    * An Moderator belongs to a Session
    * An Moderator has a name
