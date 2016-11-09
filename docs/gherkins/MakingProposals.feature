Feature: Once a user has joined a Session they are then able to make Proposals
  to the Moderator.

  Moderators cannot make Proposals. This is so that they cannot abuse the
  system to only make Proposals they want to moderate. However, there's nothing
  stopping the same person from logging on in two different sessions as both
  Moderator and Proposer.

  See the following files for more information:

  - [Proposers](Proposers.feature)
  - [Proposals](Proposals.feature)
  - [Sessions](Sessions.feature)

  Scenario: Authorization
    * A Proposers in a Session can make a Proposal in that Session
    * Proposerss in different Sessions cannot make Proposals in that Session
    * Moderators cannot make Proposals

  Scenario: Prerequisites for making a Proposal
    * The Session in which a Proposal is being made must be unlocked

  Scenario: Making a Proposal
    Given a Session
    And a Proposer in that Session
    And a Proposal
    Then the Proposal can be added by the Proposer in the Session
