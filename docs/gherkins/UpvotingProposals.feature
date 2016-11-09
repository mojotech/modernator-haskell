Feature: When a Session has many Proposals it can be difficult for the
  Moderator to determine which Proposals are most valuable. Proposers indicate
  that they value a Proposal by upvoting it. This increases the number of votes
  on the Proposal.

  Moderators cannot upvote Proposals. This is so that they cannot abuse the
  system to only moderatoe Proposals they want to moderate. However, there's
  nothing stopping the same person from logging on in two different sessions as
  both Moderator and Proposer

  See the following files for more information:

  - [Moderators](Moderators.feature)
  - [Propsers](Propsers.feature)
  - [Proposals](Proposals.feature)
  - [Sessions](Sessions.feature)

  Scenario: Authorization
    * A Propeser in a Session can upvote a Proposal in that Session
    * Proposers in different Sessions cannot upvote Proposals in that Session
    * Moderators cannot upvote Proposals

  Scenario: Prerequisites for upvoting a Proposal
    * The Session in which a Proposal is being upvoted must be unlocked
    * The Proposer upvoting a Proposal cannot have upvoted it previously
    * The Proposal being upvoted must not be moderated

  Scenario: Upvoting a Proposal
    Given a Session
    And a Proposal in that session
    And a Proposer in that session
    When the Proposal is upvoted by the Proposer
    Then the Proposal has one more vote than it had before it was upvoted
