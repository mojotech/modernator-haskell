Feature: While using Modernator it becomes important to indicate which
  Proposals have been moderated. This indicates that Proposers should focus
  their attention on unmoderated and new Proposals. The moderation pattern is
  one that is suitable for a number of different tasks, below are some
  examples:

  - Question and Answer sessions
  - Deciding what to eat for lunch
  - Ranking the popularity of different team names

  See the following files for more information:

  - [Moderators](Moderators.feature)
  - [Proposals](Proposals.feature)
  - [Sessions](Sessions.feature)

  Scenario: Authorization
    * The Moderator for the Session a Proposal is in can moderate that Proposal
    * Other Moderators cannot moderate Proposals in the Session
    * Proposers cannot moderate Proposals

  Scenario: Prerequisites for moderating a Proposal
    * The Session in which the Proposal is being moderating must be unlocked
    * The Proposal must not be moderated

  Scenario: Moderating a Proposal
    Given a Session
    And a Moderator for the Session
    And a Proposal in that Session
    Then the Proposal can be moderated by the Moderator

