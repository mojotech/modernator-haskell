Feature: A Proposal is made in a Session of a Moderators. Proposals have votes,
  and can be upvoted to raise their votes. A Proposal's votes indicate to the
  Moderator how important it is for the Proposal to be moderated. Once a Proposal
  is moderated, it can be marked as such so both Moderators and Proposers know
  it is moderated.

  See the following files for more information:

  - [Moderating a Proposal](ModeratingProposals.feature)
  - [Making a Proposal](MakingProposals.feature)
  - [Upvoting a Proposal](UpvotingProposals.feature)
  - [Proposers](Proposers.feature)
  - [Sessions](Sessions.feature)

  Scenario: A Proposal can be made
    * See MakingProposals.feature

  Scenario: A Proposal can be upvoted
    * See UpvotingProposals.feature

  Scenario: A Proposal can be moderated
    * See ModeratingProposals.feature

  Scenario: Proposal properties
    * A Proposal has some text describing the Proposal
    * A Proposal has a number of votes
    * A Proposal is either moderated or unmoderated
    * A Proposal belongs to a Session
    * A Proposal belongs to a Proposer

  Scenario: Proposal defaults
    * The number of votes on a Proposal defaults to 0
