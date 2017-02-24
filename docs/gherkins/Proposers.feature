Feature: A user takes on the role of Proposer when they join a Session.
  Proposers may ask and upvote Proposals in a Session. Proposers are free
  to provide a name, but can remain anonymous if desired.

  See the following files for more information:

  - [Making a Proposal](MakingProposals.feature)
  - [Joining a Session](JoiningSessions.feature)
  - [Upvoting a Proposal](UpvotingProposals.feature)
  - [Sessions](Sessions.feature)
  - [Users](Users.feature)

  Scenario: A Proposer may make a Proposal
    * See MakingProposals.feature

  Scenario: A Proposer may upvote a Proposal
    * See UpvotingProposals.feature

  Scenario: A Proposer may join a Session
    * See JoiningSessions.feature

  Scenario: Properties of Proposers
    * A Proposer belongs to a Session
    * A Proposer may have a name

  Scenario: Proposer defaults
    * A Proposer defaults to no name
