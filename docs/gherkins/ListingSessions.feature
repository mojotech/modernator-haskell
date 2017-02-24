Feature: In order to determine which Sessions to join a user may list all
  current Sessions.

  See the following files for more information:

  - [Moderators](Moderators.feature)
  - [Proposers](Proposers.feature)
  - [Proposals](Proposals.feature)
  - [Sessions](Sessions.feature)
  - [Users](Users.feature)

  Scenario: Authorization
    * There is no authorization for listing Sessions

  Scenario: Listing Sessions
    * Users may list all sessions
    * A Session in the list includes the following information:
      | Session Name | Session Locked Status | Moderator | Number of Proposals | Number of Proposers |
