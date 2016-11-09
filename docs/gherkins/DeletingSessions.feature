Feature: Sessions can be deleted when they are no longer valuable.

  See the following files for more information:

  - [Moderators](Moderators.feature)
  - [Proposers](Proposers.feature)
  - [Proposals](Proposals.feature)
  - [Sessions](Sessions.feature)

  Scenario: Authorization
    * The Moderator for a Session can delete that Session
    * Other Moderators cannot delete that Session
    * Proposers cannot delete any Session

  Scenario: Deleting a Session
    Given a Session
    When that Session is deleted
    Then all of its Proposals are deleted
    And all of its Proposers are deleted
    And the Moderator is deleted

  Scenario: Prerequisites for deleting a Session
    * There are no prerequisites for deleting a Session
