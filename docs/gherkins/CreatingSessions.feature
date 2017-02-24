Feature: Sessions are started by a single user who then takes the role of
  Moderator. Once a Session is created, other users can join in the role of
  Proposers.

  See the following files for more information:

  - [Moderators](Moderators.feature)
  - [Sessions](Sessions.feature)
  - [Users](Users.feature)

  Scenario: Authorization
    * There is no authorization required for creating a session

  Scenario: Creating a new session
    Given a user
    And a name for the Moderator
    And a name for the Session
    Then a Session can be created with a Moderator using the provided name
    And the user is now the Moderator for the Session
