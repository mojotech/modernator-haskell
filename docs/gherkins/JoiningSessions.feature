Feature: Once a Session has been created other users will want to join that
  Session and ask Questions of the Answerer. These new users are called
  Questioners.

  Users cannot join locked Sessions. Locked Sessions can't be modified, so
  simply previewing that Session fulfills any needs that joining the Session
  would provide.

  See the following files for more information:

  - [Questioners](Questioners.feature)
  - [Sessions](Sessions.feature)

  Scenario: Authorization
    * There is no authorization required for joining a Session

  Scenario: Prerequisites for joining a Session
    * The Session must be unlocked

  Scenario: Joining a Session
    Given a user
    And a Session that is unlocked
    Then the Session can be joined by the user
    And the user is now a Questioner for the Session
    And the user can provide a name for the Questioner
