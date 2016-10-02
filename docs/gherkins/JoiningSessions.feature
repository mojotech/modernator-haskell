Feature: Once a Session has been created other users will want to join that
  Session and ask Questions of the Answerer. These new users are called
  Questioners.

  See the following files for more information:

  - [Questioners](Questioners.feature)
  - [Sessions](Sessions.feature)

  Scenario: Authorization
    * There is no authorization required for joining a Session

  Scenario: Joining a Session
    Given a user
    And a Session
    Then the Session can be joined by the user
    And the user is now a Questioner for the Session
    And the user can provide a name for the Questioner
