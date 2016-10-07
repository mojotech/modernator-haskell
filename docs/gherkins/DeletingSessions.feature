Feature: Sessions can be deleted when they are no longer valuable.

  See the following files for more information:

  - [Answerers](Answerers.feature)
  - [Questioners](Questioners.feature)
  - [Questions](Questions.feature)
  - [Sessions](Sessions.feature)

  Scenario: Authorization
    * The Answerer for a Session can delete that Session
    * Other Answerers cannot delete that Session
    * Questioners cannot delete any Session

  Scenario: Deleting a Session
    Given a Session
    When that Session is deleted
    Then all of its Questions are deleted
    And all of its Questioners are deleted
    And the Answerer is deleted

  Scenario: Prerequisites for deleting a Session
    * There are no prerequisites for deleting a Session
