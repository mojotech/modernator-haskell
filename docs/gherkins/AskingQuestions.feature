Feature: Once a user has joined a Session they are then able to ask Questions
  of the Answerer.

  Answerers cannot ask Questions. This is so that they cannot abuse the system
  to only ask Questions they want to hear. However, there's nothing stopping
  the same person from logging on in two different sessions as both Answerer
  and Questioner.

  See the following files for more information:

  - [Questioners](Questioners.feature)
  - [Questions](Questions.feature)
  - [Sessions](Sessions.feature)

  Scenario: Authorization
    * A Questioner in a Session can ask a Question in that Session
    * Questioners in different Sessions cannot ask Questions in that Session
    * Answerers cannot ask Questions

  Scenario: Prerequisites for asking a Question
    * The Session in which a Question is being asked must be unlocked

  Scenario: Asking a Question
    Given a Session
    And a Questioner in that Session
    And a Question
    Then the Question can be asked by the Questioner in the Session
