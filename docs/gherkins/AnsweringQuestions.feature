Feature: While Modernator does not provide facilities to distribute an answer
  to a Question, it is still important to indicate that a Question has been
  answered. This indicates that the Answerer and Questioners need not spend
  more time on it.

  See the following files for more information:

  - [Answerers](Answerers.feature)
  - [Questions](Questions.feature)
  - [Sessions](Sessions.feature)

  Scenario: Authorization
    * The Answerer for the Session a Question is in can answer that Question
    * Other Answerers cannot answer Questions in the Session
    * Questioners cannot answer Questions

  Scenario: Prerequisites for answering a Question
    * The Session in which the Question is being answered must be unlocked
    * The Question must not be answered

  Scenario: Answering a Question
    Given a Session
    And an Answerer for the Session
    And a Question in that Session
    Then the Question can be answered by the Answerer

