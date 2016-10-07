Feature: In order to determine which Sessions to join a user may list all
  current Sessions.

  See the following files for more information:

  - [Answerer](Answerers.feature)
  - [Questioners](Questioners.feature)
  - [Questions](Questions.feature)
  - [Sessions](Sessions.feature)
  - [Users](Users.feature)

  Scenario: Authorization
    * There is no authorization for listing Sessions

  Scenario: Listing Sessions
    * Users may list all sessions
    * A Session in the list includes the following information:
      | Session Name | Session Locked Status | Answerer | Number of Questions | Number of Questioners |
