Feature: A Session may be viewed in a number of different situations. Answerers
  and Questioners need to view a Session as that's how they interact with each
  other. Users also may view a Session in order to determine if they want to
  join the Session.

  See the following files for more information:

  - [Answerers](Answerers.feature)
  - [Questioners](Questioners.feature)
  - [Questions](Questions.feature)
  - [Sessions](Sessions.feature)
  - [Users](Users.feature)

  Scenario: Authorization
    * There is no authorization for listing Sessions

  Scenario: Previewing a Session
    * A user may preview a single Session
    * A previewed Session includes the following information:
      | Session Name | Session Locked Status | Answerer | All Questions | All Questioners |
