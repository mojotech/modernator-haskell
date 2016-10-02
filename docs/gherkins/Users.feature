Feature: The users of Modernator are free to take on one of two defined roles.
  A user may take on the role of an Answerer in order to answer Questions posed
  by Questioners. A user may also take on the role of a Questioner to pose
  Questions to Answerers. A user may be many Answerers and many Questioners at
  the same time, but a user may not be both an Answerer and a Questioner in the
  same Session.

  See the following files for more information:

  - [Creating a Session](CreatingSessions.feature)
  - [Listing all Sessions](ListingSessions.feature)
  - [Viewing a Session](ViewingSessions.feature)
  - [Answerers](Answerers.feature)
  - [Questioners](Questioners.feature)
  - [Sessions](Sessions.feature)

  Scenario: A user may take on the role of an Answerer
    * See Answerers.feature

  Scenario: A user may take on the role of a Questioner
    * See Questioners.feature

  Scenario: A user may list all Sessions
    * See ListingSessions.feature

  Scenario: A user may view a Session
    * See ViewingSessions.feature

  Scenario: A user may create a Session
    * See CreatingSessions.feature

  Scenario: Restrictions
    * A user may not be both an Answerer and a Questioner in the same Session

  Scenario: Multiplicity
    * A user may be a Questioner in many Sessions
    * A user may be an Answerer in many Sessions
