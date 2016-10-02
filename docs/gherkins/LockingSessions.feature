Feature: Sometimes it is necessary to prevent existing Questions from being
  modified and further Questions from being asked during a Session. Commonly
  this occurs when a Session is over, but can also be necessary in the case of
  abuse or overwhelming volume. To do this a Session must be locked.

  See the following files for more information:

  - [Answerers](Answerers.feature)
  - [Questioners](Questioners.feature)
  - [Sessions](Sessions.feature)

  Scenario: Authorization
    * The Answerer for a Session can lock that Session
    * Other Answerers cannot lock that Session
    * Questioners cannot lock any Session

  Scenario: Prerequisites for locking a Session
    * Only unlocked Sessions may be locked
    * Locked Sessions cannot be locked again
