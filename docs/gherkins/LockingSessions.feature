Feature: Sometimes it is necessary to prevent existing Proposals from being
  modified and further Proposals from being added during a Session. Commonly
  this occurs when a Session is over, but can also be necessary in the case of
  abuse or overwhelming volume. To do this a Session must be locked.

  See the following files for more information:

  - [Moderators](Moderators.feature)
  - [Proposers](Proposers.feature)
  - [Sessions](Sessions.feature)

  Scenario: Authorization
    * The Moderator for a Session can lock that Session
    * Other Moderators cannot lock that Session
    * Proposers cannot lock any Session

  Scenario: Prerequisites for locking a Session
    * Only unlocked Sessions may be locked
    * Locked Sessions cannot be locked again
