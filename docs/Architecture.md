# Architecture

This application has four main segments: the domain model structure, commands
for working with the domain model, the main API, and the Websocket API.

## Entry Point

The file `src/Modernator/App.hs` contains the entry point for the application.
All initial setup and configuration goes here as well. The function `mkApp` is
the main entry point of the application.

The file `src/Main.hs` contains the logic for parsing command line and
environment arguments. It is also responsible for setting up any middleware
applications such as logging.

## Domain Model

The file `src/Modernator/Types.hs` contains all of the structures and
miscellaneous helper functions for working with the domain model.

### App

The domain model starts with the App structure. It contains four pairs of sets
and the next identifier in sequence for new members of those sets: Sessions,
Questions, Answerers, and Questioners. Every field is required. Each set of
objects starts empty and their sequences start at 1. This structure essentially
mimics what would be done in a traditional RDBMS with tables and automatically
incrementing primary keys. Because this is not a traditional RDBMS constraints
and automatic functions such as foreign keys and autoincrementing primary keys
must be handled manually. This is a trade-off I think was worth it in order to
dodge the object-relational impedance mismatch. Do note that each primary
identifier for an object is a distinct type, so it's impossible to mix up
primary keys accidentally.

### Sessions

A Session is a combination of numeric identifier, name, expiration date, and
whether or not the session is locked. The expiration date is optional. When a
session's expiration date is met, the session is locked. When a session is
locked no new questions can be asked, and existing questions can't be modified.
A potential simplification to architecture made possible by AcidState would be
to store the Answerer, Questioner set, and Questions set on the session as well.
This better specifies the structure and relationship of those types, and makes
session management simpler. However it also makes management of those individual
types more difficult as they are now always nested under a Session.

### Questions

A Question is a combination of numeric identifier, session identifier, text,
number of votes, and answered status. Votes are implemented as an integer with a
subset of integer functionality. Votes can only be incremented, and cannot be
manipulated otherwise. If a question is answered, then it can no longer be
voted.

### Answerers

An Answerer is a combination of numeric identifier, session identifier, and
name. The Answerer is the person who is answering questions. They are the only
user allowed to mark a Question as answered. There is allowed one Answerer per
Session.

### Questioners

A Questioner is a combination of numeric identifier, session identifier, and
name. Names are optional for a Questioner. Questioners are the only users
allowed to submit and vote on questions. There can be many Questioners per
Session.

### FullSession

A FullSession is a transient model that contains a Session, Answerer, set of
Questions, and set of Questioners. It is only used when fetching the full state
of a Session.

### SessionMessage

SessionMessages are used in the websocket API for notifying clients of events
that happened in a session. They are structured as a sum type with separate
constructors for each message type.

### AppError

A custom error type allows the application to be more specific when handling
situations that are erroneous. This type is used directly when sending
SessionMessages, but is translated to more generic HTTP error codes when an
error occurs over HTTP.

### Migrations

The file `src/Modernator/Migrations.hs` contains the safecopy implementations of
the domain model structures. It should also contain any migrations necessary
when the application structure changes.

## Commands

The file `src/Modernator/Commands.hs` contains all of the commands for
interacting with the domain model. Each command runs inside of a transaction.
There are also some miscellaneous private helper functions.

## API

The file `src/Modernator/API.hs` contains the entry point for the API
definition. It imports both `src/Modernator/SessionsAPI.hs` and
`src/Modernator/WebsocketsAPI.hs`. It also sets up the SwaggerUI and swagger.js
endpoints.

### Sessions API

The file `src/Modernator/SessionsAPI.hs` contains the definitions and handlers
for the endpoints for managing sessions.

### Websockets API

The file `src/Modernator/WebsocketsAPI.hs` contains the definitions and WAI
application for the websockets endpoint. The websocket application requires both
the application for handling websockets, as well as a backup application for
when a non-websocket request is made to that endpoint. The backup application
re-uses the `fullSessionHandler` from the sessions API to serve a single
`SessionStarted` message.

## Miscellaneous Datatypes

There are a few miscellaneous datatypes used in the processing of requests that
don't fit with the main application model. These are cookie structures, and
request body structures.

### Cookies

The file `src/Modernator/Cookies.hs` contains the data structures and functions
for representing and handling request and response cookies.

### Request Bodies

The file `src/Modernator/RequestBodies.hs` contains the data structures and
functions for representing and handling request bodies.
