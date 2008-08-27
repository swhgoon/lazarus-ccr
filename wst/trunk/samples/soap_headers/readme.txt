Web Services Toolkit Soap headers sample
========================================

This sample presents server side and client header management.

Use Case :
  The first time the client connect to the server, the client
  send its user name and password using the TLoginHeader class.

  The server accepts the client and returns a session token that
  should be used by the client for its next invocations. The
  server uses the TSessionHeader class to return the session
  token.

  The client uses the server's returned session token for its
  next calls to the server.

Note : Define the LOG_TO_FILE symbol {$DEFINE LOG_TO_FILE} in
       the server program to log the exchanged messages of the
       communication with clients.