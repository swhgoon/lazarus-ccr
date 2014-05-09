Apache must be configured to route requests to wst services

  <Location /wst>
    SetHandler wst-handler
  </Location>

Services can then be invoked through the following addressing schema
http://127.0.0.1:8080/wst/services/UserService

  UserService  : the target service
  wst/services : constant.