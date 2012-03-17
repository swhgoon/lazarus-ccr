type

  nsIControllers = interface(nsISupports)
  ['{f36e3ec1-9197-4ad8-8d4c-d3b1927fd6df}']
    function getControllerForCommand(command: PAnsiChar) : nsIController; safecall;
    procedure insertControllerAt(index: idlulong; controller: nsIController); safecall;
    function removeControllerAt(index: idlulong) : nsIController; safecall;
    function getControllerAt(index: idlulong) : nsIController; safecall;
    procedure appendController(controller: nsIController); safecall;
    procedure removeController(controller: nsIController); safecall;
    function getControllerId(controller: nsIController) : idlulong; safecall;
    function getControllerById(controllerID: idlulong) : nsIController; safecall;
    function getControllerCount() : idlulong; safecall;

  end;

