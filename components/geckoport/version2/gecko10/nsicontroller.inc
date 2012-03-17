type

  nsIController = interface(nsISupports)
  ['{D5B61B82-1DA4-11d3-BF87-00105A1B0627}']
    function isCommandEnabled(command: PAnsiChar) : longbool; safecall;
    function supportsCommand(command: PAnsiChar) : longbool; safecall;
    procedure doCommand(command: PAnsiChar); safecall;
    procedure onEvent(eventName: PAnsiChar); safecall;

  end;


  nsICommandController = interface(nsISupports)
  ['{EBE55080-C8A9-11D5-A73C-DD620D6E04BC}']
    procedure getCommandStateWithParams(command: PAnsiChar; aCommandParams: nsICommandParams); safecall;
    procedure doCommandWithParams(command: PAnsiChar; aCommandParams: nsICommandParams); safecall;

  end;


  nsIControllerCommandGroup = interface(nsISupports)
  ['{9F82C404-1C7B-11D5-A73C-ECA43CA836FC}']
    procedure addCommandToGroup(aCommand: PAnsiChar; aGroup: PAnsiChar); safecall;
    procedure removeCommandFromGroup(aCommand: PAnsiChar; aGroup: PAnsiChar); safecall;
    function isCommandInGroup(aCommand: PAnsiChar; aGroup: PAnsiChar) : longbool; safecall;
    function getGroupsEnumerator() : nsISimpleEnumerator; safecall;
    function getEnumeratorForGroup(aGroup: PAnsiChar) : nsISimpleEnumerator; safecall;

  end;

