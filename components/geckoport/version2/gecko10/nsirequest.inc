type

  nsIRequest = interface(nsISupports)
  ['{ef6bfbd2-fd46-48d8-96b7-9f8f0fd387fe}']
    function Getname(): AUTF8String; safecall;
    property name : AUTF8String read Getname;
    function isPending() : longbool; safecall;
    function Getstatus(): nsresult; safecall;
    property status : nsresult read Getstatus;
    procedure cancel(aStatus: nsresult); safecall;
    procedure suspend(); safecall;
    procedure resume(); safecall;
    function GetloadGroup(): nsILoadGroup; safecall;
    procedure SetloadGroup(aloadGroup: nsILoadGroup); safecall;
    property loadGroup : nsILoadGroup read GetloadGroup write SetloadGroup;
    function GetloadFlags(): nsLoadFlags; safecall;
    procedure SetloadFlags(aloadFlags: nsLoadFlags); safecall;
    property loadFlags : nsLoadFlags read GetloadFlags write SetloadFlags;

  end;

const
  nsIRequest_LOAD_REQUESTMASK=$FFFF;
  nsIRequest_LOAD_NORMAL=0;
  nsIRequest_LOAD_BACKGROUND=1 << 0;
  nsIRequest_INHIBIT_CACHING=1 << 7;
  nsIRequest_INHIBIT_PERSISTENT_CACHING=1 << 8;
  nsIRequest_LOAD_BYPASS_CACHE=1 << 9;
  nsIRequest_LOAD_FROM_CACHE=1 << 10;
  nsIRequest_VALIDATE_ALWAYS=1 << 11;
  nsIRequest_VALIDATE_NEVER=1 << 12;
  nsIRequest_VALIDATE_ONCE_PER_SESSION=1 << 13;
  nsIRequest_LOAD_ANONYMOUS=1 << 14;
  nsIRequest_LOAD_FRESH_CONNECTION=1 << 15;

