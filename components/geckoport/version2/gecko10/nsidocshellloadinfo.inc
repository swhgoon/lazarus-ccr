type
  nsDocShellInfoLoadType = idllong;

type

  nsIDocShellLoadInfo = interface(nsISupports)
  ['{92a0a637-373e-4647-9476-ead11e005c75}']
    function Getreferrer(): nsIURI; safecall;
    procedure Setreferrer(areferrer: nsIURI); safecall;
    property referrer : nsIURI read Getreferrer write Setreferrer;
    function Getowner(): nsISupports; safecall;
    procedure Setowner(aowner: nsISupports); safecall;
    property owner : nsISupports read Getowner write Setowner;
    function GetinheritOwner(): longbool; safecall;
    procedure SetinheritOwner(ainheritOwner: longbool); safecall;
    property inheritOwner : longbool read GetinheritOwner write SetinheritOwner;
    function GetownerIsExplicit(): longbool; safecall;
    procedure SetownerIsExplicit(aownerIsExplicit: longbool); safecall;
    property ownerIsExplicit : longbool read GetownerIsExplicit write SetownerIsExplicit;
    function GetloadType(): nsDocShellInfoLoadType; safecall;
    procedure SetloadType(aloadType: nsDocShellInfoLoadType); safecall;
    property loadType : nsDocShellInfoLoadType read GetloadType write SetloadType;
    function GetSHEntry(): nsISHEntry; safecall;
    procedure SetSHEntry(aSHEntry: nsISHEntry); safecall;
    property SHEntry : nsISHEntry read GetSHEntry write SetSHEntry;
    function Gettarget(): PWideChar; safecall;
    procedure Settarget(atarget: PWideChar); safecall;
    property target : PWideChar read Gettarget write Settarget;
    function GetpostDataStream(): nsIInputStream; safecall;
    procedure SetpostDataStream(apostDataStream: nsIInputStream); safecall;
    property postDataStream : nsIInputStream read GetpostDataStream write SetpostDataStream;
    function GetheadersStream(): nsIInputStream; safecall;
    procedure SetheadersStream(aheadersStream: nsIInputStream); safecall;
    property headersStream : nsIInputStream read GetheadersStream write SetheadersStream;
    function GetsendReferrer(): longbool; safecall;
    procedure SetsendReferrer(asendReferrer: longbool); safecall;
    property sendReferrer : longbool read GetsendReferrer write SetsendReferrer;

  end;

const
  nsIDocShellLoadInfo_loadNormal=0;
  nsIDocShellLoadInfo_loadNormalReplace=1;
  nsIDocShellLoadInfo_loadHistory=2;
  nsIDocShellLoadInfo_loadReloadNormal=3;
  nsIDocShellLoadInfo_loadReloadBypassCache=4;
  nsIDocShellLoadInfo_loadReloadBypassProxy=5;
  nsIDocShellLoadInfo_loadReloadBypassProxyAndCache=6;
  nsIDocShellLoadInfo_loadLink=7;
  nsIDocShellLoadInfo_loadRefresh=8;
  nsIDocShellLoadInfo_loadReloadCharsetChange=9;
  nsIDocShellLoadInfo_loadBypassHistory=10;
  nsIDocShellLoadInfo_loadStopContent=11;
  nsIDocShellLoadInfo_loadStopContentAndReplace=12;
  nsIDocShellLoadInfo_loadNormalExternal=13;
  nsIDocShellLoadInfo_loadNormalBypassCache=14;
  nsIDocShellLoadInfo_loadNormalBypassProxy=15;
  nsIDocShellLoadInfo_loadNormalBypassProxyAndCache=16;
  nsIDocShellLoadInfo_loadPushState=17;

