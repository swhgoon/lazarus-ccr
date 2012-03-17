type

  nsIHistoryEntry = interface(nsISupports)
  ['{A41661D4-1417-11D5-9882-00C04FA02F40}']
    function GetURI(): nsIURI; safecall;
    property URI : nsIURI read GetURI;
    function Gettitle(): PWideChar; safecall;
    property title : PWideChar read Gettitle;
    function GetisSubFrame(): longbool; safecall;
    property isSubFrame : longbool read GetisSubFrame;

  end;

