type

  nsIDocumentCharsetInfo = interface(nsISupports)
  ['{2d40b291-01e1-11d4-9d0e-0050040007b2}']
    function GetforcedCharset(): nsIAtom; safecall;
    procedure SetforcedCharset(aforcedCharset: nsIAtom); safecall;
    property forcedCharset : nsIAtom read GetforcedCharset write SetforcedCharset;
    function GetforcedDetector(): longbool; safecall;
    procedure SetforcedDetector(aforcedDetector: longbool); safecall;
    property forcedDetector : longbool read GetforcedDetector write SetforcedDetector;
    function GetparentCharset(): nsIAtom; safecall;
    procedure SetparentCharset(aparentCharset: nsIAtom); safecall;
    property parentCharset : nsIAtom read GetparentCharset write SetparentCharset;
    function GetparentCharsetSource(): PRInt32; safecall;
    procedure SetparentCharsetSource(aparentCharsetSource: PRInt32); safecall;
    property parentCharsetSource : PRInt32 read GetparentCharsetSource write SetparentCharsetSource;

  end;

