type

  nsIWebBrowserFind = interface(nsISupports)
  ['{2f977d44-5485-11d4-87e2-0010a4e75ef2}']
    function findNext() : longbool; safecall;
    function GetsearchString(): PWideChar; safecall;
    procedure SetsearchString(asearchString: PWideChar); safecall;
    property searchString : PWideChar read GetsearchString write SetsearchString;
    function GetfindBackwards(): longbool; safecall;
    procedure SetfindBackwards(afindBackwards: longbool); safecall;
    property findBackwards : longbool read GetfindBackwards write SetfindBackwards;
    function GetwrapFind(): longbool; safecall;
    procedure SetwrapFind(awrapFind: longbool); safecall;
    property wrapFind : longbool read GetwrapFind write SetwrapFind;
    function GetentireWord(): longbool; safecall;
    procedure SetentireWord(aentireWord: longbool); safecall;
    property entireWord : longbool read GetentireWord write SetentireWord;
    function GetmatchCase(): longbool; safecall;
    procedure SetmatchCase(amatchCase: longbool); safecall;
    property matchCase : longbool read GetmatchCase write SetmatchCase;
    function GetsearchFrames(): longbool; safecall;
    procedure SetsearchFrames(asearchFrames: longbool); safecall;
    property searchFrames : longbool read GetsearchFrames write SetsearchFrames;

  end;


  nsIWebBrowserFindInFrames = interface(nsISupports)
  ['{e0f5d182-34bc-11d5-be5b-b760676c6ebc}']
    function GetcurrentSearchFrame(): nsIDOMWindow; safecall;
    procedure SetcurrentSearchFrame(acurrentSearchFrame: nsIDOMWindow); safecall;
    property currentSearchFrame : nsIDOMWindow read GetcurrentSearchFrame write SetcurrentSearchFrame;
    function GetrootSearchFrame(): nsIDOMWindow; safecall;
    procedure SetrootSearchFrame(arootSearchFrame: nsIDOMWindow); safecall;
    property rootSearchFrame : nsIDOMWindow read GetrootSearchFrame write SetrootSearchFrame;
    function GetsearchSubframes(): longbool; safecall;
    procedure SetsearchSubframes(asearchSubframes: longbool); safecall;
    property searchSubframes : longbool read GetsearchSubframes write SetsearchSubframes;
    function GetsearchParentFrames(): longbool; safecall;
    procedure SetsearchParentFrames(asearchParentFrames: longbool); safecall;
    property searchParentFrames : longbool read GetsearchParentFrames write SetsearchParentFrames;

  end;

