type

  nsIEmbeddingSiteWindow = interface(nsISupports)
  ['{3E5432CD-9568-4bd1-8CBE-D50ABA110743}']
    procedure setDimensions(flags: idlulong; x: idllong; y: idllong; cx: idllong; cy: idllong); safecall;
    procedure getDimensions(flags: idlulong; out x: idllong; out y: idllong; out cx: idllong; out cy: idllong); safecall;
    procedure setFocus(); safecall;
    function Getvisibility(): longbool; safecall;
    procedure Setvisibility(avisibility: longbool); safecall;
    property visibility : longbool read Getvisibility write Setvisibility;
    function Gettitle(): PWideChar; safecall;
    procedure Settitle(atitle: PWideChar); safecall;
    property title : PWideChar read Gettitle write Settitle;
    function GetsiteWindow(): voidPtr; safecall;
    property siteWindow : voidPtr read GetsiteWindow;

  end;

const
  nsIEmbeddingSiteWindow_DIM_FLAGS_POSITION=1;
  nsIEmbeddingSiteWindow_DIM_FLAGS_SIZE_INNER=2;
  nsIEmbeddingSiteWindow_DIM_FLAGS_SIZE_OUTER=4;

