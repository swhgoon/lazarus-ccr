type

  nsIEventSource = interface(nsISupports)
  ['{755e2d2d-a836-4539-83f4-16b51156341f}']
    function Geturl(): DOMString; safecall;
    property url : DOMString read Geturl;
    function GetreadyState(): idllong; safecall;
    property readyState : idllong read GetreadyState;
    function Getonopen(): nsIDOMEventListener; safecall;
    procedure Setonopen(aonopen: nsIDOMEventListener); safecall;
    property onopen : nsIDOMEventListener read Getonopen write Setonopen;
    function Getonmessage(): nsIDOMEventListener; safecall;
    procedure Setonmessage(aonmessage: nsIDOMEventListener); safecall;
    property onmessage : nsIDOMEventListener read Getonmessage write Setonmessage;
    function Getonerror(): nsIDOMEventListener; safecall;
    procedure Setonerror(aonerror: nsIDOMEventListener); safecall;
    property onerror : nsIDOMEventListener read Getonerror write Setonerror;
    procedure close(); safecall;
    procedure init(principal: nsIPrincipal; scriptContext: nsIScriptContext; ownerWindow: nsPIDOMWindow; anurl: DOMString); safecall;

  end;

const
  nsIEventSource_CONNECTING=0;
  nsIEventSource_OPEN=1;
  nsIEventSource_CLOSED=2;

