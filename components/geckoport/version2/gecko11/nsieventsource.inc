type

  nsIEventSource = interface(nsISupports)
  ['{a3d3181e-47c1-4f2e-b2c7-94775a86f5c5}']
    procedure Geturl( result_: DOMString); safecall;
    function GetreadyState(): idllong; safecall;
    property readyState : idllong read GetreadyState;
    function GetwithCredentials(): longbool; safecall;
    property withCredentials : longbool read GetwithCredentials;
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
    procedure init(principal: nsIPrincipal; scriptContext: nsIScriptContext; ownerWindow: nsPIDOMWindow; anurl: DOMString; awithCredentials: longbool); safecall;

  end;

const
  nsIEventSource_CONNECTING=0;
  nsIEventSource_OPEN=1;
  nsIEventSource_CLOSED=2;

