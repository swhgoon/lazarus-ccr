type

  nsIDOMEvent = interface(nsISupports)
  ['{e85cff74-951f-45c1-be0c-89442ea2f500}']
    function Gettype(): DOMString; safecall;
    property atype : DOMString read Gettype;
    function Gettarget(): nsIDOMEventTarget; safecall;
    property target : nsIDOMEventTarget read Gettarget;
    function GetcurrentTarget(): nsIDOMEventTarget; safecall;
    property currentTarget : nsIDOMEventTarget read GetcurrentTarget;
    function GeteventPhase(): idlushort; safecall;
    property eventPhase : idlushort read GeteventPhase;
    function Getbubbles(): longbool; safecall;
    property bubbles : longbool read Getbubbles;
    function Getcancelable(): longbool; safecall;
    property cancelable : longbool read Getcancelable;
    function GettimeStamp(): DOMTimeStamp; safecall;
    property timeStamp : DOMTimeStamp read GettimeStamp;
    procedure stopPropagation(); safecall;
    procedure preventDefault(); safecall;
    procedure initEvent(eventTypeArg: DOMString; canBubbleArg: longbool; cancelableArg: longbool); safecall;
    function GetdefaultPrevented(): longbool; safecall;
    property defaultPrevented : longbool read GetdefaultPrevented;
    procedure stopImmediatePropagation(); safecall;

  end;

const
  nsIDOMEvent_CAPTURING_PHASE=1;
  nsIDOMEvent_AT_TARGET=2;
  nsIDOMEvent_BUBBLING_PHASE=3;

