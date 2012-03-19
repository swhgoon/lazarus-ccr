type

  nsIDOMEvent = interface(nsISupports)
  ['{548137e8-fd2c-48c4-8635-3033f7db79e0}']
    procedure Gettype( result_: DOMString); safecall;
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

  end;

const
  nsIDOMEvent_CAPTURING_PHASE=1;
  nsIDOMEvent_AT_TARGET=2;
  nsIDOMEvent_BUBBLING_PHASE=3;

