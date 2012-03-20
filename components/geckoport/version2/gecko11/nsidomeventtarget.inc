type

  nsIDOMEventTarget = interface(nsISupports)
  ['{8e375931-298d-4d0a-9cb4-5668f0cdc5a8}']
    procedure addEventListener(atype: DOMString; listener: nsIDOMEventListener; useCapture: longbool; wantsUntrusted: longbool); safecall;
    procedure addSystemEventListener(atype: DOMString; listener: nsIDOMEventListener; aUseCapture: longbool; aWantsUntrusted: longbool); safecall;
    procedure removeEventListener(atype: DOMString; listener: nsIDOMEventListener; useCapture: longbool); safecall;
    procedure removeSystemEventListener(atype: DOMString; listener: nsIDOMEventListener; aUseCapture: longbool); safecall;
    function dispatchEvent(evt: nsIDOMEvent) : longbool; safecall;
    function GetTargetForDOMEvent() : nsIDOMEventTarget; safecall;
    function GetTargetForEventTargetChain() : nsIDOMEventTarget; safecall;
{    procedure PreHandleEvent(aVisitor: nsEventChainPreVisitorRef); safecall;
    procedure WillHandleEvent(aVisitor: nsEventChainPostVisitorRef); safecall;
    procedure PostHandleEvent(aVisitor: nsEventChainPostVisitorRef); safecall;
    procedure DispatchDOMEvent(aEvent: nsEventPtr; aDOMEvent: nsIDOMEvent; aPresContext: nsPresContextPtr; aEventStatus: nsEventStatusPtr); safecall;
    function GetListenerManager(aMayCreate: longbool) : nsEventListenerManagerPtr; safecall;
    function GetContextForEventHandlers(out aRv: nsresult) : nsIScriptContext; safecall;
    function GetJSContextForEventHandlers() : JSContextPtr; safecall;
 }
  end;

