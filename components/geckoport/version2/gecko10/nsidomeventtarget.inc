type

  nsIDOMEventTarget = interface(nsISupports)
  ['{1797d5a4-b12a-428d-9eef-a0e13839728c}']
    procedure addEventListener(atype: DOMString; listener: nsIDOMEventListener; useCapture: longbool; wantsUntrusted: longbool); safecall;
    procedure removeEventListener(atype: DOMString; listener: nsIDOMEventListener; useCapture: longbool); safecall;
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

