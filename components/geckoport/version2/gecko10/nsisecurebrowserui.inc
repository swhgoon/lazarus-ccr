type

  nsISecureBrowserUI = interface(nsISupports)
  ['{081e31e0-a144-11d3-8c7c-00609792278c}']
    procedure init(window: nsIDOMWindow); safecall;
    function Getstate(): idlulong; safecall;
    property state : idlulong read Getstate;
    function GettooltipText(): AString; safecall;
    property tooltipText : AString read GettooltipText;

  end;

