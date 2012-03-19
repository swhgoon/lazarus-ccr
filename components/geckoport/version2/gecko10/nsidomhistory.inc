type

  nsIDOMHistory = interface(nsISupports)
  ['{d5a3006b-dd6b-4ba3-81be-6559f8889e60}']
    function Getlength(): idllong; safecall;
    property length : idllong read Getlength;
    procedure Getcurrent( result_: DOMString); safecall;
    procedure Getprevious( result_: DOMString); safecall;
    procedure Getnext( result_: DOMString); safecall;
    procedure back(); safecall;
    procedure forward(); safecall;
    procedure go(aDelta: idllong); safecall;
    procedure item(index: idlulong; result_: DOMString); safecall;
    procedure pushState(aData: nsIVariant; aTitle: DOMString; aURL: DOMString); safecall;
    procedure replaceState(aData: nsIVariant; aTitle: DOMString; aURL: DOMString); safecall;
    function Getstate(): nsIVariant; safecall;
    property state : nsIVariant read Getstate;

  end;

