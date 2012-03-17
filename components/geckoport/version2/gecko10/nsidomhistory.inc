type

  nsIDOMHistory = interface(nsISupports)
  ['{d5a3006b-dd6b-4ba3-81be-6559f8889e60}']
    function Getlength(): idllong; safecall;
    property length : idllong read Getlength;
    function Getcurrent(): DOMString; safecall;
    property current : DOMString read Getcurrent;
    function Getprevious(): DOMString; safecall;
    property previous : DOMString read Getprevious;
    function Getnext(): DOMString; safecall;
    property next : DOMString read Getnext;
    procedure back(); safecall;
    procedure forward(); safecall;
    procedure go(aDelta: idllong); safecall;
    function item(index: idlulong) : DOMString; safecall;
    procedure pushState(aData: nsIVariant; aTitle: DOMString; aURL: DOMString); safecall;
    procedure replaceState(aData: nsIVariant; aTitle: DOMString; aURL: DOMString); safecall;
    function Getstate(): nsIVariant; safecall;
    property state : nsIVariant read Getstate;

  end;

