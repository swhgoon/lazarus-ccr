type

  nsIDOMMediaList = interface(nsISupports)
  ['{9b0c2ed7-111c-4824-adf9-ef0da6dad371}']
    function GetmediaText(): DOMString; safecall;
    procedure SetmediaText(amediaText: DOMString); safecall;
    property mediaText : DOMString read GetmediaText write SetmediaText;
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    function item(index: idlulong) : DOMString; safecall;
    procedure deleteMedium(oldMedium: DOMString); safecall;
    procedure appendMedium(newMedium: DOMString); safecall;

  end;

