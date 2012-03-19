type

  nsIDOMMediaList = interface(nsISupports)
  ['{9b0c2ed7-111c-4824-adf9-ef0da6dad371}']
    procedure GetmediaText( result_: DOMString); safecall;
    procedure SetmediaText(amediaText: DOMString); safecall;
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    procedure item(index: idlulong; result_: DOMString); safecall;
    procedure deleteMedium(oldMedium: DOMString); safecall;
    procedure appendMedium(newMedium: DOMString); safecall;

  end;

