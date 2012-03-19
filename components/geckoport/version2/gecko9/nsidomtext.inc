type

  nsIDOMText = interface(nsIDOMCharacterData)
  ['{92b0df87-78a1-4e3b-a23c-d0c5bb2b83f9}']
    function splitText(offset: idlulong) : nsIDOMText; safecall;
    function GetisElementContentWhitespace(): longbool; safecall;
    property isElementContentWhitespace : longbool read GetisElementContentWhitespace;
    procedure GetwholeText( result_: DOMString); safecall;
    function replaceWholeText(content: DOMString) : nsIDOMText; safecall;

  end;

