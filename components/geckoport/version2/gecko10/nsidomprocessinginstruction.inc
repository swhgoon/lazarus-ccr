type

  nsIDOMProcessingInstruction = interface(nsIDOMCharacterData)
  ['{d754433f-4637-4a5f-9034-0388173ea254}']
    function Gettarget(): DOMString; safecall;
    property target : DOMString read Gettarget;

  end;

