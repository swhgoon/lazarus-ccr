type

  nsIDOMText = interface(nsIDOMCharacterData)
  ['{437ed60c-febd-4bd0-892f-cf358adc3c96}']
    function splitText(offset: idlulong) : nsIDOMText; safecall;
    procedure GetwholeText( result_: DOMString); safecall;

  end;

