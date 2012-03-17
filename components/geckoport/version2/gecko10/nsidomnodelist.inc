type

  nsIDOMNodeList = interface(nsISupports)
  ['{496852ba-e48d-4fa5-982e-e0dc1b475bf1}']
    function item(index: idlulong) : nsIDOMNode; safecall;
    function Getlength(): idlulong; safecall;
    property length : idlulong read Getlength;
    function getNodeAt(index: idlulong) : nsIContent; safecall;

  end;

