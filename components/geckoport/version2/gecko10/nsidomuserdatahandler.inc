type

  nsIDOMUserDataHandler = interface(nsISupports)
  ['{91afebdd-a201-4db0-b728-9d59580f0cfd}']
    procedure handle(operation: idlushort; key: DOMString; data: nsIVariant; src: nsIDOMNode; dst: nsIDOMNode); safecall;

  end;

const
  nsIDOMUserDataHandler_NODE_CLONED=1;
  nsIDOMUserDataHandler_NODE_IMPORTED=2;
  nsIDOMUserDataHandler_NODE_DELETED=3;
  nsIDOMUserDataHandler_NODE_RENAMED=4;
  nsIDOMUserDataHandler_NODE_ADOPTED=5;

