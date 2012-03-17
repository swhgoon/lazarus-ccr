type

  nsISerializable = interface(nsISupports)
  ['{91cca981-c26d-44a8-bebe-d9ed4891503a}']
    procedure read(aInputStream: nsIObjectInputStream); safecall;
    procedure write(aOutputStream: nsIObjectOutputStream); safecall;

  end;

