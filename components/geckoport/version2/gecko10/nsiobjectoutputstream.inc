type

  nsIObjectOutputStream = interface(nsIBinaryOutputStream)
  ['{92c898ac-5fde-4b99-87b3-5d486422094b}']
    procedure writeObject(aObject: nsISupports; aIsStrongRef: longbool); safecall;
    procedure writeSingleRefObject(aObject: nsISupports); safecall;
    procedure writeCompoundObject(aObject: nsISupports; constref aIID: TGuid; aIsStrongRef: longbool); safecall;
    procedure writeID(aID: nsIDRef); safecall;
    function getBuffer(aLength: PRUint32; aAlignMask: PRUint32) : charPtr; safecall;
    procedure putBuffer(aBuffer: charPtr; aLength: PRUint32); safecall;

  end;

