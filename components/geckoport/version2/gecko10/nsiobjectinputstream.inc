type

  nsIObjectInputStream = interface(nsIBinaryInputStream)
  ['{6c248606-4eae-46fa-9df0-ba58502368eb}']
    function readObject(aIsStrongRef: longbool) : nsISupports; safecall;
    function readID(out aID: nsID) : nsresult; safecall;
    function getBuffer(aLength: PRUint32; aAlignMask: PRUint32) : charPtr; safecall;
    procedure putBuffer(aBuffer: charPtr; aLength: PRUint32); safecall;

  end;

