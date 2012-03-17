type

  nsIDOMPkcs11 = interface(nsISupports)
  ['{9fd42950-25e7-11d4-8a7d-006008c844c3}']
    function deletemodule(moduleName: DOMString) : idllong; safecall;
    function addmodule(moduleName: DOMString; libraryFullPath: DOMString; cryptoMechanismFlags: idllong; cipherFlags: idllong) : idllong; safecall;

  end;

