type

  nsIDOMFileError = interface(nsISupports)
  ['{4BDAFB64-15E2-49C1-A090-4315A7884A56}']
    function Getcode(): idlushort; safecall;
    property code : idlushort read Getcode;

  end;

const
  nsIDOMFileError_NOT_FOUND_ERR=1;
  nsIDOMFileError_SECURITY_ERR=2;
  nsIDOMFileError_ABORT_ERR=3;
  nsIDOMFileError_NOT_READABLE_ERR=4;
  nsIDOMFileError_ENCODING_ERR=5;

