type

  nsIDirectoryServiceProvider = interface(nsISupports)
  ['{bbf8cab0-d43a-11d3-8cc2-00609792278c}']
    function getFile(prop: PAnsiChar; out persistent: longbool) : nsIFile; safecall;

  end;


  nsIDirectoryServiceProvider2 = interface(nsIDirectoryServiceProvider)
  ['{2f977d4b-5485-11d4-87e2-0010a4e75ef2}']
    function getFiles(prop: PAnsiChar) : nsISimpleEnumerator; safecall;

  end;


  nsIDirectoryService = interface(nsISupports)
  ['{57a66a60-d43a-11d3-8cc2-00609792278c}']
    procedure init(); safecall;
    procedure registerProvider(prov: nsIDirectoryServiceProvider); safecall;
    procedure unregisterProvider(prov: nsIDirectoryServiceProvider); safecall;

  end;

