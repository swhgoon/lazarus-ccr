type

  nsITraceRefcnt = interface(nsISupports)
  ['{273dc92f-0fe6-4545-96a9-21be77828039}']
    procedure logAddRef(aPtr: voidPtr; aNewRefcnt: nsrefcnt; aTypeName: PAnsiChar; aInstanceSize: idlulong); safecall;
    procedure logRelease(aPtr: voidPtr; aNewRefcnt: nsrefcnt; aTypeName: PAnsiChar); safecall;
    procedure logCtor(aPtr: voidPtr; aTypeName: PAnsiChar; aInstanceSize: idlulong); safecall;
    procedure logDtor(aPtr: voidPtr; aTypeName: PAnsiChar; aInstanceSize: idlulong); safecall;
    procedure logAddCOMPtr(aPtr: voidPtr; aObject: nsISupports); safecall;
    procedure logReleaseCOMPtr(aPtr: voidPtr; aObject: nsISupports); safecall;

  end;

