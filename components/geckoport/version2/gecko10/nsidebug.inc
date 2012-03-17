type

  nsIDebug = interface(nsISupports)
  ['{3bf0c3d7-3bd9-4cf2-a971-33572c503e1e}']
    procedure assertion(aStr: PAnsiChar; aExpr: PAnsiChar; aFile: PAnsiChar; aLine: idllong); safecall;
    procedure warning(aStr: PAnsiChar; aFile: PAnsiChar; aLine: idllong); safecall;
    procedure break(aFile: PAnsiChar; aLine: idllong); safecall;
    procedure abort(aFile: PAnsiChar; aLine: idllong); safecall;

  end;

