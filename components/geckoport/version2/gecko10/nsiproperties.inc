type

  nsIProperties = interface(nsISupports)
  ['{78650582-4e93-4b60-8e85-26ebd3eb14ca}']
  // iid parameter is manually converted to constref/TGuid, so
  // that it is possible to assign an interface to this parameter.
    procedure get(prop: PAnsiChar; constref iid: TGuid; out result); safecall;
    procedure aset(prop: PAnsiChar; value: nsISupports); safecall;
    function has(prop: PAnsiChar) : longbool; safecall;
    procedure undefine(prop: PAnsiChar); safecall;
    procedure getKeys(out count: PRUint32; out keys: PAnsiChar); safecall;

  end;

