type

  nsICancelable = interface(nsISupports)
  ['{d94ac0a0-bb18-46b8-844e-84159064b0bd}']
    procedure cancel(aReason: nsresult); safecall;

  end;

