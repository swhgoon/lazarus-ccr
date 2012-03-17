type

  nsITooltipListener = interface(nsISupports)
  ['{44b78386-1dd2-11b2-9ad2-e4eee2ca1916}']
    procedure onShowTooltip(aXCoords: idllong; aYCoords: idllong; aTipText: PWideChar); safecall;
    procedure onHideTooltip(); safecall;

  end;

