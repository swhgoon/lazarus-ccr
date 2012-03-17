type

  nsIDOMBarProp = interface(nsISupports)
  ['{9eb2c150-1d56-11d3-8221-0060083a0bcf}']
    function Getvisible(): longbool; safecall;
    procedure Setvisible(avisible: longbool); safecall;
    property visible : longbool read Getvisible write Setvisible;

  end;

