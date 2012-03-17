type
  nsDOMPerformanceNavigationType = cushort;


type

  nsIDOMPerformanceNavigation = interface(nsISupports)
  ['{a2132ad8-a841-4285-a140-338e8de6c2e0}']
    function Gettype(): nsDOMPerformanceNavigationType; safecall;
    property atype : nsDOMPerformanceNavigationType read Gettype;
    function GetredirectCount(): idlushort; safecall;
    property redirectCount : idlushort read GetredirectCount;

  end;

const
  nsIDOMPerformanceNavigation_TYPE_NAVIGATE=0;
  nsIDOMPerformanceNavigation_TYPE_RELOAD=1;
  nsIDOMPerformanceNavigation_TYPE_BACK_FORWARD=2;
  nsIDOMPerformanceNavigation_TYPE_RESERVED=255;

