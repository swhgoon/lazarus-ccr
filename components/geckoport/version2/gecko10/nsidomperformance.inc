type

  nsIDOMPerformance = interface(nsISupports)
  ['{446faf26-000b-4e66-a5fd-ae37c5ed6beb}']
    function Gettiming(): nsIDOMPerformanceTiming; safecall;
    property timing : nsIDOMPerformanceTiming read Gettiming;
    function Getnavigation(): nsIDOMPerformanceNavigation; safecall;
    property navigation : nsIDOMPerformanceNavigation read Getnavigation;

  end;

