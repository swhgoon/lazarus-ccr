type

  nsILoadGroup = interface(nsIRequest)
  ['{3de0a31c-feaf-400f-9f1e-4ef71f8b20cc}']
    function GetgroupObserver(): nsIRequestObserver; safecall;
    procedure SetgroupObserver(agroupObserver: nsIRequestObserver); safecall;
    property groupObserver : nsIRequestObserver read GetgroupObserver write SetgroupObserver;
    function GetdefaultLoadRequest(): nsIRequest; safecall;
    procedure SetdefaultLoadRequest(adefaultLoadRequest: nsIRequest); safecall;
    property defaultLoadRequest : nsIRequest read GetdefaultLoadRequest write SetdefaultLoadRequest;
    procedure addRequest(aRequest: nsIRequest; aContext: nsISupports); safecall;
    procedure removeRequest(aRequest: nsIRequest; aContext: nsISupports; aStatus: nsresult); safecall;
    function Getrequests(): nsISimpleEnumerator; safecall;
    property requests : nsISimpleEnumerator read Getrequests;
    function GetactiveCount(): idlulong; safecall;
    property activeCount : idlulong read GetactiveCount;
    function GetnotificationCallbacks(): nsIInterfaceRequestor; safecall;
    procedure SetnotificationCallbacks(anotificationCallbacks: nsIInterfaceRequestor); safecall;
    property notificationCallbacks : nsIInterfaceRequestor read GetnotificationCallbacks write SetnotificationCallbacks;

  end;

