type

  nsIHttpChannel = interface(nsIChannel)
  ['{9277fe09-f0cc-4cd9-bbce-581dd94b0260}']
    function GetrequestMethod(): ACString; safecall;
    procedure SetrequestMethod(arequestMethod: ACString); safecall;
    property requestMethod : ACString read GetrequestMethod write SetrequestMethod;
    function Getreferrer(): nsIURI; safecall;
    procedure Setreferrer(areferrer: nsIURI); safecall;
    property referrer : nsIURI read Getreferrer write Setreferrer;
    function getRequestHeader(aHeader: ACString) : ACString; safecall;
    procedure setRequestHeader(aHeader: ACString; aValue: ACString; aMerge: longbool); safecall;
    procedure visitRequestHeaders(aVisitor: nsIHttpHeaderVisitor); safecall;
    function GetallowPipelining(): longbool; safecall;
    procedure SetallowPipelining(aallowPipelining: longbool); safecall;
    property allowPipelining : longbool read GetallowPipelining write SetallowPipelining;
    function GetredirectionLimit(): idlulong; safecall;
    procedure SetredirectionLimit(aredirectionLimit: idlulong); safecall;
    property redirectionLimit : idlulong read GetredirectionLimit write SetredirectionLimit;
    function GetresponseStatus(): idlulong; safecall;
    property responseStatus : idlulong read GetresponseStatus;
    function GetresponseStatusText(): ACString; safecall;
    property responseStatusText : ACString read GetresponseStatusText;
    function GetrequestSucceeded(): longbool; safecall;
    property requestSucceeded : longbool read GetrequestSucceeded;
    function getResponseHeader(header: ACString) : ACString; safecall;
    procedure setResponseHeader(header: ACString; value: ACString; merge: longbool); safecall;
    procedure visitResponseHeaders(aVisitor: nsIHttpHeaderVisitor); safecall;
    function isNoStoreResponse() : longbool; safecall;
    function isNoCacheResponse() : longbool; safecall;

  end;

