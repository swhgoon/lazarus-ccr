type

  nsIDOMOfflineResourceList = interface(nsISupports)
  ['{f394a721-66e9-46fc-bb24-b980bb732dd0}']
    function GetmozItems(): nsIDOMDOMStringList; safecall;
    property mozItems : nsIDOMDOMStringList read GetmozItems;
    function mozHasItem(uri: DOMString) : longbool; safecall;
    function GetmozLength(): idlulong; safecall;
    property mozLength : idlulong read GetmozLength;
    function mozItem(index: idlulong) : DOMString; safecall;
    procedure mozAdd(uri: DOMString); safecall;
    procedure mozRemove(uri: DOMString); safecall;
    function Getstatus(): idlushort; safecall;
    property status : idlushort read Getstatus;
    procedure update(); safecall;
    procedure swapCache(); safecall;
    function Getonchecking(): nsIDOMEventListener; safecall;
    procedure Setonchecking(aonchecking: nsIDOMEventListener); safecall;
    property onchecking : nsIDOMEventListener read Getonchecking write Setonchecking;
    function Getonerror(): nsIDOMEventListener; safecall;
    procedure Setonerror(aonerror: nsIDOMEventListener); safecall;
    property onerror : nsIDOMEventListener read Getonerror write Setonerror;
    function Getonnoupdate(): nsIDOMEventListener; safecall;
    procedure Setonnoupdate(aonnoupdate: nsIDOMEventListener); safecall;
    property onnoupdate : nsIDOMEventListener read Getonnoupdate write Setonnoupdate;
    function Getondownloading(): nsIDOMEventListener; safecall;
    procedure Setondownloading(aondownloading: nsIDOMEventListener); safecall;
    property ondownloading : nsIDOMEventListener read Getondownloading write Setondownloading;
    function Getonprogress(): nsIDOMEventListener; safecall;
    procedure Setonprogress(aonprogress: nsIDOMEventListener); safecall;
    property onprogress : nsIDOMEventListener read Getonprogress write Setonprogress;
    function Getonupdateready(): nsIDOMEventListener; safecall;
    procedure Setonupdateready(aonupdateready: nsIDOMEventListener); safecall;
    property onupdateready : nsIDOMEventListener read Getonupdateready write Setonupdateready;
    function Getoncached(): nsIDOMEventListener; safecall;
    procedure Setoncached(aoncached: nsIDOMEventListener); safecall;
    property oncached : nsIDOMEventListener read Getoncached write Setoncached;
    function Getonobsolete(): nsIDOMEventListener; safecall;
    procedure Setonobsolete(aonobsolete: nsIDOMEventListener); safecall;
    property onobsolete : nsIDOMEventListener read Getonobsolete write Setonobsolete;

  end;

const
  nsIDOMOfflineResourceList_UNCACHED=0;
  nsIDOMOfflineResourceList_IDLE=1;
  nsIDOMOfflineResourceList_CHECKING=2;
  nsIDOMOfflineResourceList_DOWNLOADING=3;
  nsIDOMOfflineResourceList_UPDATEREADY=4;
  nsIDOMOfflineResourceList_OBSOLETE=5;

