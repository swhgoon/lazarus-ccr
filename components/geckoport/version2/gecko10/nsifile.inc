type

  nsIFile = interface(nsISupports)
  ['{c8c0a080-0868-11d3-915f-d9d889d48e3c}']
    procedure append(node: AString); safecall;
    procedure appendNative(node: ACString); safecall;
    procedure normalize(); safecall;
    procedure create(atype: idlulong; apermissions: idlulong); safecall;
    function GetleafName(): AString; safecall;
    procedure SetleafName(aleafName: AString); safecall;
    property leafName : AString read GetleafName write SetleafName;
    function GetnativeLeafName(): ACString; safecall;
    procedure SetnativeLeafName(anativeLeafName: ACString); safecall;
    property nativeLeafName : ACString read GetnativeLeafName write SetnativeLeafName;
    procedure copyTo(newParentDir: nsIFile; newName: AString); safecall;
    procedure CopyToNative(newParentDir: nsIFile; newName: ACString); safecall;
    procedure copyToFollowingLinks(newParentDir: nsIFile; newName: AString); safecall;
    procedure copyToFollowingLinksNative(newParentDir: nsIFile; newName: ACString); safecall;
    procedure moveTo(newParentDir: nsIFile; newName: AString); safecall;
    procedure moveToNative(newParentDir: nsIFile; newName: ACString); safecall;
    procedure remove(recursive: longbool); safecall;
    function Getpermissions(): idlulong; safecall;
    procedure Setpermissions(apermissions: idlulong); safecall;
    property permissions : idlulong read Getpermissions write Setpermissions;
    function GetpermissionsOfLink(): idlulong; safecall;
    procedure SetpermissionsOfLink(apermissionsOfLink: idlulong); safecall;
    property permissionsOfLink : idlulong read GetpermissionsOfLink write SetpermissionsOfLink;
    function GetlastModifiedTime(): PRInt64; safecall;
    procedure SetlastModifiedTime(alastModifiedTime: PRInt64); safecall;
    property lastModifiedTime : PRInt64 read GetlastModifiedTime write SetlastModifiedTime;
    function GetlastModifiedTimeOfLink(): PRInt64; safecall;
    procedure SetlastModifiedTimeOfLink(alastModifiedTimeOfLink: PRInt64); safecall;
    property lastModifiedTimeOfLink : PRInt64 read GetlastModifiedTimeOfLink write SetlastModifiedTimeOfLink;
    function GetfileSize(): PRInt64; safecall;
    procedure SetfileSize(afileSize: PRInt64); safecall;
    property fileSize : PRInt64 read GetfileSize write SetfileSize;
    function GetfileSizeOfLink(): PRInt64; safecall;
    property fileSizeOfLink : PRInt64 read GetfileSizeOfLink;
    function Gettarget(): AString; safecall;
    property target : AString read Gettarget;
    function GetnativeTarget(): ACString; safecall;
    property nativeTarget : ACString read GetnativeTarget;
    function Getpath(): AString; safecall;
    property path : AString read Getpath;
    function GetnativePath(): ACString; safecall;
    property nativePath : ACString read GetnativePath;
    function exists() : longbool; safecall;
    function isWritable() : longbool; safecall;
    function isReadable() : longbool; safecall;
    function isExecutable() : longbool; safecall;
    function isHidden() : longbool; safecall;
    function isDirectory() : longbool; safecall;
    function isFile() : longbool; safecall;
    function isSymlink() : longbool; safecall;
    function isSpecial() : longbool; safecall;
    procedure createUnique(atype: idlulong; apermissions: idlulong); safecall;
    function clone() : nsIFile; safecall;
    function equals(inFile: nsIFile) : longbool; safecall;
    function contains(inFile: nsIFile; recur: longbool) : longbool; safecall;
    function Getparent(): nsIFile; safecall;
    property parent : nsIFile read Getparent;
    function GetdirectoryEntries(): nsISimpleEnumerator; safecall;
    property directoryEntries : nsISimpleEnumerator read GetdirectoryEntries;

  end;

const
  nsIFile_NORMAL_FILE_TYPE=0;
  nsIFile_DIRECTORY_TYPE=1;

