type

  nsILocalFile = interface(nsIFile)
  ['{aa610f20-a889-11d3-8c81-000064657374}']
    procedure initWithPath(filePath: AString); safecall;
    procedure initWithNativePath(filePath: ACString); safecall;
    procedure initWithFile(aFile: nsILocalFile); safecall;
    function GetfollowLinks(): longbool; safecall;
    procedure SetfollowLinks(afollowLinks: longbool); safecall;
    property followLinks : longbool read GetfollowLinks write SetfollowLinks;
    function openNSPRFileDesc(flags: idllong; mode: idllong) : PRFileDescStar; safecall;
    function openANSIFileDesc(mode: PAnsiChar) : PFILE; safecall;
    function load() : PRLibraryStar; safecall;
    function GetdiskSpaceAvailable(): PRInt64; safecall;
    property diskSpaceAvailable : PRInt64 read GetdiskSpaceAvailable;
    procedure appendRelativePath(relativeFilePath: AString); safecall;
    procedure appendRelativeNativePath(relativeFilePath: ACString); safecall;
    function GetpersistentDescriptor(): ACString; safecall;
    procedure SetpersistentDescriptor(apersistentDescriptor: ACString); safecall;
    property persistentDescriptor : ACString read GetpersistentDescriptor write SetpersistentDescriptor;
    procedure reveal(); safecall;
    procedure launch(); safecall;
    function getRelativeDescriptor(fromFile: nsILocalFile) : ACString; safecall;
    procedure setRelativeDescriptor(fromFile: nsILocalFile; relativeDesc: ACString); safecall;

  end;

const
  nsILocalFile_OS_READAHEAD=$40000000;
  nsILocalFile_DELETE_ON_CLOSE=$80000000;

