unit rxFileUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

function GetFileOwnerUser(const SearchDomain, FileName:String):String;
procedure GetFileOwnerData(const SearchDomain, FileName:String;out UserName, DomainName:string);
function NormalizeDirectoryName(const DirName:string):string;
implementation
uses
{$IFDEF WINDOWS}
   Windows,
{$ELSE}
{$ENDIF}
   FileUtil;

{$IFDEF WINDOWS}
function LStrError(const Ernum: Longint; const UseUTF8: Boolean = False): string;
const
  MAX_ERROR = 1024;
var
  Tmp: string;
  TmpW: widestring;
begin
  Result := ' [' + IntToStr(Ernum) + ']: ';
  if USEUtf8 then begin
    SetLength(TmpW, MAX_ERROR);
    SetLength(TmpW, FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or
                                   FORMAT_MESSAGE_IGNORE_INSERTS or
                                   FORMAT_MESSAGE_ARGUMENT_ARRAY,
                                   nil, Ernum, 0, @TmpW[1], MAX_ERROR, nil));
    Tmp := UTF8Encode(TmpW);
  end else begin
    SetLength(Tmp, MAX_ERROR);
    SetLength(Tmp, FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
                                 FORMAT_MESSAGE_IGNORE_INSERTS or
                                 FORMAT_MESSAGE_ARGUMENT_ARRAY,
                                 nil, Ernum, 0, @Tmp[1], MAX_ERROR, nil));
  end;
  if Length(Tmp) > 2 then
    Delete(Tmp, Length(Tmp)-1, 2);
  Result := Result + Tmp;
end;

{ TODO -oalexs : In future need rewrite this code for fix mem leak }

procedure GetFileNameOwner(const SearchDomain, FileName: String; out UserName, DomainName: string);
var
  RCode, RC1:WINBOOL;
  SDSize:DWORD;      // Size of security descriptor

  FAccountName:PChar;   // Account name
  lngAccountLen:DWORD;  // Length of account name
  FDomainName:PChar;    // Domain name
  lngDomainLen:DWORD;   // Length of domain name

  ptrUse:SID_NAME_USE;         // Pointer to SID_NAME_USE
  ptrOwner:PSID;
  P:PByteArray;
begin
  ptrOwner:=nil;
  SDSize:=0;
  P:=nil;
  UserName:='';
  DomainName:='';

  RCode := GetFileSecurity(PChar(FileName), OWNER_SECURITY_INFORMATION, nil, 0, @SDSize);
  GetMem(P, SDSize);
  FillChar(P^, SDSize, 0);
  RCode := GetFileSecurity(PChar(FileName), OWNER_SECURITY_INFORMATION, Pointer(P), SDSize, @SDSize);
  if not RCode then
    raise Exception.Create(LStrError(GetLastError, true));

  RCode := GetSecurityDescriptorOwner(Pointer(P), ptrOwner, @RC1);
  if not RCode then
    raise Exception.Create(LStrError(GetLastError, true));

  lngAccountLen:=0;
  lngDomainLen:=0;
  RCode := LookupAccountSid(PChar(SearchDomain), ptrOwner, nil, lngAccountLen, nil, lngDomainLen, ptrUse);
  //' Configure the strings' buffer sizes
  GetMem(FAccountName, lngAccountLen);
  FillChar(FAccountName^, lngAccountLen, 0);
  GetMem(FDomainName, lngDomainLen);
  FillChar(FDomainName^, lngDomainLen, 0);

  RCode:=LookupAccountSid(PChar(SearchDomain), ptrOwner, FAccountName, lngAccountLen, FDomainName, lngDomainLen, ptrUse);

  if not RCode then
    raise Exception.Create(LStrError(GetLastError, true));

  UserName:=FAccountName;
  DomainName:=FDomainName;

  Freemem(P, SDSize);
  Freemem(FAccountName, lngAccountLen);
  Freemem(FDomainName, lngDomainLen);
end;
{$ELSE}
{$ENDIF}

function GetFileOwnerUser(const SearchDomain, FileName: String): String;
var
  S:string;
begin
  {$IFDEF WINDOWS}
  GetFileNameOwner(UTF8ToSys(SearchDomain), UTF8ToSys(FileName), Result, S);
  Result:=UTF8Encode(Result);
  {$ELSE}
  Result:='';
  {$ENDIF}
end;

procedure GetFileOwnerData(const SearchDomain, FileName: String; out UserName,
  DomainName: string);
begin
  {$IFDEF WINDOWS}
  GetFileNameOwner(UTF8ToSys(SearchDomain), UTF8ToSys(FileName), UserName, DomainName);
  UserName:=UTF8Encode(UserName);
  DomainName:=UTF8Encode(DomainName);
  {$ELSE}
  UserName:='';
  DomainName:='';
  {$ENDIF}
end;

{replase any dir separators '\' or '/' to system directory separator }
function NormalizeDirectoryName(const DirName: string): string;
var
  i:integer;
begin
  Result:=DirName;
  for i:=1 to Length(Result) do
    if Result[i] in ['/', '\'] then
      Result[i]:=DirectorySeparator;
end;

end.

