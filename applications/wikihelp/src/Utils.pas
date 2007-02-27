UNIT Utils;
 
INTERFACE

{$H+}

uses Classes,SysUtils,Graphics,Forms,Process,Dialogs,Clipbrd,FileUtil,Translations
{$IFDEF MSWINDOWS}
,Registry,Windows
{$ENDIF}
;

{$IFNDEF FPC}
CONST
  DirectorySeparator = '\';
{$ENDIF}

type
 TRoundToRange = -37..37;
 TProcessinfoTyp = (piOpen,piPrint);
 {$ifdef WINDOWS}
 PFNSHGetFolderPath = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall;
 {$endif}

function ClearDir (Path: string): boolean;
function RPos(const Substr: string; const S: string): Integer;
FUNCTION IsNumeric(s: STRING): boolean;
FUNCTION StrTimeToValue(val : string) : LongInt;
procedure DrawText(Canvas : TCanvas;Rect : TRect;Str : string;CenterV : Boolean = False;CenterH : Boolean = False);
function InstallExt(Extension, ExtDescription, FileDescription,OpenWith, ParamString: string; IconIndex: Integer = 0): Boolean;
function SystemUserName : string;
function HTTPEncode(const str : String) : string;
function StripHTML(S: string): string;
function ValidateFileName(old : string) : string;
function ValidateFileDir(old : string) : string;
function ValidateDate(D : string) : string;
function GetTempPath : string;
function GetConfigDir(app : string) : string;
function GetGlobalConfigDir(app : string) : string;
function SizeToText(size : Longint) : string;
function GetMainIconHandle : Cardinal;
function CanWriteToProgramDir : Boolean;
function OpenBrowser(Site : string) : Boolean;
function HexToBin(h: STRING): dword;
procedure LoadLanguage(lang : string);
function RoundTo(const AValue : extended ; const ADigit : TRoundToRange) : extended ;
function TimeTotext(Seconds : Integer) : string;
procedure ExecProcess(CommandLine : string;CurDir : string = '';Waitfor : Boolean = True);
procedure ExecVisualProcess(CommandLine : string;CurDir : string = '';Waitfor : Boolean = True);
function ExecProcessEx(CommandLine : string;CurDir : string = '') : string;
function GetProcessforExtension(InfoTyp : TProcessinfoTyp;Extension : string) : string;
function GetMimeTypeforExtension(Extension : string) : string;
function GetSystemLang : string;

IMPLEMENTATION

function ExecProcessEx(CommandLine : string;CurDir : string = '') : string;
const
  READ_BYTES = 2048;
var
  process : TProcess;
  ms: tmemorystream;
  bytesread: integer;
  n: longint;
  tmps: tstringlist;
  err : string;
begin
  BytesRead := 0;
  Process := TProcess.Create(nil);
  Process.Options := [poUsePipes];
  Process.ShowWindow := swoHide;
  Process.CommandLine := CommandLine;
  if CurDir <> '' then
    Process.CurrentDirectory := CurDir;
  BytesRead := 0;
  MS := TmemoryStream.create;
  try
  Process.Execute;
  while Process.Running do
    begin
      MS.SetSize(BytesRead+READ_BYTES);
      n := Process.OutPut.Read((MS.Memory+BytesRead)^,READ_BYTES);
      if n > 0 then
        inc(BytesRead,n)
      else
        sleep(50);
    end;
  except
    on e : exception do
      err := err+#13+e.Message;
  end;
  MS.SetSize(BytesRead+READ_BYTES);
  n := Process.OutPut.Read((MS.Memory+BytesRead)^,READ_BYTES);
  if n > 0 then
    inc(Bytesread,n);
  MS.SetSize(BytesRead);
  Process.Free;
  tmps := TStringList.Create;
  tmps.LoadFromStream(MS);
  Result := tmps.Text;
  tmps.Free;
  MS.Free;
  if err <> '' then
    Result := 'errors:'+err+#13+Result;
end;

procedure ExecProcess(CommandLine : string;CurDir : string = '';Waitfor : Boolean = True);
var
{$IFDEF WINDOWS}
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: string;
  Res: Boolean;
{$ELSE}
  process : TProcess;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := SW_HIDE
  end;
  Res := CreateProcess(NIL, PChar(CommandLine), NIL, NIL, FALSE,
                          CREATE_NEW_CONSOLE or
                          NORMAL_PRIORITY_CLASS, NIL,
                          PChar(CurDir),
                          SUInfo, ProcInfo);
  { Wait for it to finish. }
  if Res and Waitfor then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
{$ELSE}
  Process := TProcess.Create(nil);
  if CurDir <> '' then
    Process.CurrentDirectory := CurDir;
  Process.CommandLine := CommandLine;
  if Waitfor then
    Process.Options := [poNewConsole{poNoConsole},poWaitOnExit]
  else
    Process.Options := [poNewConsole{poNoConsole}];
//  Process.ShowWindow := swoHide;
  Process.Execute;
  if Waitfor then Process.Free;
{$ENDIF}
end;

procedure ExecVisualProcess(CommandLine : string;CurDir : string = '';Waitfor : Boolean = True);
var
  process : TProcess;
begin
  Process := TProcess.Create(nil);
  if CurDir <> '' then
    Process.CurrentDirectory := CurDir;
  Process.CommandLine := CommandLine;
  if Waitfor then
    Process.Options := [poWaitOnExit]
  else
    Process.Options := [];
  Process.Execute;
  if Waitfor then Process.Free;
end;

function TimeTotext(Seconds : Integer) : string;
var
  tmp : Integer;
begin
  if Seconds > 60*60 then
    begin
      Result := IntToStr(Trunc(Seconds/(60*60))) +' h';
      tmp := Seconds mod (60*60);
      Result := Result +' '+IntToStr(Trunc(tmp/(60))) +' m';
      tmp := Seconds mod 60;
      Result := Result +' '+IntToStr(tmp) +' s';
    end
  else if Seconds > 60 then
    begin
      Result := IntToStr(Trunc(Seconds/(60))) +' m';
      tmp := Seconds mod 60;
      Result := Result +' '+IntToStr(tmp) +' s';
    end
  else
    begin
      Result := IntToStr(Seconds)+' s';
    end
end;

function RoundTo(const AValue : extended ; const ADigit : TRoundToRange) : extended ;
var X : extended ; i : integer ;
begin
  X := 1.0 ;
  for i := 1 to Abs(ADigit) do X := X * 10 ;
  if ADigit<0 then
    Result := Round(AValue * X) / X
  else
    Result := Round(AValue / X) * X;
end;
  
function HexToBin(h: STRING): dword;
  FUNCTION HexDigitToInt(c: Char): Integer;
  BEGIN
    IF (c >= '0') AND (c <= '9') THEN Result := Ord(c) - Ord('0')
    ELSE IF (c >= 'A') AND (c <= 'F') THEN Result := Ord(c) - Ord('A') + 10
    ELSE IF (c >= 'a') AND (c <= 'f') THEN Result := Ord(c) - Ord('a') + 10
    ELSE Result := -1;
  END;
VAR
  buf: ARRAY[0..16] OF Byte;
  digit1: Integer;
  bytes: Integer;
  index: Integer;
BEGIN
  bytes := 0;
  index := 0;
  result := 0;
  IF frac(length(h) / 2) = 0.5 THEN
    h := '0' + h;
  WHILE (bytes < 16) DO
    BEGIN
      if length(h) > index+1 then
        digit1 := HexDigitToInt(h[index + 1])
      else
        digit1 := -1;
      IF digit1 < 0 THEN
        break;
      buf[bytes] := (digit1 SHL 4) OR HexDigitToInt(h[index + 2]);
      Inc(index, 2);
      Inc(bytes);
    END;
  dec(bytes);
  FOR index := bytes DOWNTO 0 DO
    Result := Result + (buf[index] shl ((bytes-index)*8));
END;

procedure LoadLanguage(lang: string);
begin
  if FileExists(ProgramDirectory+'languages'+Directoryseparator+Lang+'.po') then
    TranslateUnitResourceStrings('uintfstrconsts',ProgramDirectory+'languages'+Directoryseparator+Lang+'.po');
//    TranslateResourcestrings(ProgramDirectory+'languages'+Directoryseparator+Lang+'.mo');
end;

function GetProcessforExtension(InfoTyp : TProcessinfoTyp;Extension : string) : string;
{$ifdef MSWINDOWS}
var
  reg : TRegistry;
  ot : string;
  FileClass: string;
  chrResult: array[0..1023] of Char;
  wrdReturn: DWORD;
{$endif}
begin
{$ifdef WINDOWS}
  case InfoTyp of
  piOpen:ot := 'open';
  piPrint:ot := 'print';
  end;
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  Reg.RootKey := HKEY_CLASSES_ROOT;
  FileClass := '';
  if Reg.OpenKeyReadOnly(ExtractFileExt('.'+Extension)) then
  begin
    FileClass := Reg.ReadString('');
    Reg.CloseKey;
  end;
  if FileClass <> '' then begin
    if Reg.OpenKeyReadOnly(FileClass + '\Shell\'+ot+'\Command') then
    begin
      wrdReturn := ExpandEnvironmentStrings(PChar(StringReplace(Reg.ReadString(''),'%1','%s',[rfReplaceAll])), chrResult, 1024);
      if wrdReturn = 0 then
        Result := StringReplace(Reg.ReadString(''),'%1','%s',[rfReplaceAll])
      else
        Result := Trim(chrResult);
      Reg.CloseKey;
    end;
  end;
  Reg.Free;
{$ELSE}
{$endif}
end;

function GetMimeTypeforExtension(Extension : string) : string;
{$ifdef MSWINDOWS}
var
  reg : TRegistry;
{$endif}
begin
{$ifdef WINDOWS}
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  Reg.RootKey := HKEY_CLASSES_ROOT;
  if Reg.OpenKeyReadOnly(ExtractFileExt('.'+Extension)) then
  begin
    Result := Reg.ReadString('Content Type');
    Reg.CloseKey;
  end;
  Reg.Free;
{$ELSE}
{$endif}
end;

function GetSystemLang: string;
{$IFDEF WINDOWS}
var
  Ident: Integer;
  MyLang: PChar;
const
  Size: Integer = 250;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  GetMem(MyLang, Size);
  Ident:=GetSystemDefaultLangID;
  VerLanguageName(Ident, MyLang, Size);
  Result:=StrPas(MyLang);
  FreeMem(MyLang);
{$ELSE}
  Result := GetEnvironmentVariable('LANG');
{$ENDIF}
end;

function OpenBrowser(Site : string) : Boolean;
var
  cmd : string;
  proc : TProcess;
begin
  cmd := GetProcessforExtension(piOpen,'html');
  {$ifndef WINDOWS}
  cmd := ExecProcessEx('gconftool-2 --get /desktop/gnome/url-handlers/http/command');
  if (cmd = '') or (pos('errors',cmd) > 0) then
    cmd := GetEnvironmentVariable('BROWSER');
  if (cmd = '') then
    cmd := 'kfmclient openURL %s';
  {$ENDIF}
  if cmd = '' then exit;
  proc := TProcess.Create(nil);
  proc.CommandLine := StringReplace(cmd,'%s',Site,[rfReplaceAll]);
  proc.Options := [poNewConsole];
  proc.Execute;
  while Proc.Running do
    Application.ProcessMessages;
  proc.free;
end;


function CanWriteToProgramDir : Boolean;
var
  f : TextFile;
begin
  AssignFile(f,ExtractFilePath(Application.Exename)+'writetest.tmp');
  try
    Rewrite(f);
  except
    Result := False;
    exit;
  end;
  CloseFile(f);
  SysUtils.DeleteFile(ExtractFilePath(Application.Exename)+'writetest.tmp');
  Result := True;
end;

function SizeToText(size : Longint) : string;
begin
  if size > 1024*1024*1024 then
    Result := FormatFloat('0.00',size/(1024*1024*1024))+' Gb'
  else if size > 1024*1024 then
    Result := FormatFloat('0.00',size/(1024*1024))+' Mb'
  else if size > 1024 then
    Result := FormatFloat('0.00',size/(1024))+' Kb'
  else
    Result := IntToStr(size)+' byte'
end;

function GetMainIconHandle : Cardinal;
begin
{$ifdef MSWINDOWS}
  Result := LoadIcon(hInstance, 'MAINICON');
{$else}
  Result := 0;
{$endif}
end;

function GetConfigDir(app : string) : string;
begin
{$IFDEF MSWINDOWS}
  Result := copy(GetAppConfigDir(False),0,length(GetAppConfigDir(False))-length(ApplicationName))+app;
{$ELSE}
  Result:=GetEnvironmentVariable('HOME');
  If (Result<>'') then
    Result:=IncludeTrailingPathDelimiter(Result)+'.'+app;
{$ENDIF}
  Result := IncludeTrailingPathDelimiter(result);
end;

function GetGlobalConfigDir(app : string) : string;
{$IFDEF MSWINDOWS}
const
  CSIDL_COMMON_APPDATA  = $0023; // All Users\Application Data
  CSIDL_FLAG_CREATE     = $8000; { (force creation of requested folder if it doesn't exist yet)     }
var
  Path: array [0..1024] of char;
  P : Pointer;
  SHGetFolderPath : PFNSHGetFolderPath = Nil;
  CFGDLLHandle : THandle = 0;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  CFGDLLHandle:=LoadLibrary('shell32.dll');
  if (CFGDLLHandle<>0) then
    begin
    P:=GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
    If (P=Nil) then
      begin
      FreeLibrary(CFGDLLHandle);
      CFGDllHandle:=0;
      end
    else
      SHGetFolderPath:=PFNSHGetFolderPath(P);
    end;
  If (P=Nil) then
    begin
    CFGDLLHandle:=LoadLibrary('shfolder.dll');
    if (CFGDLLHandle<>0) then
      begin
      P:=GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
      If (P=Nil) then
        begin
        FreeLibrary(CFGDLLHandle);
        CFGDllHandle:=0;
        end
      else
        ShGetFolderPath:=PFNSHGetFolderPath(P);
      end;
    end;
  Result := ExtractFilePath(Application.Exename);
  If (@ShGetFolderPath<>Nil) then
    if SHGetFolderPath(0,CSIDL_COMMON_APPDATA or CSIDL_FLAG_CREATE,0,0,@PATH[0])=S_OK then
      Result:=IncludeTrailingPathDelimiter(StrPas(@Path[0]))+app;
{$ELSE}
{$ENDIF}
  Result := IncludeTrailingPathDelimiter(result);
end;

function GetTempPath : string;
{$IFDEF MSWINDOWS}
var
  TD                : PChar;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(TD, 256);
  try
    FillChar(TD^, 256, 0);
    Windows.GetTempPath(256, TD);
    Result := TD;
  finally
    FreeMem(TD, 256);
  end;
{$ELSE}
  Result := '/temp';
{$ENDIF}
end;

function ValidateFileDir(old: string): string;
begin
  Result := old;
  if DirectorySeparator <> '/' then
    Result := StringReplace(Result,'/','',[rfReplaceAll]);
  Result := StringReplace(Result,'@','',[rfReplaceAll]);
  Result := StringReplace(Result,';','',[rfReplaceAll]);
end;

function ValidateDate(D : string) : string;
begin
  if pos('.',D) > 0 then
    Result := StringReplace(D,'-','.',[rfReplaceAll]);
  if length(D) = 4 then
    Result := '01.01.'+D;
end;

function ValidateFileName(old : string) : string;
begin
  Result := StringReplace(old,'\','',[rfReplaceAll]);
  Result := StringReplace(Result,'/','',[rfReplaceAll]);
  Result := StringReplace(Result,'@','',[rfReplaceAll]);
  Result := StringReplace(Result,';','',[rfReplaceAll]);
end;

function StripHTML(S: string): string;
var
  TagBegin, TagEnd, TagLength: integer;
begin
  TagBegin := Pos( '<', S);      // search position of first <

  while (TagBegin > 0) do begin  // while there is a < in S
    TagEnd := Pos('>', S);              // find the matching >
    TagLength := TagEnd - TagBegin + 1;
    Delete(S, TagBegin, TagLength);     // delete the tag
    TagBegin:= Pos( '<', S);            // search for next <
  end;

  S := Stringreplace(S,'&nbsp;',' ',[rfReplaceAll]);
  S := Stringreplace(S,'&amp;','&',[rfReplaceAll]);
  S := Stringreplace(S,'&lt;','<',[rfReplaceAll]);
  S := Stringreplace(S,'&gt;','>',[rfReplaceAll]);
  S := Stringreplace(S,'&quot;','"',[rfReplaceAll]);
  Result := S;                   // give the result
end;

function HTTPEncode(const str : String) : string;
const
  noconvert = ['A'..'Z','a'..'z','*','@','.','_','-','0'..'9','$','!','''','(',')'];
  hex2str : array[0..15] of char = '0123456789ABCDEF';
var
  i : integer;
  c : char;
begin
  Result := '';
  for i:=1 to length(str) do
    begin
      c:=str[i];
      if c in noconvert then
        Result:=Result+c
      else
        Result:=Result+'%'+hex2str[ord(c) shr 4]+hex2str[ord(c) and $f];
    end;
end;

{$IFDEF MSWINDOWS}
function SystemUserName : string;
var userNameBuffer : string[255];
    sizeBuffer : DWord;
begin
  SizeBuffer := 256;
  getUserName(@userNameBuffer+1, sizeBuffer);
  result := userNameBuffer;
end;
{$ELSIF LINUX}
{$IFNDEF WINDOWS}
function SystemUserName : string;
begin
  Result := GetEnvironmentVariable('USERNAME');
  if Result = '' then
    Result := GetEnvironmentVariable('USER');
end;
{$ENDIF}
{$ELSE}
function SystemUserName : string;
begin
end;
{$ENDIF}

function InstallExt(Extension, ExtDescription, FileDescription,OpenWith, ParamString: string; IconIndex: Integer = 0): Boolean;
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
{$ENDIF}
begin
  Result := False;
  if Extension <> '' then
    begin
{$IFDEF MSWINDOWS}
      if Extension[1] <> '.' then
        Extension := '.' + Extension;
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_CLASSES_ROOT;
        if Reg.OpenKey(Extension, True) then
          begin
            Reg.WriteString('', ExtDescription);
            if Reg.OpenKey('\' + ExtDescription, True) then
              begin
                Reg.WriteString('', FileDescription);
                if Reg.OpenKey('DefaultIcon', True) then
                  begin
                    Reg.WriteString('', Format('%s,%d', [OpenWith, IconIndex]));
                    if Reg.OpenKey('\' + ExtDescription + '\Shell\Open\Command', True) then
                      begin
                        Reg.WriteString('', Format('"%s" "%s"', [OpenWith, ParamString]));
                        Result:=True;
                      end;
                  end;
              end;
          end;
      finally
        Reg.Free;
      end;
{$ENDIF}
    end;
end;

procedure DrawText(Canvas : TCanvas;Rect : TRect;Str : string;CenterV : Boolean = False;CenterH : Boolean = False);
var
  TextPosY,
  TextPosX: Integer;
begin
  TextPosX := Rect.Left;
  if CenterH then
    TextPosX := TextPosX+((Rect.Right-Rect.Left-Canvas.TextWidth(Str)) div 2);
  TextPosY := Rect.Top;
  if CenterV then
  TextPosY := TextPosY+((Rect.Bottom-Rect.Top)-Canvas.TextHeight(Str)) div 2;
  Canvas.TextOut(TextPosX,TextPosY,Str);
end;

FUNCTION StrTimeToValue(val : string) : LongInt;
var
  i : Integer;
  un : string;
begin
  //TODO:replace ',' with system delemiter
  un := '';
  FOR i := 1 TO length(val) DO
    IF NOT ((Char(Val[i]) IN ['0'..'9']) or (Char(Val[i]) = DecimalSeparator)) THEN
      begin
        un := trim(copy(Val,i,length(Val)));
        break;
      end;
  if copy(Val,0,i-1) = '' then
    begin
      Result := -1;
      exit;
    end;
  if (UpperCase(un) = 'MS') or (un = '') then
    Result := Round(StrToFloat(copy(Val,0,i-1)))
  else if UpperCase(un) = 'S' then
    Result := Round(1000*StrToFloat(copy(Val,0,i-1)))
  else if UpperCase(un) = 'M' then
    Result := Round(60*1000*StrToFloat(copy(Val,0,i-1)))
  else
    Result := -1;
end;

FUNCTION IsNumeric(s: STRING): boolean;
VAR
  i      : integer;
  hassign: boolean;
BEGIN
  // TODO:Replace ',' with Systemdelemiter
  // geändert von schnullerbacke
  // DecimalSeparator auf . gesetzt falls ,
  // doppeltes Vorkommen von sign verhindert
  if DecimalSeparator = ',' then begin
    // replace DecimalSeparator wenn nötig
    i:= Pos(',', s);
    s[i]:= '.';
  end;
  Result:= (Pos('.', s) > 0) and (length(s) > 0);
  if Result then begin
    // lösche DecimalSeparator
    Delete(s, i, 1);
    hassign:= (Pos('-', s) > 0) or (Pos('+',s) > 0);
    if hassign then begin
      // lösche Vorkommen eines signs
      i:= Pos('-', s);
      if i = 0 then i:= Pos('+', s);
      if i > 0 then Delete(s, i, 1);
    end;
    i:= 1;
    while (i <= length(s)) and Result do begin
      // nur noch Test auf Digit nötig
      Result:= (Char(s[i]) IN ['0'..'9']);
      IF Result then inc(i);
    END; // of while (i <= length(s)) and Result do begin
  end; // of if Result then begin
END;

function RPos(const Substr: string; const S: string): Integer;
var
  SL, i : Integer;
begin
  SL := Length(Substr);
  i := Length(S);
  if (Substr = '') or (S = '') or (SL > i) then begin
    Result := 0;
    Exit;
  end;

  while i >= SL do begin
    if S[i] = Substr[SL] then begin
      if Copy(S, i - SL + 1, SL) = Substr then begin
        Result := i - SL + 1;
        Exit;
      end;
    end;
    Dec(i);
  end;
  Result := i;
end;

 { Make sure given file path is ended with backslash ("\") }
 { Clears Directory: Removes all files and directories contained }
function ClearDir (Path: string): boolean;
var
  Res: integer;
  SRec: SysUtils.TSearchRec;
begin
  Result := false;
  try
    if copy(path,length(path)-1,1) <> DirectorySeparator then
      Path := Path+DirectorySeparator;
    Res := FindFirst (Path + '*.*', faAnyFile, SRec);
    while Res = 0 do
      begin
        if (SRec.Attr = faDirectory) and (SRec.Name[1] <> '.') then
          begin
            ClearDir (Path + SRec.Name); { Clear before removing }
            if not RemoveDir (pchar(Path + SRec.Name)) then
              exit;
          end
        else
          SysUtils.DeleteFile(Path + SRec.Name);
      Res := FindNext(SRec);
    end;
    SysUtils.FindClose(SRec);
    Result := true;
  except
  end;
end;


END.

 
