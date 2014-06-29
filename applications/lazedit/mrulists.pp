{ MruLists

  Copyright (C) 2007, 2011 by Flying Sheep Inc.
  Portions Copyright (C) by Lazarus development team http://www.lazarus.freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit MruLists;

interface

uses
  SysUtils, Classes, Controls, {Registry, IniFiles,} FileUtil;


{$if defined(Windows) or defined(darwin)}
{$define CaseInsensitiveFilenames}
{$endif}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
{$DEFINE NotLiteralFilenames}
{$ENDIF}

type

  { TMruList }

  TMruList = class(TComponent)
  private
    { Private declarations }
    FList: TStringList;
    FMaxEntries: Integer;
    //FIniName: String;
    //FIniSection: String;
    //FRegRoot: HKEY;
    //FRegKey: String;
    FOnChange: TNotifyEvent;
  protected
    { Protected declarations }
    function IndexInBounds(const Index: Integer): Boolean;
    function GetItem(const Index: Integer): String;
    procedure SetMaxEntries(Value: Integer);
    function GetCount: Integer;
    function HasDuplicate(const Value: String; out Index: Integer): Boolean;
    function GetFileNameOnDisk(const Utf8Fn: String): String;
    procedure DoChange;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Item: String; const DoNormalizeName: Boolean = False);
    procedure AddAnsi(AnsiItem: String; const DoNormalizeName: Boolean = False);
    procedure Delete(const Index: Integer);

    {
    function LoadFromFile(const AnsiFn: String): Boolean;
    function LoadFromFileUtf8(const Utf8Fn: String): Boolean;
    function LoadFromIni(Ini: TIniFile): Boolean;
    function SaveToFile(const AnsiFn: String): Boolean;
    function SaveToFileUtf8(const Utf8Fn: String): Boolean;
    function SaveToIni(Ini: TIniFile): Boolean;
    function LoadFromRegistry: Boolean;
    function SaveToRegistry: Boolean;
    }
    //Note: Items are internally treated as UTF8
    property Items[const Index: Integer]:String read GetItem; default;
  published
    { Published declarations }
    property Count: Integer read GetCount;
    property MaxEntries: Integer read FMaxEntries write SetMaxEntries default 5;
    //property IniFileName: String read FIniName write FIniName;
    //property IniSectionName: String read FIniSection write FIniSection;
    //property RegRoot: HKEY read FRegRoot write FRegRoot default HKEY_CURRENT_USER;
    //property RegKey: String read FRegKey write FRegKey;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


type
  EMruListError = class(Exception);


implementation

const
  EntryLimit = 50;  //I don't think one needs a 50-items long MRU list, but feel free to alter

//Helper functions



function CompareFilenamesIgnoreCase(const Filename1, Filename2: string): integer;
{$IFDEF darwin}
var
  F1: CFStringRef;
  F2: CFStringRef;
{$ENDIF}
begin
  {$IFDEF darwin}
  if Filename1=Filename2 then exit(0);
  F1:=CFStringCreateWithCString(nil,Pointer(Filename1),kCFStringEncodingUTF8);
  F2:=CFStringCreateWithCString(nil,Pointer(Filename2),kCFStringEncodingUTF8);
  Result:=CFStringCompare(F1,F2,kCFCompareNonliteral+kCFCompareCaseInsensitive);
  CFRelease(F1);
  CFRelease(F2);
  {$ELSE}
  Result:=AnsiCompareText(Filename1, Filename2);
  {$ENDIF}
end;

function FindDiskFilename(const Filename: string): string;
   // Searches for the filename case on disk.
   // if it does not exist, only the found path will be improved
   // For example:
   //   If Filename='file' and there is only a 'File' then 'File' will be returned.
var
 StartPos: Integer;
 EndPos: LongInt;
 FileInfo: TSearchRec;
 CurDir: String;
 CurFile: String;
 AliasFile: String;
 Ambiguous: Boolean;
 FileNotFound: Boolean;
begin
 Result:=Filename;
 // check every directory and filename
 StartPos:=1;
 {$IFDEF Windows}
 // uppercase Drive letter and skip it
 if ((length(Result)>=2) and (Result[1] in ['A'..'Z','a'..'z'])
 and (Result[2]=':')) then begin
   StartPos:=3;
   if Result[1] in ['a'..'z'] then
     Result[1] := UpCase(Result[1]);
 end;
 {$ENDIF}
 FileNotFound:=false;
 repeat
   // skip PathDelim
   while (StartPos<=length(Result)) and (Result[StartPos]=PathDelim) do
     inc(StartPos);
   // find end of filename part
   EndPos:=StartPos;
   while (EndPos<=length(Result)) and (Result[EndPos]<>PathDelim) do
     inc(EndPos);
   if EndPos>StartPos then begin
     // search file
     CurDir:=copy(Result,1,StartPos-1);
     CurFile:=copy(Result,StartPos,EndPos-StartPos);
     AliasFile:='';
     Ambiguous:=false;
     if FindFirstUTF8(CurDir+AllFilesMask,faAnyFile,FileInfo)=0 then
     begin
       repeat
         // check if special file
         if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
         then
           continue;
         if CompareFilenamesIgnoreCase(FileInfo.Name,CurFile)=0 then begin
           //writeln('FindDiskFilename ',FileInfo.Name,' ',CurFile);
           if FileInfo.Name=CurFile then begin
             // file found, has already the correct name
             AliasFile:='';
             break;
           end else begin
             // alias found, but has not the correct name
             if AliasFile='' then begin
               AliasFile:=FileInfo.Name;
             end else begin
               // there are more than one candidate
               Ambiguous:=true;
             end;
           end;
         end;
       until FindNextUTF8(FileInfo)<>0;
     end else
       FileNotFound:=true;
     FindCloseUTF8(FileInfo);
     if FileNotFound then break;
     if (AliasFile<>'') and (not Ambiguous) then begin
       // better filename found -> replace
       Result:=CurDir+AliasFile+copy(Result,EndPos,length(Result));
     end;
   end;
   StartPos:=EndPos+1;
 until StartPos>length(Result);
end;




constructor TMruList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TStringList.Create;
  FMaxEntries := 5;
  //FIniSection := 'MruList';
  //FRegRoot := HKEY_CURRENT_USER;
end;

destructor TMruList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TMruList.IndexInBounds(const Index: Integer): Boolean;
begin
  Result := (Index < FMaxEntries) and
            (Index >= 0) and (Index < FList.Count);
end;


function TMruList.GetItem(const Index: Integer): String;
begin
  if IndexInBounds(Index) then Result := FList.Strings[Index]
  else Result := '';
end;



function TMruList.HasDuplicate(const Value: String; out Index: Integer): Boolean;
//Returns True if Filename exists in the list, then Index is set appropriate
var
  i: Integer;
begin
  Index := -1;
  Result := False;
  for i := 0 to FList.Count - 1 do
  begin
    if CompareFileNames(FList.Strings[i], Value) = 0 then
    begin
      Result := True;
      Index := i;
      Break;
    end;
  end;
end;


procedure TMruList.SetMaxEntries(Value: Integer);
var i: Integer;
begin
  if (Value = FMaxEntries) then Exit; //status quo
  if (Value < 0) then Value := 0;
  if (Value > EntryLimit) then Value := EntryLimit;
  if (Value < FMaxEntries) and (Value < FList.Count) then
  begin
    for i := FList.Count - 1 downto Value do FList.Delete(i);
    DoChange;
  end;
  FMaxEntries := Value;
end;

function TMruList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TMruList.Clear;
begin
  FList.Clear;
  DoChange;
end;


procedure TMruList.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TMruList.Add(Item: String; const DoNormalizeName: Boolean = False);
//The MRU list is always sorted in a anti-chronological order
//that is: the most recent added item gets index 0.
//If the list is full (FList.Count = FMaxEntries) the last Item is deleted
//then the new Item is added
//If Item is already in the list, then it gets moved to Index = 0
var Index: Integer;
begin
  if (FMaxEntries <= 0) or (Item = '') then Exit;
  Item := ExpandFileName(Item);
  if DoNormalizeName then Item := GetFileNameOnDisk(Item);
  if HasDuplicate(Item, Index) then
  begin//Filename already in list
    if (Index = 0) then Exit;
    FList.Delete(Index);
    FList.Insert(0, Item);
  end
  else
  begin
    if (FList.Count >= FMaxEntries) and (FList.Count > 0) then
    begin
      FList.Delete(FList.Count - 1);
    end;
    FList.Insert(0, Item);
  end;
  DoChange;
end;

procedure TMruList.AddAnsi(AnsiItem: String; const DoNormalizeName: Boolean);
begin
  Add(SysToUtf8(AnsiItem), DoNormalizeName);
end;

procedure TMruList.Delete(const Index: Integer);
begin
  if IndexInBounds(Index) then
  begin
    FList.Delete(Index);
    DoChange;
  end;
end;

{
function TMruList.LoadFromFile(const AnsiFn: String): Boolean;
//Return True if succes
//Return False if the ini file does not exist or we fail on getting read access
//or the read throws an exception
//No validation on correct sequence.
//If only file1 and file3 exist, for example, they are added in the list as entry 0 and 1
var IniFile: TIniFile;
    i, dummy: Integer;
    S: String;
begin
  Result := False;
  if not FileExists(AnsiFn) then Exit;
  FList.Clear;
  IniFile := TIniFile.Create(AnsiFn);
  try
    try
      for i := 0 to FMaxEntries - 1 do
      begin
        S := IniFile.ReadString(FIniSection, FilePrefix+IntToStr(i),'');
        if (S <> '') and (not HasDuplicate(S, dummy)) then FList.Add(S);
      end;
      Result := True;
    except
      //Catch any exception during read access
      Result := False;
    end;
  finally
    IniFile.Free;
    DoChange;
  end;
end;
}

{
function TMruList.LoadFromFileUtf8(const Utf8Fn: String): Boolean;
begin
  Result := LoadFromFile(Utf8ToSys(Utf8Fn));
end;
}


{
function TMruList.LoadFromIni(Ini: TIniFile): Boolean;
var
  i: Integer;
  S: String;
  dummy: Integer;
begin
  Result := False;
  if not Assigned(Ini) then Exit;
  try
    try
      for i := 0 to FMaxEntries - 1 do
      begin
        S := Ini.ReadString(FIniSection, FilePrefix+IntToStr(i),'');
        if (S <> '') and (not HasDuplicate(S, dummy)) then FList.Add(S);
      end;
      Result := True;
    except
      //Catch any exception during read access
      Result := False;
    end;
  finally
    DoChange;
  end;
end;
}

{
function TMruList.SaveToFile(const AnsiFn: String): Boolean;
//Return True if succes
//Return False on write errors
var IniFile: TIniFile;
    i: Integer;
begin
  Result := False;
  IniFile := TIniFile.Create(AnsiFn);
  IniFile.CacheUpdates := True;
  Try
    Try
      for i := 0 to FList.Count - 1 do IniFile.WriteString(FIniSection, FilePrefix+IntToStr(i), FList.Strings[i]);
      IniFile.UpdateFile;
      Result := True;
    Except
      //Catch UpdateFile failures (e.g. file is read-only) that result in Exception (of class Exception)
      Result := False;
    end;
  finally
    IniFile.Free;
  end;
end;
}

{
function TMruList.SaveToFileUtf8(const Utf8Fn: String): Boolean;
begin
  Result := SaveToFile(Utf8ToSys(Utf8Fn));
end;
}


{
function TMruList.SaveToIni(Ini: TIniFile): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Assigned(Ini) then Exit;
  Try
    for i := 0 to FList.Count - 1 do Ini.WriteString(FIniSection, FilePrefix+IntToStr(i), FList.Strings[i]);
    //if Cached do not update
    Result := True;
  Except
    //Catch UpdateFile/WrieteString failures (e.g. file is read-only) that result in Exception (of class Exception)
    Result := False;
  end;
end;
}


{
function TMruList.LoadFromRegistry: Boolean;
//Return True if succes
//Return False on read errors
//No validation on correct sequence.
//If only file1 and file3 exist, for example, they are added in the list as entry 0 and 1
var Reg: TRegistry;
    i, dummy: Integer;
    Error: Boolean;
    S: String;
begin
  Result := False;
  Reg := TRegistry.Create;
  FList.Clear;
  try
    Reg.RootKey := FRegRoot;
    //if Reg.KeyExists(FRegKey) then
    //begin
      if Reg.OpenKeyReadOnly(FRegKey) then
      begin
        Error := False;
        for i := 0 to FMaxEntries - 1 do
        begin
          Try
            S := Reg.ReadString(FilePrefix+IntToStr(i));
            if (S <> '') and (not HasDuplicate(S, dummy)) then FList.Add(S);
          Except
            Error := true;
          end;
        end;
        Result := not Error;
      end;//OpenKey
    //end;//KeyExists
  finally
    Reg.Free;
    DoChange;
  end;
end;
}

{
function TMruList.SaveToRegistry: Boolean;
//Return True if succes
//Return False on write errors
var Reg: TRegistry;
    i: Integer;
    Error: Boolean;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := FRegRoot;
    if Reg.OpenKey(FRegKey, True) then
    begin
      Error := False;
      for i := 0 to FList.Count - 1 do
      begin
        Try
          Reg.WriteString(FilePrefix+IntToStr(i), FList.Strings[i]);
        Except
          Error := True;
        end;
      end;
      Result := not Error;
    end;//if OpenKey
  finally
    Reg.Free;
  end;
end;
}


function TMruList.GetFileNameOnDisk(const Utf8Fn: String): String;
begin
  {$IF defined(CaseInsensitiveFilenames) or defined(NotLiteralFilenames)}
  Result := FindDiskFilename(Utf8Fn);
  {$ELSE}
  Result := Utf8Fn;
  {$ENDIF}
end;



end.

