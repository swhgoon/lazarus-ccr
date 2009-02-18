unit rxapputils;

{$I rx.inc}

interface

uses
  Classes, SysUtils, Controls, IniFiles;

const
  {$IFNDEF LINUX}
  AllMask = '*.*';
  {$ELSE}
  AllMask = '*';
  {$ENDIF}

var
  DefCompanyName: string = '';
  RegUseAppTitle: Boolean = False;


function GetDefaultSection(Component: TComponent): string;
procedure GetDefaultIniData(Control: TControl; var IniFileName,
  Section: string; UseRegistry: Boolean = false);
function GetDefaultIniName: string;

type
  TOnGetDefaultIniName = function: string;

const
  OnGetDefaultIniName: TOnGetDefaultIniName = nil;

//Save to IniFile or TRegIniFile string value
procedure IniWriteString(IniFile: TObject; const Section, Ident,
  Value: string);
function IniReadString(IniFile: TObject; const Section, Ident,
  Value: string):string;

//Save to IniFile or TRegIniFile integer value
procedure IniWriteInteger(IniFile: TObject; const Section, Ident:string;
  const Value: integer);
function IniReadInteger(IniFile: TObject; const Section, Ident:string;
  const Value: integer):integer;

function GetDefaultIniRegKey: string;
implementation
uses Registry, Forms, FileUtil;

function GetDefaultSection(Component: TComponent): string;
var
  F: TCustomForm;
  Owner: TComponent;
begin
  if Component <> nil then begin
    if Component is TCustomForm then Result := Component.ClassName
    else begin
      Result := Component.Name;
      if Component is TControl then begin
        F := GetParentForm(TControl(Component));
        if F <> nil then Result := F.ClassName + Result
        else begin
          if TControl(Component).Parent <> nil then
            Result := TControl(Component).Parent.Name + Result;
        end;
      end
      else begin
        Owner := Component.Owner;
        if Owner is TForm then
          Result := Format('%s.%s', [Owner.ClassName, Result]);
      end;
    end;
  end
  else Result := '';
end;

function GetDefaultIniName: string;
var
  S:string;
begin
  if Assigned(OnGetDefaultIniName) then
    Result:= OnGetDefaultIniName()
  else
  begin
    Result := ExtractFileName(ChangeFileExt(Application.ExeName, '.ini'));
{$IFNDEF WIN32}
    S:=UTF8ToSys(GetAppConfigDir(false));
    if not DirectoryExists(S) then
      mkdir(S);
    Result:=S+Result;
{$ENDIF}
  end;
end;

procedure GetDefaultIniData(Control: TControl; var IniFileName,
  Section: string; UseRegistry: Boolean );
var
  I: Integer;
begin
  IniFileName := EmptyStr;
{  with Control do
    if Owner is TCustomForm then
      for I := 0 to Owner.ComponentCount - 1 do
        if (Owner.Components[I] is TFormPropertyStorage) then
        begin
          IniFileName := TFormPropertyStorage(Owner.Components[I]).IniFileName;
          Break;
        end;}
  Section := GetDefaultSection(Control);
  if IniFileName = EmptyStr then
    if UseRegistry then IniFileName := GetDefaultIniRegKey
    else
      IniFileName := GetDefaultIniName;
end;

procedure IniWriteString(IniFile: TObject; const Section, Ident,
  Value: string);
var
  S: string;
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteString(Section, Ident, Value)
  else
  begin
    S := Value;
    if S <> '' then
    begin
      if ((S[1] = '"') and (S[Length(S)] = '"')) or
        ((S[1] = '''') and (S[Length(S)] = '''')) then
        S := '"' + S + '"';
    end;
    if IniFile is TIniFile then
      TIniFile(IniFile).WriteString(Section, Ident, S);
  end;
end;

function IniReadString(IniFile: TObject; const Section, Ident, Value: string
  ): string;
var
  S: string;
begin
  if IniFile is TRegIniFile then
    Result:=TRegIniFile(IniFile).ReadString(Section, Ident, Value)
  else
  begin
    S := Value;
    if S <> '' then begin
      if ((S[1] = '"') and (S[Length(S)] = '"')) or
        ((S[1] = '''') and (S[Length(S)] = '''')) then
        S := '"' + S + '"';
    end;
    if IniFile is TIniFile then
      Result:=TIniFile(IniFile).ReadString(Section, Ident, S);
  end;
end;

procedure IniWriteInteger(IniFile: TObject; const Section, Ident: string;
  const Value: integer);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteInteger(Section, Ident, Value)
  else
  begin
    if IniFile is TIniFile then
      TIniFile(IniFile).WriteInteger(Section, Ident, Value);
  end;
end;

function IniReadInteger(IniFile: TObject; const Section, Ident: string;
  const Value: integer): integer;
begin
  if IniFile is TRegIniFile then
    Result:=TRegIniFile(IniFile).ReadInteger(Section, Ident, Value)
  else
  begin
    if IniFile is TIniFile then
      Result:=TIniFile(IniFile).ReadInteger(Section, Ident, Value);
  end;
end;

function GetDefaultIniRegKey: string;
begin
  if RegUseAppTitle and (Application.Title <> '') then
    Result := Application.Title
  else Result := ExtractFileName(ChangeFileExt(Application.ExeName, ''));
  if DefCompanyName <> '' then
    Result := DefCompanyName + '\' + Result;
  Result := 'Software\' + Result;
end;


end.

