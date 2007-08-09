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

procedure IniWriteString(IniFile: TObject; const Section, Ident,
  Value: string);
function GetDefaultIniRegKey: string;
implementation
uses Registry, Forms;

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
begin
  if Assigned(OnGetDefaultIniName) then
    Result:= OnGetDefaultIniName()
  else
    Result := ExtractFileName(ChangeFileExt(Application.ExeName, '.INI'));
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
  else begin
    S := Value;
    if S <> '' then begin
      if ((S[1] = '"') and (S[Length(S)] = '"')) or
        ((S[1] = '''') and (S[Length(S)] = '''')) then
        S := '"' + S + '"';
    end;
    if IniFile is TIniFile then
      TIniFile(IniFile).WriteString(Section, Ident, S);
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

{
procedure IniWriteString(IniFile: TIniFile; const Section, Ident,
  Value: string);
var
  S: string;
begin
    S := Value;
    if S <> '' then
    begin
      if ((S[1] = '"') and (S[Length(S)] = '"')) or
        ((S[1] = '''') and (S[Length(S)] = ''''))  then
        S := '"' + S + '"';
    end;
    if IniFile is TIniFile then
      TIniFile(IniFile).WriteString(Section, Ident, S);
end;
}
end.

