unit paradoxreg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Paradoxds, LazarusPackageIntf, PropEdits;

resourcestring
  dbfsAllparadoxfiles = 'Paradox Files';
  
procedure Register;

implementation

type

  TParadoxFileNamePropertyEditor=class(TFileNamePropertyEditor)
  protected
    function GetFilter: String; override;
  end;

function TParadoxFileNamePropertyEditor.GetFilter: String;
begin
  Result := dbfsAllParadoxFiles+' (*.db)|*.db;*.DB';
  Result:= Result+ '|'+ inherited GetFilter;
end;

procedure RegisterUnitParadox;
begin
  RegisterComponents('Data Access',[TParadoxDataSet]);
  RegisterPropertyEditor(TypeInfo(AnsiString), TParadoxDataSet, 'TableName', TParadoxFileNamePropertyEditor);
end;

procedure Register;
begin
  RegisterUnit('paradoxds',@RegisterUnitParadox);
end;

initialization

end.
