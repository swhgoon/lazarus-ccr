unit RegisterSpkToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazarusPackageIntf, SpkToolbar, PropEdits,
  ComponentEditors, SpkToolbarEditor;

procedure Register;

implementation

procedure RegisterUnitSpkToolbar;
begin
  RegisterComponents('SpkToolbar', [TSpkToolbar]);
end;

procedure Register;
begin
  RegisterUnit('SpkToolbar', @RegisterUnitSpkToolbar);
end;

end.

