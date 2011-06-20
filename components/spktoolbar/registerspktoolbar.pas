unit RegisterSpkToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazarusPackageIntf, SpkToolbar, PropEdits, ComponentEditors,
  SpkToolbarEditor, spkt_Buttons, spkt_Pane, spkt_Tab, spkt_Appearance;

procedure Register;

implementation

procedure RegisterUnitSpkToolbar;
begin
  RegisterComponents('SpkToolbar', [TSpkToolbar]);
end;

procedure RegisterUnitSpkt_Buttons;
begin
  RegisterNoIcon([TSpkLargeButton, TSpkSmallButton]);
end;

procedure RegisterUnitSpkt_Pane;
begin
  RegisterNoIcon([TSpkPane]);
end;

procedure RegisterUnitSpkt_Tab;
begin
  RegisterNoIcon([TSpkTab]);
end;

procedure Register;
begin
  RegisterUnit('SpkToolbar', @RegisterUnitSpkToolbar);
  RegisterUnit('spkt_Buttons', @RegisterUnitSpkt_Buttons);
  RegisterUnit('spkt_Pane', @RegisterUnitSpkt_Pane);
  RegisterUnit('spkt_Tab', @RegisterUnitSpkt_Tab);

  RegisterComponentEditor(TSpkToolbar, TSpkToolbarEditor);
  RegisterPropertyEditor(TypeInfo(TSpkToolbarAppearance), TSpkToolbar,
    'Appearance', TSpkToolbarAppearanceEditor);
  //todo: register Caption Editor
end;

end.

