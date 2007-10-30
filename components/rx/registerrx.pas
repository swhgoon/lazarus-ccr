unit registerrx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf;

procedure Register;

implementation
uses
  PropEdits, dbdateedit, rxlookup, folderlister, rxdbgrid, rxmemds, duallist,
  curredit, rxswitch, rxdice, rxdbcomb, rxtoolbar, rxxpman, PageMngr, RxAppIcon,
  Dialogs, ComponentEditors, seldsfrm, DBPropEdits, DB, rxctrls, RxLogin,
  RxCustomChartPanel, AutoPanel, pickdate, rxconst, tooledit,
  rxceEditLookupFields;

type

  { TRxAppIcon }

  TRxAppIconEditor = class(TComponentEditor)
  public
    DefaultEditor: TBaseComponentEditor;
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    function GetVerbCount:integer;override;
    function GetVerb(Index:integer):string;override;
    procedure ExecuteVerb(Index:integer);override;
  end;

{ TRxDBGridFieldProperty }
  TRxDBGridFieldProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

{ TRxDBGridFieldProperty }

procedure TRxDBGridFieldProperty.FillValues(const Values: TStringList);
var
  Column: TRxColumn;
  Grid: TRxDBGrid;
  DataSource: TDataSource;
begin
  Column:=TRxColumn(GetComponent(0));
  if not (Column is TRxColumn) then exit;
  Grid:=TRxDBGrid(Column.Grid);
  if not (Grid is TRxDBGrid) then exit;
  DataSource := Grid.DataSource;
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    DataSource.DataSet.GetFieldNames(Values);
end;


{ TRxAppIcon }

type
  PClass = ^TClass;

constructor TRxAppIconEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
var
  CompClass: TClass;
begin
  inherited Create(AComponent, ADesigner);
  CompClass := PClass(Acomponent)^;
  try
    PClass(AComponent)^ := TComponent;
    DefaultEditor := GetComponentEditor(AComponent, ADesigner);
  finally
    PClass(AComponent)^ := CompClass;
  end;
end;

destructor TRxAppIconEditor.Destroy;
begin
  DefaultEditor.Free;
  inherited Destroy;
end;

function TRxAppIconEditor.GetVerbCount: integer;
begin
  Result:=DefaultEditor.GetVerbCount + 1;
end;

function TRxAppIconEditor.GetVerb(Index: integer): string;
begin
  if Index < DefaultEditor.GetVerbCount then
    Result := DefaultEditor.GetVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:Result:=sLoadIcon;
    end;
  end;
end;

procedure TRxAppIconEditor.ExecuteVerb(Index: integer);
var
  OpenDialog1: TOpenDialog;
begin
  if Index < DefaultEditor.GetVerbCount then
    DefaultEditor.ExecuteVerb(Index)
  else
  begin
    case Index - DefaultEditor.GetVerbCount of
      0:begin
          OpenDialog1:=TOpenDialog.Create(nil);
          OpenDialog1.Filter:=sWindowsIcoFiles;
          try
            if OpenDialog1.Execute then
              (Component as TRxAppIcon).LoadFromFile(OpenDialog1.FileName);
          finally
            OpenDialog1.Free;
          end;
          Modified;
        end;
    end;
  end;
end;


procedure RegisterRxAppIcon;
begin
  RegisterComponents('RX',[TRxAppIcon]);
end;

procedure RegisterRxXPMan;
begin
  RegisterComponents('RX',[TRXXPManifest]);
end;

procedure RegisterPageMngr;
begin
  RegisterComponents('RX',[TPageManager]);
end;

procedure RegisterUnitDBDateEdit;
begin
  RegisterComponents('RX DBAware',[TDBDateEdit, TRxDBCalcEdit]);
end;

procedure RegisterRXLookup;
begin
  RegisterComponents('RX DBAware',[TRXLookupEdit, TRxDBLookupCombo]);
end;

procedure RegisterRxDbGrid;
begin
  RegisterComponents('RX DBAware',[TRxDBGrid]);
end;

procedure RegisterRxMemDS;
begin
  RegisterComponents('RX DBAware',[TRxMemoryData]);
end;

procedure RegisterRxDBComb;
begin
  RegisterComponents('RX DBAware',[TRxDBComboBox]);
end;

procedure RegisterDualList;
begin
  RegisterComponents('RX',[TDualListDialog]);
end;

procedure RegisterCurrEdit;
begin
  RegisterComponents('RX',[TCurrencyEdit]);
end;

procedure RegisterRXSwitch;
begin
  RegisterComponents('RX',[TRxSwitch]);
end;

procedure RegisterRXDice;
begin
  RegisterComponents('RX',[TRxDice]);
end;

procedure RegisterFolderLister;
begin
  RegisterComponents('RX',[TFolderLister]);
end;

procedure RegisterRxToolBar;
begin
  RegisterComponents('RX',[TToolPanel]);
end;

procedure RegisterRxCtrls;
begin
  RegisterComponents('RX',[TRxLabel, TSecretPanel]);
end;

procedure RegisterRxLogin;
begin
  RegisterComponents('RX',[TRxLoginDialog]);
end;

procedure RegisterChartPanel;
begin
  RegisterComponents('RX',[TRxChart]);
end;

procedure RegisterAutoPanel;
begin
  RegisterComponents('RX',[TAutoPanel]);
end;

procedure RegisterPickDate;
begin
  RegisterComponents('RX',[TRxCalendarGrid]);
end;

procedure RegisterToolEdit;
begin
  RegisterComponents('RX',[TRxDateEdit]);
end;

procedure Register;
begin
  //RX
  RegisterUnit('folderlister', @RegisterFolderLister);
  RegisterUnit('duallist', @RegisterDualList);
  RegisterUnit('curredit', @RegisterCurrEdit);
  RegisterUnit('rxswitch', @RegisterRXSwitch);
  RegisterUnit('rxdice', @RegisterRXDice);
  RegisterUnit('RxXPMan', @RegisterRxXPMan);
  RegisterUnit('PageMngr', @RegisterPageMngr);
  RegisterUnit('rxtoolbar', @RegisterRxToolBar);
  RegisterUnit('rxappicon', @RegisterRxAppIcon);
  RegisterUnit('rxctrls', @RegisterRxCtrls);
  RegisterUnit('RxLogin', @RegisterRxLogin);
  RegisterUnit('RxCustomChartPanel', @RegisterChartPanel);
  RegisterUnit('AutoPanel', @RegisterAutoPanel);
  RegisterUnit('pickdate', @RegisterPickDate);
  RegisterUnit('tooledit', @RegisterToolEdit);

  //RX DBAware
  RegisterUnit('dbdateedit', @RegisterUnitDBDateEdit);
  RegisterUnit('rxlookup', @RegisterRXLookup);
  RegisterUnit('rxdbgrid', @RegisterRxDbGrid);
  RegisterUnit('rxmemds', @RegisterRxMemDS);
  RegisterUnit('rxdbcomb', @RegisterRxDBComb);


  //Component Editors
  RegisterComponentEditor(TRxAppIcon, TRxAppIconEditor);
  RegisterComponentEditor(TRxMemoryData, TMemDataSetEditor);
  //
  RegisterPropertyEditor(TypeInfo(string), TRxColumn, 'FieldName', TRxDBGridFieldProperty);
  RegisterCEEditLookupFields;
end;

initialization
  {$i rx.lrs}

end.
