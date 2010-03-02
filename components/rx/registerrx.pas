unit registerrx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf;

procedure Register;

implementation
uses
  PropEdits, dbdateedit, rxlookup, folderlister, rxmemds, duallist,
  curredit, rxswitch, rxdice, rxdbcomb, rxtoolbar, rxxpman, PageMngr, RxAppIcon,
  Dialogs, ComponentEditors, seldsfrm, DBPropEdits, DB, rxctrls, RxLogin,
  RxCustomChartPanel, AutoPanel, pickdate, rxconst, tooledit, rxclock,
  rxceEditLookupFields, rxpopupunit, rxspin, RxTimeEdit, RxVersInfo,
  RxAboutDialog;

type


  { TPopUpColumnFieldProperty }

  TPopUpColumnFieldProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

{ TPopUpColumnFieldProperty }

procedure TPopUpColumnFieldProperty.FillValues(const Values: TStringList);
var
  Column: TPopUpColumn;
  DataSource: TDataSource;
begin
  Column:=TPopUpColumn(GetComponent(0));
  if not (Column is TPopUpColumn) then exit;
  DataSource := TPopUpFormColumns(Column.Collection).PopUpFormOptions.DataSource;
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    DataSource.DataSet.GetFieldNames(Values);
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
  RegisterComponents('RX',[TRxLabel, TSecretPanel, TRxSpeedButton]);
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

procedure RegisterRxClock;
begin
  RegisterComponents('RX',[TRxClock]);
end;

procedure RegisterRxSpin;
begin
  RegisterComponents('RX',[TRxSpinButton, TRxSpinEdit]);
end;

procedure RegisterRxTimeEdit;
begin
  RegisterComponents('RX',[TRxTimeEdit]);
end;

procedure RegisterRxLogin;
begin
  RegisterComponents('RX',[TRxLoginDialog]);
end;

procedure RegisterRxVersInfo;
begin
  RegisterComponents('RX',[TRxVersionInfo]);
end;

procedure RegisterRxAboutDialog;
begin
  RegisterComponents('RX',[TRxAboutDialog]);
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
  RegisterUnit('RxCustomChartPanel', @RegisterChartPanel);
  RegisterUnit('AutoPanel', @RegisterAutoPanel);
  RegisterUnit('pickdate', @RegisterPickDate);
  RegisterUnit('tooledit', @RegisterToolEdit);
  RegisterUnit('rxclock', @RegisterRxClock);
  RegisterUnit('rxspin', @RegisterRxSpin);
  RegisterUnit('RxTimeEdit', @RegisterRxTimeEdit);
  RegisterUnit('RxLogin', @RegisterRxLogin);
  RegisterUnit('RxVersInfo', @RegisterRxVersInfo);
  RegisterUnit('RxAboutDialog', @RegisterRxAboutDialog);

  //RX DBAware
  RegisterUnit('dbdateedit', @RegisterUnitDBDateEdit);
  RegisterUnit('rxlookup', @RegisterRXLookup);
  RegisterUnit('rxmemds', @RegisterRxMemDS);
  RegisterUnit('rxdbcomb', @RegisterRxDBComb);

  //Component Editors
  RegisterComponentEditor(TRxMemoryData, TMemDataSetEditor);
  //

  RegisterPropertyEditor(TypeInfo(string), TPopUpColumn, 'FieldName', TPopUpColumnFieldProperty);
  RegisterCEEditLookupFields;
end;

initialization
  {$i rx.lrs}
end.
