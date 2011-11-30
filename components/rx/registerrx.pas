{ registerrx unit

  Copyright (C) 2005-2010 Lagunov Aleksey alexs@hotbox.ru and Lazarus team
  original conception from rx library for Delphi (c)

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

unit registerrx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf;

procedure Register;

implementation
uses
  PropEdits, folderlister, duallist, RxHistoryNavigator,
  curredit, rxswitch, rxdice, rxtoolbar, rxxpman, PageMngr, RxAppIcon,
  Dialogs, ComponentEditors, DBPropEdits, DB, rxctrls,
  RxCustomChartPanel, AutoPanel, pickdate, rxconst, tooledit, rxclock,
  rxceEditLookupFields, rxpopupunit, rxspin, RxTimeEdit,
  RxAboutDialog, RxViewsPanel;

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

type

  { THistoryButtonProperty }

  THistoryButtonProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;



{ THistoryButtonProperty }

function THistoryButtonProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList, paMultiSelect];
end;

procedure THistoryButtonProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Navigator:TRxHistoryNavigator;
begin
  Navigator:=TRxHistoryNavigator(GetComponent(0));
  if Assigned(Navigator) then
  begin
    if Assigned(Navigator.ToolPanel) then
    begin
      for i:=0 to Navigator.ToolPanel.Items.Count - 1 do
      begin
        if Assigned(Navigator.ToolPanel.Items[i].Action) then
          Proc(Navigator.ToolPanel.Items[i].Action.Name);
      end;
    end;
  end;
end;

{$IFDEF USE_TRxAppIcon}
procedure RegisterRxAppIcon;
begin
  RegisterComponents('RX',[TRxAppIcon]);
end;
{$ENDIF}
{$IFDEF USE_TRXXPManifest}
procedure RegisterRxXPMan;
begin
  RegisterComponents('RX',[TRXXPManifest]);
end;
{$ENDIF}

procedure RegisterPageMngr;
begin
  RegisterComponents('RX',[TPageManager]);
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
  RegisterComponents('RX',[TRxLabel, TSecretPanel, TRxSpeedButton, TRxRadioGroup]);
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

procedure RegisterRxAboutDialog;
begin
  RegisterComponents('RX',[TRxAboutDialog]);
end;

procedure RegisterRxViewsPanel;
begin
  RegisterComponents('RX',[TRxViewsPanel]);
end;

procedure RegisterRxHistoryNavigator;
begin
  RegisterComponents('RX Tools',[TRxHistoryNavigator]);
end;

procedure Register;
begin
  //RX
  RegisterUnit('folderlister', @RegisterFolderLister);
  RegisterUnit('duallist', @RegisterDualList);
  RegisterUnit('curredit', @RegisterCurrEdit);
  RegisterUnit('rxswitch', @RegisterRXSwitch);
  RegisterUnit('rxdice', @RegisterRXDice);
  {$IFDEF USE_TRXXPManifest}
  RegisterUnit('RxXPMan', @RegisterRxXPMan);
  {$ENDIF}
  RegisterUnit('PageMngr', @RegisterPageMngr);
  RegisterUnit('rxtoolbar', @RegisterRxToolBar);
  {$IFDEF USE_TRxAppIcon}
  RegisterUnit('rxappicon', @RegisterRxAppIcon);
  {$ENDIF}
  RegisterUnit('rxctrls', @RegisterRxCtrls);
  RegisterUnit('RxCustomChartPanel', @RegisterChartPanel);
  RegisterUnit('AutoPanel', @RegisterAutoPanel);
  RegisterUnit('pickdate', @RegisterPickDate);
  RegisterUnit('tooledit', @RegisterToolEdit);
  RegisterUnit('rxclock', @RegisterRxClock);
  RegisterUnit('rxspin', @RegisterRxSpin);
  RegisterUnit('RxTimeEdit', @RegisterRxTimeEdit);
  RegisterUnit('RxAboutDialog', @RegisterRxAboutDialog);
  RegisterUnit('RxViewsPanel', @RegisterRxViewsPanel);
  RegisterUnit('RxHistoryNavigator', @RegisterRxHistoryNavigator);


//
  RegisterPropertyEditor(TypeInfo(string), TPopUpColumn, 'FieldName', TPopUpColumnFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxHistoryNavigator, 'BackBtn', THistoryButtonProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxHistoryNavigator, 'ForwardBtn', THistoryButtonProperty);
  RegisterCEEditLookupFields;
end;

initialization
  {$i rx.lrs}
end.
