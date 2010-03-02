unit RegisterRxDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf;

procedure Register;
implementation
uses DB, DBPropEdits, rxdbgrid, RxDBSpinEdit, RxDBTimeEdit, RxDBCtrls, PropEdits;

type

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

type
{ TRxDBGridFooterFieldProperty }
  TRxDBGridFooterFieldProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

{ TRxDBGridFieldProperty }

procedure TRxDBGridFooterFieldProperty.FillValues(const Values: TStringList);
var
  Footer: TRxColumnFooter;
  Grid: TRxDBGrid;
  DataSource: TDataSource;
begin
  Footer:=TRxColumnFooter(GetComponent(0));
  Grid:=TRxDBGrid(Footer.Owner.Grid);
  if not (Grid is TRxDBGrid) then exit;
  DataSource := Grid.DataSource;
  if Assigned(DataSource) and Assigned(DataSource.DataSet) then
    DataSource.DataSet.GetFieldNames(Values);
end;


procedure RegisterRxDBSpinEdit;
begin
  RegisterComponents('RX DBAware',[TRxDBSpinEdit]);
end;

procedure RegisterRxDBTimeEdit;
begin
  RegisterComponents('RX DBAware',[TRxDBTimeEdit]);
end;

procedure RegisterRxDBCtrls;
begin
  RegisterComponents('RX DBAware',[TRxDBProgressBar, TRxDBTrackBar]);
end;

procedure RegisterRxDbGrid;
begin
  RegisterComponents('RX DBAware',[TRxDBGrid]);
end;

procedure Register;
begin
  RegisterUnit('RxDBTimeEdit', @RegisterRxDBTimeEdit);
  RegisterUnit('RxDBSpinEdit', @RegisterRxDBSpinEdit);
  RegisterUnit('RxDBCtrls', @RegisterRxDBCtrls);
  RegisterUnit('rxdbgrid', @RegisterRxDbGrid);
//
  RegisterPropertyEditor(TypeInfo(string), TRxColumn, 'FieldName', TRxDBGridFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxColumnFooter, 'FieldName', TRxDBGridFooterFieldProperty);
end;

end.

