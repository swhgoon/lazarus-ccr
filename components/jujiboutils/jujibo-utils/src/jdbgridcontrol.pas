unit JDBGridControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, DB, Grids, DBGrids,
  Dialogs, jdbgridutils;

type

  { TJDBColumn }

  TJDBColumn = class(TColumn)
  private
    fDecimals: integer;
    function getDecimals: integer;
    procedure setDecimals(AValue: integer);
  published
    property Decimals: integer read getDecimals write setDecimals;
  end;

  { TJDBGridColumns }

  TJDBGridColumns = class(TDBGridColumns)
  private
    function GetColumn(Index: integer): TJDBColumn;
    procedure SetColumn(Index: integer; AValue: TJDBColumn);
  public
    function add: TJDBColumn;
    property Items[Index: integer]: TJDBColumn read GetColumn write SetColumn; default;
  published

  end;

  { TJDBGridControl }

  TJDBGridControl = class(TDBGrid)
  private
    { Private declarations }
    dateDbGridControl: TJDbGridDateCtrl;
    timeDbGridControl: TJDbGridTimeCtrl;
    integerDbGridControl: TJDbGridIntegerCtrl;
    doubleDbGridControl: TJDbGridDoubleCtrl;
    dateTimeDbGridControl: TJDbGridDateTimeCtrl;
    function GetColumns: TJDBGridColumns;
    procedure SetColumns(AValue: TJDBGridColumns);
  protected
    { Protected declarations }
    //procedure SelectEditor; override;
    function CreateColumns: TGridColumns; override;
    function GetDefaultEditor(Column: integer): TWinControl; override;
    procedure UpdateData; override;
    property Columns: TJDBGridColumns read GetColumns write SetColumns;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I jdbgridcontrol_icon.lrs}
  RegisterComponents('Data Controls', [TJDBGridControl]);
end;

function TJDBColumn.getDecimals: integer;
begin
  Result := fDecimals;
end;

procedure TJDBColumn.setDecimals(AValue: integer);
begin
  if (AValue >= 0) and (AValue <= 10) then
    fDecimals := AValue;
end;

{ TJDBGridColumns }

function TJDBGridColumns.GetColumn(Index: integer): TJDBColumn;
begin
  Result := TJDBColumn(inherited Items[Index]);
end;

procedure TJDBGridColumns.SetColumn(Index: integer; AValue: TJDBColumn);
begin
  Items[Index].Assign(AValue);
end;

function TJDBGridColumns.add: TJDBColumn;
begin
  Result := TJDBColumn(inherited add);
end;

{ TJDBGridControl }

//procedure TJDBGridControl.SelectEditor;
//begin
//  inherited SelectEditor;
//  if Editor <> nil then
//  begin
//    case SelectedField.DataType of
//      ftSmallint, ftInteger: Editor := integerDbGridControl.Editor(Self);
//      ftDate: Editor := dateDbGridControl.Editor(Self);    // TODO: ftDateTime ftTime
//      ftCurrency, ftFloat, ftBCD: Editor := doubleDbGridControl.Editor(Self);
//      // TODO: strings?
//    end;
//  end;
//end;

function TJDBGridControl.GetColumns: TJDBGridColumns;
begin
  Result := TJDBGridColumns(inherited Columns);
end;

procedure TJDBGridControl.SetColumns(AValue: TJDBGridColumns);
begin
  inherited Columns := TDBGridColumns(AValue);
end;

function TJDBGridControl.CreateColumns: TGridColumns;
begin
  Result := TJDBGridColumns.Create(Self, TJDBColumn);
end;

function TJDBGridControl.GetDefaultEditor(Column: integer): TWinControl;
var
  //aColumn: TColumn;
  aField: TField;
begin
  Result := inherited GetDefaultEditor(Column);
  if Result <> nil then
  begin
    //aColumn := Columns.Items[Column - 1];
    aField := GetFieldFromGridColumn(Column);
    //if aColumn <> nil then
    if aField <> nil then;
    //case aColumn.Field.DataType of
    case aField.DataType of
      ftSmallint, ftInteger: Result := integerDbGridControl.Editor(Self);
      ftDate: Result := dateDbGridControl.Editor(Self);
      ftTime: Result := timeDbGridControl.Editor(Self);
      ftDateTime: Result := dateTimeDbGridControl.Editor(Self);
      ftCurrency, ftFloat, ftBCD: Result := doubleDbGridControl.Editor(Self);
      // TODO: ftDateTime. strings?
    end;
  end;
end;

procedure TJDBGridControl.UpdateData;
begin
  if not (SelectedField.DataType in [ftSmallInt, ftInteger, ftDate,
    ftTime, ftDateTime, ftCurrency, ftFloat, ftBCD]) then
    inherited UpdateData;
  // TODO... think more about this
end;

constructor TJDBGridControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  dateDbGridControl := TJDbGridDateCtrl.Create;
  timeDbGridControl := TJDbGridTimeCtrl.Create;
  integerDbGridControl := TJDbGridIntegerCtrl.Create;
  doubleDbGridControl := TJDbGridDoubleCtrl.Create;
  dateTimeDbGridControl := TJDbGridDateTimeCtrl.Create;
end;

destructor TJDBGridControl.Destroy;
begin
  dateDbGridControl.Free;
  timeDbGridControl.Free;
  integerDbGridControl.Free;
  doubleDbGridControl.Free;
  dateTimeDbGridControl.Free;
  inherited Destroy;
end;

end.

