unit JDBGridControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, DB, DBGrids,
  Dialogs, jdbgridutils;

type

  { TJDBGridControl }

  TJDBGridControl = class(TDBGrid)
  private
    { Private declarations }
    dateDbGridControl: TJDbGridDateCtrl;
    timeDbGridControl: TJDbGridTimeCtrl;
    integerDbGridControl: TJDbGridIntegerCtrl;
    doubleDbGridControl: TJDbGridDoubleCtrl;
  protected
    { Protected declarations }
    //procedure SelectEditor; override;
    function GetDefaultEditor(Column: integer): TWinControl; override;
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

function TJDBGridControl.GetDefaultEditor(Column: integer): TWinControl;
var
  //aColumn: TColumn;
  aField: TField;
begin
  Result := inherited GetDefaultEditor(Column);
  if Result <> nil then
  begin
    //aColumn := Columns.Items[Column - 1];
    aField:= GetFieldFromGridColumn(Column);
    //if aColumn <> nil then
    if aField <> nil then;
      //case aColumn.Field.DataType of
      case aField.DataType of
        ftSmallint, ftInteger: Result := integerDbGridControl.Editor(Self);
        ftDate: Result := dateDbGridControl.Editor(Self);
        ftTime: Result := timeDbGridControl.Editor(Self);
        ftCurrency, ftFloat, ftBCD: Result := doubleDbGridControl.Editor(Self);
        // TODO: ftDateTime. strings?
      end;
  end;
end;

constructor TJDBGridControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  dateDbGridControl := TJDbGridDateCtrl.Create;
  timeDbGridControl := TJDbGridTimeCtrl.Create;
  integerDbGridControl := TJDbGridIntegerCtrl.Create;
  doubleDbGridControl := TJDbGridDoubleCtrl.Create;
end;

destructor TJDBGridControl.Destroy;
begin
  dateDbGridControl.Free;
  timeDbGridControl.Free;
  integerDbGridControl.Free;
  doubleDbGridControl.Free;
  inherited Destroy;
end;

end.

