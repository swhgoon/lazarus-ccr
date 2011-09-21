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
    integerDbGridControl: TJDbGridIntegerCtrl;
    doubleDbGridControl: TJDbGridDoubleCtrl;
  protected
    { Protected declarations }
    procedure SelectEditor; override;
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

procedure TJDBGridControl.SelectEditor;
begin
  inherited SelectEditor;
  if Editor <> nil then
  begin
    case SelectedField.DataType of
      ftSmallint, ftInteger: Editor := integerDbGridControl.Editor(Self);
      ftDate: Editor := dateDbGridControl.Editor(Self);    // TODO: ftDateTime ftTime
      ftCurrency, ftFloat, ftBCD: Editor := doubleDbGridControl.Editor(Self);
      // TODO: strings?
    end;
  end;
end;

constructor TJDBGridControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  dateDbGridControl := TJDbGridDateCtrl.Create;
  SelectEditor;
  integerDbGridControl := TJDbGridIntegerCtrl.Create;
  doubleDbGridControl := TJDbGridDoubleCtrl.Create;
end;

destructor TJDBGridControl.Destroy;
begin
  dateDbGridControl.Free;
  integerDbGridControl.Free;
  doubleDbGridControl.Free;
  inherited Destroy;
end;

end.

