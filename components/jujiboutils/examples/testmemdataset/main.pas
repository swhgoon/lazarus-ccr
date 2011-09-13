unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, DB, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, DBCtrls, StdCtrls, LCLType, jdbgridutils, jdbutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Datasource1: TDatasource;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit4: TDBEdit;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MemDataset1: TMemDataset;
    procedure DBGrid1SelectEditor(Sender: TObject; Column: TColumn;
      var Editor: TWinControl);
    procedure FormCreate(Sender: TObject);
    procedure OnCurrencyDBControlEnter(Sender: TObject);
    procedure OnDateDBControlEnter(Sender: TObject);
    procedure OnIntegerDBControlEnter(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.DBGrid1SelectEditor(Sender: TObject; Column: TColumn;
  var Editor: TWinControl);
begin
  case Column.DesignIndex of
    0, 2: Editor := integerDbGridControl.Editor(Sender as TDBGrid);
    1: Editor := dateDbGridControl.Editor(Sender as TDBGrid);
    3: Editor := currencyDbGridControl.Editor(Sender as TDBGrid);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  // populate the memDataset
  for i := 1 to 10 do
  begin
    MemDataset1.Append;
    MemDataset1.FieldByName('ID').AsInteger := i;
    MemDataset1.FieldByName('DATE').AsDateTime := Now;
    MemDataset1.FieldByName('ID2').AsInteger := i * i;
    MemDataset1.FieldByName('TOTAL').AsFloat := i * i * i;
    MemDataset1.Post;
  end;
  MemDataset1.First;
end;

procedure TForm1.OnCurrencyDBControlEnter(Sender: TObject);
begin
  currencyDbControl.Enter(Sender as TDBEdit);
end;

procedure TForm1.OnDateDBControlEnter(Sender: TObject);
begin
  dateDbControl.Enter(Sender as TDBEdit);
end;

procedure TForm1.OnIntegerDBControlEnter(Sender: TObject);
begin
  integerDbControl.Enter(Sender as TDBEdit);
end;

end.

