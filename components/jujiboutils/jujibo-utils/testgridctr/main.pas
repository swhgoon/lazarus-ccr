unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DbCtrls, DBGrids, JDBGridControl;

type

  { TForm1 }

  TForm1 = class(TForm)
    Datasource1: TDatasource;
    DBNavigator1: TDBNavigator;
    JDBGridControl1: TJDBGridControl;
    MemDataset1: TMemDataset;
    procedure FormCreate(Sender: TObject);
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
    MemDataset1.FieldByName('QUANTITY').AsFloat := i * i * i;
    MemDataset1.Post;
  end;
  MemDataset1.First;
end;

end.

