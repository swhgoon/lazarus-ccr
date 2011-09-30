unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, memds, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DBCtrls, Buttons, jdblabeledcurrencyedit, jdblabeledintegeredit,
  jdblabeleddateedit;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Datasource1: TDatasource;
    DBNavigator1: TDBNavigator;
    JDBLabeledCurrencyEdit1: TJDBLabeledCurrencyEdit;
    JDBLabeledDateEdit1: TJDBLabeledDateEdit;
    JDBLabeledIntegerEdit1: TJDBLabeledIntegerEdit;
    JDBLabeledIntegerEdit2: TJDBLabeledIntegerEdit;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
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
    MemDataset1.FieldByName('ID2').AsInteger := i * i;
    MemDataset1.FieldByName('TOTAL').AsFloat := i * i * i;
    MemDataset1.Post;
  end;
  MemDataset1.First;
end;

end.

