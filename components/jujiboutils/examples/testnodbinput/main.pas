unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, jinpututils, Buttons, ExtCtrls, Spin, LCLType, EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    campoImporte : TJCurrencyCtrl;
    campoLitros : TJCurrencyCtrl;
    campoGrados : TJCurrencyCtrl;
    campoEntero : TJIntegerCtrl;
    campoFecha : TJDateCtrl;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  campoImporte:= TJCurrencyCtrl.Create(Edit2);
  campoLitros:= TJCurrencyCtrl.Create(Edit3, 2, '#,0.0000 L');
  campoGrados:= TJCurrencyCtrl.Create(LabeledEdit1);
  campoGrados.format:= '#,0.00 º';
  campoEntero:= TJIntegerCtrl.Create(LabeledEdit2);
  campoEntero.format := ',0 º';
  campoFecha := TJDateCtrl.Create(LabeledEdit3);
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if campoFecha.isNull then
    ShowMessage('No Date')
  else
    ShowMessage('has Date');
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  campoImporte.Free;
  campoLitros.Free;
  campoGrados.Free;
  campoEntero.Free;
  campoFecha.Free;
end;

initialization
  {$I main.lrs}

end.

