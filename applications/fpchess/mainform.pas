unit mainform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    btnSinglePlayer: TBitBtn;
    buttonDirectComm: TBitBtn;
    BitBtn3: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    pageStart: TUNBPage;
    pageConfigConnection: TUNBPage;
    notebookMain: TUntabbedNotebook;
    pageConnecting: TUNBPage;
    ProgressBar1: TProgressBar;
    procedure buttonDirectCommClick(Sender: TObject);
    procedure HandleMainScreenButton(Sender: TObject);
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

procedure TForm1.buttonDirectCommClick(Sender: TObject);
begin
  notebookMain.PageIndex := 1;
end;

procedure TForm1.HandleMainScreenButton(Sender: TObject);
begin
  if Sender = btnSinglePlayer then
  begin
    notebookMain.PageIndex := 2;
  end;
end;

end.

