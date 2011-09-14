unit gameplayform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    btnExit: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure btnExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }

    backgroundImage: TJpegImage;

  end; 

var
  Form2: TForm2; 

implementation

uses gameconfigform;

{$R *.lfm}

{ TForm2 }

procedure TForm2.btnExitClick(Sender: TObject);
begin
  Close;
  Form1.Show;

end;

procedure TForm2.FormCreate(Sender: TObject);
begin

end;

end.

