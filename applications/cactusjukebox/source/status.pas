unit status;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons;

type

  { TStatus }

  TStatus = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Statuslabel: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    cancel: boolean;
  end; 

var
  statuswin: TStatus;

implementation

{ TStatus }

procedure TStatus.Button1Click(Sender: TObject);
begin
  cancel:=true;
end;

procedure TStatus.FormCreate(Sender: TObject);
begin
  cancel:=false;
end;

initialization
  {$I status.lrs}

end.

