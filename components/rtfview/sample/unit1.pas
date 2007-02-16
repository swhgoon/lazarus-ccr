unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, RTFView,
  Buttons;

type
  { TForm1 }

  TForm1 = class(TForm)
    Button2: TButton;
    View: TRTFView;
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
begin
  View.LoadFromFile('overview.rtf');
end;

initialization
  {$I unit1.lrs}

end.

