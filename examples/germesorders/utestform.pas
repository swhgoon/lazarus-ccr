unit uTestForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmTestForm }

  TfrmTestForm = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmTestForm: TfrmTestForm;

implementation

{ TfrmTestForm }

procedure TfrmTestForm.Button1Click(Sender: TObject);
begin
  Close;
end;

initialization
  {$I utestform.lrs}

end.

