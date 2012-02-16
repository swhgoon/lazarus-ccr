unit lazedit_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  lazedit_constants;

type

  { TformAbout }

  TformAbout = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    labelCopyright: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  formAbout: TformAbout;

implementation

{$R *.lfm}

{ TformAbout }

procedure TformAbout.FormCreate(Sender: TObject);
begin
  labelCopyright.Caption := CopyLeftStatement;
end;

end.

