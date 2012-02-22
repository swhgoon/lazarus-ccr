unit dlgconfig;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, browserconfig;

type

  { TformConfig }

  TformConfig = class(TForm)
    btnClose: TButton;
    btnCancel: TButton;
    comboUserAgent: TComboBox;
    Label1: TLabel;
    procedure btnCloseClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  formConfig: TformConfig;

implementation

{ TformConfig }

procedure TformConfig.btnCloseClick(Sender: TObject);
begin
  FPBrowserConfig.UserAgent := comboUserAgent.Text;
  Close;
end;

initialization
  {$I dlgconfig.lrs}

end.

