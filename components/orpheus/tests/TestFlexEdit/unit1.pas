unit Unit1;

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, LResources, {$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, o32editf, o32flxed;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    O32FlexEdit1: TO32FlexEdit;
    Label2: TLabel;
    O32FlexEdit2: TO32FlexEdit;
    procedure O32FlexEdit1UserValidation(Sender: TObject;
      var ValidEntry: Boolean);
    procedure O32FlexEditValidationError(Sender: TObject; ErrorCode: Word;
      ErrorMsg: String);
    procedure O32FlexEdit2UserValidation(Sender: TObject;
      var ValidEntry: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFNDEF LCL}
{$R *.dfm}
{$ENDIF}

procedure TForm1.O32FlexEdit1UserValidation(Sender: TObject;
  var ValidEntry: Boolean);
begin
  ValidEntry := StrToIntDef(TO32FlexEdit(Sender).Text, 0) > 0;
end;

procedure TForm1.O32FlexEditValidationError(Sender: TObject;
  ErrorCode: Word; ErrorMsg: String);
begin
  MessageDlg(ErrorMsg + #13#10 + 'Press Ctrl+Z to undo.', mtError, [mbOK], 0);
end;

procedure TForm1.O32FlexEdit2UserValidation(Sender: TObject;
  var ValidEntry: Boolean);
begin
  ValidEntry := StrToFloatDef(TO32FlexEdit(Sender).Text, 0) > 0;
end;

initialization
{$IFDEF LCL}
{$I unit1.lrs}  {Include form's resource file}
{$ENDIF}

end.
