unit Unit1;

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, LResources, {$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ovcbase, ovcvlb, StdCtrls;

type
  TForm1 = class(TForm)
    OvcVirtualListBox1: TOvcVirtualListBox;
    Label1: TLabel;
    procedure OvcVirtualListBox1GetItem(Sender: TObject; Index: Integer;
      var ItemString: String);
    procedure OvcVirtualListBox1DblClick(Sender: TObject);
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

procedure TForm1.OvcVirtualListBox1GetItem(Sender: TObject; Index: Integer;
  var ItemString: String);
begin
  ItemString := 'Item ' + IntToStr(Index);
end;

procedure TForm1.OvcVirtualListBox1DblClick(Sender: TObject);
begin
  Label1.Caption := 
   'You double-clicked item ' + IntToStr(OvcVirtualListBox1.ItemIndex);
end;

initialization
{$IFDEF LCL}
{$I unit1.lrs}  {Include form's resource file}
{$ENDIF}

end.
