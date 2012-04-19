unit uMainForm ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, Buttons, StdCtrls, ExtCtrls, TDIClass , types, SynMemo, IpHtml;

type

  { TfMainForm }

  TfMainForm = class(TForm)
    cbxBackgroundCorner : TComboBox ;
    Image1 : TImage ;
    ImageList1 : TImageList ;
    IpHtmlPanel1 : TIpHtmlPanel ;
    Label1 : TLabel ;
    Label2 : TLabel ;
    MainMenu1 : TMainMenu ;
    mEvents : TMemo ;
    MenuItem1 : TMenuItem ;
    MenuItem2 : TMenuItem ;
    miExit : TMenuItem ;
    miForm2 : TMenuItem ;
    miForm1 : TMenuItem ;
    pBottom : TPanel ;
    Splitter1 : TSplitter ;
    StatusBar1 : TStatusBar ;
    TabSheet1 : TTabSheet ;
    TDINoteBook1 : TTDINoteBook ;
    procedure cbxBackgroundCornerChange(Sender : TObject) ;
    procedure FormClose(Sender : TObject ; var CloseAction : TCloseAction) ;
    procedure FormCloseQuery(Sender : TObject ; var CanClose : boolean) ;
    procedure FormCreate(Sender : TObject) ;
    procedure miExitClick(Sender : TObject) ;
    procedure miForm1Click(Sender : TObject) ;
    procedure TDINoteBook1Change(Sender : TObject) ;
  private
    { private declarations }
    Procedure ShowNewControl(Sender: TObject);

  public
    { public declarations }
  end ;

var
  fMainForm : TfMainForm ;

implementation

Uses uForm1 ;

{$R *.lfm}

{ TfMainForm }

procedure TfMainForm.FormCreate(Sender : TObject) ;
begin
  // This will show how TDI can preserve ActiveControl in Forms, when Changing Pages //
  Screen.OnActiveControlChange := @ShowNewControl;

  cbxBackgroundCorner.ItemIndex := Integer(TDINoteBook1.BackgroundCorner);
end;

procedure TfMainForm.cbxBackgroundCornerChange(Sender : TObject) ;
begin
  TDINoteBook1.BackgroundCorner := TTDIBackgroundCorner( cbxBackgroundCorner.ItemIndex );
end;

procedure TfMainForm.FormClose(Sender : TObject ; var CloseAction : TCloseAction
  ) ;
begin
  Screen.OnActiveControlChange := nil;
end;

procedure TfMainForm.FormCloseQuery(Sender : TObject ; var CanClose : boolean) ;
begin
  CanClose := TDINoteBook1.CanCloseAllPages;
end;

procedure TfMainForm.miExitClick(Sender : TObject) ;
begin
  Close;
end;

procedure TfMainForm.miForm1Click(Sender : TObject) ;
begin
  // You can Use Form Types //
  TDINoteBook1.CreateFormInNewPage( TForm1, 3 );
end;

procedure TfMainForm.TDINoteBook1Change(Sender : TObject) ;
begin
  mEvents.Lines.Add('OnChange');
end;

procedure TfMainForm.ShowNewControl(Sender : TObject) ;
var
  ControlCaption : String ;
begin

  if Assigned( Screen.ActiveControl ) then
    ControlCaption := Screen.ActiveControl.ClassName + ' - '+
                      Screen.ActiveControl.Name
  else
    ControlCaption := 'nil' ;

  StatusBar1.Panels[1].Text := ControlCaption;
  mEvents.Lines.Add( 'New Control: '+ControlCaption );
end ;

end.

