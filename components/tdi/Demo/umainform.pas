unit uMainForm ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, Buttons, StdCtrls, ExtCtrls, Spin, TDIClass , types, IpHtml;

type

  { TfMainForm }

  TfMainForm = class(TForm)
    bToggleLog : TButton ;
    cbxBackgroundCorner : TComboBox ;
    Image1 : TImage ;
    ImageList1 : TImageList ;
    IpHtmlPanel1 : TIpHtmlPanel ;
    Label1 : TLabel ;
    Label2 : TLabel ;
    Label3 : TLabel ;
    MainMenu1 : TMainMenu ;
    MenuItem3 : TMenuItem ;
    mEvents : TMemo ;
    MenuItem1 : TMenuItem ;
    MenuItem2 : TMenuItem ;
    miExit : TMenuItem ;
    miForm2 : TMenuItem ;
    miForm1 : TMenuItem ;
    pBottom : TPanel ;
    seFixedPages : TSpinEdit ;
    Splitter1 : TSplitter ;
    StatusBar1 : TStatusBar ;
    TabSheet1 : TTabSheet ;
    tsFixed : TTabSheet ;
    TDINoteBook1 : TTDINoteBook ;
    procedure bToggleLogClick(Sender : TObject) ;
    procedure cbxBackgroundCornerChange(Sender : TObject) ;
    procedure FormClose(Sender : TObject ; var CloseAction : TCloseAction) ;
    procedure FormCloseQuery(Sender : TObject ; var CanClose : boolean) ;
    procedure FormCreate(Sender : TObject) ;
    procedure FormDestroy(Sender : TObject) ;
    procedure MenuItem3Click(Sender : TObject) ;
    procedure miExitClick(Sender : TObject) ;
    procedure miForm1Click(Sender : TObject) ;
    procedure miForm2Click(Sender : TObject) ;
    procedure seFixedPagesChange(Sender : TObject) ;
    procedure TDINoteBook1Change(Sender : TObject) ;
    procedure TDINoteBook1CloseTabClicked(Sender : TObject) ;
  private
    { private declarations }
    Procedure ShowNewControl(Sender: TObject);

  public
    { public declarations }
  end ;

var
  fMainForm : TfMainForm ;

implementation

Uses uForm1, uForm2 ;

{$R *.lfm}

{ TfMainForm }

procedure TfMainForm.FormCreate(Sender : TObject) ;
begin
  // This will show how TDI can preserve ActiveControl in Forms, when Changing Pages //
  Screen.OnActiveControlChange := @ShowNewControl;

  cbxBackgroundCorner.ItemIndex := Integer(TDINoteBook1.BackgroundCorner);
end;

procedure TfMainForm.FormDestroy(Sender : TObject) ;
begin
  mEvents.Lines.Add('fMainForm.Destroy');
end;

procedure TfMainForm.MenuItem3Click(Sender : TObject) ;
begin
  Form2.Show;
end;

procedure TfMainForm.cbxBackgroundCornerChange(Sender : TObject) ;
begin
  TDINoteBook1.BackgroundCorner := TTDIBackgroundCorner( cbxBackgroundCorner.ItemIndex );
end;

procedure TfMainForm.bToggleLogClick(Sender : TObject) ;
begin
  mEvents.Visible := not mEvents.Visible ;

  if mEvents.Visible then
    bToggleLog.Caption := 'Hide Log >'
  else
    bToggleLog.Caption := '< Show Log' ;
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

procedure TfMainForm.miForm2Click(Sender : TObject) ;
begin
  TDINoteBook1.ShowForInNewPage( Form2, 4 );
end;

procedure TfMainForm.seFixedPagesChange(Sender : TObject) ;
begin
  TDINoteBook1.FixedPages := seFixedPages.Value;
end;

procedure TfMainForm.TDINoteBook1Change(Sender : TObject) ;
begin
  mEvents.Lines.Add('OnChange');
end;

procedure TfMainForm.TDINoteBook1CloseTabClicked(Sender : TObject) ;
begin
  mEvents.Lines.Add( 'TDINoteBook1.OnCloseTabClicked' );
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

