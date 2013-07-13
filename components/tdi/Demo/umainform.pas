unit uMainForm ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, Buttons, StdCtrls, ExtCtrls, Spin, TDIClass;

type

  { TfMainForm }

  TfMainForm = class(TForm)
    bToggleLog : TButton ;
    Button1 : TButton ;
    cbxBackgroundCorner : TComboBox ;
    Image1 : TImage ;
    ImageList1 : TImageList ;
    Label1 : TLabel ;
    Label2 : TLabel ;
    Label3 : TLabel ;
    MainMenu1 : TMainMenu ;
    Memo1 : TMemo ;
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
    tsFixed : TTabSheet ;
    TDINoteBook1 : TTDINoteBook ;
    procedure bToggleLogClick(Sender : TObject) ;
    procedure Button1Click(Sender : TObject) ;
    procedure cbxBackgroundCornerChange(Sender : TObject) ;
    procedure FormClose(Sender : TObject ; var CloseAction : TCloseAction) ;
    procedure FormCloseQuery(Sender : TObject ; var CanClose : boolean) ;
    procedure FormCreate(Sender : TObject) ;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItem3Click(Sender : TObject) ;
    procedure miExitClick(Sender : TObject) ;
    procedure miForm1Click(Sender : TObject) ;
    procedure miForm2Click(Sender : TObject) ;
    procedure seFixedPagesChange(Sender : TObject) ;
    procedure TDINoteBook1Change(Sender : TObject) ;
    procedure TDINoteBook1CloseTabClicked(Sender : TObject) ;
    procedure TDINoteBook1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    Procedure ShowNewControl(Sender: TObject);

  public
    { public declarations }
    procedure AddToLog( AStr: String) ;
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
  try
    Memo1.Lines.LoadFromFile( ExtractFilePath(Application.ExeName)+'..'+PathDelim+'read-me.txt' );
  except
  end;
end;

procedure TfMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  AddToLog('fMainForm.OnKeyDown');
end;

procedure TfMainForm.MenuItem3Click(Sender : TObject) ;
begin
  if not Assigned( Form2 ) then
    Form2 := TForm2.Create(Self);
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

procedure TfMainForm.Button1Click(Sender : TObject) ;
begin
  Form2.Free;
  Form2 := nil;
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
  if not Assigned( Form2 ) then
    Form2 := TForm2.Create(Self);
  TDINoteBook1.ShowFormInPage( Form2, 4 );
end;

procedure TfMainForm.seFixedPagesChange(Sender : TObject) ;
begin
  TDINoteBook1.FixedPages := seFixedPages.Value;
end;

procedure TfMainForm.TDINoteBook1Change(Sender : TObject) ;
begin
  AddToLog('TDINoteBook1.OnChange');
end;

procedure TfMainForm.TDINoteBook1CloseTabClicked(Sender : TObject) ;
begin
  AddToLog( 'TDINoteBook1.OnCloseTabClicked' );
end;

procedure TfMainForm.TDINoteBook1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  AddToLog( 'TDINoteBook1.OnMouseDown' );
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
  AddToLog( 'Screen.OnActiveControlChange: '+ControlCaption );
end ;

procedure TfMainForm.AddToLog(AStr : String) ;
begin
  if ([csDesigning, csDestroying] * ComponentState <> []) then exit ;

  mEvents.Lines.Add( AStr ) ;
end ;

end.

