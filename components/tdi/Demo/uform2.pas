unit uForm2 ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons ;

type

  { TForm2 }

  TForm2 = class(TForm)
    bClose : TButton ;
    bToggle1 : TButton ;
    bToggle2 : TButton ;
    CheckBox1 : TCheckBox ;
    Edit1 : TEdit ;
    Edit2 : TEdit ;
    Label1 : TLabel ;
    Label2 : TLabel ;
    procedure bCloseClick(Sender : TObject) ;
    procedure bToggle1Click(Sender : TObject) ;
    procedure CheckBox1Change(Sender : TObject) ;
    procedure FormActivate(Sender : TObject) ;
    procedure FormClose(Sender : TObject ; var CloseAction : TCloseAction) ;
    procedure FormCloseQuery(Sender : TObject ; var CanClose : boolean) ;
    procedure FormCreate(Sender : TObject) ;
    procedure FormDeactivate(Sender : TObject) ;
    procedure FormDestroy(Sender : TObject) ;
    procedure FormHide(Sender : TObject) ;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender : TObject) ;
  private
    { private declarations }
  public
    { public declarations }
  end ;

var
  Form2 : TForm2 ;

implementation

Uses uMainForm;

{ TForm2 }

procedure TForm2.FormClose(Sender : TObject ; var CloseAction : TCloseAction) ;
begin
  fMainForm.AddToLog( 'Form2.OnClose' );
end;

procedure TForm2.bCloseClick(Sender : TObject) ;
begin
  Close;
  ClientRect;
end;

procedure TForm2.bToggle1Click(Sender : TObject) ;
begin
  bToggle1.Enabled := not bToggle1.Enabled;
  bToggle2.Enabled := not bToggle1.Enabled;
end;

procedure TForm2.CheckBox1Change(Sender : TObject) ;
begin
  Edit1.Enabled := CheckBox1.Checked;
end;

procedure TForm2.FormActivate(Sender : TObject) ;
begin
  fMainForm.AddToLog( 'Form2.OnActivate' );
end;

procedure TForm2.FormCloseQuery(Sender : TObject ; var CanClose : boolean) ;
begin
  fMainForm.AddToLog( 'Form2.OnCloseQuery: '+BoolToStr(CanClose,'True','False') );
end;

procedure TForm2.FormCreate(Sender : TObject) ;
begin
  fMainForm.AddToLog( 'Form2.OnCreate' );
end;

procedure TForm2.FormDeactivate(Sender : TObject) ;
begin
  fMainForm.AddToLog( 'Form2.OnDeactivate' );
end;

procedure TForm2.FormDestroy(Sender : TObject) ;
begin
  fMainForm.AddToLog( 'Form2.OnDestroy' );
end;

procedure TForm2.FormHide(Sender : TObject) ;
begin
  fMainForm.AddToLog( 'Form2.OnHide' );
end;

procedure TForm2.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  fMainForm.AddToLog( 'Form2.OnKeyDown');
end;

procedure TForm2.FormShow(Sender : TObject) ;
begin
  fMainForm.AddToLog( 'Form2.OnShow' );
end;

{$R *.lfm}

end.

