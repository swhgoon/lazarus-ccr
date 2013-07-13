unit uForm1 ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls ;

type

  { TForm1 }

  TForm1 = class(TForm)
    bClose : TButton ;
    bHide : TButton ;
    Edit1 : TEdit ;
    Edit2 : TEdit ;
    Label1 : TLabel ;
    Label2 : TLabel ;
    Label3 : TLabel ;
    Label4 : TLabel ;
    Label5 : TLabel ;
    Label6 : TLabel ;
    Label7 : TLabel ;
    lShowmeAgain : TLabel ;
    tShowmeAgain : TTimer ;
    procedure bCloseClick(Sender : TObject) ;
    procedure bHideClick(Sender : TObject) ;
    procedure Edit2Exit(Sender : TObject) ;
    procedure FormActivate(Sender : TObject) ;
    procedure FormClose(Sender : TObject ; var CloseAction : TCloseAction) ;
    procedure FormCloseQuery(Sender : TObject ; var CanClose : boolean) ;
    procedure FormDeactivate(Sender : TObject) ;
    procedure FormDestroy(Sender : TObject) ;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tShowmeAgainTimer(Sender : TObject) ;
  private
    { private declarations }
  public
    { public declarations }
  end ;

var
  Form1 : TForm1 ;

implementation

Uses uMainForm;

{$R *.lfm}

{ TForm1 }

procedure TForm1.bCloseClick(Sender : TObject) ;
begin
  // You can close Forms from Inside, TDINoteBook will detect and close the parent Page //
  Close ;
end;

procedure TForm1.bHideClick(Sender : TObject) ;
begin
  Hide;
  tShowmeAgain.Enabled := True;
end;

procedure TForm1.Edit2Exit(Sender : TObject) ;
begin
  if Edit2.Text = '' then
  begin
    Label1.Caption := 'Edit2 cannot be empty';
    ShowMessage( 'Edit2 cannot be empty' );
    Edit2.SetFocus;
  end ;
end;

procedure TForm1.FormActivate(Sender : TObject) ;
begin
  fMainForm.AddToLog( 'Form1.OnActivate' );
end;

procedure TForm1.FormClose(Sender : TObject ; var CloseAction : TCloseAction) ;
begin
  CloseAction := caFree;
  fMainForm.AddToLog( 'Form1.OnClose' );
end;

procedure TForm1.FormCloseQuery(Sender : TObject ; var CanClose : boolean) ;
begin
  if Edit2.Text = '' then
  begin
    CanClose := False;
    ShowMessage( 'Remember... Edit2 cannot be empty' );
    Edit2.Text := 'Ok, fixed';
  end ;

  fMainForm.AddToLog( 'Form1.CloseQuery: '+BoolToStr(CanClose,'True','False') );
end;

procedure TForm1.FormDeactivate(Sender : TObject) ;
begin
  fMainForm.AddToLog( 'Form1.OnDeactivate' );
end;

procedure TForm1.FormDestroy(Sender : TObject) ;
begin
  fMainForm.AddToLog( 'Form1.OnDestroy' );
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  fMainForm.AddToLog( 'Form1.OnKeyDown' );
end;

procedure TForm1.tShowmeAgainTimer(Sender : TObject) ;
begin
  tShowmeAgain.Enabled := False;
  Show;
  lShowmeAgain.Visible := True;
end;

end.

