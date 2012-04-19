unit uForm1 ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1 : TButton ;
    Edit1 : TEdit ;
    Edit2 : TEdit ;
    Label1 : TLabel ;
    procedure Button1Click(Sender : TObject) ;
    procedure Edit2Exit(Sender : TObject) ;
    procedure FormClose(Sender : TObject ; var CloseAction : TCloseAction) ;
    procedure FormCloseQuery(Sender : TObject ; var CanClose : boolean) ;
    procedure FormDestroy(Sender : TObject) ;
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

procedure TForm1.Button1Click(Sender : TObject) ;
begin
  // You can close Forms from Inside, TDI will detect and close the parent Page //
  Close ;
end;

procedure TForm1.Edit2Exit(Sender : TObject) ;
begin
  if Edit2.Text = '' then
  begin
    Label1.Caption := 'Edit2 cant be empty';
    ShowMessage( 'Edit2 cant be empty' );
    Edit2.SetFocus;
  end ;
end;

procedure TForm1.FormClose(Sender : TObject ; var CloseAction : TCloseAction) ;
begin
  CloseAction := caFree;
  fMainForm.mEvents.Lines.Add( 'Form1.Close' );
end;

procedure TForm1.FormCloseQuery(Sender : TObject ; var CanClose : boolean) ;
begin
  if Edit2.Text = '' then
  begin
    CanClose := False;
    ShowMessage( 'Remember... Edit2 cant be empty' );
    Edit2.Text := 'Ok, fixed';
  end ;
  fMainForm.mEvents.Lines.Add( 'Form1.CloseQuery: '+BoolToStr(CanClose,'True','False') );
end;

procedure TForm1.FormDestroy(Sender : TObject) ;
begin
  fMainForm.mEvents.Lines.Add( 'Form1.Destroy' );
end;

end.

