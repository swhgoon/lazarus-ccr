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
    CheckBox1 : TCheckBox ;
    CheckBox2 : TCheckBox ;
    Edit1 : TEdit ;
    Edit2 : TEdit ;
    Label1 : TLabel ;
    Label2 : TLabel ;
    procedure bCloseClick(Sender : TObject) ;
    procedure FormClose(Sender : TObject ; var CloseAction : TCloseAction) ;
    procedure FormCloseQuery(Sender : TObject ; var CanClose : boolean) ;
    procedure FormDestroy(Sender : TObject) ;
    procedure FormHide(Sender : TObject) ;
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
  fMainForm.mEvents.Lines.Add( 'Form2.Close' );
end;

procedure TForm2.bCloseClick(Sender : TObject) ;
begin
  Close;
  ClientRect;
end;

procedure TForm2.FormCloseQuery(Sender : TObject ; var CanClose : boolean) ;
begin
  fMainForm.mEvents.Lines.Add( 'Form2.CloseQuery: '+BoolToStr(CanClose,'True','False') );
end;

procedure TForm2.FormDestroy(Sender : TObject) ;
begin
  fMainForm.mEvents.Lines.Add( 'Form2.Destroy' );
end;

procedure TForm2.FormHide(Sender : TObject) ;
begin
  fMainForm.mEvents.Lines.Add( 'Form2.Hide' );
end;

procedure TForm2.FormShow(Sender : TObject) ;
begin
  fMainForm.mEvents.Lines.Add( 'Form2.Show' );
end;

{$R *.lfm}

end.

