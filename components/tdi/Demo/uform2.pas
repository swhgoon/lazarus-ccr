unit uForm2 ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons ;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1 : TButton ;
    CheckBox1 : TCheckBox ;
    CheckBox2 : TCheckBox ;
    Edit1 : TEdit ;
    Edit2 : TEdit ;
    procedure Button1Click(Sender : TObject) ;
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

procedure TForm2.Button1Click(Sender : TObject) ;
begin
  Close;
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

