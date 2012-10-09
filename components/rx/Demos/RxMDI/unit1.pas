unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, rxtoolbar, RxMDI, Forms, Controls, Graphics,
  Dialogs, ActnList, Menus, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Action1: TAction;
    Action2: TAction;
    Action3: TAction;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    sysClose: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    RxMDICloseButton1: TRxMDICloseButton;
    RxMDIPanel1: TRxMDIPanel;
    RxMDITasks1: TRxMDITasks;
    StatusBar1: TStatusBar;
    ToolPanel1: TToolPanel;
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
    procedure sysCloseExecute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2, Unit3, Unit4;

{$R *.lfm}

{ TForm1 }

procedure TForm1.sysCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Action1Execute(Sender: TObject);
begin
  RxMDIPanel1.ChildWindowsCreate(Form2, TForm2);
end;

procedure TForm1.Action2Execute(Sender: TObject);
begin
  RxMDIPanel1.ChildWindowsCreate(Form3, TForm3);
end;

procedure TForm1.Action3Execute(Sender: TObject);
var
  Form4: TForm4;
begin
  Form4:=TForm4.Create(Application);
  RxMDIPanel1.ChildWindowsAdd(Form4);
end;

end.

