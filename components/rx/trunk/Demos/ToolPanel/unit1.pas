unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, rxtoolbar, ActnList, XMLPropStorage, Menus;


type
  { TMainForm }

  TMainForm = class(TForm)
    actExit: TAction;
    actSysMenu: TAction;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    PopupMenu2: TPopupMenu;
    sysAbout: TAction;
    actNew: TAction;
    actNext: TAction;
    actPrior: TAction;
    actCustom: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    SpeedButton2: TSpeedButton;
    ToolPanel1: TToolPanel;
    XMLPropStorage1: TXMLPropStorage;
    procedure Action1Execute(Sender: TObject);
    procedure actCustomExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure sysAboutExecute(Sender: TObject);
  private
   //
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;


implementation
uses AboutUnit;

{ TMainForm }

procedure TMainForm.Action1Execute(Sender: TObject);
begin
  ShowMessage('Hi');
end;

procedure TMainForm.actCustomExecute(Sender: TObject);
begin
  ToolPanel1.Customize(0);
end;


procedure TMainForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.sysAboutExecute(Sender: TObject);
begin
  AboutForm:=TAboutForm.Create(Application);
  AboutForm.ShowModal;
  AboutForm.Free;
end;

initialization
  {$I unit1.lrs}

end.

