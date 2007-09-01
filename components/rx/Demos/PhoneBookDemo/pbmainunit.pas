unit pbMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, rxtoolbar,
  rxdbgrid, ComCtrls, ActnList, Menus, dbf, db;

type

  { TpbMainForm }

  TpbMainForm = class(TForm)
    ActionList1: TActionList;
    Datasource1: TDatasource;
    Dbf1: TDbf;
    Dbf1ICQ1: TStringField;
    Dbf1ID1: TLongintField;
    Dbf1MEMO1: TMemoField;
    Dbf1NAME1: TStringField;
    Dbf1PATRONYMIC1: TStringField;
    Dbf1PHONE1: TStringField;
    Dbf1SURNAME1: TStringField;
    ImageList1: TImageList;
    ImageList2: TImageList;
    MainMenu1: TMainMenu;
    RxDBGrid1: TRxDBGrid;
    StatusBar1: TStatusBar;
    ToolPanel1: TToolPanel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  pbMainForm: TpbMainForm;

implementation

{ TpbMainForm }

procedure TpbMainForm.FormCreate(Sender: TObject);
begin
  Dbf1.FilePathFull:=ExtractFilePath(ParamStr(0))+DirectorySeparator+'bases';
  Dbf1.Open;
end;

initialization
  {$I pbmainunit.lrs}

end.

