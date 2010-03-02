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
uses FileUtil;

{$R *.lfm}

{ TpbMainForm }

procedure TpbMainForm.FormCreate(Sender: TObject);
var
  S:string;
begin
  S:=AppendPathDelim(ExtractFilePath(ParamStr(0)))+'bases';
  Dbf1.FilePathFull:=S;
  Dbf1.Open;
end;

end.

