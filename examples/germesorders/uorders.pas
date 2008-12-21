unit uOrders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, rxdbgrid, sqlite3ds, db, ComCtrls, StdCtrls, uDbTypes, DBGrids,
  rxdbcomb, LMessages, ufrmParent;

type

  { TfrmOrders }

  TfrmOrders = class(TfrmParent)
    btnOrderAdd: TBitBtn;
    btnClose: TBitBtn;
    btnOrderOpen: TBitBtn;
    btnOrderDel: TBitBtn;
    dsrcOrders: TDatasource;
    grdOrders: TRxDBGrid;
    memAbout: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    dsOrders: TSqlite3Dataset;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    procedure btnAcceptOptionsClick(Sender: TObject);
    procedure btnCancelOptionsClick(Sender: TObject);
    procedure btnOrderOpenClick(Sender: TObject);
    procedure btnOrderAddClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOrderDelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure grdOrdersDblClick(Sender: TObject);
    procedure tbOptionsShow(Sender: TObject);
  private
    { private declarations }
  protected
    procedure WndProc(var TheMessage : TLMessage);override;
  public
    { public declarations }
  end;

var
  frmOrders: TfrmOrders;

implementation
uses uConfig, uDebug, uUtils, uBase, variants, uOrderGoods, uOrder, LCLType,
  uOptionConst;

{ TfrmOrders }

procedure TfrmOrders.btnCloseClick(Sender: TObject);
begin
  GlobalLogger.Log('Закрытие формы TfrmOrders');
  Close;
end;

procedure TfrmOrders.btnOrderDelClick(Sender: TObject);
var O:TDbKeyType;
begin
  if Application.MessageBox('Подтверждение',
     'Вы уверены, что хотите удалить выделенный заказ?',
     MB_YESNO) = IDYES then
    begin
      O:=DBFieldAsDBKey(dsOrders, 'ID');
      GlobalLogger.Log('Удаление заказа %d', [O]);
      
      BaseConnect.SQLExec('DELETE FROM Orders where ID=%d', [O]);
      dsOrders.RefetchData;
    end;
end;

procedure TfrmOrders.FormActivate(Sender: TObject);
begin
  dsOrders.RefetchData;
end;

procedure TfrmOrders.btnOrderAddClick(Sender: TObject);
var G:TGuid;
    Gs:string;
    DateStr:string;
begin
  CreateGUID(G);

  Gs:=GUIDToString(G);
  DateStr:=DateToStr(Now);

  GlobalLogger.Log('Создание заказа. GUID=%s, Дата=%s', [Gs, DateStr]);
  BaseConnect.SQLExec(
    Format('INSERT INTO Orders(ID, ID_GUID, DateCreate, DateCreateT, Org, CacheLess) ' +
           'VALUES (NULL, ''%s'', ''%s'', DATETIME(''NOW''), 0, 0)',
           [ Gs, DateStr ]));
           
  dsOrders.RefetchData;
end;

procedure TfrmOrders.btnOrderOpenClick(Sender: TObject);
begin
  grdOrdersDblClick(Sender);
end;

procedure TfrmOrders.FormCreate(Sender: TObject);
var S:string;
    BuildDate:String;
begin
  BuildDate:={$I build-date};
  memAbout.Text:=Format(memAbout.Text, [BuildDate]);

  GlobalLogger.Log('Открытие формы TfrmOrders');

  S:= 'SELECT Orders.ID as ID, ID_GUID, DateCreate, DateCreateT, CacheLess, ' +
      'Creator, Orgs.Name as ORG_NAME ' +
      'FROM Orders join Orgs on Orders.Org=Orgs.ID ' +
      'Order By DateCreateT Desc';

  BaseConnect.ConnectToBase(dsOrders);
  dsOrders.SQL := S;
  dsOrders.PrimaryKey:='ID';
  dsOrders.AutoIncrementKey:=True;
  dsOrders.Open;
end;

procedure TfrmOrders.grdOrdersDblClick(Sender: TObject);
begin
  {
  with TfrmOrder.Create(self) do
  begin
    Id_Order:=DBFieldAsDBKey(dsOrders, 'ID');
    Show;
  end;
  }
  if frmOrder = nil then
    begin
      frmOrder:=TfrmOrder.Create(Application);
    end;
  
  with frmOrder do
  begin
    Id_Order:=DBFieldAsDBKey(dsOrders, 'ID');
    {$IFDEF LCLwince}
    WindowResize;
    {$ENDIF}
    Show;
  end;
end;

procedure TfrmOrders.tbOptionsShow(Sender: TObject);
begin
  //OptionsLoad;
end;

procedure TfrmOrders.WndProc(var TheMessage: TLMessage);
begin
  //GlobalLogger.Log(GetMessageName(TheMessage.Msg));

  inherited WndProc(TheMessage);
end;

procedure TfrmOrders.btnAcceptOptionsClick(Sender: TObject);
begin
  //OptionsSave;
  //btnAcceptOptions.Enabled:=false;
end;

procedure TfrmOrders.btnCancelOptionsClick(Sender: TObject);
begin
  //OptionsLoad;
end;

initialization
  {$I uordergoods.lrs}

end.

