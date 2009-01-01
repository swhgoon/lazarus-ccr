unit uOrder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, rxdbgrid, sqlite3ds, db, ComCtrls, StdCtrls, uDbTypes, memds2,
  DbCtrls, rxdbcomb, rxlookup, dbdateedit, ufrmParent;

type

  { TfrmOrder }

  TfrmOrder = class(TfrmParent)
    btnOrderList: TButton;
    btnOrderList1: TButton;
    btnSave: TBitBtn;
    btnCancel: TBitBtn;
    cbxGroup: TComboBox;
    chkCache: TDBCheckBox;
    DBEdit1: TDBEdit;
    dsOrgs: TSqlite3Dataset;
    dsrcOrder: TDatasource;
    dsrcOrgs: TDatasource;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lbxSubGroups: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    cbxOrg: TRxDBLookupCombo;
    dsOrder: TSqlite3Dataset;
    Panel3: TPanel;
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOrderListClick(Sender: TObject);
    procedure cbxGroupChange(Sender: TObject);
    procedure cbxSubGroupChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FId_Order: TDbKeyType;
    { private declarations }
    GroupOpt, SubGroupOpt:TDbKeyType;
    GroupIndex, SubGroupIndex:Integer;

    procedure CloseAndFree;

    procedure OptionsLoad;
    procedure OptionsSubGroupLoad(const ParentID:string; const Id:string);
    procedure OptionsSave;

    function GroupChosen:string;
    function SubGroupChosen:string;

    procedure GroupIndexFind(D:TDataset);
    procedure SubGroupIndexFind(D:TDataset);

  public
    { public declarations }
    property Id_Order:TDbKeyType read FId_Order write FId_Order;
  end; 
  
var frmOrder:TfrmOrder;

implementation
uses uDebug, uBase, uOrderGoods, uUtils, uTestForm, uOptionConst;

{ TfrmOrder }

procedure TfrmOrder.btnCancelClick(Sender: TObject);
begin
  GlobalLogger.Log('Отмена изменений заказа %d', [Id_Order]);
  dsOrder.Cancel;

  CloseAndFree;
end;

procedure TfrmOrder.btnOrderListClick(Sender: TObject);
begin
  OptionsSave;

  GlobalLogger.Log('Открытие формы редактирования состава заказа');

  if frmOrderGoods = nil then
    frmOrderGoods:=TfrmOrderGoods.Create(Application);
  with frmOrderGoods do
  begin
    GoodShowType:=TGoodShowType( TComponent(Sender).Tag );
    Id_Order:=Self.Id_Order;
    Id_Org:=DBFieldAsDBKey(dsOrder, 'Org');
    {$IFDEF LCLwince}
    WindowResize;
    {$ENDIF}
    Show;
  end;

  {
  with TfrmTestForm.Create(self) do
  begin
    ShowModal;
    Free;
  end;
  }
  
  GlobalLogger.Log('Форма редактирования состава заказа успешно отработала');
end;

procedure TfrmOrder.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caHide;
end;

procedure TfrmOrder.FormCreate(Sender: TObject);
begin
  BaseConnect.ConnectToBase(dsOrder);
  dsOrder.TableName:='Orders';
  dsOrder.PrimaryKey:='ID';

  //workaround to prevent mask exception :(
  DBEdit1.EditMask:='';

  BaseConnect.ConnectToBase(dsOrgs);
  dsOrgs.SQL:='select ID, Name from Orgs order by Name';
  dsOrgs.Open;
end;

procedure TfrmOrder.FormShow(Sender: TObject);
var W:String;
begin
  GlobalLogger.Log('Переход на заказ с ID=%d', [Id_Order]);
  dsOrder.Open;
  if not dsOrder.Locate('ID', Id_Order, []) then
    begin
      GlobalLogger.Log('Заказ с ID=%d не найден', [Id_Order]);
      ShowMessage(Format('Заказ с ID=%d не найден', [Id_Order]));
      Exit;
    end;
  dsOrder.Edit;

  W:=BaseConnect.OptionUser[goOptWorkerCurrent];

  if W <> '' then
    DbFieldAssignAsDbKey(dsOrder, 'Creator', W);
  //cbxOrg.Update;
  
  OptionsLoad;
end;

procedure TfrmOrder.CloseAndFree;
begin
  dsOrder.Close;

  GlobalLogger.Log('Закрытие формы TfrmOrder');
  Close;
end;

procedure TfrmOrder.btnSaveClick(Sender: TObject);
var S:String;
begin
  GlobalLogger.Log('Сохранение заказа %d', [Id_Order]);
  dsOrder.Post;

  if dsOrder.UpdatesPending then
    begin
      GlobalLogger.Log('Применение изменений заказа %d', [Id_Order]);
      S:='Изменения заказа %d ' + Iif(dsOrder.ApplyUpdates, 'успешно применены', 'применить не удалось.');
      GlobalLogger.Log(S, [Id_Order]);
    end;
  CloseAndFree;
end;

function IdExtract(const S:string):string;
var i:Integer;
begin
  i:=Pos('|', S);
  if i = 0 then raise Exception.Create('Не найден id');
  Result:=Copy(S, 1,i-1);
end;

procedure TfrmOrder.OptionsLoad;
var S, SubGr:String;
begin
  GlobalLogger.Log('Загрузка опций');

  GlobalLogger.Log('Заполнение выпадающего списка групп');

  S:=BaseConnect.OptionUser[goOptGroupCurrent];
  if S = '' then
    begin
      BaseConnect.StringsFill('select ID, Name from Goods where ID<0 and ID > -10000000 order by Name', '%ID%| %Name%', cbxGroup.Items, nil);
      cbxGroup.ItemIndex:=0;
    end
    else
    begin
      GroupOpt:=StrToDBKey(S);
      BaseConnect.StringsFill('select ID, Name from Goods where ID<0 and ID > -10000000 order by Name', '%ID%| %Name%', cbxGroup.Items, @GroupIndexFind);
      cbxGroup.ItemIndex:=GroupIndex-1;
    end;
  GlobalLogger.Log('Заполнение выпадающего списка групп успешно завершено');

  SubGr:=BaseConnect.OptionUser[goOptSubGroupCurrent];
  OptionsSubGroupLoad(GroupChosen, SubGr );
end;

procedure TfrmOrder.OptionsSubGroupLoad(const ParentID:string; const Id:string);
var SGList:TStrings;
    SGCnt:TListBox;
begin
  GlobalLogger.Log('Заполнение выпадающего списка подгрупп');
  
  SGCnt:=lbxSubGroups;
  SGList:=SGCnt.Items;
  
  SGList.BeginUpdate;
  try
    SGList.Clear;
    SGList.Add('0| Не выбрана');
    if ParentID = '' then
      begin
        //cbxSubGroup.Items.Clear;
        exit;
      end
      else
      if (ID = '') or (ID = '0') then
      begin
        BaseConnect.StringsFill('select g.ID, g.Name from Goods g '+
                            'join HierGoods h on g.ID=h.Good and ParentID=' + ParentID + ' ' +
                            'order by Name', '%ID%| %Name%',
                            SGList, nil, false);
        SGCnt.ItemIndex:=0;
      end
      else
      begin
        SubGroupOpt:=StrToDBKey(ID);
        BaseConnect.StringsFill('select g.ID, g.Name from Goods g '+
                            'join HierGoods h on g.ID=h.Good and ParentID=' + ParentID + ' ' +
                            'order by Name', '%ID%| %Name%',
                            SGList, @SubGroupIndexFind, false);
        //cbxSubGroup.ItemIndex:=SubGroupIndex-1;
        SGCnt.ItemIndex:=SubGroupIndex;
      end;
  finally
    SGList.EndUpdate;
  end;
  GlobalLogger.Log('Заполнение выпадающего списка подгрупп успешно завершено');
end;

procedure TfrmOrder.cbxGroupChange(Sender: TObject);
begin
  //btnAcceptOptions.Enabled:=True;
  OptionsSubGroupLoad( GroupChosen, '' );
end;

procedure TfrmOrder.cbxSubGroupChange(Sender: TObject);
begin
  //btnAcceptOptions.Enabled:=True;
end;

procedure TfrmOrder.OptionsSave;
begin
  BaseConnect.OptionUser[goOptGroupCurrent]:=GroupChosen;
  BaseConnect.OptionUser[goOptSubGroupCurrent]:=SubGroupChosen;
end;

function TfrmOrder.GroupChosen: string;
begin
  if (cbxGroup.Items.Count <= 0) or
     (cbxGroup.ItemIndex < 0) or
     (cbxGroup.ItemIndex >= cbxGroup.Items.Count)
    then
    begin
      Result:='';
      exit;
    end;

  Result:=IdExtract( cbxGroup.Items[cbxGroup.ItemIndex] );
end;

function TfrmOrder.SubGroupChosen: string;
begin
  if (lbxSubGroups.Items.Count <= 0) or
     (lbxSubGroups.ItemIndex < 0) or
     (lbxSubGroups.ItemIndex >= lbxSubGroups.Items.Count)
    then
    begin
      Result:='';
      exit;
    end;

  Result:=IdExtract( lbxSubGroups.Items[lbxSubGroups.ItemIndex] );
end;

procedure TfrmOrder.GroupIndexFind(D: TDataset);
begin
  if GroupOpt = D.FieldByName('ID').AsInteger then
    GroupIndex:=D.RecNo;
end;

procedure TfrmOrder.SubGroupIndexFind(D: TDataset);
begin
  if SubGroupOpt = D.FieldByName('ID').AsInteger then
    SubGroupIndex:=D.RecNo;
end;



initialization
  {$I uordergoods.lrs}

end.

