unit uOrderGoods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, rxdbgrid, sqlite3ds, db, ComCtrls, StdCtrls, uDbTypes, memds2, ufrmParent,
  contnrs;

type
  TGoodShowType = (gstBusket=1, gstOrder=2);

  { TfrmOrderGoods }

  TfrmOrderGoods = class(TfrmParent)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    dsrcOrderGoods: TDatasource;
    dsBusket: TMemDataset;
    ledtOrderSum: TLabeledEdit;
    memGoodHint: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    grdBusket: TRxDBGrid;
    Panel2: TPanel;
    pnlKeyboard: TPanel;
    TabSheet1: TTabSheet;
    tbOrderSum: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure dsBusketAfterEdit(DataSet: TDataSet);
    procedure dsBusketAfterScroll(DataSet: TDataSet);
    procedure dsBusketBeforeEdit(DataSet: TDataSet);
    procedure dsBusketBeforePost(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tbOrderSumShow(Sender: TObject);
  private
    CopyingRecords:boolean;
    FGoodShowType: TGoodShowType;
    FId_Order: TDbKeyType;
    DealerDefault:string;
    FId_Org: TDbKeyType;
    KeyboardControls:TComponentList;
    { private declarations }
    procedure DealerPickListFill;
    procedure SaveOrder;
    procedure Reload;
    procedure BusketFieldsDefine;
    procedure KeyboardCreate;
    procedure OnKeyboardClick(Sender:TObject);
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
  
    { public declarations }
    property Id_Order:TDbKeyType read FId_Order write FId_Order;
    property Id_Org:TDbKeyType read FId_Org write FId_Org;
    property GoodShowType:TGoodShowType read FGoodShowType write FGoodShowType;
  end; 

var
  frmOrderGoods: TfrmOrderGoods;

implementation
uses uConfig, uDebug, uUtils, uBase, variants, uOptionConst, LCLType;

{ TfrmOrderGoods }

procedure TfrmOrderGoods.BitBtn2Click(Sender: TObject);
begin
  GlobalLogger.Log('Закрытие формы TfrmOrderGoods');
  Close;
end;

procedure TfrmOrderGoods.dsBusketAfterEdit(DataSet: TDataSet);
begin
  dsBusketAfterScroll(DataSet);
end;

procedure TfrmOrderGoods.dsBusketAfterScroll(DataSet: TDataSet);
begin
  if CopyingRecords then Exit;
  memGoodHint.Lines.Text:=Dataset.FieldByName('Good_Name').AsString;
  memGoodHint.Invalidate;
end;

procedure TfrmOrderGoods.dsBusketBeforeEdit(DataSet: TDataSet);
var D:String;
begin
  if Dataset.FieldByName('Dealer').IsNull then
    begin
      D:=DealerDefault;
      {
      //последний дилер
      if D = '' then
        D:=BaseConnect.OptionUser[goOptDealerCurrent];}
      if D = '' then
        D:='0'; //розница
      Dataset.FieldByName('Dealer').AsString:=D;
    end;

  dsBusketAfterScroll(DataSet);
end;

procedure TfrmOrderGoods.dsBusketBeforePost(DataSet: TDataSet);
var S:String;
    P:Variant;
    PS:Variant;
begin
  if CopyingRecords then Exit;

  S:='';
  if dsBusket.FieldByName('Selected').AsInteger = 1 then
    begin
      if dsBusket.FieldByName('Quantity').IsNull or
         (dsBusket.FieldByName('Quantity').AsFloat <= 0.000001) then
         begin
           S:='Поле Количество не заполнено';
         end;

      if dsBusket.FieldByName('Dealer').IsNull then
        S:='Поле Дилер должно быть заполнено'
        else
        begin
          P:=BaseConnect.DLookup(Format('select Price from Price where Good=%d and Dealer=%d',
                                  [dsBusket.FieldByName('ID_Good').AsInteger,
                                   dsBusket.FieldByName('Dealer').AsInteger]), 'Price');
          dsBusket.FieldByName('Price').AsVariant:=P;
          if VarIsNull(P) then
            begin
              S:='Для Дилера не задана цена.' + #13#10 +
                 Format('Товар=%d, дилер=%d',
                    [dsBusket.FieldByName('ID_Good').AsInteger,
                     dsBusket.FieldByName('Dealer').AsInteger]);
              PS:=null;
            end
            else
            PS:=P*dsBusket.FieldByName('Quantity').AsFloat;
          dsBusket.FieldByName('PriceSum').AsVariant:=PS;

          //запоминание номера дилера
          BaseConnect.OptionUser[goOptDealerCurrent]:=dsBusket.FieldByName('Dealer').AsString;
        end;
    end;

  dsBusket.FieldByName('RemainsCurrent').AsFloat:=
     dsBusket.FieldByName('Remains').AsFloat - dsBusket.FieldByName('Quantity').AsFloat;

  if S <> '' then
    begin
      ShowMessage(S);

      //снимаем выделение
      dsBusket.FieldByName('Selected').AsInteger:=0;
    end;
end;

procedure TfrmOrderGoods.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caHide;
end;

procedure TfrmOrderGoods.FormResize(Sender: TObject);
begin
  KeyboardCreate;
end;

procedure TfrmOrderGoods.FormShow(Sender: TObject);
begin
  GlobalLogger.Log('Открытие формы TfrmOrderGoods');
  Reload;
end;

procedure TfrmOrderGoods.tbOrderSumShow(Sender: TObject);
var OrderSum:Double;
begin
  if not dsBusket.Active then Exit;

  {
  //по неизвестной причине в ARM считает не правильно
  SaveOrder;
  ledtOrderSum.Text:=VarToStr(BaseConnect.DLookup(
    'SELECT SUM(Price*Quantity) as S FROM Order_List Where ID_Order=%d', [ID_Order], 'S'));
    //'SELECT SUM((Price + 0.0)*(Quantity + 0.0) + 0.0) as S FROM Order_List Where ID_Order=%d', [ID_Order], 'S'));
  }
  GlobalLogger.Log('Подсчет суммы заказа');
  CopyingRecords:=true;
  try
    dsBusket.DisableControls;
    dsBusket.First;
    OrderSum:=0;
    while not dsBusket.EOF do
    begin
      if dsBusket.FieldByName('Selected').AsInteger <> 0 then
        begin
          OrderSum:=OrderSum + dsBusket.FieldByName('Price').AsFloat * dsBusket.FieldByName('Quantity').AsFloat;
        end;

      dsBusket.Next;
    end;
    
    dsBusket.First;
    ledtOrderSum.Text:=FormatFloat('#,###.##', OrderSum);
    GlobalLogger.Log('Подсчет суммы заказа успешно завершено');
  finally
    CopyingRecords:=false;
    dsBusket.EnableControls;
  end;
end;

procedure TfrmOrderGoods.BitBtn1Click(Sender: TObject);
begin
  SaveOrder;
  Close;
end;

{
procedure TfrmOrderGoods.SaveOrder;
var PostDS:TSqlite3Dataset;
begin
  GlobalLogger.Log('Сохранение заказа с id=%d. Старт', [ID_Order]);

  GlobalLogger.Log('Очистка существующего содержимого заказа');
  //сначала удалим все из существующего заказа
  BaseConnect.SQLExec('DELETE FROM Order_List WHERE ID_Order=' + IntToStr(ID_Order));

  GlobalLogger.Log('Добавление товаров в заказ');
  PostDS:=nil;
  CopyingRecords:=true;
  try
    PostDS:=BaseConnect.DatasetCreate('Order_List', 'ID');
    
    //dsrcOrderGoods.Enabled:=False;
    dsBusket.DisableControls;
    dsBusket.First;
    while not dsBusket.EOF do
    begin
      if dsBusket.FieldByName('Selected').AsInteger <> 0 then
        begin
          PostDS.Append;
          PostDS.FieldByName('ID_Order').AsInteger:=Id_Order;
          PostDS.FieldByName('Good').AsInteger  :=dsBusket.FieldByName('ID_Good').AsInteger;
          PostDS.FieldByName('Price').AsFloat   :=dsBusket.FieldByName('Price').AsFloat;
          PostDS.FieldByName('Quantity').AsFloat:=dsBusket.FieldByName('Quantity').AsFloat;
          PostDS.FieldByName('Dealer').AsInteger:=dsBusket.FieldByName('Dealer').AsInteger;
          PostDS.Post;
        end;
      
      dsBusket.Next;
    end;
    
    GlobalLogger.Log('Применение заказа');
    PostDS.ApplyUpdates;
  finally
    //dsrcOrderGoods.Enabled:=true;
    CopyingRecords:=false;
    dsBusket.EnableControls;
    PostDS.Free;

    GlobalLogger.Log('Сохранение заказа с id=%d. Финиш', [ID_Order]);
  end;
end;
}
procedure TfrmOrderGoods.SaveOrder;
var Fmt: TFormatSettings;
    S:String;
begin
  GlobalLogger.Log('Сохранение заказа с id=%d. Старт', [ID_Order]);

  GlobalLogger.Log('Очистка существующего содержимого заказа');
  //сначала удалим все из существующего заказа
  BaseConnect.SQLExec('DELETE FROM Order_List WHERE ID_Order=' + IntToStr(ID_Order));

  GlobalLogger.Log('Добавление товаров в заказ');
  CopyingRecords:=true;
  
  Fmt:=DefaultFormatSettings;
  Fmt.DecimalSeparator:='.';
  try
    dsBusket.DisableControls;
    dsBusket.First;
    while not dsBusket.EOF do
    begin
      if dsBusket.FieldByName('Selected').AsInteger <> 0 then
        begin
          S:=Format('INSERT INTO Order_List(ID, ID_Order, Good, Price, Quantity, Dealer) ' +
                              'VALUES(NULL, %d, %d, %s, %s, %d)',
                              [Id_Order, dsBusket.FieldByName('ID_Good').AsInteger,
                               FloatToStr(dsBusket.FieldByName('Price').AsFloat, Fmt),
                               FloatToStr(dsBusket.FieldByName('Quantity').AsFloat, Fmt),
                               dsBusket.FieldByName('Dealer').AsInteger
                               ]);
          BaseConnect.SQLExec(S);
        end;

      dsBusket.Next;
    end;

    GlobalLogger.Log('Применение заказа');
  finally
    //dsrcOrderGoods.Enabled:=true;
    CopyingRecords:=false;
    dsBusket.EnableControls;

    GlobalLogger.Log('Сохранение заказа с id=%d. Финиш', [ID_Order]);
  end;
end;

procedure TfrmOrderGoods.Reload;
var dsOrder_Busket:TSqlite3Dataset;
    S:string;
    SubGroupId, GroupId:String;
    Filter, ShowTypeFilter:string;
    Q, P:Variant;
begin
  GlobalLogger.Log('Заполнение корзины товаров');
  DealerPickListFill;

  SubGroupId:=BaseConnect.OptionUser[goOptSubGroupCurrent];
  if SubGroupId = '' then
    begin
      ShowMessage('Не выбрана текущая подгруппа товаров');
      dsBusket.Close;
    end;
    
  GroupId:=BaseConnect.OptionUser[goOptGroupCurrent];
  if GroupId = '' then
    begin
      ShowMessage('Не выбрана текущая группа товаров');
      dsBusket.Close;
    end;

  {
  if SubGroupId = '0' then
    Filter:='left join Good_Groups h on (o.ID_Good = h.Good and h.ID_Group=' + GroupId + ') '
    else
    Filter:='left join HierGoods h on (o.ID_Good = h.Good and h.ParentId=' + SubGroupId + ') ';
  }
  if SubGroupId = '0' then
    Filter:='left join Good_Groups h on (g.ID = h.Good and h.ID_Group=' + GroupId + ') '
    else
    Filter:='left join HierGoods h on (g.ID = h.Good and h.ParentId=' + SubGroupId + ') ';

  DealerDefault:=VarToStr(BaseConnect.DLookup('SELECT DealerDefault FROM Orgs WHERE Id = %d', [Id_Org], 'DealerDefault'));

  ShowTypeFilter:='';
  {
  case GoodShowType of
    gstBusket:
      ShowTypeFilter:=Iif(Id_Order <> -1, Format(' WHERE (ID_Order=-1 or ID_Order=%d) and ((o.Selected=1) or not (h.Good is null)) ', [Id_Order]), '');
    gstOrder:
      ShowTypeFilter:=Format(' WHERE (ID_Order=%d) and (o.Selected=1) ', [Id_Order]);
    else
      begin
        ShowMessage('Неизвестный тип показа заказа');
      end;
  end;
  }
  case GoodShowType of
    gstBusket:
      //ShowTypeFilter:=Iif(Id_Order <> -1, Format(' WHERE (ol.ID_Order is null or ol.ID_Order=%d) and ((ol.ID is not null) or (h.Good is not null)) ', [Id_Order]), '');
      //ShowTypeFilter:=Iif(Id_Order <> -1, Format(' WHERE (ol.ID_Order is null and (h.Good is not null)) or (ol.ID_Order=%d and (ol.ID is not null)) ', [Id_Order]), '');
      ShowTypeFilter:=Iif(Id_Order <> -1, Format(' WHERE (ol.ID_Order is null and (h.Good is not null)) or (ol.ID is not null) ', [Id_Order]), '');
    gstOrder:
      ShowTypeFilter:=Format(' WHERE (ol.ID_Order=%d) and (ol.ID is not null) ', [Id_Order]);
    else
      begin
        ShowMessage('Неизвестный тип показа заказа');
      end;
  end;

  dsOrder_Busket:=nil;
  try
    {S:=
      'SELECT ID_Order, Selected, ID_Order_List, o.ID_Good as ID_Good, ' +
      'Good_Name, Price, Quantity, Remains, ' +
      'Dealer '+
      'FROM Order_Busket o ' +
      Filter +
      ShowTypeFilter +
      ' ORDER BY Good_Name';
    }
    S:=Format(
      'SELECT coalesce(ol.ID_Order, -1) as ID_Order, ' +
             'coalesce(ol.ID+1, 0)/coalesce(ol.ID+1, 1) as Selected, ' +
             'coalesce(ol.ID, -1) as ID_Order_List, '+
             'g.ID as ID_Good, ' +
             'g.Name as Good_Name, ' +
             'ol.Price as Price, ' +
             'ol.Quantity as Quantity, ' +
             'coalesce(g.Remains, 0) as Remains, ' +
             'ol.Dealer as Dealer '+
      'FROM Goods g left join Order_List ol on (g.ID=ol.Good and ol.ID_Order=%d) ' +
      Filter +
      ShowTypeFilter +
      ' ORDER BY Good_Name', [Id_Order]);
      
    GlobalLogger.Log('Открытие датасета dsOrder_Busket (%s)', [S]);
    dsOrder_Busket:=BaseConnect.DatasetCreate(S);

    dsBusket.DisableControls;
    try
      GlobalLogger.Log('Заполнение временного датасета. Старт');
      CopyingRecords:=true;
      //dsBusket.CopyFromDataset(dsOrder_Busket, true);
      
      dsBusket.Clear(False);
      dsBusket.Open;
      
      GlobalLogger.LogDatasetFieldNames('dsOrder_Busket', dsOrder_Busket);

      while not dsOrder_Busket.EOF do
      begin
        dsBusket.Append;
        DbFieldAssignAsDbKey(dsBusket, 'ID_Order', dsOrder_Busket.FieldByName('ID_Order'));
        dsBusket.FieldByName('Selected').AsVariant:=dsOrder_Busket.FieldByName('Selected').AsVariant;
        dsBusket.FieldByName('ID_Order_List').AsVariant:=dsOrder_Busket.FieldByName('ID_Order_List').AsVariant;
        dsBusket.FieldByName('ID_Good').AsVariant:=dsOrder_Busket.FieldByName('ID_Good').AsVariant;
        dsBusket.FieldByName('Good_Name').AsVariant:=dsOrder_Busket.FieldByName('Good_Name').AsVariant;
        dsBusket.FieldByName('Dealer').AsVariant:=dsOrder_Busket.FieldByName('Dealer').AsVariant;
        
        P:=dsOrder_Busket.FieldByName('Price').AsVariant;
        dsBusket.FieldByName('Price').AsVariant:=P;
        if VarIsNull(P) then P:=0;
        
        Q:=dsOrder_Busket.FieldByName('Quantity').AsVariant;
        dsBusket.FieldByName('Quantity').AsVariant:=Q;

        if VarIsNull(Q) then Q:=0;
        dsBusket.FieldByName('PriceSum').AsFloat:=Q*P;
        
        dsBusket.FieldByName('Remains').AsFloat:=dsOrder_Busket.FieldByName('Remains').AsFloat + Q;
        dsBusket.FieldByName('RemainsCurrent').AsVariant:=dsOrder_Busket.FieldByName('Remains').AsVariant;
        dsBusket.Post;

        dsOrder_Busket.Next;
      end;

      dsBusket.First;
      
      GlobalLogger.Log('Заполнение временного датасета. Финиш');
    finally
      dsBusket.EnableControls;
      CopyingRecords:=False;
    end;
  finally
    dsOrder_Busket.Free;
  end;
end;

procedure TfrmOrderGoods.BusketFieldsDefine;
begin
  dsBusket.FieldDefs.Add('ID_Order', ftDbKey);
  dsBusket.FieldDefs.Add('Selected', ftInteger);
  dsBusket.FieldDefs.Add('ID_Order_List', ftDbKey);
  dsBusket.FieldDefs.Add('ID_Good', ftDbKey);
  dsBusket.FieldDefs.Add('Good_Name', ftString, 250);
  dsBusket.FieldDefs.Add('Price', ftFloat);
  dsBusket.FieldDefs.Add('PriceSum', ftFloat);
  dsBusket.FieldDefs.Add('Quantity', ftFloat);
  dsBusket.FieldDefs.Add('Dealer', ftInteger);
  dsBusket.FieldDefs.Add('Remains', ftFloat);
  dsBusket.FieldDefs.Add('RemainsCurrent', ftFloat);
  dsBusket.CreateTable;
end;

const
  BackSpaceTag = 100;

procedure TfrmOrderGoods.OnKeyboardClick(Sender: TObject);
var S,Q:String;
begin
  if ActiveControl = nil then Exit;
  
  Q:=dsBusket.FieldByName('Quantity').AsString;
  if TComponent(Sender).Tag = BackSpaceTag then
     begin
       S:='';
       if Length(Q) > 0 then S:=Copy(Q, 1, Length(Q)-1);
     end
    else
     begin
       S:=Q + IntToStr(TComponent(Sender).Tag);
     end;
  dsBusket.FieldByName('Quantity').AsString:=S;
  dsBusketBeforePost(dsBusket);
end;

procedure TfrmOrderGoods.KeyboardCreate;
var n:integer;
    ButWidth:Integer;
    ButWidthF:double;
    
function ButtonCreate(Caption:string; LeftPos, TagN:Integer):TButton;
var B:TButton;
begin
  B:=TButton.Create(self);
  B.Parent:=pnlKeyboard;

  B.BorderWidth:=0;
  B.Font.Name:='Sans';
  B.Font.Size:=7;

  B.Left:=LeftPos;
  B.Top:=0;
  B.Width:=ButWidth;
  B.Height:=pnlKeyboard.Height;
  B.Caption:=Caption;
  B.OnClick:=@OnKeyboardClick;
  B.Tag:=TagN;

  KeyboardControls.Add(B);
  Result:=B;
end;
    
begin
  KeyboardControls.Clear;
  
  ButWidthF:=pnlKeyboard.Width/11;
  ButWidth:=trunc(ButWidthF);
  
  for n:=0 to 9 do
    begin
      ButtonCreate(IntToStr(n), trunc(n*ButWidthF), n);
    end;
  ButtonCreate('<', trunc((n+1)*ButWidthF), BackSpaceTag);
end;

procedure TfrmOrderGoods.DealerPickListFill;
var SQL:TSqlite3Dataset;
    C:TRxColumn;
begin
  GlobalLogger.Log('Заполнение picklist для столбца дилер');

  C:=grdBusket.ColumnByFieldName('Dealer');
  C.KeyList.Clear;
  C.PickList.Clear;

  SQL:=nil;
  try
    SQL:=BaseConnect.DatasetCreate('select d.id, d.Name from Dealers d order by d.Name');
    while not SQL.EOF do
    begin
      C.KeyList.Add( SQL.FieldByName('id').AsString );
      C.PickList.Add( SQL.FieldByName('Name').AsString );
      SQL.Next;
    end;
  finally
    SQL.Free;
  end;
end;

constructor TfrmOrderGoods.Create(AOwner: TComponent);
begin
  GlobalLogger.Log('constructor TfrmOrderGoods.Create. Старт');

  FId_Order:=-1;
  FGoodShowType:=gstBusket;
  CopyingRecords:=false;
  KeyboardControls:=TComponentList.create(true);
  KeyboardControls.OwnsObjects:=True;
  
  Inherited;

  BusketFieldsDefine;
  KeyboardCreate;
  
  GlobalLogger.Log('constructor TfrmOrderGoods.Create. Финиш');
end;

destructor TfrmOrderGoods.Destroy;
begin
  KeyboardControls.Free;

  inherited Destroy;
end;


initialization
  {$I uordergoods.lrs}

end.

