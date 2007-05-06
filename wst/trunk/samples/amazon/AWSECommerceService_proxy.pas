{
This unit has been produced by ws_helper.
  Input unit name : "AWSECommerceService".
  This unit name  : "AWSECommerceService_proxy".
  Date            : "6-5-07 19:37:08".
}

Unit AWSECommerceService_proxy;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, AWSECommerceService;

Type


  TAWSECommerceServicePortType_Proxy=class(TBaseProxy,AWSECommerceServicePortType)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function Help(
      Const HelpParam : Help_Type
    ):HelpResponse_Type;
    function ItemSearch(
      Const ItemSearchParam : ItemSearch_Type
    ):ItemSearchResponse_Type;
    function ItemLookup(
      Const ItemLookupParam : ItemLookup_Type
    ):ItemLookupResponse_Type;
    function BrowseNodeLookup(
      Const BrowseNodeLookupParam : BrowseNodeLookup_Type
    ):BrowseNodeLookupResponse_Type;
    function ListSearch(
      Const ListSearchParam : ListSearch_Type
    ):ListSearchResponse_Type;
    function ListLookup(
      Const ListLookupParam : ListLookup_Type
    ):ListLookupResponse_Type;
    function CustomerContentSearch(
      Const CustomerContentSearchParam : CustomerContentSearch_Type
    ):CustomerContentSearchResponse_Type;
    function CustomerContentLookup(
      Const CustomerContentLookupParam : CustomerContentLookup_Type
    ):CustomerContentLookupResponse_Type;
    function SimilarityLookup(
      Const SimilarityLookupParam : SimilarityLookup_Type
    ):SimilarityLookupResponse_Type;
    function SellerLookup(
      Const SellerLookupParam : SellerLookup_Type
    ):SellerLookupResponse_Type;
    function CartGet(
      Const CartGetParam : CartGet_Type
    ):CartGetResponse_Type;
    function CartAdd(
      Const CartAddParam : CartAdd_Type
    ):CartAddResponse_Type;
    function CartCreate(
      Const CartCreateParam : CartCreate_Type
    ):CartCreateResponse_Type;
    function CartModify(
      Const CartModifyParam : CartModify_Type
    ):CartModifyResponse_Type;
    function CartClear(
      Const CartClearParam : CartClear_Type
    ):CartClearResponse_Type;
    function TransactionLookup(
      Const TransactionLookupParam : TransactionLookup_Type
    ):TransactionLookupResponse_Type;
    function SellerListingSearch(
      Const SellerListingSearchParam : SellerListingSearch_Type
    ):SellerListingSearchResponse_Type;
    function SellerListingLookup(
      Const SellerListingLookupParam : SellerListingLookup_Type
    ):SellerListingLookupResponse_Type;
    function MultiOperation(
      Const MultiOperationParam : MultiOperationType
    ):MultiOperationResponse;
  End;

  Function wst_CreateInstance_AWSECommerceServicePortType(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'):AWSECommerceServicePortType;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_AWSECommerceServicePortType(const AFormat : string; const ATransport : string):AWSECommerceServicePortType;
Begin
  Result := TAWSECommerceServicePortType_Proxy.Create('AWSECommerceServicePortType',AFormat+GetServiceDefaultFormatProperties(TypeInfo(AWSECommerceServicePortType)),ATransport + 'address=' + GetServiceDefaultAddress(TypeInfo(AWSECommerceServicePortType)));
End;

{ TAWSECommerceServicePortType_Proxy implementation }

class function TAWSECommerceServicePortType_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(AWSECommerceServicePortType);
end;

function TAWSECommerceServicePortType_Proxy.Help(
  Const HelpParam : Help_Type
):HelpResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Help', GetTarget(),(Self as ICallContext));
      locSerializer.Put('Help', TypeInfo(Help_Type), HelpParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'HelpResponse';
      locSerializer.Get(TypeInfo(HelpResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.ItemSearch(
  Const ItemSearchParam : ItemSearch_Type
):ItemSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ItemSearch', GetTarget(),(Self as ICallContext));
      locSerializer.Put('ItemSearch', TypeInfo(ItemSearch_Type), ItemSearchParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'ItemSearchResponse';
      locSerializer.Get(TypeInfo(ItemSearchResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.ItemLookup(
  Const ItemLookupParam : ItemLookup_Type
):ItemLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ItemLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('ItemLookup', TypeInfo(ItemLookup_Type), ItemLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'ItemLookupResponse';
      locSerializer.Get(TypeInfo(ItemLookupResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.BrowseNodeLookup(
  Const BrowseNodeLookupParam : BrowseNodeLookup_Type
):BrowseNodeLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('BrowseNodeLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('BrowseNodeLookup', TypeInfo(BrowseNodeLookup_Type), BrowseNodeLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'BrowseNodeLookupResponse';
      locSerializer.Get(TypeInfo(BrowseNodeLookupResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.ListSearch(
  Const ListSearchParam : ListSearch_Type
):ListSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ListSearch', GetTarget(),(Self as ICallContext));
      locSerializer.Put('ListSearch', TypeInfo(ListSearch_Type), ListSearchParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'ListSearchResponse';
      locSerializer.Get(TypeInfo(ListSearchResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.ListLookup(
  Const ListLookupParam : ListLookup_Type
):ListLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ListLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('ListLookup', TypeInfo(ListLookup_Type), ListLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'ListLookupResponse';
      locSerializer.Get(TypeInfo(ListLookupResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CustomerContentSearch(
  Const CustomerContentSearchParam : CustomerContentSearch_Type
):CustomerContentSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CustomerContentSearch', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CustomerContentSearch', TypeInfo(CustomerContentSearch_Type), CustomerContentSearchParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'CustomerContentSearchResponse';
      locSerializer.Get(TypeInfo(CustomerContentSearchResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CustomerContentLookup(
  Const CustomerContentLookupParam : CustomerContentLookup_Type
):CustomerContentLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CustomerContentLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CustomerContentLookup', TypeInfo(CustomerContentLookup_Type), CustomerContentLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'CustomerContentLookupResponse';
      locSerializer.Get(TypeInfo(CustomerContentLookupResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.SimilarityLookup(
  Const SimilarityLookupParam : SimilarityLookup_Type
):SimilarityLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SimilarityLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('SimilarityLookup', TypeInfo(SimilarityLookup_Type), SimilarityLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'SimilarityLookupResponse';
      locSerializer.Get(TypeInfo(SimilarityLookupResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.SellerLookup(
  Const SellerLookupParam : SellerLookup_Type
):SellerLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SellerLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('SellerLookup', TypeInfo(SellerLookup_Type), SellerLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'SellerLookupResponse';
      locSerializer.Get(TypeInfo(SellerLookupResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CartGet(
  Const CartGetParam : CartGet_Type
):CartGetResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartGet', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CartGet', TypeInfo(CartGet_Type), CartGetParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'CartGetResponse';
      locSerializer.Get(TypeInfo(CartGetResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CartAdd(
  Const CartAddParam : CartAdd_Type
):CartAddResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartAdd', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CartAdd', TypeInfo(CartAdd_Type), CartAddParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'CartAddResponse';
      locSerializer.Get(TypeInfo(CartAddResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CartCreate(
  Const CartCreateParam : CartCreate_Type
):CartCreateResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartCreate', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CartCreate', TypeInfo(CartCreate_Type), CartCreateParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'CartCreateResponse';
      locSerializer.Get(TypeInfo(CartCreateResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CartModify(
  Const CartModifyParam : CartModify_Type
):CartModifyResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartModify', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CartModify', TypeInfo(CartModify_Type), CartModifyParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'CartModifyResponse';
      locSerializer.Get(TypeInfo(CartModifyResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CartClear(
  Const CartClearParam : CartClear_Type
):CartClearResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartClear', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CartClear', TypeInfo(CartClear_Type), CartClearParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'CartClearResponse';
      locSerializer.Get(TypeInfo(CartClearResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.TransactionLookup(
  Const TransactionLookupParam : TransactionLookup_Type
):TransactionLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('TransactionLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('TransactionLookup', TypeInfo(TransactionLookup_Type), TransactionLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'TransactionLookupResponse';
      locSerializer.Get(TypeInfo(TransactionLookupResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.SellerListingSearch(
  Const SellerListingSearchParam : SellerListingSearch_Type
):SellerListingSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SellerListingSearch', GetTarget(),(Self as ICallContext));
      locSerializer.Put('SellerListingSearch', TypeInfo(SellerListingSearch_Type), SellerListingSearchParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'SellerListingSearchResponse';
      locSerializer.Get(TypeInfo(SellerListingSearchResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.SellerListingLookup(
  Const SellerListingLookupParam : SellerListingLookup_Type
):SellerListingLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SellerListingLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('SellerListingLookup', TypeInfo(SellerListingLookup_Type), SellerListingLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'SellerListingLookupResponse';
      locSerializer.Get(TypeInfo(SellerListingLookupResponse_Type), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.MultiOperation(
  Const MultiOperationParam : MultiOperationType
):MultiOperationResponse;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('MultiOperation', GetTarget(),(Self as ICallContext));
      locSerializer.Put('MultiOperation', TypeInfo(MultiOperationType), MultiOperationParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead((Self as ICallContext));
      TObject(Result) := Nil;
      strPrmName := 'MultiOperationResponse';
      locSerializer.Get(TypeInfo(MultiOperationResponse), strPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i AWSECommerceService.wst}

  {$IF DECLARED(Register_AWSECommerceService_ServiceMetadata)}
  Register_AWSECommerceService_ServiceMetadata();
  {$IFEND}
End.
