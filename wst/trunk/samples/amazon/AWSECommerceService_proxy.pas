{
This unit has been produced by ws_helper.
  Input unit name : "AWSECommerceService".
  This unit name  : "AWSECommerceService_proxy".
  Date            : "11/07/2007 22:01:03".
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
      const  HelpParam : Help_Type
    ):HelpResponse_Type;
    function ItemSearch(
      const  ItemSearchParam : ItemSearch_Type
    ):ItemSearchResponse_Type;
    function ItemLookup(
      const  ItemLookupParam : ItemLookup_Type
    ):ItemLookupResponse_Type;
    function BrowseNodeLookup(
      const  BrowseNodeLookupParam : BrowseNodeLookup_Type
    ):BrowseNodeLookupResponse_Type;
    function ListSearch(
      const  ListSearchParam : ListSearch_Type
    ):ListSearchResponse_Type;
    function ListLookup(
      const  ListLookupParam : ListLookup_Type
    ):ListLookupResponse_Type;
    function CustomerContentSearch(
      const  CustomerContentSearchParam : CustomerContentSearch_Type
    ):CustomerContentSearchResponse_Type;
    function CustomerContentLookup(
      const  CustomerContentLookupParam : CustomerContentLookup_Type
    ):CustomerContentLookupResponse_Type;
    function SimilarityLookup(
      const  SimilarityLookupParam : SimilarityLookup_Type
    ):SimilarityLookupResponse_Type;
    function SellerLookup(
      const  SellerLookupParam : SellerLookup_Type
    ):SellerLookupResponse_Type;
    function CartGet(
      const  CartGetParam : CartGet_Type
    ):CartGetResponse_Type;
    function CartAdd(
      const  CartAddParam : CartAdd_Type
    ):CartAddResponse_Type;
    function CartCreate(
      const  CartCreateParam : CartCreate_Type
    ):CartCreateResponse_Type;
    function CartModify(
      const  CartModifyParam : CartModify_Type
    ):CartModifyResponse_Type;
    function CartClear(
      const  CartClearParam : CartClear_Type
    ):CartClearResponse_Type;
    function TransactionLookup(
      const  TransactionLookupParam : TransactionLookup_Type
    ):TransactionLookupResponse_Type;
    function SellerListingSearch(
      const  SellerListingSearchParam : SellerListingSearch_Type
    ):SellerListingSearchResponse_Type;
    function SellerListingLookup(
      const  SellerListingLookupParam : SellerListingLookup_Type
    ):SellerListingLookupResponse_Type;
    function MultiOperation(
      const  MultiOperationParam : MultiOperation_Type
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
  const  HelpParam : Help_Type
):HelpResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Help', GetTarget(),(Self as ICallContext));
      locSerializer.Put('HelpParam', TypeInfo(Help_Type), HelpParam);
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
  const  ItemSearchParam : ItemSearch_Type
):ItemSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ItemSearch', GetTarget(),(Self as ICallContext));
      locSerializer.Put('ItemSearchParam', TypeInfo(ItemSearch_Type), ItemSearchParam);
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
  const  ItemLookupParam : ItemLookup_Type
):ItemLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ItemLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('ItemLookupParam', TypeInfo(ItemLookup_Type), ItemLookupParam);
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
  const  BrowseNodeLookupParam : BrowseNodeLookup_Type
):BrowseNodeLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('BrowseNodeLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('BrowseNodeLookupParam', TypeInfo(BrowseNodeLookup_Type), BrowseNodeLookupParam);
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
  const  ListSearchParam : ListSearch_Type
):ListSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ListSearch', GetTarget(),(Self as ICallContext));
      locSerializer.Put('ListSearchParam', TypeInfo(ListSearch_Type), ListSearchParam);
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
  const  ListLookupParam : ListLookup_Type
):ListLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ListLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('ListLookupParam', TypeInfo(ListLookup_Type), ListLookupParam);
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
  const  CustomerContentSearchParam : CustomerContentSearch_Type
):CustomerContentSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CustomerContentSearch', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CustomerContentSearchParam', TypeInfo(CustomerContentSearch_Type), CustomerContentSearchParam);
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
  const  CustomerContentLookupParam : CustomerContentLookup_Type
):CustomerContentLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CustomerContentLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CustomerContentLookupParam', TypeInfo(CustomerContentLookup_Type), CustomerContentLookupParam);
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
  const  SimilarityLookupParam : SimilarityLookup_Type
):SimilarityLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SimilarityLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('SimilarityLookupParam', TypeInfo(SimilarityLookup_Type), SimilarityLookupParam);
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
  const  SellerLookupParam : SellerLookup_Type
):SellerLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SellerLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('SellerLookupParam', TypeInfo(SellerLookup_Type), SellerLookupParam);
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
  const  CartGetParam : CartGet_Type
):CartGetResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartGet', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CartGetParam', TypeInfo(CartGet_Type), CartGetParam);
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
  const  CartAddParam : CartAdd_Type
):CartAddResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartAdd', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CartAddParam', TypeInfo(CartAdd_Type), CartAddParam);
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
  const  CartCreateParam : CartCreate_Type
):CartCreateResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartCreate', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CartCreateParam', TypeInfo(CartCreate_Type), CartCreateParam);
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
  const  CartModifyParam : CartModify_Type
):CartModifyResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartModify', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CartModifyParam', TypeInfo(CartModify_Type), CartModifyParam);
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
  const  CartClearParam : CartClear_Type
):CartClearResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartClear', GetTarget(),(Self as ICallContext));
      locSerializer.Put('CartClearParam', TypeInfo(CartClear_Type), CartClearParam);
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
  const  TransactionLookupParam : TransactionLookup_Type
):TransactionLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('TransactionLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('TransactionLookupParam', TypeInfo(TransactionLookup_Type), TransactionLookupParam);
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
  const  SellerListingSearchParam : SellerListingSearch_Type
):SellerListingSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SellerListingSearch', GetTarget(),(Self as ICallContext));
      locSerializer.Put('SellerListingSearchParam', TypeInfo(SellerListingSearch_Type), SellerListingSearchParam);
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
  const  SellerListingLookupParam : SellerListingLookup_Type
):SellerListingLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SellerListingLookup', GetTarget(),(Self as ICallContext));
      locSerializer.Put('SellerListingLookupParam', TypeInfo(SellerListingLookup_Type), SellerListingLookupParam);
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
  const  MultiOperationParam : MultiOperation_Type
):MultiOperationResponse;
Var
  locSerializer : IFormatterClient;
  strPrmName : string;
Begin
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('MultiOperation', GetTarget(),(Self as ICallContext));
      locSerializer.Put('MultiOperationParam', TypeInfo(MultiOperation_Type), MultiOperationParam);
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
