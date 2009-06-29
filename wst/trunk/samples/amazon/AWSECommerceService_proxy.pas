{
This unit has been produced by ws_helper.
  Input unit name : "AWSECommerceService".
  This unit name  : "AWSECommerceService_proxy".
  Date            : "28/06/2009 21:23:50".
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
    function TagLookup(
      const  TagLookupParam : TagLookup_Type
    ):TagLookupResponse_Type;
    function VehicleSearch(
      const  VehicleSearchParam : VehicleSearch_Type
    ):VehicleSearchResponse_Type;
    function VehiclePartSearch(
      const  VehiclePartSearchParam : VehiclePartSearch_Type
    ):VehiclePartSearchResponse_Type;
    function VehiclePartLookup(
      const  VehiclePartLookupParam : VehiclePartLookup_Type
    ):VehiclePartLookupResponse_Type;
    function MultiOperation(
      const  MultiOperationParam : MultiOperation_Type
    ):MultiOperationResponse;
  End;

  Function wst_CreateInstance_AWSECommerceServicePortType(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'; const AAddress : string = ''):AWSECommerceServicePortType;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_AWSECommerceServicePortType(const AFormat : string; const ATransport : string; const AAddress : string):AWSECommerceServicePortType;
Var
  locAdr : string;
Begin
  locAdr := AAddress;
  if ( locAdr = '' ) then
    locAdr := GetServiceDefaultAddress(TypeInfo(AWSECommerceServicePortType));
  Result := TAWSECommerceServicePortType_Proxy.Create('AWSECommerceServicePortType',AFormat+GetServiceDefaultFormatProperties(TypeInfo(AWSECommerceServicePortType)),ATransport + 'address=' + locAdr);
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
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Help', GetTarget(),locCallContext);
      locSerializer.Put('Help', TypeInfo(Help_Type), HelpParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'HelpResponse';
      locSerializer.Get(TypeInfo(HelpResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.ItemSearch(
  const  ItemSearchParam : ItemSearch_Type
):ItemSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ItemSearch', GetTarget(),locCallContext);
      locSerializer.Put('ItemSearch', TypeInfo(ItemSearch_Type), ItemSearchParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'ItemSearchResponse';
      locSerializer.Get(TypeInfo(ItemSearchResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.ItemLookup(
  const  ItemLookupParam : ItemLookup_Type
):ItemLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ItemLookup', GetTarget(),locCallContext);
      locSerializer.Put('ItemLookup', TypeInfo(ItemLookup_Type), ItemLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'ItemLookupResponse';
      locSerializer.Get(TypeInfo(ItemLookupResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.BrowseNodeLookup(
  const  BrowseNodeLookupParam : BrowseNodeLookup_Type
):BrowseNodeLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('BrowseNodeLookup', GetTarget(),locCallContext);
      locSerializer.Put('BrowseNodeLookup', TypeInfo(BrowseNodeLookup_Type), BrowseNodeLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'BrowseNodeLookupResponse';
      locSerializer.Get(TypeInfo(BrowseNodeLookupResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.ListSearch(
  const  ListSearchParam : ListSearch_Type
):ListSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ListSearch', GetTarget(),locCallContext);
      locSerializer.Put('ListSearch', TypeInfo(ListSearch_Type), ListSearchParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'ListSearchResponse';
      locSerializer.Get(TypeInfo(ListSearchResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.ListLookup(
  const  ListLookupParam : ListLookup_Type
):ListLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ListLookup', GetTarget(),locCallContext);
      locSerializer.Put('ListLookup', TypeInfo(ListLookup_Type), ListLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'ListLookupResponse';
      locSerializer.Get(TypeInfo(ListLookupResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CustomerContentSearch(
  const  CustomerContentSearchParam : CustomerContentSearch_Type
):CustomerContentSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CustomerContentSearch', GetTarget(),locCallContext);
      locSerializer.Put('CustomerContentSearch', TypeInfo(CustomerContentSearch_Type), CustomerContentSearchParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'CustomerContentSearchResponse';
      locSerializer.Get(TypeInfo(CustomerContentSearchResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CustomerContentLookup(
  const  CustomerContentLookupParam : CustomerContentLookup_Type
):CustomerContentLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CustomerContentLookup', GetTarget(),locCallContext);
      locSerializer.Put('CustomerContentLookup', TypeInfo(CustomerContentLookup_Type), CustomerContentLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'CustomerContentLookupResponse';
      locSerializer.Get(TypeInfo(CustomerContentLookupResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.SimilarityLookup(
  const  SimilarityLookupParam : SimilarityLookup_Type
):SimilarityLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SimilarityLookup', GetTarget(),locCallContext);
      locSerializer.Put('SimilarityLookup', TypeInfo(SimilarityLookup_Type), SimilarityLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'SimilarityLookupResponse';
      locSerializer.Get(TypeInfo(SimilarityLookupResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.SellerLookup(
  const  SellerLookupParam : SellerLookup_Type
):SellerLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SellerLookup', GetTarget(),locCallContext);
      locSerializer.Put('SellerLookup', TypeInfo(SellerLookup_Type), SellerLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'SellerLookupResponse';
      locSerializer.Get(TypeInfo(SellerLookupResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CartGet(
  const  CartGetParam : CartGet_Type
):CartGetResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartGet', GetTarget(),locCallContext);
      locSerializer.Put('CartGet', TypeInfo(CartGet_Type), CartGetParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'CartGetResponse';
      locSerializer.Get(TypeInfo(CartGetResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CartAdd(
  const  CartAddParam : CartAdd_Type
):CartAddResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartAdd', GetTarget(),locCallContext);
      locSerializer.Put('CartAdd', TypeInfo(CartAdd_Type), CartAddParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'CartAddResponse';
      locSerializer.Get(TypeInfo(CartAddResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CartCreate(
  const  CartCreateParam : CartCreate_Type
):CartCreateResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartCreate', GetTarget(),locCallContext);
      locSerializer.Put('CartCreate', TypeInfo(CartCreate_Type), CartCreateParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'CartCreateResponse';
      locSerializer.Get(TypeInfo(CartCreateResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CartModify(
  const  CartModifyParam : CartModify_Type
):CartModifyResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartModify', GetTarget(),locCallContext);
      locSerializer.Put('CartModify', TypeInfo(CartModify_Type), CartModifyParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'CartModifyResponse';
      locSerializer.Get(TypeInfo(CartModifyResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.CartClear(
  const  CartClearParam : CartClear_Type
):CartClearResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('CartClear', GetTarget(),locCallContext);
      locSerializer.Put('CartClear', TypeInfo(CartClear_Type), CartClearParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'CartClearResponse';
      locSerializer.Get(TypeInfo(CartClearResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.TransactionLookup(
  const  TransactionLookupParam : TransactionLookup_Type
):TransactionLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('TransactionLookup', GetTarget(),locCallContext);
      locSerializer.Put('TransactionLookup', TypeInfo(TransactionLookup_Type), TransactionLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'TransactionLookupResponse';
      locSerializer.Get(TypeInfo(TransactionLookupResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.SellerListingSearch(
  const  SellerListingSearchParam : SellerListingSearch_Type
):SellerListingSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SellerListingSearch', GetTarget(),locCallContext);
      locSerializer.Put('SellerListingSearch', TypeInfo(SellerListingSearch_Type), SellerListingSearchParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'SellerListingSearchResponse';
      locSerializer.Get(TypeInfo(SellerListingSearchResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.SellerListingLookup(
  const  SellerListingLookupParam : SellerListingLookup_Type
):SellerListingLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SellerListingLookup', GetTarget(),locCallContext);
      locSerializer.Put('SellerListingLookup', TypeInfo(SellerListingLookup_Type), SellerListingLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'SellerListingLookupResponse';
      locSerializer.Get(TypeInfo(SellerListingLookupResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.TagLookup(
  const  TagLookupParam : TagLookup_Type
):TagLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('TagLookup', GetTarget(),locCallContext);
      locSerializer.Put('TagLookup', TypeInfo(TagLookup_Type), TagLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'TagLookupResponse';
      locSerializer.Get(TypeInfo(TagLookupResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.VehicleSearch(
  const  VehicleSearchParam : VehicleSearch_Type
):VehicleSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('VehicleSearch', GetTarget(),locCallContext);
      locSerializer.Put('VehicleSearch', TypeInfo(VehicleSearch_Type), VehicleSearchParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'VehicleSearchResponse';
      locSerializer.Get(TypeInfo(VehicleSearchResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.VehiclePartSearch(
  const  VehiclePartSearchParam : VehiclePartSearch_Type
):VehiclePartSearchResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('VehiclePartSearch', GetTarget(),locCallContext);
      locSerializer.Put('VehiclePartSearch', TypeInfo(VehiclePartSearch_Type), VehiclePartSearchParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'VehiclePartSearchResponse';
      locSerializer.Get(TypeInfo(VehiclePartSearchResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.VehiclePartLookup(
  const  VehiclePartLookupParam : VehiclePartLookup_Type
):VehiclePartLookupResponse_Type;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('VehiclePartLookup', GetTarget(),locCallContext);
      locSerializer.Put('VehiclePartLookup', TypeInfo(VehiclePartLookup_Type), VehiclePartLookupParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'VehiclePartLookupResponse';
      locSerializer.Get(TypeInfo(VehiclePartLookupResponse_Type), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TAWSECommerceServicePortType_Proxy.MultiOperation(
  const  MultiOperationParam : MultiOperation_Type
):MultiOperationResponse;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('MultiOperation', GetTarget(),locCallContext);
      locSerializer.Put('MultiOperation', TypeInfo(MultiOperation_Type), MultiOperationParam);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'MultiOperationResponse';
      locSerializer.Get(TypeInfo(MultiOperationResponse), locStrPrmName, Result);

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
