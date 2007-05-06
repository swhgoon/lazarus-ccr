{
This unit has been produced by ws_helper.
  Input unit name : "AWSECommerceService".
  This unit name  : "AWSECommerceService".
  Date            : "6-5-07 19:37:08".
}
unit AWSECommerceService;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'http://webservices.amazon.com/AWSECommerceService/2007-04-04';
  sUNIT_NAME = 'AWSECommerceService';

type

  HelpRequest = class;
  Help_RequestArray = class;
  Help_Type = class;
  OperationRequest_Type = class;
  Information_Type = class;
  HelpResponse_InformationArray = class;
  HelpResponse_Type = class;
  ItemSearchRequest = class;
  ItemSearch_RequestArray = class;
  ItemSearch_Type = class;
  Items_Type = class;
  ItemSearchResponse_ItemsArray = class;
  ItemSearchResponse_Type = class;
  ItemLookupRequest = class;
  ItemLookup_RequestArray = class;
  ItemLookup_Type = class;
  ItemLookupResponse_ItemsArray = class;
  ItemLookupResponse_Type = class;
  BrowseNodeLookupRequest = class;
  BrowseNodeLookup_RequestArray = class;
  BrowseNodeLookup_Type = class;
  BrowseNodes_Type = class;
  BrowseNodeLookupResponse_BrowseNodesArray = class;
  BrowseNodeLookupResponse_Type = class;
  ListSearchRequest = class;
  ListSearch_RequestArray = class;
  ListSearch_Type = class;
  Lists_Type = class;
  ListSearchResponse_ListsArray = class;
  ListSearchResponse_Type = class;
  ListLookupRequest = class;
  ListLookup_RequestArray = class;
  ListLookup_Type = class;
  ListLookupResponse_ListsArray = class;
  ListLookupResponse_Type = class;
  CustomerContentSearchRequest = class;
  CustomerContentSearch_RequestArray = class;
  CustomerContentSearch_Type = class;
  Customers_Type = class;
  CustomerContentSearchResponse_CustomersArray = class;
  CustomerContentSearchResponse_Type = class;
  CustomerContentLookupRequest = class;
  CustomerContentLookup_RequestArray = class;
  CustomerContentLookup_Type = class;
  CustomerContentLookupResponse_CustomersArray = class;
  CustomerContentLookupResponse_Type = class;
  SimilarityLookupRequest = class;
  SimilarityLookup_RequestArray = class;
  SimilarityLookup_Type = class;
  SimilarityLookupResponse_ItemsArray = class;
  SimilarityLookupResponse_Type = class;
  SellerLookupRequest = class;
  SellerLookup_RequestArray = class;
  SellerLookup_Type = class;
  Sellers_Type = class;
  SellerLookupResponse_SellersArray = class;
  SellerLookupResponse_Type = class;
  CartGetRequest = class;
  CartGet_RequestArray = class;
  CartGet_Type = class;
  Cart_Type = class;
  CartGetResponse_CartArray = class;
  CartGetResponse_Type = class;
  CartAddRequest = class;
  CartAdd_RequestArray = class;
  CartAdd_Type = class;
  CartAddResponse_CartArray = class;
  CartAddResponse_Type = class;
  CartCreateRequest = class;
  CartCreate_RequestArray = class;
  CartCreate_Type = class;
  CartCreateResponse_CartArray = class;
  CartCreateResponse_Type = class;
  CartModifyRequest = class;
  CartModify_RequestArray = class;
  CartModify_Type = class;
  CartModifyResponse_CartArray = class;
  CartModifyResponse_Type = class;
  CartClearRequest = class;
  CartClear_RequestArray = class;
  CartClear_Type = class;
  CartClearResponse_CartArray = class;
  CartClearResponse_Type = class;
  TransactionLookupRequest = class;
  TransactionLookup_RequestArray = class;
  TransactionLookup_Type = class;
  Transactions_Type = class;
  TransactionLookupResponse_TransactionsArray = class;
  TransactionLookupResponse_Type = class;
  SellerListingSearchRequest = class;
  SellerListingSearch_RequestArray = class;
  SellerListingSearch_Type = class;
  SellerListings_Type = class;
  SellerListingSearchResponse_SellerListingsArray = class;
  SellerListingSearchResponse_Type = class;
  SellerListingLookupRequest = class;
  SellerListingLookup_RequestArray = class;
  SellerListingLookup_Type = class;
  SellerListingLookupResponse_SellerListingsArray = class;
  SellerListingLookupResponse_Type = class;
  MultiOperationType = class;
  MultiOperationResponse = class;
  Bin_BinParameter_Type = class;
  Bin_BinParameterArray = class;
  Bin_Type = class;
  SearchBinSet_BinArray = class;
  SearchBinSet_Type = class;
  SearchBinSets_Type = class;
  HelpRequest_ResponseGroupArray = class;
  ItemSearchRequest_AudienceRatingArray = class;
  ItemSearchRequest_ResponseGroupArray = class;
  ItemLookupRequest_ItemIdArray = class;
  ItemLookupRequest_ResponseGroupArray = class;
  ListSearchRequest_ResponseGroupArray = class;
  ListLookupRequest_ResponseGroupArray = class;
  CustomerContentSearchRequest_ResponseGroupArray = class;
  CustomerContentLookupRequest_ResponseGroupArray = class;
  SimilarityLookupRequest_ItemIdArray = class;
  SimilarityLookupRequest_ResponseGroupArray = class;
  SellerLookupRequest_ResponseGroupArray = class;
  SellerLookupRequest_SellerIdArray = class;
  CartGetRequest_ResponseGroupArray = class;
  CartAddRequest_Items_Type_Item_Type = class;
  CartAddRequest_Items_Type = class;
  CartAddRequest_ResponseGroupArray = class;
  CartCreateRequest_Items_Type_Item_Type = class;
  CartCreateRequest_Items_Type = class;
  CartCreateRequest_ResponseGroupArray = class;
  CartModifyRequest_Items_Type_Item_Type = class;
  CartModifyRequest_Items_Type = class;
  CartModifyRequest_ResponseGroupArray = class;
  CartClearRequest_ResponseGroupArray = class;
  TransactionLookupRequest_ResponseGroupArray = class;
  TransactionLookupRequest_TransactionIdArray = class;
  SellerListingSearchRequest_ResponseGroupArray = class;
  SellerListingLookupRequest_ResponseGroupArray = class;
  BrowseNodeLookupRequest_BrowseNodeIdArray = class;
  BrowseNodeLookupRequest_ResponseGroupArray = class;
  HTTPHeaders_Type = class;
  Arguments_Type = class;
  Errors_Type = class;
  Request_Type = class;
  Arguments_Argument_Type = class;
  HTTPHeaders_Header_Type = class;
  Errors_Error_Type = class;
  OperationInformation_Type = class;
  ResponseGroupInformation_Type = class;
  Information_OperationInformationArray = class;
  Information_ResponseGroupInformationArray = class;
  CorrectedQuery_Type = class;
  SearchResultsMap_Type = class;
  Item_Type = class;
  Items__ItemArray = class;
  List_Type = class;
  Lists_ListArray = class;
  Customer_Type = class;
  Customers_CustomerArray = class;
  Price = class;
  CartItems_Type = class;
  SavedForLaterItems_Type = class;
  SimilarProducts_Type = class;
  TopSellers_Type = class;
  NewReleases_Type = class;
  SimilarViewedProducts_Type = class;
  OtherCategoriesSimilarProducts_Type = class;
  Transaction_Type = class;
  Transactions_TransactionArray = class;
  Seller_Type = class;
  Sellers_SellerArray = class;
  SellerListing_Type = class;
  SellerListings_SellerListingArray = class;
  OperationInformation_RequiredParameters_Type = class;
  OperationInformation_AvailableParameters_Type = class;
  OperationInformation_DefaultResponseGroups_Type = class;
  OperationInformation_AvailableResponseGroups_Type = class;
  ResponseGroupInformation_ValidOperations_Type = class;
  ResponseGroupInformation_Elements_Type = class;
  Image = class;
  ListItem_Type = class;
  List_ListItemArray = class;
  Customer_Location_Type = class;
  CustomerReviews_Type = class;
  Customer_CustomerReviewsArray = class;
  SearchResultsMap_SearchIndex_Type_ASINArray = class;
  SearchResultsMap_SearchIndex_Type = class;
  ImageSet_Type = class;
  Item_ImageSets_Type_ImageSetArray = class;
  Item_ImageSets_Type = class;
  ItemAttributes_Type = class;
  MerchantItemAttributes_Type = class;
  Collections_Type = class;
  Item_Subjects_Type = class;
  OfferSummary_Type = class;
  Offers_Type = class;
  VariationSummary_Type = class;
  Variations_Type = class;
  EditorialReviews_Type = class;
  Accessories_Type = class;
  Tracks_Type = class;
  ListmaniaLists_Type = class;
  SearchInside_Type = class;
  Item_AlternateVersions_Type_AlternateVersion_Type = class;
  Item_AlternateVersions_Type = class;
  _Item_ImageSetsArray = class;
  Offer_Type = class;
  Offers_OfferArray = class;
  Merchant_Type = class;
  OfferAttributes_Type = class;
  OfferListing_Type = class;
  LoyaltyPoints_Type = class;
  Promotions_Type = class;
  Offer_OfferListingArray = class;
  OfferListing_AvailabilityAttributes_Type = class;
  Address = class;
  OfferListing_ShippingCharge_Type = class;
  OfferListing_ShippingChargeArray = class;
  VariationDimensions_Type = class;
  Variations__ItemArray = class;
  EditorialReview_Type = class;
  Collections_Collection_Type_CollectionSummary_Type = class;
  Collections_Collection_Type_CollectionParent_Type = class;
  Collections_Collection_Type_CollectionItem_Type = class;
  Collections_Collection_Type_CollectionItemArray = class;
  Collections_Collection_Type = class;
  Review_Type = class;
  CustomerReviews_ReviewArray = class;
  Reviewer_Type = class;
  Tracks_Disc_Type_Track_Type = class;
  Tracks_Disc_Type_TrackArray = class;
  Tracks_Disc_Type = class;
  SimilarProducts_SimilarProduct_Type = class;
  TopSellers_TopSeller_Type = class;
  NewReleases_NewRelease_Type = class;
  SimilarViewedProducts_SimilarViewedProduct_Type = class;
  OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type = class;
  Accessories_Accessory_Type = class;
  Promotion_Type = class;
  Promotion_Summary_Type = class;
  PromotionEligibilityRequirements = class;
  PromotionBenefits = class;
  PromotionItemApplicability = class;
  Promotion_Details_Type = class;
  PromotionEligibilityRequirement = class;
  PromotionBenefit = class;
  BrowseNode_Type = class;
  BrowseNodes_BrowseNodeArray = class;
  BrowseNode_Children_Type = class;
  BrowseNode_Ancestors_Type = class;
  ListmaniaLists_ListmaniaList_Type = class;
  SearchInside_Excerpt_Type = class;
  CartItem = class;
  CartItems_CartItemArray = class;
  SavedForLaterItems_SavedForLaterItemArray = class;
  Transaction_Totals_Type = class;
  TransactionItem_Type = class;
  Transaction_TransactionItems_Type = class;
  Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type = class;
  Transaction_Shipments_Type_Shipment_Type_Packages_Type_Package_Type = class;
  Transaction_Shipments_Type_Shipment_Type_Packages_Type = class;
  Transaction_Shipments_Type_Shipment_Type = class;
  Transaction_Shipments_Type = class;
  TransactionItem_ChildTransactionItems_Type = class;
  Seller_Location_Type = class;
  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type = class;
  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray = class;
  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type = class;
  Seller_SellerFeedbackSummary_Type = class;
  SellerFeedback_Type = class;
  SellerFeedback_Feedback_Type = class;
  DecimalWithUnits = class;
  NonNegativeIntegerWithUnits = class;
  ItemAttributes_Creator_Type = class;
  StringWithUnits = class;
  ItemAttributes_ItemDimensions_Type = class;
  ItemAttributes_Languages_Type_Language_Type = class;
  ItemAttributes_Languages_Type = class;
  ItemAttributes_PackageDimensions_Type = class;
  ItemAttributes_ActorArray = class;
  ItemAttributes_ArtistArray = class;
  ItemAttributes_AudioFormatArray = class;
  ItemAttributes_AuthorArray = class;
  ItemAttributes_CameraManualFeaturesArray = class;
  ItemAttributes_CompatibleDevicesArray = class;
  ItemAttributes_CreatorArray = class;
  ItemAttributes_DataLinkProtocolArray = class;
  ItemAttributes_DirectorArray = class;
  ItemAttributes_FeatureArray = class;
  ItemAttributes_FormatArray = class;
  ItemAttributes_FormFactorArray = class;
  ItemAttributes_PhotoFlashTypeArray = class;
  ItemAttributes_PictureFormatArray = class;
  ItemAttributes_PlatformArray = class;
  ItemAttributes_ReturnMethodArray = class;
  ItemAttributes_SpecialFeaturesArray = class;
  ItemAttributes_SupportedImageTypeArray = class;
  MerchantItemAttributes_Creator_Type = class;
  MerchantItemAttributes_ItemDimensions_Type = class;
  MerchantItemAttributes_Languages_Type_Language_Type = class;
  MerchantItemAttributes_Languages_Type = class;
  MerchantItemAttributes_PackageDimensions_Type = class;
  MerchantItemAttributes_VendorRebate_Type = class;
  MerchantItemAttributes_ActorArray = class;
  MerchantItemAttributes_ArtistArray = class;
  MerchantItemAttributes_AudioFormatArray = class;
  MerchantItemAttributes_AuthorArray = class;
  MerchantItemAttributes_CameraManualFeaturesArray = class;
  MerchantItemAttributes_CreatorArray = class;
  MerchantItemAttributes_DirectorArray = class;
  MerchantItemAttributes_FeatureArray = class;
  MerchantItemAttributes_FormatArray = class;
  MerchantItemAttributes_PhotoFlashTypeArray = class;
  MerchantItemAttributes_PictureFormatArray = class;
  MerchantItemAttributes_PlatformArray = class;
  MerchantItemAttributes_PurchasingChannelArray = class;
  MerchantItemAttributes_ReturnMethodArray = class;
  MerchantItemAttributes_SpecialFeaturesArray = class;
  MerchantItemAttributes_SupportedImageTypeArray = class;

  HelpRequest_HelpType_Type = ( 
    Operation
    ,ResponseGroup
  );

  ItemSearchRequest_Availability_Type = ( 
    Available
  );

  AudienceRating_Type = ( 
    G
    ,PG
    ,AudienceRatingPG_13
    ,R
    ,AudienceRatingNC_17
    ,NR
    ,Unrated
    ,AudienceRating_6
    ,AudienceRating_12
    ,AudienceRating_16
    ,AudienceRating_18
    ,FamilyViewing
  );

  Condition_Type = ( 
    All
    ,New
    ,Used
    ,Collectible
    ,Refurbished
  );

  DeliveryMethod_Type = ( 
    Ship
    ,ISPU
  );

  ItemLookupRequest_IdType_Type = ( 
    ASIN
    ,UPC
    ,SKU
    ,EAN
    ,ISBN
  );

  ListSearchRequest_ListType_Type = ( 
    WishList
    ,WeddingRegistry
    ,BabyRegistry
  );

  ListLookupRequest_ListType_Type = ( 
    ListLookupRequest_ListType_TypeWishList
    ,Listmania
    ,ListLookupRequest_ListType_TypeWeddingRegistry
  );

  SimilarityLookupRequest_SimilarityType_Type = ( 
    Intersection
    ,Random
  );

  CartModifyRequest_Items_Type_Item_Type_Action_Type = ( 
    MoveToCart
    ,SaveForLater
  );

  SellerListingSearchRequest_OfferStatus_Type = ( 
    Open
    ,Closed
  );

  SellerListingLookupRequest_IdType_Type = ( 
    Exchange
    ,Listing
    ,SellerListingLookupRequest_IdType_TypeASIN
    ,SellerListingLookupRequest_IdType_TypeSKU
  );

  List_ListType_Type = ( 
    List_ListType_TypeWishList
    ,List_ListType_TypeWeddingRegistry
    ,List_ListType_TypeBabyRegistry
    ,List_ListType_TypeListmania
  );

  positiveIntegerOrAll = type string;

  HelpRequest = class(TBaseComplexRemotable)
  private
    FAbout : string;
    FHelpType : HelpRequest_HelpType_Type;
    FResponseGroup : HelpRequest_ResponseGroupArray;
  private
    function HasAbout() : Boolean;
    function HasHelpType() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property About : string read FAbout write FAbout stored HasAbout;
    property HelpType : HelpRequest_HelpType_Type read FHelpType write FHelpType stored HasHelpType;
    property ResponseGroup : HelpRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
  end;

  Help_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FShared : HelpRequest;
    FRequest : Help_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property Shared : HelpRequest read FShared write FShared stored HasShared;
    property Request : Help_RequestArray read FRequest write FRequest;
  end;

  OperationRequest_Type = class(TBaseComplexRemotable)
  private
    FHTTPHeaders : HTTPHeaders_Type;
    FRequestId : string;
    FArguments : Arguments_Type;
    FErrors : Errors_Type;
    FRequestProcessingTime : float;
  private
    function HasHTTPHeaders() : Boolean;
    function HasRequestId() : Boolean;
    function HasArguments() : Boolean;
    function HasErrors() : Boolean;
    function HasRequestProcessingTime() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property HTTPHeaders : HTTPHeaders_Type read FHTTPHeaders write FHTTPHeaders stored HasHTTPHeaders;
    property RequestId : string read FRequestId write FRequestId stored HasRequestId;
    property Arguments : Arguments_Type read FArguments write FArguments stored HasArguments;
    property Errors : Errors_Type read FErrors write FErrors stored HasErrors;
    property RequestProcessingTime : float read FRequestProcessingTime write FRequestProcessingTime stored HasRequestProcessingTime;
  end;

  Information_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FOperationInformation : Information_OperationInformationArray;
    FResponseGroupInformation : Information_ResponseGroupInformationArray;
  private
    function HasRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property OperationInformation : Information_OperationInformationArray read FOperationInformation write FOperationInformation;
    property ResponseGroupInformation : Information_ResponseGroupInformationArray read FResponseGroupInformation write FResponseGroupInformation;
  end;

  HelpResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FInformation : HelpResponse_InformationArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Information : HelpResponse_InformationArray read FInformation write FInformation;
  end;

  ItemSearchRequest = class(TBaseComplexRemotable)
  private
    FActor : string;
    FArtist : string;
    FAvailability : ItemSearchRequest_Availability_Type;
    FAudienceRating : ItemSearchRequest_AudienceRatingArray;
    FAuthor : string;
    FBrand : string;
    FBrowseNode : string;
    FCity : string;
    FComposer : string;
    FCondition : Condition_Type;
    FConductor : string;
    FCount : positiveInteger;
    FCuisine : string;
    FDeliveryMethod : DeliveryMethod_Type;
    FDirector : string;
    FFutureLaunchDate : string;
    FISPUPostalCode : string;
    FItemPage : positiveInteger;
    FKeywords : string;
    FManufacturer : string;
    FMaximumPrice : nonNegativeInteger;
    FMerchantId : string;
    FMinimumPrice : nonNegativeInteger;
    FMusicLabel : string;
    FNeighborhood : string;
    FOrchestra : string;
    FPostalCode : string;
    FPower : string;
    FPublisher : string;
    FResponseGroup : ItemSearchRequest_ResponseGroupArray;
    FReviewSort : string;
    FSearchIndex : string;
    FSort : string;
    FState : string;
    FTextStream : string;
    FTitle : string;
    FReleaseDate : string;
  private
    function HasActor() : Boolean;
    function HasArtist() : Boolean;
    function HasAvailability() : Boolean;
    function HasAuthor() : Boolean;
    function HasBrand() : Boolean;
    function HasBrowseNode() : Boolean;
    function HasCity() : Boolean;
    function HasComposer() : Boolean;
    function HasCondition() : Boolean;
    function HasConductor() : Boolean;
    function HasCount() : Boolean;
    function HasCuisine() : Boolean;
    function HasDeliveryMethod() : Boolean;
    function HasDirector() : Boolean;
    function HasFutureLaunchDate() : Boolean;
    function HasISPUPostalCode() : Boolean;
    function HasItemPage() : Boolean;
    function HasKeywords() : Boolean;
    function HasManufacturer() : Boolean;
    function HasMaximumPrice() : Boolean;
    function HasMerchantId() : Boolean;
    function HasMinimumPrice() : Boolean;
    function HasMusicLabel() : Boolean;
    function HasNeighborhood() : Boolean;
    function HasOrchestra() : Boolean;
    function HasPostalCode() : Boolean;
    function HasPower() : Boolean;
    function HasPublisher() : Boolean;
    function HasReviewSort() : Boolean;
    function HasSearchIndex() : Boolean;
    function HasSort() : Boolean;
    function HasState() : Boolean;
    function HasTextStream() : Boolean;
    function HasTitle() : Boolean;
    function HasReleaseDate() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Actor : string read FActor write FActor stored HasActor;
    property Artist : string read FArtist write FArtist stored HasArtist;
    property Availability : ItemSearchRequest_Availability_Type read FAvailability write FAvailability stored HasAvailability;
    property AudienceRating : ItemSearchRequest_AudienceRatingArray read FAudienceRating write FAudienceRating;
    property Author : string read FAuthor write FAuthor stored HasAuthor;
    property Brand : string read FBrand write FBrand stored HasBrand;
    property BrowseNode : string read FBrowseNode write FBrowseNode stored HasBrowseNode;
    property City : string read FCity write FCity stored HasCity;
    property Composer : string read FComposer write FComposer stored HasComposer;
    property Condition : Condition_Type read FCondition write FCondition stored HasCondition;
    property Conductor : string read FConductor write FConductor stored HasConductor;
    property Count : positiveInteger read FCount write FCount stored HasCount;
    property Cuisine : string read FCuisine write FCuisine stored HasCuisine;
    property DeliveryMethod : DeliveryMethod_Type read FDeliveryMethod write FDeliveryMethod stored HasDeliveryMethod;
    property Director : string read FDirector write FDirector stored HasDirector;
    property FutureLaunchDate : string read FFutureLaunchDate write FFutureLaunchDate stored HasFutureLaunchDate;
    property ISPUPostalCode : string read FISPUPostalCode write FISPUPostalCode stored HasISPUPostalCode;
    property ItemPage : positiveInteger read FItemPage write FItemPage stored HasItemPage;
    property Keywords : string read FKeywords write FKeywords stored HasKeywords;
    property Manufacturer : string read FManufacturer write FManufacturer stored HasManufacturer;
    property MaximumPrice : nonNegativeInteger read FMaximumPrice write FMaximumPrice stored HasMaximumPrice;
    property MerchantId : string read FMerchantId write FMerchantId stored HasMerchantId;
    property MinimumPrice : nonNegativeInteger read FMinimumPrice write FMinimumPrice stored HasMinimumPrice;
    property MusicLabel : string read FMusicLabel write FMusicLabel stored HasMusicLabel;
    property Neighborhood : string read FNeighborhood write FNeighborhood stored HasNeighborhood;
    property Orchestra : string read FOrchestra write FOrchestra stored HasOrchestra;
    property PostalCode : string read FPostalCode write FPostalCode stored HasPostalCode;
    property Power : string read FPower write FPower stored HasPower;
    property Publisher : string read FPublisher write FPublisher stored HasPublisher;
    property ResponseGroup : ItemSearchRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
    property ReviewSort : string read FReviewSort write FReviewSort stored HasReviewSort;
    property SearchIndex : string read FSearchIndex write FSearchIndex stored HasSearchIndex;
    property Sort : string read FSort write FSort stored HasSort;
    property State : string read FState write FState stored HasState;
    property TextStream : string read FTextStream write FTextStream stored HasTextStream;
    property Title : string read FTitle write FTitle stored HasTitle;
    property ReleaseDate : string read FReleaseDate write FReleaseDate stored HasReleaseDate;
  end;

  ItemSearch_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FXMLEscaping : string;
    FValidate : string;
    FShared : ItemSearchRequest;
    FRequest : ItemSearch_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasValidate() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property Shared : ItemSearchRequest read FShared write FShared stored HasShared;
    property Request : ItemSearch_RequestArray read FRequest write FRequest;
  end;

  Items_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FCorrectedQuery : CorrectedQuery_Type;
    FQid : string;
    FTotalResults : nonNegativeInteger;
    FTotalPages : nonNegativeInteger;
    FSearchResultsMap : SearchResultsMap_Type;
    F_Item : Items__ItemArray;
    FSearchBinSets : SearchBinSets_Type;
  private
    function HasRequest() : Boolean;
    function HasCorrectedQuery() : Boolean;
    function HasQid() : Boolean;
    function HasTotalResults() : Boolean;
    function HasTotalPages() : Boolean;
    function HasSearchResultsMap() : Boolean;
    function HasSearchBinSets() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property CorrectedQuery : CorrectedQuery_Type read FCorrectedQuery write FCorrectedQuery stored HasCorrectedQuery;
    property Qid : string read FQid write FQid stored HasQid;
    property TotalResults : nonNegativeInteger read FTotalResults write FTotalResults stored HasTotalResults;
    property TotalPages : nonNegativeInteger read FTotalPages write FTotalPages stored HasTotalPages;
    property SearchResultsMap : SearchResultsMap_Type read FSearchResultsMap write FSearchResultsMap stored HasSearchResultsMap;
    property _Item : Items__ItemArray read F_Item write F_Item;
    property SearchBinSets : SearchBinSets_Type read FSearchBinSets write FSearchBinSets stored HasSearchBinSets;
  end;

  ItemSearchResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FItems : ItemSearchResponse_ItemsArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Items : ItemSearchResponse_ItemsArray read FItems write FItems;
  end;

  ItemLookupRequest = class(TBaseComplexRemotable)
  private
    FCondition : Condition_Type;
    FDeliveryMethod : DeliveryMethod_Type;
    FFutureLaunchDate : string;
    FIdType : ItemLookupRequest_IdType_Type;
    FISPUPostalCode : string;
    FMerchantId : string;
    FOfferPage : positiveInteger;
    FItemId : ItemLookupRequest_ItemIdArray;
    FResponseGroup : ItemLookupRequest_ResponseGroupArray;
    FReviewPage : positiveInteger;
    FReviewSort : string;
    FSearchIndex : string;
    FSearchInsideKeywords : string;
    FVariationPage : positiveIntegerOrAll;
  private
    function HasCondition() : Boolean;
    function HasDeliveryMethod() : Boolean;
    function HasFutureLaunchDate() : Boolean;
    function HasIdType() : Boolean;
    function HasISPUPostalCode() : Boolean;
    function HasMerchantId() : Boolean;
    function HasOfferPage() : Boolean;
    function HasReviewPage() : Boolean;
    function HasReviewSort() : Boolean;
    function HasSearchIndex() : Boolean;
    function HasSearchInsideKeywords() : Boolean;
    function HasVariationPage() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Condition : Condition_Type read FCondition write FCondition stored HasCondition;
    property DeliveryMethod : DeliveryMethod_Type read FDeliveryMethod write FDeliveryMethod stored HasDeliveryMethod;
    property FutureLaunchDate : string read FFutureLaunchDate write FFutureLaunchDate stored HasFutureLaunchDate;
    property IdType : ItemLookupRequest_IdType_Type read FIdType write FIdType stored HasIdType;
    property ISPUPostalCode : string read FISPUPostalCode write FISPUPostalCode stored HasISPUPostalCode;
    property MerchantId : string read FMerchantId write FMerchantId stored HasMerchantId;
    property OfferPage : positiveInteger read FOfferPage write FOfferPage stored HasOfferPage;
    property ItemId : ItemLookupRequest_ItemIdArray read FItemId write FItemId;
    property ResponseGroup : ItemLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
    property ReviewPage : positiveInteger read FReviewPage write FReviewPage stored HasReviewPage;
    property ReviewSort : string read FReviewSort write FReviewSort stored HasReviewSort;
    property SearchIndex : string read FSearchIndex write FSearchIndex stored HasSearchIndex;
    property SearchInsideKeywords : string read FSearchInsideKeywords write FSearchInsideKeywords stored HasSearchInsideKeywords;
    property VariationPage : positiveIntegerOrAll read FVariationPage write FVariationPage stored HasVariationPage;
  end;

  ItemLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : ItemLookupRequest;
    FRequest : ItemLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : ItemLookupRequest read FShared write FShared stored HasShared;
    property Request : ItemLookup_RequestArray read FRequest write FRequest;
  end;

  ItemLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FItems : ItemLookupResponse_ItemsArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Items : ItemLookupResponse_ItemsArray read FItems write FItems;
  end;

  BrowseNodeLookupRequest = class(TBaseComplexRemotable)
  private
    FBrowseNodeId : BrowseNodeLookupRequest_BrowseNodeIdArray;
    FResponseGroup : BrowseNodeLookupRequest_ResponseGroupArray;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property BrowseNodeId : BrowseNodeLookupRequest_BrowseNodeIdArray read FBrowseNodeId write FBrowseNodeId;
    property ResponseGroup : BrowseNodeLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
  end;

  BrowseNodeLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : BrowseNodeLookupRequest;
    FRequest : BrowseNodeLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : BrowseNodeLookupRequest read FShared write FShared stored HasShared;
    property Request : BrowseNodeLookup_RequestArray read FRequest write FRequest;
  end;

  BrowseNodes_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FBrowseNode : BrowseNodes_BrowseNodeArray;
  private
    function HasRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property BrowseNode : BrowseNodes_BrowseNodeArray read FBrowseNode write FBrowseNode;
  end;

  BrowseNodeLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FBrowseNodes : BrowseNodeLookupResponse_BrowseNodesArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property BrowseNodes : BrowseNodeLookupResponse_BrowseNodesArray read FBrowseNodes write FBrowseNodes;
  end;

  ListSearchRequest = class(TBaseComplexRemotable)
  private
    FCity : string;
    FEmail : string;
    FFirstName : string;
    FLastName : string;
    FListPage : positiveInteger;
    FListType : ListSearchRequest_ListType_Type;
    FName : string;
    FResponseGroup : ListSearchRequest_ResponseGroupArray;
    FState : string;
  private
    function HasCity() : Boolean;
    function HasEmail() : Boolean;
    function HasFirstName() : Boolean;
    function HasLastName() : Boolean;
    function HasListPage() : Boolean;
    function HasName() : Boolean;
    function HasState() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property City : string read FCity write FCity stored HasCity;
    property Email : string read FEmail write FEmail stored HasEmail;
    property FirstName : string read FFirstName write FFirstName stored HasFirstName;
    property LastName : string read FLastName write FLastName stored HasLastName;
    property ListPage : positiveInteger read FListPage write FListPage stored HasListPage;
    property ListType : ListSearchRequest_ListType_Type read FListType write FListType;
    property Name : string read FName write FName stored HasName;
    property ResponseGroup : ListSearchRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
    property State : string read FState write FState stored HasState;
  end;

  ListSearch_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : ListSearchRequest;
    FRequest : ListSearch_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : ListSearchRequest read FShared write FShared stored HasShared;
    property Request : ListSearch_RequestArray read FRequest write FRequest;
  end;

  Lists_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FTotalResults : nonNegativeInteger;
    FTotalPages : nonNegativeInteger;
    FList : Lists_ListArray;
  private
    function HasRequest() : Boolean;
    function HasTotalResults() : Boolean;
    function HasTotalPages() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property TotalResults : nonNegativeInteger read FTotalResults write FTotalResults stored HasTotalResults;
    property TotalPages : nonNegativeInteger read FTotalPages write FTotalPages stored HasTotalPages;
    property List : Lists_ListArray read FList write FList;
  end;

  ListSearchResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FLists : ListSearchResponse_ListsArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Lists : ListSearchResponse_ListsArray read FLists write FLists;
  end;

  ListLookupRequest = class(TBaseComplexRemotable)
  private
    FCondition : Condition_Type;
    FDeliveryMethod : DeliveryMethod_Type;
    FISPUPostalCode : string;
    FListId : string;
    FListType : ListLookupRequest_ListType_Type;
    FMerchantId : string;
    FProductGroup : string;
    FProductPage : positiveInteger;
    FResponseGroup : ListLookupRequest_ResponseGroupArray;
    FReviewSort : string;
    FSort : string;
  private
    function HasCondition() : Boolean;
    function HasDeliveryMethod() : Boolean;
    function HasISPUPostalCode() : Boolean;
    function HasListId() : Boolean;
    function HasListType() : Boolean;
    function HasMerchantId() : Boolean;
    function HasProductGroup() : Boolean;
    function HasProductPage() : Boolean;
    function HasReviewSort() : Boolean;
    function HasSort() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Condition : Condition_Type read FCondition write FCondition stored HasCondition;
    property DeliveryMethod : DeliveryMethod_Type read FDeliveryMethod write FDeliveryMethod stored HasDeliveryMethod;
    property ISPUPostalCode : string read FISPUPostalCode write FISPUPostalCode stored HasISPUPostalCode;
    property ListId : string read FListId write FListId stored HasListId;
    property ListType : ListLookupRequest_ListType_Type read FListType write FListType stored HasListType;
    property MerchantId : string read FMerchantId write FMerchantId stored HasMerchantId;
    property ProductGroup : string read FProductGroup write FProductGroup stored HasProductGroup;
    property ProductPage : positiveInteger read FProductPage write FProductPage stored HasProductPage;
    property ResponseGroup : ListLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
    property ReviewSort : string read FReviewSort write FReviewSort stored HasReviewSort;
    property Sort : string read FSort write FSort stored HasSort;
  end;

  ListLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : ListLookupRequest;
    FRequest : ListLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : ListLookupRequest read FShared write FShared stored HasShared;
    property Request : ListLookup_RequestArray read FRequest write FRequest;
  end;

  ListLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FLists : ListLookupResponse_ListsArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Lists : ListLookupResponse_ListsArray read FLists write FLists;
  end;

  CustomerContentSearchRequest = class(TBaseComplexRemotable)
  private
    FCustomerPage : positiveInteger;
    FEmail : string;
    FName : string;
    FResponseGroup : CustomerContentSearchRequest_ResponseGroupArray;
  private
    function HasCustomerPage() : Boolean;
    function HasEmail() : Boolean;
    function HasName() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CustomerPage : positiveInteger read FCustomerPage write FCustomerPage stored HasCustomerPage;
    property Email : string read FEmail write FEmail stored HasEmail;
    property Name : string read FName write FName stored HasName;
    property ResponseGroup : CustomerContentSearchRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
  end;

  CustomerContentSearch_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CustomerContentSearchRequest;
    FRequest : CustomerContentSearch_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : CustomerContentSearchRequest read FShared write FShared stored HasShared;
    property Request : CustomerContentSearch_RequestArray read FRequest write FRequest;
  end;

  Customers_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FTotalResults : nonNegativeInteger;
    FTotalPages : nonNegativeInteger;
    FCustomer : Customers_CustomerArray;
  private
    function HasRequest() : Boolean;
    function HasTotalResults() : Boolean;
    function HasTotalPages() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property TotalResults : nonNegativeInteger read FTotalResults write FTotalResults stored HasTotalResults;
    property TotalPages : nonNegativeInteger read FTotalPages write FTotalPages stored HasTotalPages;
    property Customer : Customers_CustomerArray read FCustomer write FCustomer;
  end;

  CustomerContentSearchResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCustomers : CustomerContentSearchResponse_CustomersArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Customers : CustomerContentSearchResponse_CustomersArray read FCustomers write FCustomers;
  end;

  CustomerContentLookupRequest = class(TBaseComplexRemotable)
  private
    FCustomerId : string;
    FResponseGroup : CustomerContentLookupRequest_ResponseGroupArray;
    FReviewPage : positiveInteger;
  private
    function HasCustomerId() : Boolean;
    function HasReviewPage() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CustomerId : string read FCustomerId write FCustomerId stored HasCustomerId;
    property ResponseGroup : CustomerContentLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
    property ReviewPage : positiveInteger read FReviewPage write FReviewPage stored HasReviewPage;
  end;

  CustomerContentLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CustomerContentLookupRequest;
    FRequest : CustomerContentLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : CustomerContentLookupRequest read FShared write FShared stored HasShared;
    property Request : CustomerContentLookup_RequestArray read FRequest write FRequest;
  end;

  CustomerContentLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCustomers : CustomerContentLookupResponse_CustomersArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Customers : CustomerContentLookupResponse_CustomersArray read FCustomers write FCustomers;
  end;

  SimilarityLookupRequest = class(TBaseComplexRemotable)
  private
    FCondition : Condition_Type;
    FDeliveryMethod : DeliveryMethod_Type;
    FItemId : SimilarityLookupRequest_ItemIdArray;
    FISPUPostalCode : string;
    FMerchantId : string;
    FResponseGroup : SimilarityLookupRequest_ResponseGroupArray;
    FReviewSort : string;
    FSimilarityType : SimilarityLookupRequest_SimilarityType_Type;
  private
    function HasCondition() : Boolean;
    function HasDeliveryMethod() : Boolean;
    function HasISPUPostalCode() : Boolean;
    function HasMerchantId() : Boolean;
    function HasReviewSort() : Boolean;
    function HasSimilarityType() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Condition : Condition_Type read FCondition write FCondition stored HasCondition;
    property DeliveryMethod : DeliveryMethod_Type read FDeliveryMethod write FDeliveryMethod stored HasDeliveryMethod;
    property ItemId : SimilarityLookupRequest_ItemIdArray read FItemId write FItemId;
    property ISPUPostalCode : string read FISPUPostalCode write FISPUPostalCode stored HasISPUPostalCode;
    property MerchantId : string read FMerchantId write FMerchantId stored HasMerchantId;
    property ResponseGroup : SimilarityLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
    property ReviewSort : string read FReviewSort write FReviewSort stored HasReviewSort;
    property SimilarityType : SimilarityLookupRequest_SimilarityType_Type read FSimilarityType write FSimilarityType stored HasSimilarityType;
  end;

  SimilarityLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : SimilarityLookupRequest;
    FRequest : SimilarityLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : SimilarityLookupRequest read FShared write FShared stored HasShared;
    property Request : SimilarityLookup_RequestArray read FRequest write FRequest;
  end;

  SimilarityLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FItems : SimilarityLookupResponse_ItemsArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Items : SimilarityLookupResponse_ItemsArray read FItems write FItems;
  end;

  SellerLookupRequest = class(TBaseComplexRemotable)
  private
    FResponseGroup : SellerLookupRequest_ResponseGroupArray;
    FSellerId : SellerLookupRequest_SellerIdArray;
    FFeedbackPage : positiveInteger;
  private
    function HasFeedbackPage() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property ResponseGroup : SellerLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
    property SellerId : SellerLookupRequest_SellerIdArray read FSellerId write FSellerId;
    property FeedbackPage : positiveInteger read FFeedbackPage write FFeedbackPage stored HasFeedbackPage;
  end;

  SellerLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : SellerLookupRequest;
    FRequest : SellerLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : SellerLookupRequest read FShared write FShared stored HasShared;
    property Request : SellerLookup_RequestArray read FRequest write FRequest;
  end;

  Sellers_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FTotalResults : nonNegativeInteger;
    FTotalPages : nonNegativeInteger;
    FSeller : Sellers_SellerArray;
  private
    function HasRequest() : Boolean;
    function HasTotalResults() : Boolean;
    function HasTotalPages() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property TotalResults : nonNegativeInteger read FTotalResults write FTotalResults stored HasTotalResults;
    property TotalPages : nonNegativeInteger read FTotalPages write FTotalPages stored HasTotalPages;
    property Seller : Sellers_SellerArray read FSeller write FSeller;
  end;

  SellerLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FSellers : SellerLookupResponse_SellersArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Sellers : SellerLookupResponse_SellersArray read FSellers write FSellers;
  end;

  CartGetRequest = class(TBaseComplexRemotable)
  private
    FCartId : string;
    FHMAC : string;
    FMergeCart : string;
    FResponseGroup : CartGetRequest_ResponseGroupArray;
  private
    function HasCartId() : Boolean;
    function HasHMAC() : Boolean;
    function HasMergeCart() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CartId : string read FCartId write FCartId stored HasCartId;
    property HMAC : string read FHMAC write FHMAC stored HasHMAC;
    property MergeCart : string read FMergeCart write FMergeCart stored HasMergeCart;
    property ResponseGroup : CartGetRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
  end;

  CartGet_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CartGetRequest;
    FRequest : CartGet_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : CartGetRequest read FShared write FShared stored HasShared;
    property Request : CartGet_RequestArray read FRequest write FRequest;
  end;

  Cart_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FCartId : string;
    FHMAC : string;
    FURLEncodedHMAC : string;
    FPurchaseURL : string;
    FSubTotal : Price;
    FCartItems : CartItems_Type;
    FSavedForLaterItems : SavedForLaterItems_Type;
    FSimilarProducts : SimilarProducts_Type;
    FTopSellers : TopSellers_Type;
    FNewReleases : NewReleases_Type;
    FSimilarViewedProducts : SimilarViewedProducts_Type;
    FOtherCategoriesSimilarProducts : OtherCategoriesSimilarProducts_Type;
  private
    function HasRequest() : Boolean;
    function HasPurchaseURL() : Boolean;
    function HasSubTotal() : Boolean;
    function HasCartItems() : Boolean;
    function HasSavedForLaterItems() : Boolean;
    function HasSimilarProducts() : Boolean;
    function HasTopSellers() : Boolean;
    function HasNewReleases() : Boolean;
    function HasSimilarViewedProducts() : Boolean;
    function HasOtherCategoriesSimilarProducts() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property CartId : string read FCartId write FCartId;
    property HMAC : string read FHMAC write FHMAC;
    property URLEncodedHMAC : string read FURLEncodedHMAC write FURLEncodedHMAC;
    property PurchaseURL : string read FPurchaseURL write FPurchaseURL stored HasPurchaseURL;
    property SubTotal : Price read FSubTotal write FSubTotal stored HasSubTotal;
    property CartItems : CartItems_Type read FCartItems write FCartItems stored HasCartItems;
    property SavedForLaterItems : SavedForLaterItems_Type read FSavedForLaterItems write FSavedForLaterItems stored HasSavedForLaterItems;
    property SimilarProducts : SimilarProducts_Type read FSimilarProducts write FSimilarProducts stored HasSimilarProducts;
    property TopSellers : TopSellers_Type read FTopSellers write FTopSellers stored HasTopSellers;
    property NewReleases : NewReleases_Type read FNewReleases write FNewReleases stored HasNewReleases;
    property SimilarViewedProducts : SimilarViewedProducts_Type read FSimilarViewedProducts write FSimilarViewedProducts stored HasSimilarViewedProducts;
    property OtherCategoriesSimilarProducts : OtherCategoriesSimilarProducts_Type read FOtherCategoriesSimilarProducts write FOtherCategoriesSimilarProducts stored HasOtherCategoriesSimilarProducts;
  end;

  CartGetResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCart : CartGetResponse_CartArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Cart : CartGetResponse_CartArray read FCart write FCart;
  end;

  CartAddRequest = class(TBaseComplexRemotable)
  private
    FCartId : string;
    FHMAC : string;
    FMergeCart : string;
    FItems : CartAddRequest_Items_Type;
    FResponseGroup : CartAddRequest_ResponseGroupArray;
  private
    function HasCartId() : Boolean;
    function HasHMAC() : Boolean;
    function HasMergeCart() : Boolean;
    function HasItems() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CartId : string read FCartId write FCartId stored HasCartId;
    property HMAC : string read FHMAC write FHMAC stored HasHMAC;
    property MergeCart : string read FMergeCart write FMergeCart stored HasMergeCart;
    property Items : CartAddRequest_Items_Type read FItems write FItems stored HasItems;
    property ResponseGroup : CartAddRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
  end;

  CartAdd_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CartAddRequest;
    FRequest : CartAdd_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : CartAddRequest read FShared write FShared stored HasShared;
    property Request : CartAdd_RequestArray read FRequest write FRequest;
  end;

  CartAddResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCart : CartAddResponse_CartArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Cart : CartAddResponse_CartArray read FCart write FCart;
  end;

  CartCreateRequest = class(TBaseComplexRemotable)
  private
    FMergeCart : string;
    FItems : CartCreateRequest_Items_Type;
    FResponseGroup : CartCreateRequest_ResponseGroupArray;
  private
    function HasMergeCart() : Boolean;
    function HasItems() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MergeCart : string read FMergeCart write FMergeCart stored HasMergeCart;
    property Items : CartCreateRequest_Items_Type read FItems write FItems stored HasItems;
    property ResponseGroup : CartCreateRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
  end;

  CartCreate_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CartCreateRequest;
    FRequest : CartCreate_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : CartCreateRequest read FShared write FShared stored HasShared;
    property Request : CartCreate_RequestArray read FRequest write FRequest;
  end;

  CartCreateResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCart : CartCreateResponse_CartArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Cart : CartCreateResponse_CartArray read FCart write FCart;
  end;

  CartModifyRequest = class(TBaseComplexRemotable)
  private
    FCartId : string;
    FHMAC : string;
    FMergeCart : string;
    FItems : CartModifyRequest_Items_Type;
    FResponseGroup : CartModifyRequest_ResponseGroupArray;
  private
    function HasCartId() : Boolean;
    function HasHMAC() : Boolean;
    function HasMergeCart() : Boolean;
    function HasItems() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CartId : string read FCartId write FCartId stored HasCartId;
    property HMAC : string read FHMAC write FHMAC stored HasHMAC;
    property MergeCart : string read FMergeCart write FMergeCart stored HasMergeCart;
    property Items : CartModifyRequest_Items_Type read FItems write FItems stored HasItems;
    property ResponseGroup : CartModifyRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
  end;

  CartModify_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CartModifyRequest;
    FRequest : CartModify_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : CartModifyRequest read FShared write FShared stored HasShared;
    property Request : CartModify_RequestArray read FRequest write FRequest;
  end;

  CartModifyResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCart : CartModifyResponse_CartArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Cart : CartModifyResponse_CartArray read FCart write FCart;
  end;

  CartClearRequest = class(TBaseComplexRemotable)
  private
    FCartId : string;
    FHMAC : string;
    FMergeCart : string;
    FResponseGroup : CartClearRequest_ResponseGroupArray;
  private
    function HasCartId() : Boolean;
    function HasHMAC() : Boolean;
    function HasMergeCart() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CartId : string read FCartId write FCartId stored HasCartId;
    property HMAC : string read FHMAC write FHMAC stored HasHMAC;
    property MergeCart : string read FMergeCart write FMergeCart stored HasMergeCart;
    property ResponseGroup : CartClearRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
  end;

  CartClear_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CartClearRequest;
    FRequest : CartClear_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : CartClearRequest read FShared write FShared stored HasShared;
    property Request : CartClear_RequestArray read FRequest write FRequest;
  end;

  CartClearResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCart : CartClearResponse_CartArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Cart : CartClearResponse_CartArray read FCart write FCart;
  end;

  TransactionLookupRequest = class(TBaseComplexRemotable)
  private
    FResponseGroup : TransactionLookupRequest_ResponseGroupArray;
    FTransactionId : TransactionLookupRequest_TransactionIdArray;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property ResponseGroup : TransactionLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
    property TransactionId : TransactionLookupRequest_TransactionIdArray read FTransactionId write FTransactionId;
  end;

  TransactionLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : TransactionLookupRequest;
    FRequest : TransactionLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : TransactionLookupRequest read FShared write FShared stored HasShared;
    property Request : TransactionLookup_RequestArray read FRequest write FRequest;
  end;

  Transactions_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FTotalResults : nonNegativeInteger;
    FTotalPages : nonNegativeInteger;
    FTransaction : Transactions_TransactionArray;
  private
    function HasRequest() : Boolean;
    function HasTotalResults() : Boolean;
    function HasTotalPages() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property TotalResults : nonNegativeInteger read FTotalResults write FTotalResults stored HasTotalResults;
    property TotalPages : nonNegativeInteger read FTotalPages write FTotalPages stored HasTotalPages;
    property Transaction : Transactions_TransactionArray read FTransaction write FTransaction;
  end;

  TransactionLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FTransactions : TransactionLookupResponse_TransactionsArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Transactions : TransactionLookupResponse_TransactionsArray read FTransactions write FTransactions;
  end;

  SellerListingSearchRequest = class(TBaseComplexRemotable)
  private
    FKeywords : string;
    FListingPage : positiveInteger;
    FOfferStatus : SellerListingSearchRequest_OfferStatus_Type;
    FResponseGroup : SellerListingSearchRequest_ResponseGroupArray;
    FSellerId : string;
    FSort : string;
    FTitle : string;
  private
    function HasKeywords() : Boolean;
    function HasListingPage() : Boolean;
    function HasOfferStatus() : Boolean;
    function HasSort() : Boolean;
    function HasTitle() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Keywords : string read FKeywords write FKeywords stored HasKeywords;
    property ListingPage : positiveInteger read FListingPage write FListingPage stored HasListingPage;
    property OfferStatus : SellerListingSearchRequest_OfferStatus_Type read FOfferStatus write FOfferStatus stored HasOfferStatus;
    property ResponseGroup : SellerListingSearchRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
    property SellerId : string read FSellerId write FSellerId;
    property Sort : string read FSort write FSort stored HasSort;
    property Title : string read FTitle write FTitle stored HasTitle;
  end;

  SellerListingSearch_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : SellerListingSearchRequest;
    FRequest : SellerListingSearch_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : SellerListingSearchRequest read FShared write FShared stored HasShared;
    property Request : SellerListingSearch_RequestArray read FRequest write FRequest;
  end;

  SellerListings_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FTotalResults : nonNegativeInteger;
    FTotalPages : nonNegativeInteger;
    FSellerListing : SellerListings_SellerListingArray;
  private
    function HasRequest() : Boolean;
    function HasTotalResults() : Boolean;
    function HasTotalPages() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property TotalResults : nonNegativeInteger read FTotalResults write FTotalResults stored HasTotalResults;
    property TotalPages : nonNegativeInteger read FTotalPages write FTotalPages stored HasTotalPages;
    property SellerListing : SellerListings_SellerListingArray read FSellerListing write FSellerListing;
  end;

  SellerListingSearchResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FSellerListings : SellerListingSearchResponse_SellerListingsArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property SellerListings : SellerListingSearchResponse_SellerListingsArray read FSellerListings write FSellerListings;
  end;

  SellerListingLookupRequest = class(TBaseComplexRemotable)
  private
    FId : string;
    FSellerId : string;
    FIdType : SellerListingLookupRequest_IdType_Type;
    FResponseGroup : SellerListingLookupRequest_ResponseGroupArray;
  private
    function HasSellerId() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Id : string read FId write FId;
    property SellerId : string read FSellerId write FSellerId stored HasSellerId;
    property IdType : SellerListingLookupRequest_IdType_Type read FIdType write FIdType;
    property ResponseGroup : SellerListingLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup;
  end;

  SellerListingLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : SellerListingLookupRequest;
    FRequest : SellerListingLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property XMLEscaping : string read FXMLEscaping write FXMLEscaping stored HasXMLEscaping;
    property Shared : SellerListingLookupRequest read FShared write FShared stored HasShared;
    property Request : SellerListingLookup_RequestArray read FRequest write FRequest;
  end;

  SellerListingLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FSellerListings : SellerListingLookupResponse_SellerListingsArray;
  private
    function HasOperationRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property SellerListings : SellerListingLookupResponse_SellerListingsArray read FSellerListings write FSellerListings;
  end;

  MultiOperationType = class(TBaseComplexRemotable)
  private
    FHelp : Help_Type;
    FItemSearch : ItemSearch_Type;
    FItemLookup : ItemLookup_Type;
    FListSearch : ListSearch_Type;
    FListLookup : ListLookup_Type;
    FCustomerContentSearch : CustomerContentSearch_Type;
    FCustomerContentLookup : CustomerContentLookup_Type;
    FSimilarityLookup : SimilarityLookup_Type;
    FSellerLookup : SellerLookup_Type;
    FCartGet : CartGet_Type;
    FCartAdd : CartAdd_Type;
    FCartCreate : CartCreate_Type;
    FCartModify : CartModify_Type;
    FCartClear : CartClear_Type;
    FTransactionLookup : TransactionLookup_Type;
    FSellerListingSearch : SellerListingSearch_Type;
    FSellerListingLookup : SellerListingLookup_Type;
    FBrowseNodeLookup : BrowseNodeLookup_Type;
  private
    function HasHelp() : Boolean;
    function HasItemSearch() : Boolean;
    function HasItemLookup() : Boolean;
    function HasListSearch() : Boolean;
    function HasListLookup() : Boolean;
    function HasCustomerContentSearch() : Boolean;
    function HasCustomerContentLookup() : Boolean;
    function HasSimilarityLookup() : Boolean;
    function HasSellerLookup() : Boolean;
    function HasCartGet() : Boolean;
    function HasCartAdd() : Boolean;
    function HasCartCreate() : Boolean;
    function HasCartModify() : Boolean;
    function HasCartClear() : Boolean;
    function HasTransactionLookup() : Boolean;
    function HasSellerListingSearch() : Boolean;
    function HasSellerListingLookup() : Boolean;
    function HasBrowseNodeLookup() : Boolean;
  public
    destructor Destroy();override;
  published
    property Help : Help_Type read FHelp write FHelp stored HasHelp;
    property ItemSearch : ItemSearch_Type read FItemSearch write FItemSearch stored HasItemSearch;
    property ItemLookup : ItemLookup_Type read FItemLookup write FItemLookup stored HasItemLookup;
    property ListSearch : ListSearch_Type read FListSearch write FListSearch stored HasListSearch;
    property ListLookup : ListLookup_Type read FListLookup write FListLookup stored HasListLookup;
    property CustomerContentSearch : CustomerContentSearch_Type read FCustomerContentSearch write FCustomerContentSearch stored HasCustomerContentSearch;
    property CustomerContentLookup : CustomerContentLookup_Type read FCustomerContentLookup write FCustomerContentLookup stored HasCustomerContentLookup;
    property SimilarityLookup : SimilarityLookup_Type read FSimilarityLookup write FSimilarityLookup stored HasSimilarityLookup;
    property SellerLookup : SellerLookup_Type read FSellerLookup write FSellerLookup stored HasSellerLookup;
    property CartGet : CartGet_Type read FCartGet write FCartGet stored HasCartGet;
    property CartAdd : CartAdd_Type read FCartAdd write FCartAdd stored HasCartAdd;
    property CartCreate : CartCreate_Type read FCartCreate write FCartCreate stored HasCartCreate;
    property CartModify : CartModify_Type read FCartModify write FCartModify stored HasCartModify;
    property CartClear : CartClear_Type read FCartClear write FCartClear stored HasCartClear;
    property TransactionLookup : TransactionLookup_Type read FTransactionLookup write FTransactionLookup stored HasTransactionLookup;
    property SellerListingSearch : SellerListingSearch_Type read FSellerListingSearch write FSellerListingSearch stored HasSellerListingSearch;
    property SellerListingLookup : SellerListingLookup_Type read FSellerListingLookup write FSellerListingLookup stored HasSellerListingLookup;
    property BrowseNodeLookup : BrowseNodeLookup_Type read FBrowseNodeLookup write FBrowseNodeLookup stored HasBrowseNodeLookup;
  end;

  MultiOperationResponse = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FHelpResponse : HelpResponse_Type;
    FItemSearchResponse : ItemSearchResponse_Type;
    FItemLookupResponse : ItemLookupResponse_Type;
    FListSearchResponse : ListSearchResponse_Type;
    FListLookupResponse : ListLookupResponse_Type;
    FCustomerContentSearchResponse : CustomerContentSearchResponse_Type;
    FCustomerContentLookupResponse : CustomerContentLookupResponse_Type;
    FSimilarityLookupResponse : SimilarityLookupResponse_Type;
    FSellerLookupResponse : SellerLookupResponse_Type;
    FCartGetResponse : CartGetResponse_Type;
    FCartAddResponse : CartAddResponse_Type;
    FCartCreateResponse : CartCreateResponse_Type;
    FCartModifyResponse : CartModifyResponse_Type;
    FCartClearResponse : CartClearResponse_Type;
    FTransactionLookupResponse : TransactionLookupResponse_Type;
    FSellerListingSearchResponse : SellerListingSearchResponse_Type;
    FSellerListingLookupResponse : SellerListingLookupResponse_Type;
    FBrowseNodeLookupResponse : BrowseNodeLookupResponse_Type;
  private
    function HasOperationRequest() : Boolean;
    function HasHelpResponse() : Boolean;
    function HasItemSearchResponse() : Boolean;
    function HasItemLookupResponse() : Boolean;
    function HasListSearchResponse() : Boolean;
    function HasListLookupResponse() : Boolean;
    function HasCustomerContentSearchResponse() : Boolean;
    function HasCustomerContentLookupResponse() : Boolean;
    function HasSimilarityLookupResponse() : Boolean;
    function HasSellerLookupResponse() : Boolean;
    function HasCartGetResponse() : Boolean;
    function HasCartAddResponse() : Boolean;
    function HasCartCreateResponse() : Boolean;
    function HasCartModifyResponse() : Boolean;
    function HasCartClearResponse() : Boolean;
    function HasTransactionLookupResponse() : Boolean;
    function HasSellerListingSearchResponse() : Boolean;
    function HasSellerListingLookupResponse() : Boolean;
    function HasBrowseNodeLookupResponse() : Boolean;
  public
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property HelpResponse : HelpResponse_Type read FHelpResponse write FHelpResponse stored HasHelpResponse;
    property ItemSearchResponse : ItemSearchResponse_Type read FItemSearchResponse write FItemSearchResponse stored HasItemSearchResponse;
    property ItemLookupResponse : ItemLookupResponse_Type read FItemLookupResponse write FItemLookupResponse stored HasItemLookupResponse;
    property ListSearchResponse : ListSearchResponse_Type read FListSearchResponse write FListSearchResponse stored HasListSearchResponse;
    property ListLookupResponse : ListLookupResponse_Type read FListLookupResponse write FListLookupResponse stored HasListLookupResponse;
    property CustomerContentSearchResponse : CustomerContentSearchResponse_Type read FCustomerContentSearchResponse write FCustomerContentSearchResponse stored HasCustomerContentSearchResponse;
    property CustomerContentLookupResponse : CustomerContentLookupResponse_Type read FCustomerContentLookupResponse write FCustomerContentLookupResponse stored HasCustomerContentLookupResponse;
    property SimilarityLookupResponse : SimilarityLookupResponse_Type read FSimilarityLookupResponse write FSimilarityLookupResponse stored HasSimilarityLookupResponse;
    property SellerLookupResponse : SellerLookupResponse_Type read FSellerLookupResponse write FSellerLookupResponse stored HasSellerLookupResponse;
    property CartGetResponse : CartGetResponse_Type read FCartGetResponse write FCartGetResponse stored HasCartGetResponse;
    property CartAddResponse : CartAddResponse_Type read FCartAddResponse write FCartAddResponse stored HasCartAddResponse;
    property CartCreateResponse : CartCreateResponse_Type read FCartCreateResponse write FCartCreateResponse stored HasCartCreateResponse;
    property CartModifyResponse : CartModifyResponse_Type read FCartModifyResponse write FCartModifyResponse stored HasCartModifyResponse;
    property CartClearResponse : CartClearResponse_Type read FCartClearResponse write FCartClearResponse stored HasCartClearResponse;
    property TransactionLookupResponse : TransactionLookupResponse_Type read FTransactionLookupResponse write FTransactionLookupResponse stored HasTransactionLookupResponse;
    property SellerListingSearchResponse : SellerListingSearchResponse_Type read FSellerListingSearchResponse write FSellerListingSearchResponse stored HasSellerListingSearchResponse;
    property SellerListingLookupResponse : SellerListingLookupResponse_Type read FSellerListingLookupResponse write FSellerListingLookupResponse stored HasSellerListingLookupResponse;
    property BrowseNodeLookupResponse : BrowseNodeLookupResponse_Type read FBrowseNodeLookupResponse write FBrowseNodeLookupResponse stored HasBrowseNodeLookupResponse;
  end;

  Bin_BinParameter_Type = class(TBaseComplexRemotable)
  private
    FName : string;
    FValue : string;
  published
    property Name : string read FName write FName;
    property Value : string read FValue write FValue;
  end;

  Bin_Type = class(TBaseComplexRemotable)
  private
    FBinName : string;
    FBinItemCount : positiveInteger;
    FBinParameter : Bin_BinParameterArray;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property BinName : string read FBinName write FBinName;
    property BinItemCount : positiveInteger read FBinItemCount write FBinItemCount;
    property BinParameter : Bin_BinParameterArray read FBinParameter write FBinParameter;
  end;

  SearchBinSet_Type = class(TBaseComplexRemotable)
  private
    FBin : SearchBinSet_BinArray;
    FNarrowBy : string;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Bin : SearchBinSet_BinArray read FBin write FBin;
    property NarrowBy : string read FNarrowBy write FNarrowBy;
  end;

  CartAddRequest_Items_Type_Item_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FOfferListingId : string;
    FQuantity : positiveInteger;
    FAssociateTag : string;
    FListItemId : string;
  private
    function HasASIN() : Boolean;
    function HasOfferListingId() : Boolean;
    function HasQuantity() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasListItemId() : Boolean;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property OfferListingId : string read FOfferListingId write FOfferListingId stored HasOfferListingId;
    property Quantity : positiveInteger read FQuantity write FQuantity stored HasQuantity;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property ListItemId : string read FListItemId write FListItemId stored HasListItemId;
  end;

  CartCreateRequest_Items_Type_Item_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FOfferListingId : string;
    FQuantity : positiveInteger;
    FAssociateTag : string;
    FListItemId : string;
  private
    function HasASIN() : Boolean;
    function HasOfferListingId() : Boolean;
    function HasQuantity() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasListItemId() : Boolean;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property OfferListingId : string read FOfferListingId write FOfferListingId stored HasOfferListingId;
    property Quantity : positiveInteger read FQuantity write FQuantity stored HasQuantity;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property ListItemId : string read FListItemId write FListItemId stored HasListItemId;
  end;

  CartModifyRequest_Items_Type_Item_Type = class(TBaseComplexRemotable)
  private
    FAction : CartModifyRequest_Items_Type_Item_Type_Action_Type;
    FCartItemId : string;
    FQuantity : nonNegativeInteger;
  private
    function HasAction() : Boolean;
    function HasCartItemId() : Boolean;
    function HasQuantity() : Boolean;
  published
    property Action : CartModifyRequest_Items_Type_Item_Type_Action_Type read FAction write FAction stored HasAction;
    property CartItemId : string read FCartItemId write FCartItemId stored HasCartItemId;
    property Quantity : nonNegativeInteger read FQuantity write FQuantity stored HasQuantity;
  end;

  Request_Type = class(TBaseComplexRemotable)
  private
    FIsValid : string;
    FHelpRequest : HelpRequest;
    FBrowseNodeLookupRequest : BrowseNodeLookupRequest;
    FItemSearchRequest : ItemSearchRequest;
    FItemLookupRequest : ItemLookupRequest;
    FListSearchRequest : ListSearchRequest;
    FListLookupRequest : ListLookupRequest;
    FCustomerContentSearchRequest : CustomerContentSearchRequest;
    FCustomerContentLookupRequest : CustomerContentLookupRequest;
    FSimilarityLookupRequest : SimilarityLookupRequest;
    FCartGetRequest : CartGetRequest;
    FCartAddRequest : CartAddRequest;
    FCartCreateRequest : CartCreateRequest;
    FCartModifyRequest : CartModifyRequest;
    FCartClearRequest : CartClearRequest;
    FTransactionLookupRequest : TransactionLookupRequest;
    FSellerListingSearchRequest : SellerListingSearchRequest;
    FSellerListingLookupRequest : SellerListingLookupRequest;
    FSellerLookupRequest : SellerLookupRequest;
    FErrors : Errors_Type;
  private
    function HasIsValid() : Boolean;
    function HasHelpRequest() : Boolean;
    function HasBrowseNodeLookupRequest() : Boolean;
    function HasItemSearchRequest() : Boolean;
    function HasItemLookupRequest() : Boolean;
    function HasListSearchRequest() : Boolean;
    function HasListLookupRequest() : Boolean;
    function HasCustomerContentSearchRequest() : Boolean;
    function HasCustomerContentLookupRequest() : Boolean;
    function HasSimilarityLookupRequest() : Boolean;
    function HasCartGetRequest() : Boolean;
    function HasCartAddRequest() : Boolean;
    function HasCartCreateRequest() : Boolean;
    function HasCartModifyRequest() : Boolean;
    function HasCartClearRequest() : Boolean;
    function HasTransactionLookupRequest() : Boolean;
    function HasSellerListingSearchRequest() : Boolean;
    function HasSellerListingLookupRequest() : Boolean;
    function HasSellerLookupRequest() : Boolean;
    function HasErrors() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property IsValid : string read FIsValid write FIsValid stored HasIsValid;
    property HelpRequest : HelpRequest read FHelpRequest write FHelpRequest stored HasHelpRequest;
    property BrowseNodeLookupRequest : BrowseNodeLookupRequest read FBrowseNodeLookupRequest write FBrowseNodeLookupRequest stored HasBrowseNodeLookupRequest;
    property ItemSearchRequest : ItemSearchRequest read FItemSearchRequest write FItemSearchRequest stored HasItemSearchRequest;
    property ItemLookupRequest : ItemLookupRequest read FItemLookupRequest write FItemLookupRequest stored HasItemLookupRequest;
    property ListSearchRequest : ListSearchRequest read FListSearchRequest write FListSearchRequest stored HasListSearchRequest;
    property ListLookupRequest : ListLookupRequest read FListLookupRequest write FListLookupRequest stored HasListLookupRequest;
    property CustomerContentSearchRequest : CustomerContentSearchRequest read FCustomerContentSearchRequest write FCustomerContentSearchRequest stored HasCustomerContentSearchRequest;
    property CustomerContentLookupRequest : CustomerContentLookupRequest read FCustomerContentLookupRequest write FCustomerContentLookupRequest stored HasCustomerContentLookupRequest;
    property SimilarityLookupRequest : SimilarityLookupRequest read FSimilarityLookupRequest write FSimilarityLookupRequest stored HasSimilarityLookupRequest;
    property CartGetRequest : CartGetRequest read FCartGetRequest write FCartGetRequest stored HasCartGetRequest;
    property CartAddRequest : CartAddRequest read FCartAddRequest write FCartAddRequest stored HasCartAddRequest;
    property CartCreateRequest : CartCreateRequest read FCartCreateRequest write FCartCreateRequest stored HasCartCreateRequest;
    property CartModifyRequest : CartModifyRequest read FCartModifyRequest write FCartModifyRequest stored HasCartModifyRequest;
    property CartClearRequest : CartClearRequest read FCartClearRequest write FCartClearRequest stored HasCartClearRequest;
    property TransactionLookupRequest : TransactionLookupRequest read FTransactionLookupRequest write FTransactionLookupRequest stored HasTransactionLookupRequest;
    property SellerListingSearchRequest : SellerListingSearchRequest read FSellerListingSearchRequest write FSellerListingSearchRequest stored HasSellerListingSearchRequest;
    property SellerListingLookupRequest : SellerListingLookupRequest read FSellerListingLookupRequest write FSellerListingLookupRequest stored HasSellerListingLookupRequest;
    property SellerLookupRequest : SellerLookupRequest read FSellerLookupRequest write FSellerLookupRequest stored HasSellerLookupRequest;
    property Errors : Errors_Type read FErrors write FErrors stored HasErrors;
  end;

  Arguments_Argument_Type = class(TBaseComplexRemotable)
  private
    FName : string;
    FValue : string;
  published
    property Name : string read FName write FName;
    property Value : string read FValue write FValue;
  end;

  HTTPHeaders_Header_Type = class(TBaseComplexRemotable)
  private
    FName : string;
    FValue : string;
  published
    property Name : string read FName write FName;
    property Value : string read FValue write FValue;
  end;

  Errors_Error_Type = class(TBaseComplexRemotable)
  private
    FCode : string;
    FMessage : string;
  published
    property Code : string read FCode write FCode;
    property Message : string read FMessage write FMessage;
  end;

  OperationInformation_Type = class(TBaseComplexRemotable)
  private
    FName : string;
    FDescription : string;
    FRequiredParameters : OperationInformation_RequiredParameters_Type;
    FAvailableParameters : OperationInformation_AvailableParameters_Type;
    FDefaultResponseGroups : OperationInformation_DefaultResponseGroups_Type;
    FAvailableResponseGroups : OperationInformation_AvailableResponseGroups_Type;
  private
    function HasName() : Boolean;
    function HasDescription() : Boolean;
    function HasRequiredParameters() : Boolean;
    function HasAvailableParameters() : Boolean;
    function HasDefaultResponseGroups() : Boolean;
    function HasAvailableResponseGroups() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Name : string read FName write FName stored HasName;
    property Description : string read FDescription write FDescription stored HasDescription;
    property RequiredParameters : OperationInformation_RequiredParameters_Type read FRequiredParameters write FRequiredParameters stored HasRequiredParameters;
    property AvailableParameters : OperationInformation_AvailableParameters_Type read FAvailableParameters write FAvailableParameters stored HasAvailableParameters;
    property DefaultResponseGroups : OperationInformation_DefaultResponseGroups_Type read FDefaultResponseGroups write FDefaultResponseGroups stored HasDefaultResponseGroups;
    property AvailableResponseGroups : OperationInformation_AvailableResponseGroups_Type read FAvailableResponseGroups write FAvailableResponseGroups stored HasAvailableResponseGroups;
  end;

  ResponseGroupInformation_Type = class(TBaseComplexRemotable)
  private
    FName : string;
    FCreationDate : string;
    FValidOperations : ResponseGroupInformation_ValidOperations_Type;
    FElements : ResponseGroupInformation_Elements_Type;
  private
    function HasName() : Boolean;
    function HasCreationDate() : Boolean;
    function HasValidOperations() : Boolean;
    function HasElements() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Name : string read FName write FName stored HasName;
    property CreationDate : string read FCreationDate write FCreationDate stored HasCreationDate;
    property ValidOperations : ResponseGroupInformation_ValidOperations_Type read FValidOperations write FValidOperations stored HasValidOperations;
    property Elements : ResponseGroupInformation_Elements_Type read FElements write FElements stored HasElements;
  end;

  CorrectedQuery_Type = class(TBaseComplexRemotable)
  private
    FKeywords : string;
    FMessage : string;
  private
    function HasKeywords() : Boolean;
    function HasMessage() : Boolean;
  published
    property Keywords : string read FKeywords write FKeywords stored HasKeywords;
    property Message : string read FMessage write FMessage stored HasMessage;
  end;

  Item_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FParentASIN : string;
    FErrors : Errors_Type;
    FDetailPageURL : string;
    FSalesRank : string;
    FSmallImage : Image;
    FMediumImage : Image;
    FLargeImage : Image;
    FImageSets : _Item_ImageSetsArray;
    FItemAttributes : ItemAttributes_Type;
    FMerchantItemAttributes : MerchantItemAttributes_Type;
    FCollections : Collections_Type;
    FSubjects : Item_Subjects_Type;
    FOfferSummary : OfferSummary_Type;
    FOffers : Offers_Type;
    FVariationSummary : VariationSummary_Type;
    FVariations : Variations_Type;
    FCustomerReviews : CustomerReviews_Type;
    FEditorialReviews : EditorialReviews_Type;
    FSimilarProducts : SimilarProducts_Type;
    FAccessories : Accessories_Type;
    FTracks : Tracks_Type;
    FBrowseNodes : BrowseNodes_Type;
    FListmaniaLists : ListmaniaLists_Type;
    FSearchInside : SearchInside_Type;
    FAlternateVersions : Item_AlternateVersions_Type;
  private
    function HasParentASIN() : Boolean;
    function HasErrors() : Boolean;
    function HasDetailPageURL() : Boolean;
    function HasSalesRank() : Boolean;
    function HasSmallImage() : Boolean;
    function HasMediumImage() : Boolean;
    function HasLargeImage() : Boolean;
    function HasItemAttributes() : Boolean;
    function HasMerchantItemAttributes() : Boolean;
    function HasCollections() : Boolean;
    function HasSubjects() : Boolean;
    function HasOfferSummary() : Boolean;
    function HasOffers() : Boolean;
    function HasVariationSummary() : Boolean;
    function HasVariations() : Boolean;
    function HasCustomerReviews() : Boolean;
    function HasEditorialReviews() : Boolean;
    function HasSimilarProducts() : Boolean;
    function HasAccessories() : Boolean;
    function HasTracks() : Boolean;
    function HasBrowseNodes() : Boolean;
    function HasListmaniaLists() : Boolean;
    function HasSearchInside() : Boolean;
    function HasAlternateVersions() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property ASIN : string read FASIN write FASIN;
    property ParentASIN : string read FParentASIN write FParentASIN stored HasParentASIN;
    property Errors : Errors_Type read FErrors write FErrors stored HasErrors;
    property DetailPageURL : string read FDetailPageURL write FDetailPageURL stored HasDetailPageURL;
    property SalesRank : string read FSalesRank write FSalesRank stored HasSalesRank;
    property SmallImage : Image read FSmallImage write FSmallImage stored HasSmallImage;
    property MediumImage : Image read FMediumImage write FMediumImage stored HasMediumImage;
    property LargeImage : Image read FLargeImage write FLargeImage stored HasLargeImage;
    property ImageSets : _Item_ImageSetsArray read FImageSets write FImageSets;
    property ItemAttributes : ItemAttributes_Type read FItemAttributes write FItemAttributes stored HasItemAttributes;
    property MerchantItemAttributes : MerchantItemAttributes_Type read FMerchantItemAttributes write FMerchantItemAttributes stored HasMerchantItemAttributes;
    property Collections : Collections_Type read FCollections write FCollections stored HasCollections;
    property Subjects : Item_Subjects_Type read FSubjects write FSubjects stored HasSubjects;
    property OfferSummary : OfferSummary_Type read FOfferSummary write FOfferSummary stored HasOfferSummary;
    property Offers : Offers_Type read FOffers write FOffers stored HasOffers;
    property VariationSummary : VariationSummary_Type read FVariationSummary write FVariationSummary stored HasVariationSummary;
    property Variations : Variations_Type read FVariations write FVariations stored HasVariations;
    property CustomerReviews : CustomerReviews_Type read FCustomerReviews write FCustomerReviews stored HasCustomerReviews;
    property EditorialReviews : EditorialReviews_Type read FEditorialReviews write FEditorialReviews stored HasEditorialReviews;
    property SimilarProducts : SimilarProducts_Type read FSimilarProducts write FSimilarProducts stored HasSimilarProducts;
    property Accessories : Accessories_Type read FAccessories write FAccessories stored HasAccessories;
    property Tracks : Tracks_Type read FTracks write FTracks stored HasTracks;
    property BrowseNodes : BrowseNodes_Type read FBrowseNodes write FBrowseNodes stored HasBrowseNodes;
    property ListmaniaLists : ListmaniaLists_Type read FListmaniaLists write FListmaniaLists stored HasListmaniaLists;
    property SearchInside : SearchInside_Type read FSearchInside write FSearchInside stored HasSearchInside;
    property AlternateVersions : Item_AlternateVersions_Type read FAlternateVersions write FAlternateVersions stored HasAlternateVersions;
  end;

  List_Type = class(TBaseComplexRemotable)
  private
    FListId : string;
    FListURL : string;
    FRegistryNumber : string;
    FListName : string;
    FListType : List_ListType_Type;
    FTotalItems : nonNegativeInteger;
    FTotalPages : nonNegativeInteger;
    FDateCreated : string;
    FOccasionDate : string;
    FCustomerName : string;
    FPartnerName : string;
    FAdditionalName : string;
    FComment : string;
    FImage : Image;
    FAverageRating : Extended;
    FTotalVotes : nonNegativeInteger;
    FTotalTimesRead : nonNegativeInteger;
    FListItem : List_ListItemArray;
  private
    function HasListURL() : Boolean;
    function HasRegistryNumber() : Boolean;
    function HasListName() : Boolean;
    function HasListType() : Boolean;
    function HasTotalItems() : Boolean;
    function HasTotalPages() : Boolean;
    function HasDateCreated() : Boolean;
    function HasOccasionDate() : Boolean;
    function HasCustomerName() : Boolean;
    function HasPartnerName() : Boolean;
    function HasAdditionalName() : Boolean;
    function HasComment() : Boolean;
    function HasImage() : Boolean;
    function HasAverageRating() : Boolean;
    function HasTotalVotes() : Boolean;
    function HasTotalTimesRead() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property ListId : string read FListId write FListId;
    property ListURL : string read FListURL write FListURL stored HasListURL;
    property RegistryNumber : string read FRegistryNumber write FRegistryNumber stored HasRegistryNumber;
    property ListName : string read FListName write FListName stored HasListName;
    property ListType : List_ListType_Type read FListType write FListType stored HasListType;
    property TotalItems : nonNegativeInteger read FTotalItems write FTotalItems stored HasTotalItems;
    property TotalPages : nonNegativeInteger read FTotalPages write FTotalPages stored HasTotalPages;
    property DateCreated : string read FDateCreated write FDateCreated stored HasDateCreated;
    property OccasionDate : string read FOccasionDate write FOccasionDate stored HasOccasionDate;
    property CustomerName : string read FCustomerName write FCustomerName stored HasCustomerName;
    property PartnerName : string read FPartnerName write FPartnerName stored HasPartnerName;
    property AdditionalName : string read FAdditionalName write FAdditionalName stored HasAdditionalName;
    property Comment : string read FComment write FComment stored HasComment;
    property Image : Image read FImage write FImage stored HasImage;
    property AverageRating : Extended read FAverageRating write FAverageRating stored HasAverageRating;
    property TotalVotes : nonNegativeInteger read FTotalVotes write FTotalVotes stored HasTotalVotes;
    property TotalTimesRead : nonNegativeInteger read FTotalTimesRead write FTotalTimesRead stored HasTotalTimesRead;
    property ListItem : List_ListItemArray read FListItem write FListItem;
  end;

  Customer_Type = class(TBaseComplexRemotable)
  private
    FCustomerId : string;
    FNickname : string;
    FBirthday : string;
    FWishListId : string;
    FLocation : Customer_Location_Type;
    FCustomerReviews : Customer_CustomerReviewsArray;
  private
    function HasNickname() : Boolean;
    function HasBirthday() : Boolean;
    function HasWishListId() : Boolean;
    function HasLocation() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CustomerId : string read FCustomerId write FCustomerId;
    property Nickname : string read FNickname write FNickname stored HasNickname;
    property Birthday : string read FBirthday write FBirthday stored HasBirthday;
    property WishListId : string read FWishListId write FWishListId stored HasWishListId;
    property Location : Customer_Location_Type read FLocation write FLocation stored HasLocation;
    property CustomerReviews : Customer_CustomerReviewsArray read FCustomerReviews write FCustomerReviews;
  end;

  Price = class(TBaseComplexRemotable)
  private
    FAmount : integer;
    FCurrencyCode : string;
    FFormattedPrice : string;
  private
    function HasAmount() : Boolean;
    function HasCurrencyCode() : Boolean;
  published
    property Amount : integer read FAmount write FAmount stored HasAmount;
    property CurrencyCode : string read FCurrencyCode write FCurrencyCode stored HasCurrencyCode;
    property FormattedPrice : string read FFormattedPrice write FFormattedPrice;
  end;

  CartItems_Type = class(TBaseComplexRemotable)
  private
    FSubTotal : Price;
    FCartItem : CartItems_CartItemArray;
  private
    function HasSubTotal() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property SubTotal : Price read FSubTotal write FSubTotal stored HasSubTotal;
    property CartItem : CartItems_CartItemArray read FCartItem write FCartItem;
  end;

  SavedForLaterItems_Type = class(TBaseComplexRemotable)
  private
    FSubTotal : Price;
    FSavedForLaterItem : SavedForLaterItems_SavedForLaterItemArray;
  private
    function HasSubTotal() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property SubTotal : Price read FSubTotal write FSubTotal stored HasSubTotal;
    property SavedForLaterItem : SavedForLaterItems_SavedForLaterItemArray read FSavedForLaterItem write FSavedForLaterItem;
  end;

  Transaction_Type = class(TBaseComplexRemotable)
  private
    FTransactionId : string;
    FSellerId : string;
    FCondition : string;
    FTransactionDate : string;
    FTransactionDateEpoch : string;
    FSellerName : string;
    FPayingCustomerId : string;
    FOrderingCustomerId : string;
    FTotals : Transaction_Totals_Type;
    FTransactionItems : Transaction_TransactionItems_Type;
    FShipments : Transaction_Shipments_Type;
  private
    function HasSellerName() : Boolean;
    function HasPayingCustomerId() : Boolean;
    function HasOrderingCustomerId() : Boolean;
    function HasTotals() : Boolean;
    function HasTransactionItems() : Boolean;
    function HasShipments() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property TransactionId : string read FTransactionId write FTransactionId;
    property SellerId : string read FSellerId write FSellerId;
    property Condition : string read FCondition write FCondition;
    property TransactionDate : string read FTransactionDate write FTransactionDate;
    property TransactionDateEpoch : string read FTransactionDateEpoch write FTransactionDateEpoch;
    property SellerName : string read FSellerName write FSellerName stored HasSellerName;
    property PayingCustomerId : string read FPayingCustomerId write FPayingCustomerId stored HasPayingCustomerId;
    property OrderingCustomerId : string read FOrderingCustomerId write FOrderingCustomerId stored HasOrderingCustomerId;
    property Totals : Transaction_Totals_Type read FTotals write FTotals stored HasTotals;
    property TransactionItems : Transaction_TransactionItems_Type read FTransactionItems write FTransactionItems stored HasTransactionItems;
    property Shipments : Transaction_Shipments_Type read FShipments write FShipments stored HasShipments;
  end;

  Seller_Type = class(TBaseComplexRemotable)
  private
    FSellerId : string;
    FSellerName : string;
    FSellerLegalName : string;
    FNickname : string;
    FGlancePage : string;
    FAbout : string;
    FMoreAbout : string;
    FLocation : Seller_Location_Type;
    FAverageFeedbackRating : Extended;
    FTotalFeedback : nonNegativeInteger;
    FTotalFeedbackPages : nonNegativeInteger;
    FSellerFeedbackSummary : Seller_SellerFeedbackSummary_Type;
    FSellerFeedback : SellerFeedback_Type;
  private
    function HasSellerName() : Boolean;
    function HasSellerLegalName() : Boolean;
    function HasNickname() : Boolean;
    function HasGlancePage() : Boolean;
    function HasAbout() : Boolean;
    function HasMoreAbout() : Boolean;
    function HasLocation() : Boolean;
    function HasAverageFeedbackRating() : Boolean;
    function HasTotalFeedback() : Boolean;
    function HasTotalFeedbackPages() : Boolean;
    function HasSellerFeedbackSummary() : Boolean;
    function HasSellerFeedback() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property SellerId : string read FSellerId write FSellerId;
    property SellerName : string read FSellerName write FSellerName stored HasSellerName;
    property SellerLegalName : string read FSellerLegalName write FSellerLegalName stored HasSellerLegalName;
    property Nickname : string read FNickname write FNickname stored HasNickname;
    property GlancePage : string read FGlancePage write FGlancePage stored HasGlancePage;
    property About : string read FAbout write FAbout stored HasAbout;
    property MoreAbout : string read FMoreAbout write FMoreAbout stored HasMoreAbout;
    property Location : Seller_Location_Type read FLocation write FLocation stored HasLocation;
    property AverageFeedbackRating : Extended read FAverageFeedbackRating write FAverageFeedbackRating stored HasAverageFeedbackRating;
    property TotalFeedback : nonNegativeInteger read FTotalFeedback write FTotalFeedback stored HasTotalFeedback;
    property TotalFeedbackPages : nonNegativeInteger read FTotalFeedbackPages write FTotalFeedbackPages stored HasTotalFeedbackPages;
    property SellerFeedbackSummary : Seller_SellerFeedbackSummary_Type read FSellerFeedbackSummary write FSellerFeedbackSummary stored HasSellerFeedbackSummary;
    property SellerFeedback : SellerFeedback_Type read FSellerFeedback write FSellerFeedback stored HasSellerFeedback;
  end;

  SellerListing_Type = class(TBaseComplexRemotable)
  private
    FExchangeId : string;
    FListingId : string;
    FASIN : string;
    FSKU : string;
    FUPC : string;
    FEAN : string;
    FWillShipExpedited : boolean;
    FWillShipInternational : boolean;
    FTitle : string;
    FPrice : Price;
    FStartDate : string;
    FEndDate : string;
    FStatus : string;
    FQuantity : string;
    FCondition : string;
    FSubCondition : string;
    FSeller : Seller_Type;
  private
    function HasExchangeId() : Boolean;
    function HasListingId() : Boolean;
    function HasASIN() : Boolean;
    function HasSKU() : Boolean;
    function HasUPC() : Boolean;
    function HasEAN() : Boolean;
    function HasWillShipExpedited() : Boolean;
    function HasWillShipInternational() : Boolean;
    function HasTitle() : Boolean;
    function HasPrice() : Boolean;
    function HasStartDate() : Boolean;
    function HasEndDate() : Boolean;
    function HasStatus() : Boolean;
    function HasQuantity() : Boolean;
    function HasCondition() : Boolean;
    function HasSubCondition() : Boolean;
    function HasSeller() : Boolean;
  public
    destructor Destroy();override;
  published
    property ExchangeId : string read FExchangeId write FExchangeId stored HasExchangeId;
    property ListingId : string read FListingId write FListingId stored HasListingId;
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property SKU : string read FSKU write FSKU stored HasSKU;
    property UPC : string read FUPC write FUPC stored HasUPC;
    property EAN : string read FEAN write FEAN stored HasEAN;
    property WillShipExpedited : boolean read FWillShipExpedited write FWillShipExpedited stored HasWillShipExpedited;
    property WillShipInternational : boolean read FWillShipInternational write FWillShipInternational stored HasWillShipInternational;
    property Title : string read FTitle write FTitle stored HasTitle;
    property Price : Price read FPrice write FPrice stored HasPrice;
    property StartDate : string read FStartDate write FStartDate stored HasStartDate;
    property EndDate : string read FEndDate write FEndDate stored HasEndDate;
    property Status : string read FStatus write FStatus stored HasStatus;
    property Quantity : string read FQuantity write FQuantity stored HasQuantity;
    property Condition : string read FCondition write FCondition stored HasCondition;
    property SubCondition : string read FSubCondition write FSubCondition stored HasSubCondition;
    property Seller : Seller_Type read FSeller write FSeller stored HasSeller;
  end;

  Image = class(TBaseComplexRemotable)
  private
    FURL : string;
    FHeight : DecimalWithUnits;
    FWidth : DecimalWithUnits;
    FIsVerified : string;
  private
    function HasIsVerified() : Boolean;
  public
    destructor Destroy();override;
  published
    property URL : string read FURL write FURL;
    property Height : DecimalWithUnits read FHeight write FHeight;
    property Width : DecimalWithUnits read FWidth write FWidth;
    property IsVerified : string read FIsVerified write FIsVerified stored HasIsVerified;
  end;

  ListItem_Type = class(TBaseComplexRemotable)
  private
    FListItemId : string;
    FDateAdded : string;
    FComment : string;
    FQuantityDesired : string;
    FQuantityReceived : string;
    F_Item : Item_Type;
  private
    function HasListItemId() : Boolean;
    function HasDateAdded() : Boolean;
    function HasComment() : Boolean;
    function HasQuantityDesired() : Boolean;
    function HasQuantityReceived() : Boolean;
    function Has_Item() : Boolean;
  public
    destructor Destroy();override;
  published
    property ListItemId : string read FListItemId write FListItemId stored HasListItemId;
    property DateAdded : string read FDateAdded write FDateAdded stored HasDateAdded;
    property Comment : string read FComment write FComment stored HasComment;
    property QuantityDesired : string read FQuantityDesired write FQuantityDesired stored HasQuantityDesired;
    property QuantityReceived : string read FQuantityReceived write FQuantityReceived stored HasQuantityReceived;
    property _Item : Item_Type read F_Item write F_Item stored Has_Item;
  end;

  Customer_Location_Type = class(TBaseComplexRemotable)
  private
    FUserDefinedLocation : string;
    FCity : string;
    FState : string;
    FCountry : string;
  private
    function HasUserDefinedLocation() : Boolean;
    function HasCity() : Boolean;
    function HasState() : Boolean;
    function HasCountry() : Boolean;
  published
    property UserDefinedLocation : string read FUserDefinedLocation write FUserDefinedLocation stored HasUserDefinedLocation;
    property City : string read FCity write FCity stored HasCity;
    property State : string read FState write FState stored HasState;
    property Country : string read FCountry write FCountry stored HasCountry;
  end;

  CustomerReviews_Type = class(TBaseComplexRemotable)
  private
    FAverageRating : Extended;
    FTotalReviews : nonNegativeInteger;
    FTotalReviewPages : nonNegativeInteger;
    FReview : CustomerReviews_ReviewArray;
  private
    function HasAverageRating() : Boolean;
    function HasTotalReviews() : Boolean;
    function HasTotalReviewPages() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property AverageRating : Extended read FAverageRating write FAverageRating stored HasAverageRating;
    property TotalReviews : nonNegativeInteger read FTotalReviews write FTotalReviews stored HasTotalReviews;
    property TotalReviewPages : nonNegativeInteger read FTotalReviewPages write FTotalReviewPages stored HasTotalReviewPages;
    property Review : CustomerReviews_ReviewArray read FReview write FReview;
  end;

  SearchResultsMap_SearchIndex_Type = class(TBaseComplexRemotable)
  private
    FIndexName : string;
    FResults : nonNegativeInteger;
    FPages : nonNegativeInteger;
    FCorrectedQuery : CorrectedQuery_Type;
    FRelevanceRank : positiveInteger;
    FASIN : SearchResultsMap_SearchIndex_Type_ASINArray;
  private
    function HasResults() : Boolean;
    function HasPages() : Boolean;
    function HasCorrectedQuery() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property IndexName : string read FIndexName write FIndexName;
    property Results : nonNegativeInteger read FResults write FResults stored HasResults;
    property Pages : nonNegativeInteger read FPages write FPages stored HasPages;
    property CorrectedQuery : CorrectedQuery_Type read FCorrectedQuery write FCorrectedQuery stored HasCorrectedQuery;
    property RelevanceRank : positiveInteger read FRelevanceRank write FRelevanceRank;
    property ASIN : SearchResultsMap_SearchIndex_Type_ASINArray read FASIN write FASIN;
  end;

  ImageSet_Type = class(TBaseComplexRemotable)
  private
    FSwatchImage : Image;
    FSmallImage : Image;
    FMediumImage : Image;
    FLargeImage : Image;
    FCategory : string;
  private
    function HasSwatchImage() : Boolean;
    function HasSmallImage() : Boolean;
    function HasMediumImage() : Boolean;
    function HasLargeImage() : Boolean;
  public
    destructor Destroy();override;
  published
    property SwatchImage : Image read FSwatchImage write FSwatchImage stored HasSwatchImage;
    property SmallImage : Image read FSmallImage write FSmallImage stored HasSmallImage;
    property MediumImage : Image read FMediumImage write FMediumImage stored HasMediumImage;
    property LargeImage : Image read FLargeImage write FLargeImage stored HasLargeImage;
    property Category : string read FCategory write FCategory;
  end;

  Item_ImageSets_Type = class(TBaseComplexRemotable)
  private
    FMerchantId : string;
    FImageSet : Item_ImageSets_Type_ImageSetArray;
  private
    function HasMerchantId() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MerchantId : string read FMerchantId write FMerchantId stored HasMerchantId;
    property ImageSet : Item_ImageSets_Type_ImageSetArray read FImageSet write FImageSet;
  end;

  ItemAttributes_Type = class(TBaseComplexRemotable)
  private
    FActor : ItemAttributes_ActorArray;
    FAddress : Address;
    FAmazonMaximumAge : DecimalWithUnits;
    FAmazonMinimumAge : DecimalWithUnits;
    FAnalogVideoFormat : string;
    FApertureModes : string;
    FArtist : ItemAttributes_ArtistArray;
    FAspectRatio : string;
    FAssemblyInstructions : string;
    FAssemblyRequired : string;
    FAudienceRating : string;
    FAudioFormat : ItemAttributes_AudioFormatArray;
    FAuthor : ItemAttributes_AuthorArray;
    FBackFinding : string;
    FBandMaterialType : string;
    FBatteriesIncluded : string;
    FBatteriesRequired : string;
    FBatteries : NonNegativeIntegerWithUnits;
    FBatteryDescription : string;
    FBatteryType : string;
    FBezelMaterialType : string;
    FBinding : string;
    FBrand : string;
    FCalendarType : string;
    FCameraManualFeatures : ItemAttributes_CameraManualFeaturesArray;
    FCaseDiameter : DecimalWithUnits;
    FCaseMaterialType : string;
    FCaseThickness : DecimalWithUnits;
    FCaseType : string;
    FCatalogNumber : string;
    FCDRWDescription : string;
    FChainType : string;
    FCEROAgeRating : string;
    FClaspType : string;
    FClothingSize : string;
    FClubType : string;
    FColor : string;
    FCompatibility : string;
    FCompatibleDevices : ItemAttributes_CompatibleDevicesArray;
    FComputerHardwareType : string;
    FComputerPlatform : string;
    FConnectivity : string;
    FContinuousShootingSpeed : DecimalWithUnits;
    FCountry : string;
    FCPUManufacturer : string;
    FCPUSpeed : DecimalWithUnits;
    FCPUType : string;
    FCreator : ItemAttributes_CreatorArray;
    FCuisine : string;
    FDataLinkProtocol : ItemAttributes_DataLinkProtocolArray;
    FDeliveryOption : string;
    FDelayBetweenShots : DecimalWithUnits;
    FDepartment : string;
    FDeweyDecimalNumber : string;
    FDialColor : string;
    FDialWindowMaterialType : string;
    FDigitalZoom : DecimalWithUnits;
    FDirector : ItemAttributes_DirectorArray;
    FDisplayColorSupport : string;
    FDisplaySize : DecimalWithUnits;
    FDrumSetPieceQuantity : nonNegativeInteger;
    FDVDLayers : nonNegativeInteger;
    FDVDRWDescription : string;
    FDVDSides : nonNegativeInteger;
    FDPCI : string;
    FEAN : string;
    FEdition : string;
    FESRBAgeRating : string;
    FExternalDisplaySupportDescription : string;
    FFabricType : string;
    FFaxNumber : string;
    FFeature : ItemAttributes_FeatureArray;
    FFilmColorType : string;
    FFirstIssueLeadTime : StringWithUnits;
    FFloppyDiskDriveDescription : string;
    FFormat : ItemAttributes_FormatArray;
    FFormFactor : ItemAttributes_FormFactorArray;
    FGemType : string;
    FGenre : string;
    FGraphicsCardInterface : string;
    FGraphicsDescription : string;
    FGraphicsMemorySize : DecimalWithUnits;
    FGuitarAttribute : string;
    FGuitarBridgeSystem : string;
    FGuitarPickThickness : string;
    FGuitarPickupConfiguration : string;
    FHandOrientation : string;
    FHardDiskCount : nonNegativeInteger;
    FHardDiskSize : DecimalWithUnits;
    FHardDiskInterface : string;
    FHardwarePlatform : string;
    FHasAutoFocus : boolean;
    FHasBurstMode : boolean;
    FHasInCameraEditing : boolean;
    FHasRedEyeReduction : boolean;
    FHasSelfTimer : boolean;
    FHasTripodMount : boolean;
    FHasVideoOut : boolean;
    FHasViewfinder : boolean;
    FHazardousMaterialType : string;
    FHoursOfOperation : string;
    FIncludedSoftware : string;
    FIncludesMp3Player : boolean;
    FIngredients : string;
    FInstrumentKey : string;
    FIsAdultProduct : boolean;
    FIsAutographed : boolean;
    FISBN : string;
    FIsFragile : boolean;
    FIsLabCreated : boolean;
    FIsMemorabilia : boolean;
    FISOEquivalent : NonNegativeIntegerWithUnits;
    FIsPreannounce : boolean;
    FIssuesPerYear : string;
    FItemDimensions : ItemAttributes_ItemDimensions_Type;
    FKeyboardDescription : string;
    F_Label : string;
    FLanguages : ItemAttributes_Languages_Type;
    FLegalDisclaimer : string;
    FLensType : string;
    FLineVoltage : string;
    FListPrice : Price;
    FMacroFocusRange : string;
    FMagazineType : string;
    FMalletHardness : string;
    FManufacturer : string;
    FManufacturerLaborWarrantyDescription : string;
    FManufacturerMaximumAge : DecimalWithUnits;
    FManufacturerMinimumAge : DecimalWithUnits;
    FManufacturerPartsWarrantyDescription : string;
    FMaterialType : string;
    FMaximumAperture : DecimalWithUnits;
    FMaximumColorDepth : string;
    FMaximumFocalLength : DecimalWithUnits;
    FMaximumHighResolutionImages : NonNegativeIntegerWithUnits;
    FMaximumHorizontalResolution : NonNegativeIntegerWithUnits;
    FMaximumLowResolutionImages : string;
    FMaximumResolution : DecimalWithUnits;
    FMaximumShutterSpeed : DecimalWithUnits;
    FMaximumVerticalResolution : NonNegativeIntegerWithUnits;
    FMaximumWeightRecommendation : DecimalWithUnits;
    FMediaType : string;
    FMemorySlotsAvailable : string;
    FMetalStamp : string;
    FMetalType : string;
    FMiniMovieDescription : string;
    FMinimumFocalLength : DecimalWithUnits;
    FMinimumShutterSpeed : DecimalWithUnits;
    FModel : string;
    FModelYear : nonNegativeInteger;
    FModemDescription : string;
    FMonitorSize : DecimalWithUnits;
    FMonitorViewableDiagonalSize : DecimalWithUnits;
    FMouseDescription : string;
    FMPN : string;
    FMusicalStyle : string;
    FNativeResolution : string;
    FNeighborhood : string;
    FNetworkInterfaceDescription : string;
    FNotebookDisplayTechnology : string;
    FNotebookPointingDeviceDescription : string;
    FNumberOfDiscs : nonNegativeInteger;
    FNumberOfIssues : nonNegativeInteger;
    FNumberOfItems : nonNegativeInteger;
    FNumberOfKeys : nonNegativeInteger;
    FNumberOfPages : nonNegativeInteger;
    FNumberOfPearls : nonNegativeInteger;
    FNumberOfRapidFireShots : nonNegativeInteger;
    FNumberOfStones : nonNegativeInteger;
    FNumberOfStrings : nonNegativeInteger;
    FNumberOfTracks : nonNegativeInteger;
    FOperatingSystem : string;
    FOpticalSensorResolution : DecimalWithUnits;
    FOpticalZoom : DecimalWithUnits;
    FOriginalReleaseDate : string;
    FOutputWattage : nonNegativeInteger;
    FPackageDimensions : ItemAttributes_PackageDimensions_Type;
    FPackageQuantity : nonNegativeInteger;
    FPearlLustre : string;
    FPearlMinimumColor : string;
    FPearlShape : string;
    FPearlStringingMethod : string;
    FPearlSurfaceBlemishes : string;
    FPearlType : string;
    FPearlUniformity : string;
    FPhoneNumber : string;
    FPhotoFlashType : ItemAttributes_PhotoFlashTypeArray;
    FPictureFormat : ItemAttributes_PictureFormatArray;
    FPlatform : ItemAttributes_PlatformArray;
    FPriceRating : nonNegativeInteger;
    FProcessorCount : nonNegativeInteger;
    FProductGroup : string;
    FProductSiteLaunchDate : string;
    FProductTypeName : string;
    FProductTypeSubcategory : string;
    FPromotionalTag : string;
    FPublicationDate : string;
    FPublisher : string;
    FPOBoxShippingExcluded : string;
    FReadingLevel : string;
    FReturnMethod : ItemAttributes_ReturnMethodArray;
    FRecorderTrackCount : nonNegativeInteger;
    FRegionCode : string;
    FRegionOfOrigin : string;
    FReturnPolicy : string;
    FReleaseDate : string;
    FRemovableMemory : string;
    FRemovableStorage : string;
    FRequiredVoltageRange : string;
    FResolutionModes : string;
    FRingSize : string;
    FRunningTime : DecimalWithUnits;
    FScentName : string;
    FSecondaryCacheSize : NonNegativeIntegerWithUnits;
    FSettingType : string;
    FShaftMaterialType : string;
    FSize : string;
    FSizePerPearl : string;
    FSkillLevel : string;
    FSKU : string;
    FSoldInStores : string;
    FSoundCardDescription : string;
    FSpeakerCount : nonNegativeInteger;
    FSpeakerDescription : string;
    FSpecialFeatures : ItemAttributes_SpecialFeaturesArray;
    FStoneClarity : string;
    FStoneColor : string;
    FStoneCut : string;
    FStoneShape : string;
    FStoneWeight : DecimalWithUnits;
    FStudio : string;
    FStyle : string;
    FSubscriptionLength : NonNegativeIntegerWithUnits;
    FSupportedImageType : ItemAttributes_SupportedImageTypeArray;
    FSupportedMediaSize : string;
    FSystemBusSpeed : DecimalWithUnits;
    FSystemMemorySizeMax : DecimalWithUnits;
    FSystemMemorySize : DecimalWithUnits;
    FSystemMemoryType : string;
    FTellingPageIndicator : string;
    FTheatricalReleaseDate : string;
    FTitle : string;
    FTotalDiamondWeight : DecimalWithUnits;
    FTotalExternalBaysFree : nonNegativeInteger;
    FTotalFirewirePorts : nonNegativeInteger;
    FTotalGemWeight : DecimalWithUnits;
    FTotalInternalBaysFree : nonNegativeInteger;
    FTotalMetalWeight : DecimalWithUnits;
    FTotalNTSCPALPorts : nonNegativeInteger;
    FTotalParallelPorts : nonNegativeInteger;
    FTotalPCCardSlots : nonNegativeInteger;
    FTotalPCISlotsFree : nonNegativeInteger;
    FTotalSerialPorts : nonNegativeInteger;
    FTotalSVideoOutPorts : nonNegativeInteger;
    FTotalUSB2Ports : nonNegativeInteger;
    FTotalUSBPorts : nonNegativeInteger;
    FTotalVGAOutPorts : nonNegativeInteger;
    FUPC : string;
    FVariationDenomination : string;
    FVariationDescription : string;
    FWarranty : string;
    FWatchMovementType : string;
    FWaterResistanceDepth : DecimalWithUnits;
    FWEEETaxValue : Price;
    FWirelessMicrophoneFrequency : nonNegativeInteger;
  private
    function HasAddress() : Boolean;
    function HasAmazonMaximumAge() : Boolean;
    function HasAmazonMinimumAge() : Boolean;
    function HasAnalogVideoFormat() : Boolean;
    function HasApertureModes() : Boolean;
    function HasAspectRatio() : Boolean;
    function HasAssemblyInstructions() : Boolean;
    function HasAssemblyRequired() : Boolean;
    function HasAudienceRating() : Boolean;
    function HasBackFinding() : Boolean;
    function HasBandMaterialType() : Boolean;
    function HasBatteriesIncluded() : Boolean;
    function HasBatteriesRequired() : Boolean;
    function HasBatteries() : Boolean;
    function HasBatteryDescription() : Boolean;
    function HasBatteryType() : Boolean;
    function HasBezelMaterialType() : Boolean;
    function HasBinding() : Boolean;
    function HasBrand() : Boolean;
    function HasCalendarType() : Boolean;
    function HasCaseDiameter() : Boolean;
    function HasCaseMaterialType() : Boolean;
    function HasCaseThickness() : Boolean;
    function HasCaseType() : Boolean;
    function HasCatalogNumber() : Boolean;
    function HasCDRWDescription() : Boolean;
    function HasChainType() : Boolean;
    function HasCEROAgeRating() : Boolean;
    function HasClaspType() : Boolean;
    function HasClothingSize() : Boolean;
    function HasClubType() : Boolean;
    function HasColor() : Boolean;
    function HasCompatibility() : Boolean;
    function HasComputerHardwareType() : Boolean;
    function HasComputerPlatform() : Boolean;
    function HasConnectivity() : Boolean;
    function HasContinuousShootingSpeed() : Boolean;
    function HasCountry() : Boolean;
    function HasCPUManufacturer() : Boolean;
    function HasCPUSpeed() : Boolean;
    function HasCPUType() : Boolean;
    function HasCuisine() : Boolean;
    function HasDeliveryOption() : Boolean;
    function HasDelayBetweenShots() : Boolean;
    function HasDepartment() : Boolean;
    function HasDeweyDecimalNumber() : Boolean;
    function HasDialColor() : Boolean;
    function HasDialWindowMaterialType() : Boolean;
    function HasDigitalZoom() : Boolean;
    function HasDisplayColorSupport() : Boolean;
    function HasDisplaySize() : Boolean;
    function HasDrumSetPieceQuantity() : Boolean;
    function HasDVDLayers() : Boolean;
    function HasDVDRWDescription() : Boolean;
    function HasDVDSides() : Boolean;
    function HasDPCI() : Boolean;
    function HasEAN() : Boolean;
    function HasEdition() : Boolean;
    function HasESRBAgeRating() : Boolean;
    function HasExternalDisplaySupportDescription() : Boolean;
    function HasFabricType() : Boolean;
    function HasFaxNumber() : Boolean;
    function HasFilmColorType() : Boolean;
    function HasFirstIssueLeadTime() : Boolean;
    function HasFloppyDiskDriveDescription() : Boolean;
    function HasGemType() : Boolean;
    function HasGenre() : Boolean;
    function HasGraphicsCardInterface() : Boolean;
    function HasGraphicsDescription() : Boolean;
    function HasGraphicsMemorySize() : Boolean;
    function HasGuitarAttribute() : Boolean;
    function HasGuitarBridgeSystem() : Boolean;
    function HasGuitarPickThickness() : Boolean;
    function HasGuitarPickupConfiguration() : Boolean;
    function HasHandOrientation() : Boolean;
    function HasHardDiskCount() : Boolean;
    function HasHardDiskSize() : Boolean;
    function HasHardDiskInterface() : Boolean;
    function HasHardwarePlatform() : Boolean;
    function HasHasAutoFocus() : Boolean;
    function HasHasBurstMode() : Boolean;
    function HasHasInCameraEditing() : Boolean;
    function HasHasRedEyeReduction() : Boolean;
    function HasHasSelfTimer() : Boolean;
    function HasHasTripodMount() : Boolean;
    function HasHasVideoOut() : Boolean;
    function HasHasViewfinder() : Boolean;
    function HasHazardousMaterialType() : Boolean;
    function HasHoursOfOperation() : Boolean;
    function HasIncludedSoftware() : Boolean;
    function HasIncludesMp3Player() : Boolean;
    function HasIngredients() : Boolean;
    function HasInstrumentKey() : Boolean;
    function HasIsAdultProduct() : Boolean;
    function HasIsAutographed() : Boolean;
    function HasISBN() : Boolean;
    function HasIsFragile() : Boolean;
    function HasIsLabCreated() : Boolean;
    function HasIsMemorabilia() : Boolean;
    function HasISOEquivalent() : Boolean;
    function HasIsPreannounce() : Boolean;
    function HasIssuesPerYear() : Boolean;
    function HasItemDimensions() : Boolean;
    function HasKeyboardDescription() : Boolean;
    function Has_Label() : Boolean;
    function HasLanguages() : Boolean;
    function HasLegalDisclaimer() : Boolean;
    function HasLensType() : Boolean;
    function HasLineVoltage() : Boolean;
    function HasListPrice() : Boolean;
    function HasMacroFocusRange() : Boolean;
    function HasMagazineType() : Boolean;
    function HasMalletHardness() : Boolean;
    function HasManufacturer() : Boolean;
    function HasManufacturerLaborWarrantyDescription() : Boolean;
    function HasManufacturerMaximumAge() : Boolean;
    function HasManufacturerMinimumAge() : Boolean;
    function HasManufacturerPartsWarrantyDescription() : Boolean;
    function HasMaterialType() : Boolean;
    function HasMaximumAperture() : Boolean;
    function HasMaximumColorDepth() : Boolean;
    function HasMaximumFocalLength() : Boolean;
    function HasMaximumHighResolutionImages() : Boolean;
    function HasMaximumHorizontalResolution() : Boolean;
    function HasMaximumLowResolutionImages() : Boolean;
    function HasMaximumResolution() : Boolean;
    function HasMaximumShutterSpeed() : Boolean;
    function HasMaximumVerticalResolution() : Boolean;
    function HasMaximumWeightRecommendation() : Boolean;
    function HasMediaType() : Boolean;
    function HasMemorySlotsAvailable() : Boolean;
    function HasMetalStamp() : Boolean;
    function HasMetalType() : Boolean;
    function HasMiniMovieDescription() : Boolean;
    function HasMinimumFocalLength() : Boolean;
    function HasMinimumShutterSpeed() : Boolean;
    function HasModel() : Boolean;
    function HasModelYear() : Boolean;
    function HasModemDescription() : Boolean;
    function HasMonitorSize() : Boolean;
    function HasMonitorViewableDiagonalSize() : Boolean;
    function HasMouseDescription() : Boolean;
    function HasMPN() : Boolean;
    function HasMusicalStyle() : Boolean;
    function HasNativeResolution() : Boolean;
    function HasNeighborhood() : Boolean;
    function HasNetworkInterfaceDescription() : Boolean;
    function HasNotebookDisplayTechnology() : Boolean;
    function HasNotebookPointingDeviceDescription() : Boolean;
    function HasNumberOfDiscs() : Boolean;
    function HasNumberOfIssues() : Boolean;
    function HasNumberOfItems() : Boolean;
    function HasNumberOfKeys() : Boolean;
    function HasNumberOfPages() : Boolean;
    function HasNumberOfPearls() : Boolean;
    function HasNumberOfRapidFireShots() : Boolean;
    function HasNumberOfStones() : Boolean;
    function HasNumberOfStrings() : Boolean;
    function HasNumberOfTracks() : Boolean;
    function HasOperatingSystem() : Boolean;
    function HasOpticalSensorResolution() : Boolean;
    function HasOpticalZoom() : Boolean;
    function HasOriginalReleaseDate() : Boolean;
    function HasOutputWattage() : Boolean;
    function HasPackageDimensions() : Boolean;
    function HasPackageQuantity() : Boolean;
    function HasPearlLustre() : Boolean;
    function HasPearlMinimumColor() : Boolean;
    function HasPearlShape() : Boolean;
    function HasPearlStringingMethod() : Boolean;
    function HasPearlSurfaceBlemishes() : Boolean;
    function HasPearlType() : Boolean;
    function HasPearlUniformity() : Boolean;
    function HasPhoneNumber() : Boolean;
    function HasPriceRating() : Boolean;
    function HasProcessorCount() : Boolean;
    function HasProductGroup() : Boolean;
    function HasProductSiteLaunchDate() : Boolean;
    function HasProductTypeName() : Boolean;
    function HasProductTypeSubcategory() : Boolean;
    function HasPromotionalTag() : Boolean;
    function HasPublicationDate() : Boolean;
    function HasPublisher() : Boolean;
    function HasPOBoxShippingExcluded() : Boolean;
    function HasReadingLevel() : Boolean;
    function HasRecorderTrackCount() : Boolean;
    function HasRegionCode() : Boolean;
    function HasRegionOfOrigin() : Boolean;
    function HasReturnPolicy() : Boolean;
    function HasReleaseDate() : Boolean;
    function HasRemovableMemory() : Boolean;
    function HasRemovableStorage() : Boolean;
    function HasRequiredVoltageRange() : Boolean;
    function HasResolutionModes() : Boolean;
    function HasRingSize() : Boolean;
    function HasRunningTime() : Boolean;
    function HasScentName() : Boolean;
    function HasSecondaryCacheSize() : Boolean;
    function HasSettingType() : Boolean;
    function HasShaftMaterialType() : Boolean;
    function HasSize() : Boolean;
    function HasSizePerPearl() : Boolean;
    function HasSkillLevel() : Boolean;
    function HasSKU() : Boolean;
    function HasSoldInStores() : Boolean;
    function HasSoundCardDescription() : Boolean;
    function HasSpeakerCount() : Boolean;
    function HasSpeakerDescription() : Boolean;
    function HasStoneClarity() : Boolean;
    function HasStoneColor() : Boolean;
    function HasStoneCut() : Boolean;
    function HasStoneShape() : Boolean;
    function HasStoneWeight() : Boolean;
    function HasStudio() : Boolean;
    function HasStyle() : Boolean;
    function HasSubscriptionLength() : Boolean;
    function HasSupportedMediaSize() : Boolean;
    function HasSystemBusSpeed() : Boolean;
    function HasSystemMemorySizeMax() : Boolean;
    function HasSystemMemorySize() : Boolean;
    function HasSystemMemoryType() : Boolean;
    function HasTellingPageIndicator() : Boolean;
    function HasTheatricalReleaseDate() : Boolean;
    function HasTitle() : Boolean;
    function HasTotalDiamondWeight() : Boolean;
    function HasTotalExternalBaysFree() : Boolean;
    function HasTotalFirewirePorts() : Boolean;
    function HasTotalGemWeight() : Boolean;
    function HasTotalInternalBaysFree() : Boolean;
    function HasTotalMetalWeight() : Boolean;
    function HasTotalNTSCPALPorts() : Boolean;
    function HasTotalParallelPorts() : Boolean;
    function HasTotalPCCardSlots() : Boolean;
    function HasTotalPCISlotsFree() : Boolean;
    function HasTotalSerialPorts() : Boolean;
    function HasTotalSVideoOutPorts() : Boolean;
    function HasTotalUSB2Ports() : Boolean;
    function HasTotalUSBPorts() : Boolean;
    function HasTotalVGAOutPorts() : Boolean;
    function HasUPC() : Boolean;
    function HasVariationDenomination() : Boolean;
    function HasVariationDescription() : Boolean;
    function HasWarranty() : Boolean;
    function HasWatchMovementType() : Boolean;
    function HasWaterResistanceDepth() : Boolean;
    function HasWEEETaxValue() : Boolean;
    function HasWirelessMicrophoneFrequency() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Actor : ItemAttributes_ActorArray read FActor write FActor;
    property Address : Address read FAddress write FAddress stored HasAddress;
    property AmazonMaximumAge : DecimalWithUnits read FAmazonMaximumAge write FAmazonMaximumAge stored HasAmazonMaximumAge;
    property AmazonMinimumAge : DecimalWithUnits read FAmazonMinimumAge write FAmazonMinimumAge stored HasAmazonMinimumAge;
    property AnalogVideoFormat : string read FAnalogVideoFormat write FAnalogVideoFormat stored HasAnalogVideoFormat;
    property ApertureModes : string read FApertureModes write FApertureModes stored HasApertureModes;
    property Artist : ItemAttributes_ArtistArray read FArtist write FArtist;
    property AspectRatio : string read FAspectRatio write FAspectRatio stored HasAspectRatio;
    property AssemblyInstructions : string read FAssemblyInstructions write FAssemblyInstructions stored HasAssemblyInstructions;
    property AssemblyRequired : string read FAssemblyRequired write FAssemblyRequired stored HasAssemblyRequired;
    property AudienceRating : string read FAudienceRating write FAudienceRating stored HasAudienceRating;
    property AudioFormat : ItemAttributes_AudioFormatArray read FAudioFormat write FAudioFormat;
    property Author : ItemAttributes_AuthorArray read FAuthor write FAuthor;
    property BackFinding : string read FBackFinding write FBackFinding stored HasBackFinding;
    property BandMaterialType : string read FBandMaterialType write FBandMaterialType stored HasBandMaterialType;
    property BatteriesIncluded : string read FBatteriesIncluded write FBatteriesIncluded stored HasBatteriesIncluded;
    property BatteriesRequired : string read FBatteriesRequired write FBatteriesRequired stored HasBatteriesRequired;
    property Batteries : NonNegativeIntegerWithUnits read FBatteries write FBatteries stored HasBatteries;
    property BatteryDescription : string read FBatteryDescription write FBatteryDescription stored HasBatteryDescription;
    property BatteryType : string read FBatteryType write FBatteryType stored HasBatteryType;
    property BezelMaterialType : string read FBezelMaterialType write FBezelMaterialType stored HasBezelMaterialType;
    property Binding : string read FBinding write FBinding stored HasBinding;
    property Brand : string read FBrand write FBrand stored HasBrand;
    property CalendarType : string read FCalendarType write FCalendarType stored HasCalendarType;
    property CameraManualFeatures : ItemAttributes_CameraManualFeaturesArray read FCameraManualFeatures write FCameraManualFeatures;
    property CaseDiameter : DecimalWithUnits read FCaseDiameter write FCaseDiameter stored HasCaseDiameter;
    property CaseMaterialType : string read FCaseMaterialType write FCaseMaterialType stored HasCaseMaterialType;
    property CaseThickness : DecimalWithUnits read FCaseThickness write FCaseThickness stored HasCaseThickness;
    property CaseType : string read FCaseType write FCaseType stored HasCaseType;
    property CatalogNumber : string read FCatalogNumber write FCatalogNumber stored HasCatalogNumber;
    property CDRWDescription : string read FCDRWDescription write FCDRWDescription stored HasCDRWDescription;
    property ChainType : string read FChainType write FChainType stored HasChainType;
    property CEROAgeRating : string read FCEROAgeRating write FCEROAgeRating stored HasCEROAgeRating;
    property ClaspType : string read FClaspType write FClaspType stored HasClaspType;
    property ClothingSize : string read FClothingSize write FClothingSize stored HasClothingSize;
    property ClubType : string read FClubType write FClubType stored HasClubType;
    property Color : string read FColor write FColor stored HasColor;
    property Compatibility : string read FCompatibility write FCompatibility stored HasCompatibility;
    property CompatibleDevices : ItemAttributes_CompatibleDevicesArray read FCompatibleDevices write FCompatibleDevices;
    property ComputerHardwareType : string read FComputerHardwareType write FComputerHardwareType stored HasComputerHardwareType;
    property ComputerPlatform : string read FComputerPlatform write FComputerPlatform stored HasComputerPlatform;
    property Connectivity : string read FConnectivity write FConnectivity stored HasConnectivity;
    property ContinuousShootingSpeed : DecimalWithUnits read FContinuousShootingSpeed write FContinuousShootingSpeed stored HasContinuousShootingSpeed;
    property Country : string read FCountry write FCountry stored HasCountry;
    property CPUManufacturer : string read FCPUManufacturer write FCPUManufacturer stored HasCPUManufacturer;
    property CPUSpeed : DecimalWithUnits read FCPUSpeed write FCPUSpeed stored HasCPUSpeed;
    property CPUType : string read FCPUType write FCPUType stored HasCPUType;
    property Creator : ItemAttributes_CreatorArray read FCreator write FCreator;
    property Cuisine : string read FCuisine write FCuisine stored HasCuisine;
    property DataLinkProtocol : ItemAttributes_DataLinkProtocolArray read FDataLinkProtocol write FDataLinkProtocol;
    property DeliveryOption : string read FDeliveryOption write FDeliveryOption stored HasDeliveryOption;
    property DelayBetweenShots : DecimalWithUnits read FDelayBetweenShots write FDelayBetweenShots stored HasDelayBetweenShots;
    property Department : string read FDepartment write FDepartment stored HasDepartment;
    property DeweyDecimalNumber : string read FDeweyDecimalNumber write FDeweyDecimalNumber stored HasDeweyDecimalNumber;
    property DialColor : string read FDialColor write FDialColor stored HasDialColor;
    property DialWindowMaterialType : string read FDialWindowMaterialType write FDialWindowMaterialType stored HasDialWindowMaterialType;
    property DigitalZoom : DecimalWithUnits read FDigitalZoom write FDigitalZoom stored HasDigitalZoom;
    property Director : ItemAttributes_DirectorArray read FDirector write FDirector;
    property DisplayColorSupport : string read FDisplayColorSupport write FDisplayColorSupport stored HasDisplayColorSupport;
    property DisplaySize : DecimalWithUnits read FDisplaySize write FDisplaySize stored HasDisplaySize;
    property DrumSetPieceQuantity : nonNegativeInteger read FDrumSetPieceQuantity write FDrumSetPieceQuantity stored HasDrumSetPieceQuantity;
    property DVDLayers : nonNegativeInteger read FDVDLayers write FDVDLayers stored HasDVDLayers;
    property DVDRWDescription : string read FDVDRWDescription write FDVDRWDescription stored HasDVDRWDescription;
    property DVDSides : nonNegativeInteger read FDVDSides write FDVDSides stored HasDVDSides;
    property DPCI : string read FDPCI write FDPCI stored HasDPCI;
    property EAN : string read FEAN write FEAN stored HasEAN;
    property Edition : string read FEdition write FEdition stored HasEdition;
    property ESRBAgeRating : string read FESRBAgeRating write FESRBAgeRating stored HasESRBAgeRating;
    property ExternalDisplaySupportDescription : string read FExternalDisplaySupportDescription write FExternalDisplaySupportDescription stored HasExternalDisplaySupportDescription;
    property FabricType : string read FFabricType write FFabricType stored HasFabricType;
    property FaxNumber : string read FFaxNumber write FFaxNumber stored HasFaxNumber;
    property Feature : ItemAttributes_FeatureArray read FFeature write FFeature;
    property FilmColorType : string read FFilmColorType write FFilmColorType stored HasFilmColorType;
    property FirstIssueLeadTime : StringWithUnits read FFirstIssueLeadTime write FFirstIssueLeadTime stored HasFirstIssueLeadTime;
    property FloppyDiskDriveDescription : string read FFloppyDiskDriveDescription write FFloppyDiskDriveDescription stored HasFloppyDiskDriveDescription;
    property Format : ItemAttributes_FormatArray read FFormat write FFormat;
    property FormFactor : ItemAttributes_FormFactorArray read FFormFactor write FFormFactor;
    property GemType : string read FGemType write FGemType stored HasGemType;
    property Genre : string read FGenre write FGenre stored HasGenre;
    property GraphicsCardInterface : string read FGraphicsCardInterface write FGraphicsCardInterface stored HasGraphicsCardInterface;
    property GraphicsDescription : string read FGraphicsDescription write FGraphicsDescription stored HasGraphicsDescription;
    property GraphicsMemorySize : DecimalWithUnits read FGraphicsMemorySize write FGraphicsMemorySize stored HasGraphicsMemorySize;
    property GuitarAttribute : string read FGuitarAttribute write FGuitarAttribute stored HasGuitarAttribute;
    property GuitarBridgeSystem : string read FGuitarBridgeSystem write FGuitarBridgeSystem stored HasGuitarBridgeSystem;
    property GuitarPickThickness : string read FGuitarPickThickness write FGuitarPickThickness stored HasGuitarPickThickness;
    property GuitarPickupConfiguration : string read FGuitarPickupConfiguration write FGuitarPickupConfiguration stored HasGuitarPickupConfiguration;
    property HandOrientation : string read FHandOrientation write FHandOrientation stored HasHandOrientation;
    property HardDiskCount : nonNegativeInteger read FHardDiskCount write FHardDiskCount stored HasHardDiskCount;
    property HardDiskSize : DecimalWithUnits read FHardDiskSize write FHardDiskSize stored HasHardDiskSize;
    property HardDiskInterface : string read FHardDiskInterface write FHardDiskInterface stored HasHardDiskInterface;
    property HardwarePlatform : string read FHardwarePlatform write FHardwarePlatform stored HasHardwarePlatform;
    property HasAutoFocus : boolean read FHasAutoFocus write FHasAutoFocus stored HasHasAutoFocus;
    property HasBurstMode : boolean read FHasBurstMode write FHasBurstMode stored HasHasBurstMode;
    property HasInCameraEditing : boolean read FHasInCameraEditing write FHasInCameraEditing stored HasHasInCameraEditing;
    property HasRedEyeReduction : boolean read FHasRedEyeReduction write FHasRedEyeReduction stored HasHasRedEyeReduction;
    property HasSelfTimer : boolean read FHasSelfTimer write FHasSelfTimer stored HasHasSelfTimer;
    property HasTripodMount : boolean read FHasTripodMount write FHasTripodMount stored HasHasTripodMount;
    property HasVideoOut : boolean read FHasVideoOut write FHasVideoOut stored HasHasVideoOut;
    property HasViewfinder : boolean read FHasViewfinder write FHasViewfinder stored HasHasViewfinder;
    property HazardousMaterialType : string read FHazardousMaterialType write FHazardousMaterialType stored HasHazardousMaterialType;
    property HoursOfOperation : string read FHoursOfOperation write FHoursOfOperation stored HasHoursOfOperation;
    property IncludedSoftware : string read FIncludedSoftware write FIncludedSoftware stored HasIncludedSoftware;
    property IncludesMp3Player : boolean read FIncludesMp3Player write FIncludesMp3Player stored HasIncludesMp3Player;
    property Ingredients : string read FIngredients write FIngredients stored HasIngredients;
    property InstrumentKey : string read FInstrumentKey write FInstrumentKey stored HasInstrumentKey;
    property IsAdultProduct : boolean read FIsAdultProduct write FIsAdultProduct stored HasIsAdultProduct;
    property IsAutographed : boolean read FIsAutographed write FIsAutographed stored HasIsAutographed;
    property ISBN : string read FISBN write FISBN stored HasISBN;
    property IsFragile : boolean read FIsFragile write FIsFragile stored HasIsFragile;
    property IsLabCreated : boolean read FIsLabCreated write FIsLabCreated stored HasIsLabCreated;
    property IsMemorabilia : boolean read FIsMemorabilia write FIsMemorabilia stored HasIsMemorabilia;
    property ISOEquivalent : NonNegativeIntegerWithUnits read FISOEquivalent write FISOEquivalent stored HasISOEquivalent;
    property IsPreannounce : boolean read FIsPreannounce write FIsPreannounce stored HasIsPreannounce;
    property IssuesPerYear : string read FIssuesPerYear write FIssuesPerYear stored HasIssuesPerYear;
    property ItemDimensions : ItemAttributes_ItemDimensions_Type read FItemDimensions write FItemDimensions stored HasItemDimensions;
    property KeyboardDescription : string read FKeyboardDescription write FKeyboardDescription stored HasKeyboardDescription;
    property _Label : string read F_Label write F_Label stored Has_Label;
    property Languages : ItemAttributes_Languages_Type read FLanguages write FLanguages stored HasLanguages;
    property LegalDisclaimer : string read FLegalDisclaimer write FLegalDisclaimer stored HasLegalDisclaimer;
    property LensType : string read FLensType write FLensType stored HasLensType;
    property LineVoltage : string read FLineVoltage write FLineVoltage stored HasLineVoltage;
    property ListPrice : Price read FListPrice write FListPrice stored HasListPrice;
    property MacroFocusRange : string read FMacroFocusRange write FMacroFocusRange stored HasMacroFocusRange;
    property MagazineType : string read FMagazineType write FMagazineType stored HasMagazineType;
    property MalletHardness : string read FMalletHardness write FMalletHardness stored HasMalletHardness;
    property Manufacturer : string read FManufacturer write FManufacturer stored HasManufacturer;
    property ManufacturerLaborWarrantyDescription : string read FManufacturerLaborWarrantyDescription write FManufacturerLaborWarrantyDescription stored HasManufacturerLaborWarrantyDescription;
    property ManufacturerMaximumAge : DecimalWithUnits read FManufacturerMaximumAge write FManufacturerMaximumAge stored HasManufacturerMaximumAge;
    property ManufacturerMinimumAge : DecimalWithUnits read FManufacturerMinimumAge write FManufacturerMinimumAge stored HasManufacturerMinimumAge;
    property ManufacturerPartsWarrantyDescription : string read FManufacturerPartsWarrantyDescription write FManufacturerPartsWarrantyDescription stored HasManufacturerPartsWarrantyDescription;
    property MaterialType : string read FMaterialType write FMaterialType stored HasMaterialType;
    property MaximumAperture : DecimalWithUnits read FMaximumAperture write FMaximumAperture stored HasMaximumAperture;
    property MaximumColorDepth : string read FMaximumColorDepth write FMaximumColorDepth stored HasMaximumColorDepth;
    property MaximumFocalLength : DecimalWithUnits read FMaximumFocalLength write FMaximumFocalLength stored HasMaximumFocalLength;
    property MaximumHighResolutionImages : NonNegativeIntegerWithUnits read FMaximumHighResolutionImages write FMaximumHighResolutionImages stored HasMaximumHighResolutionImages;
    property MaximumHorizontalResolution : NonNegativeIntegerWithUnits read FMaximumHorizontalResolution write FMaximumHorizontalResolution stored HasMaximumHorizontalResolution;
    property MaximumLowResolutionImages : string read FMaximumLowResolutionImages write FMaximumLowResolutionImages stored HasMaximumLowResolutionImages;
    property MaximumResolution : DecimalWithUnits read FMaximumResolution write FMaximumResolution stored HasMaximumResolution;
    property MaximumShutterSpeed : DecimalWithUnits read FMaximumShutterSpeed write FMaximumShutterSpeed stored HasMaximumShutterSpeed;
    property MaximumVerticalResolution : NonNegativeIntegerWithUnits read FMaximumVerticalResolution write FMaximumVerticalResolution stored HasMaximumVerticalResolution;
    property MaximumWeightRecommendation : DecimalWithUnits read FMaximumWeightRecommendation write FMaximumWeightRecommendation stored HasMaximumWeightRecommendation;
    property MediaType : string read FMediaType write FMediaType stored HasMediaType;
    property MemorySlotsAvailable : string read FMemorySlotsAvailable write FMemorySlotsAvailable stored HasMemorySlotsAvailable;
    property MetalStamp : string read FMetalStamp write FMetalStamp stored HasMetalStamp;
    property MetalType : string read FMetalType write FMetalType stored HasMetalType;
    property MiniMovieDescription : string read FMiniMovieDescription write FMiniMovieDescription stored HasMiniMovieDescription;
    property MinimumFocalLength : DecimalWithUnits read FMinimumFocalLength write FMinimumFocalLength stored HasMinimumFocalLength;
    property MinimumShutterSpeed : DecimalWithUnits read FMinimumShutterSpeed write FMinimumShutterSpeed stored HasMinimumShutterSpeed;
    property Model : string read FModel write FModel stored HasModel;
    property ModelYear : nonNegativeInteger read FModelYear write FModelYear stored HasModelYear;
    property ModemDescription : string read FModemDescription write FModemDescription stored HasModemDescription;
    property MonitorSize : DecimalWithUnits read FMonitorSize write FMonitorSize stored HasMonitorSize;
    property MonitorViewableDiagonalSize : DecimalWithUnits read FMonitorViewableDiagonalSize write FMonitorViewableDiagonalSize stored HasMonitorViewableDiagonalSize;
    property MouseDescription : string read FMouseDescription write FMouseDescription stored HasMouseDescription;
    property MPN : string read FMPN write FMPN stored HasMPN;
    property MusicalStyle : string read FMusicalStyle write FMusicalStyle stored HasMusicalStyle;
    property NativeResolution : string read FNativeResolution write FNativeResolution stored HasNativeResolution;
    property Neighborhood : string read FNeighborhood write FNeighborhood stored HasNeighborhood;
    property NetworkInterfaceDescription : string read FNetworkInterfaceDescription write FNetworkInterfaceDescription stored HasNetworkInterfaceDescription;
    property NotebookDisplayTechnology : string read FNotebookDisplayTechnology write FNotebookDisplayTechnology stored HasNotebookDisplayTechnology;
    property NotebookPointingDeviceDescription : string read FNotebookPointingDeviceDescription write FNotebookPointingDeviceDescription stored HasNotebookPointingDeviceDescription;
    property NumberOfDiscs : nonNegativeInteger read FNumberOfDiscs write FNumberOfDiscs stored HasNumberOfDiscs;
    property NumberOfIssues : nonNegativeInteger read FNumberOfIssues write FNumberOfIssues stored HasNumberOfIssues;
    property NumberOfItems : nonNegativeInteger read FNumberOfItems write FNumberOfItems stored HasNumberOfItems;
    property NumberOfKeys : nonNegativeInteger read FNumberOfKeys write FNumberOfKeys stored HasNumberOfKeys;
    property NumberOfPages : nonNegativeInteger read FNumberOfPages write FNumberOfPages stored HasNumberOfPages;
    property NumberOfPearls : nonNegativeInteger read FNumberOfPearls write FNumberOfPearls stored HasNumberOfPearls;
    property NumberOfRapidFireShots : nonNegativeInteger read FNumberOfRapidFireShots write FNumberOfRapidFireShots stored HasNumberOfRapidFireShots;
    property NumberOfStones : nonNegativeInteger read FNumberOfStones write FNumberOfStones stored HasNumberOfStones;
    property NumberOfStrings : nonNegativeInteger read FNumberOfStrings write FNumberOfStrings stored HasNumberOfStrings;
    property NumberOfTracks : nonNegativeInteger read FNumberOfTracks write FNumberOfTracks stored HasNumberOfTracks;
    property OperatingSystem : string read FOperatingSystem write FOperatingSystem stored HasOperatingSystem;
    property OpticalSensorResolution : DecimalWithUnits read FOpticalSensorResolution write FOpticalSensorResolution stored HasOpticalSensorResolution;
    property OpticalZoom : DecimalWithUnits read FOpticalZoom write FOpticalZoom stored HasOpticalZoom;
    property OriginalReleaseDate : string read FOriginalReleaseDate write FOriginalReleaseDate stored HasOriginalReleaseDate;
    property OutputWattage : nonNegativeInteger read FOutputWattage write FOutputWattage stored HasOutputWattage;
    property PackageDimensions : ItemAttributes_PackageDimensions_Type read FPackageDimensions write FPackageDimensions stored HasPackageDimensions;
    property PackageQuantity : nonNegativeInteger read FPackageQuantity write FPackageQuantity stored HasPackageQuantity;
    property PearlLustre : string read FPearlLustre write FPearlLustre stored HasPearlLustre;
    property PearlMinimumColor : string read FPearlMinimumColor write FPearlMinimumColor stored HasPearlMinimumColor;
    property PearlShape : string read FPearlShape write FPearlShape stored HasPearlShape;
    property PearlStringingMethod : string read FPearlStringingMethod write FPearlStringingMethod stored HasPearlStringingMethod;
    property PearlSurfaceBlemishes : string read FPearlSurfaceBlemishes write FPearlSurfaceBlemishes stored HasPearlSurfaceBlemishes;
    property PearlType : string read FPearlType write FPearlType stored HasPearlType;
    property PearlUniformity : string read FPearlUniformity write FPearlUniformity stored HasPearlUniformity;
    property PhoneNumber : string read FPhoneNumber write FPhoneNumber stored HasPhoneNumber;
    property PhotoFlashType : ItemAttributes_PhotoFlashTypeArray read FPhotoFlashType write FPhotoFlashType;
    property PictureFormat : ItemAttributes_PictureFormatArray read FPictureFormat write FPictureFormat;
    property Platform : ItemAttributes_PlatformArray read FPlatform write FPlatform;
    property PriceRating : nonNegativeInteger read FPriceRating write FPriceRating stored HasPriceRating;
    property ProcessorCount : nonNegativeInteger read FProcessorCount write FProcessorCount stored HasProcessorCount;
    property ProductGroup : string read FProductGroup write FProductGroup stored HasProductGroup;
    property ProductSiteLaunchDate : string read FProductSiteLaunchDate write FProductSiteLaunchDate stored HasProductSiteLaunchDate;
    property ProductTypeName : string read FProductTypeName write FProductTypeName stored HasProductTypeName;
    property ProductTypeSubcategory : string read FProductTypeSubcategory write FProductTypeSubcategory stored HasProductTypeSubcategory;
    property PromotionalTag : string read FPromotionalTag write FPromotionalTag stored HasPromotionalTag;
    property PublicationDate : string read FPublicationDate write FPublicationDate stored HasPublicationDate;
    property Publisher : string read FPublisher write FPublisher stored HasPublisher;
    property POBoxShippingExcluded : string read FPOBoxShippingExcluded write FPOBoxShippingExcluded stored HasPOBoxShippingExcluded;
    property ReadingLevel : string read FReadingLevel write FReadingLevel stored HasReadingLevel;
    property ReturnMethod : ItemAttributes_ReturnMethodArray read FReturnMethod write FReturnMethod;
    property RecorderTrackCount : nonNegativeInteger read FRecorderTrackCount write FRecorderTrackCount stored HasRecorderTrackCount;
    property RegionCode : string read FRegionCode write FRegionCode stored HasRegionCode;
    property RegionOfOrigin : string read FRegionOfOrigin write FRegionOfOrigin stored HasRegionOfOrigin;
    property ReturnPolicy : string read FReturnPolicy write FReturnPolicy stored HasReturnPolicy;
    property ReleaseDate : string read FReleaseDate write FReleaseDate stored HasReleaseDate;
    property RemovableMemory : string read FRemovableMemory write FRemovableMemory stored HasRemovableMemory;
    property RemovableStorage : string read FRemovableStorage write FRemovableStorage stored HasRemovableStorage;
    property RequiredVoltageRange : string read FRequiredVoltageRange write FRequiredVoltageRange stored HasRequiredVoltageRange;
    property ResolutionModes : string read FResolutionModes write FResolutionModes stored HasResolutionModes;
    property RingSize : string read FRingSize write FRingSize stored HasRingSize;
    property RunningTime : DecimalWithUnits read FRunningTime write FRunningTime stored HasRunningTime;
    property ScentName : string read FScentName write FScentName stored HasScentName;
    property SecondaryCacheSize : NonNegativeIntegerWithUnits read FSecondaryCacheSize write FSecondaryCacheSize stored HasSecondaryCacheSize;
    property SettingType : string read FSettingType write FSettingType stored HasSettingType;
    property ShaftMaterialType : string read FShaftMaterialType write FShaftMaterialType stored HasShaftMaterialType;
    property Size : string read FSize write FSize stored HasSize;
    property SizePerPearl : string read FSizePerPearl write FSizePerPearl stored HasSizePerPearl;
    property SkillLevel : string read FSkillLevel write FSkillLevel stored HasSkillLevel;
    property SKU : string read FSKU write FSKU stored HasSKU;
    property SoldInStores : string read FSoldInStores write FSoldInStores stored HasSoldInStores;
    property SoundCardDescription : string read FSoundCardDescription write FSoundCardDescription stored HasSoundCardDescription;
    property SpeakerCount : nonNegativeInteger read FSpeakerCount write FSpeakerCount stored HasSpeakerCount;
    property SpeakerDescription : string read FSpeakerDescription write FSpeakerDescription stored HasSpeakerDescription;
    property SpecialFeatures : ItemAttributes_SpecialFeaturesArray read FSpecialFeatures write FSpecialFeatures;
    property StoneClarity : string read FStoneClarity write FStoneClarity stored HasStoneClarity;
    property StoneColor : string read FStoneColor write FStoneColor stored HasStoneColor;
    property StoneCut : string read FStoneCut write FStoneCut stored HasStoneCut;
    property StoneShape : string read FStoneShape write FStoneShape stored HasStoneShape;
    property StoneWeight : DecimalWithUnits read FStoneWeight write FStoneWeight stored HasStoneWeight;
    property Studio : string read FStudio write FStudio stored HasStudio;
    property Style : string read FStyle write FStyle stored HasStyle;
    property SubscriptionLength : NonNegativeIntegerWithUnits read FSubscriptionLength write FSubscriptionLength stored HasSubscriptionLength;
    property SupportedImageType : ItemAttributes_SupportedImageTypeArray read FSupportedImageType write FSupportedImageType;
    property SupportedMediaSize : string read FSupportedMediaSize write FSupportedMediaSize stored HasSupportedMediaSize;
    property SystemBusSpeed : DecimalWithUnits read FSystemBusSpeed write FSystemBusSpeed stored HasSystemBusSpeed;
    property SystemMemorySizeMax : DecimalWithUnits read FSystemMemorySizeMax write FSystemMemorySizeMax stored HasSystemMemorySizeMax;
    property SystemMemorySize : DecimalWithUnits read FSystemMemorySize write FSystemMemorySize stored HasSystemMemorySize;
    property SystemMemoryType : string read FSystemMemoryType write FSystemMemoryType stored HasSystemMemoryType;
    property TellingPageIndicator : string read FTellingPageIndicator write FTellingPageIndicator stored HasTellingPageIndicator;
    property TheatricalReleaseDate : string read FTheatricalReleaseDate write FTheatricalReleaseDate stored HasTheatricalReleaseDate;
    property Title : string read FTitle write FTitle stored HasTitle;
    property TotalDiamondWeight : DecimalWithUnits read FTotalDiamondWeight write FTotalDiamondWeight stored HasTotalDiamondWeight;
    property TotalExternalBaysFree : nonNegativeInteger read FTotalExternalBaysFree write FTotalExternalBaysFree stored HasTotalExternalBaysFree;
    property TotalFirewirePorts : nonNegativeInteger read FTotalFirewirePorts write FTotalFirewirePorts stored HasTotalFirewirePorts;
    property TotalGemWeight : DecimalWithUnits read FTotalGemWeight write FTotalGemWeight stored HasTotalGemWeight;
    property TotalInternalBaysFree : nonNegativeInteger read FTotalInternalBaysFree write FTotalInternalBaysFree stored HasTotalInternalBaysFree;
    property TotalMetalWeight : DecimalWithUnits read FTotalMetalWeight write FTotalMetalWeight stored HasTotalMetalWeight;
    property TotalNTSCPALPorts : nonNegativeInteger read FTotalNTSCPALPorts write FTotalNTSCPALPorts stored HasTotalNTSCPALPorts;
    property TotalParallelPorts : nonNegativeInteger read FTotalParallelPorts write FTotalParallelPorts stored HasTotalParallelPorts;
    property TotalPCCardSlots : nonNegativeInteger read FTotalPCCardSlots write FTotalPCCardSlots stored HasTotalPCCardSlots;
    property TotalPCISlotsFree : nonNegativeInteger read FTotalPCISlotsFree write FTotalPCISlotsFree stored HasTotalPCISlotsFree;
    property TotalSerialPorts : nonNegativeInteger read FTotalSerialPorts write FTotalSerialPorts stored HasTotalSerialPorts;
    property TotalSVideoOutPorts : nonNegativeInteger read FTotalSVideoOutPorts write FTotalSVideoOutPorts stored HasTotalSVideoOutPorts;
    property TotalUSB2Ports : nonNegativeInteger read FTotalUSB2Ports write FTotalUSB2Ports stored HasTotalUSB2Ports;
    property TotalUSBPorts : nonNegativeInteger read FTotalUSBPorts write FTotalUSBPorts stored HasTotalUSBPorts;
    property TotalVGAOutPorts : nonNegativeInteger read FTotalVGAOutPorts write FTotalVGAOutPorts stored HasTotalVGAOutPorts;
    property UPC : string read FUPC write FUPC stored HasUPC;
    property VariationDenomination : string read FVariationDenomination write FVariationDenomination stored HasVariationDenomination;
    property VariationDescription : string read FVariationDescription write FVariationDescription stored HasVariationDescription;
    property Warranty : string read FWarranty write FWarranty stored HasWarranty;
    property WatchMovementType : string read FWatchMovementType write FWatchMovementType stored HasWatchMovementType;
    property WaterResistanceDepth : DecimalWithUnits read FWaterResistanceDepth write FWaterResistanceDepth stored HasWaterResistanceDepth;
    property WEEETaxValue : Price read FWEEETaxValue write FWEEETaxValue stored HasWEEETaxValue;
    property WirelessMicrophoneFrequency : nonNegativeInteger read FWirelessMicrophoneFrequency write FWirelessMicrophoneFrequency stored HasWirelessMicrophoneFrequency;
  end;

  MerchantItemAttributes_Type = class(TBaseComplexRemotable)
  private
    FActor : MerchantItemAttributes_ActorArray;
    FAddress : Address;
    FAmazonMaximumAge : DecimalWithUnits;
    FAmazonMinimumAge : DecimalWithUnits;
    FApertureModes : string;
    FArtist : MerchantItemAttributes_ArtistArray;
    FAspectRatio : string;
    FAssemblyInstructions : string;
    FAssemblyRequired : string;
    FAudienceRating : string;
    FAudioFormat : MerchantItemAttributes_AudioFormatArray;
    FAuthor : MerchantItemAttributes_AuthorArray;
    FBackFinding : string;
    FBandMaterialType : string;
    FBatteriesIncluded : string;
    FBatteriesRequired : string;
    FBatteries : NonNegativeIntegerWithUnits;
    FBatteryDescription : string;
    FBatteryType : string;
    FBezelMaterialType : string;
    FBinding : string;
    FBrand : string;
    FCalendarType : string;
    FCameraManualFeatures : MerchantItemAttributes_CameraManualFeaturesArray;
    FCaseDiameter : DecimalWithUnits;
    FCaseMaterialType : string;
    FCaseThickness : DecimalWithUnits;
    FCaseType : string;
    FCatalogNumber : string;
    FCDRWDescription : string;
    FChainType : string;
    FClaspType : string;
    FClothingSize : string;
    FColor : string;
    FCompatibility : string;
    FComputerHardwareType : string;
    FComputerPlatform : string;
    FConnectivity : string;
    FContinuousShootingSpeed : DecimalWithUnits;
    FCountry : string;
    FCountryOfOrigin : string;
    FCPUManufacturer : string;
    FCPUSpeed : DecimalWithUnits;
    FCPUType : string;
    FCreator : MerchantItemAttributes_CreatorArray;
    FCuisine : string;
    FCustomizable : string;
    FDelayBetweenShots : DecimalWithUnits;
    FDeliveryOption : string;
    FDepartment : string;
    FDescription : string;
    FDeweyDecimalNumber : string;
    FDialColor : string;
    FDialWindowMaterialType : string;
    FDigitalZoom : DecimalWithUnits;
    FDirector : MerchantItemAttributes_DirectorArray;
    FDisplaySize : DecimalWithUnits;
    FDrumSetPieceQuantity : nonNegativeInteger;
    FDVDLayers : nonNegativeInteger;
    FDVDRWDescription : string;
    FDVDSides : nonNegativeInteger;
    FDPCI : string;
    FEAN : string;
    FEdition : string;
    FESRBAgeRating : string;
    FExternalDisplaySupportDescription : string;
    FFabricType : string;
    FFaxNumber : string;
    FFeature : MerchantItemAttributes_FeatureArray;
    FFirstIssueLeadTime : StringWithUnits;
    FFloppyDiskDriveDescription : string;
    FFormat : MerchantItemAttributes_FormatArray;
    FFixedShippingCharge : Price;
    FGemType : string;
    FGraphicsCardInterface : string;
    FGraphicsDescription : string;
    FGraphicsMemorySize : DecimalWithUnits;
    FGuitarAttribute : string;
    FGuitarBridgeSystem : string;
    FGuitarPickThickness : string;
    FGuitarPickupConfiguration : string;
    FHardDiskCount : nonNegativeInteger;
    FHardDiskSize : NonNegativeIntegerWithUnits;
    FHasAutoFocus : boolean;
    FHasBurstMode : boolean;
    FHasInCameraEditing : boolean;
    FHasRedEyeReduction : boolean;
    FHasSelfTimer : boolean;
    FHasTripodMount : boolean;
    FHasVideoOut : boolean;
    FHasViewfinder : boolean;
    FHazardousMaterialType : string;
    FHoursOfOperation : string;
    FIncludedSoftware : string;
    FIncludesMp3Player : boolean;
    FIndications : string;
    FIngredients : string;
    FInstrumentKey : string;
    FIsAutographed : boolean;
    FISBN : string;
    FIsFragile : boolean;
    FIsLabCreated : boolean;
    FIsMemorabilia : boolean;
    FISOEquivalent : NonNegativeIntegerWithUnits;
    FIssuesPerYear : string;
    FItemDimensions : MerchantItemAttributes_ItemDimensions_Type;
    FKeyboardDescription : string;
    F_Label : string;
    FLanguages : MerchantItemAttributes_Languages_Type;
    FLegalDisclaimer : string;
    FLineVoltage : string;
    FListPrice : Price;
    FMacroFocusRange : string;
    FMagazineType : string;
    FMalletHardness : string;
    FManufacturer : string;
    FManufacturerLaborWarrantyDescription : string;
    FManufacturerMaximumAge : DecimalWithUnits;
    FManufacturerMinimumAge : DecimalWithUnits;
    FManufacturerPartsWarrantyDescription : string;
    FMaterialType : string;
    FMaximumAperture : DecimalWithUnits;
    FMaximumColorDepth : string;
    FMaximumFocalLength : DecimalWithUnits;
    FMaximumHighResolutionImages : NonNegativeIntegerWithUnits;
    FMaximumHorizontalResolution : NonNegativeIntegerWithUnits;
    FMaximumLowResolutionImages : string;
    FMaximumResolution : DecimalWithUnits;
    FMaximumShutterSpeed : DecimalWithUnits;
    FMaximumVerticalResolution : NonNegativeIntegerWithUnits;
    FMaximumWeightRecommendation : DecimalWithUnits;
    FMemorySlotsAvailable : nonNegativeInteger;
    FMetalStamp : string;
    FMetalType : string;
    FMiniMovieDescription : string;
    FMinimumFocalLength : DecimalWithUnits;
    FMinimumShutterSpeed : DecimalWithUnits;
    FModel : string;
    FModelYear : nonNegativeInteger;
    FModemDescription : string;
    FMonitorSize : DecimalWithUnits;
    FMonitorViewableDiagonalSize : DecimalWithUnits;
    FMouseDescription : string;
    FMPN : string;
    FMusicalStyle : string;
    FNativeResolution : string;
    FNeighborhood : string;
    FNetworkInterfaceDescription : string;
    FNotebookDisplayTechnology : string;
    FNotebookPointingDeviceDescription : string;
    FNumberOfDiscs : nonNegativeInteger;
    FNumberOfIssues : nonNegativeInteger;
    FNumberOfItems : nonNegativeInteger;
    FNumberOfKeys : nonNegativeInteger;
    FNumberOfPages : nonNegativeInteger;
    FNumberOfPearls : nonNegativeInteger;
    FNumberOfRapidFireShots : nonNegativeInteger;
    FNumberOfStones : nonNegativeInteger;
    FNumberOfStrings : nonNegativeInteger;
    FNumberOfTracks : nonNegativeInteger;
    FOpticalZoom : DecimalWithUnits;
    FOriginalReleaseDate : string;
    FOutputWattage : nonNegativeInteger;
    FPackageDimensions : MerchantItemAttributes_PackageDimensions_Type;
    FPearlLustre : string;
    FPearlMinimumColor : string;
    FPearlShape : string;
    FPearlStringingMethod : string;
    FPearlSurfaceBlemishes : string;
    FPearlType : string;
    FPearlUniformity : string;
    FPhoneNumber : string;
    FPhotoFlashType : MerchantItemAttributes_PhotoFlashTypeArray;
    FPictureFormat : MerchantItemAttributes_PictureFormatArray;
    FPlatform : MerchantItemAttributes_PlatformArray;
    FPriceRating : nonNegativeInteger;
    FProcessorCount : nonNegativeInteger;
    FProductGroup : string;
    FPromotionalTag : string;
    FPOBoxShippingExcluded : string;
    FPublicationDate : string;
    FPublisher : string;
    FPurchasingChannel : MerchantItemAttributes_PurchasingChannelArray;
    FReadingLevel : string;
    FRecorderTrackCount : nonNegativeInteger;
    FRegionCode : string;
    FRegionOfOrigin : string;
    FReleaseDate : string;
    FReturnMethod : MerchantItemAttributes_ReturnMethodArray;
    FRemovableMemory : string;
    FResolutionModes : string;
    FReturnPolicy : string;
    FRingSize : string;
    FSafetyWarning : string;
    FSalesRestriction : string;
    FSecondaryCacheSize : NonNegativeIntegerWithUnits;
    FSettingType : string;
    FSize : string;
    FSKU : string;
    FSoldInStores : string;
    FSizePerPearl : string;
    FSkillLevel : string;
    FSoundCardDescription : string;
    FSpeakerCount : nonNegativeInteger;
    FSpeakerDescription : string;
    FSpecialFeatures : MerchantItemAttributes_SpecialFeaturesArray;
    FStoneClarity : string;
    FStoneColor : string;
    FStoneCut : string;
    FStoneShape : string;
    FStoneWeight : DecimalWithUnits;
    FStudio : string;
    FSubscriptionLength : NonNegativeIntegerWithUnits;
    FSupportedImageType : MerchantItemAttributes_SupportedImageTypeArray;
    FSystemBusSpeed : DecimalWithUnits;
    FSystemMemorySizeMax : DecimalWithUnits;
    FSystemMemorySize : DecimalWithUnits;
    FSystemMemoryType : string;
    FTellingPageIndicator : string;
    FTheatricalReleaseDate : string;
    FTitle : string;
    FTotalDiamondWeight : DecimalWithUnits;
    FTotalExternalBaysFree : nonNegativeInteger;
    FTotalFirewirePorts : nonNegativeInteger;
    FTotalGemWeight : DecimalWithUnits;
    FTotalInternalBaysFree : nonNegativeInteger;
    FTotalMetalWeight : DecimalWithUnits;
    FTotalNTSCPALPorts : nonNegativeInteger;
    FTotalParallelPorts : nonNegativeInteger;
    FTotalPCCardSlots : nonNegativeInteger;
    FTotalPCISlotsFree : nonNegativeInteger;
    FTotalSerialPorts : nonNegativeInteger;
    FTotalSVideoOutPorts : nonNegativeInteger;
    FTotalUSB2Ports : nonNegativeInteger;
    FTotalUSBPorts : nonNegativeInteger;
    FTotalVGAOutPorts : nonNegativeInteger;
    FUPC : string;
    FVariationDenomination : string;
    FVariationDescription : string;
    FVendorRebate : MerchantItemAttributes_VendorRebate_Type;
    FWarranty : string;
    FWatchMovementType : string;
    FWebsiteBuyability : string;
    FWaterResistanceDepth : DecimalWithUnits;
    FWirelessMicrophoneFrequency : nonNegativeInteger;
  private
    function HasAddress() : Boolean;
    function HasAmazonMaximumAge() : Boolean;
    function HasAmazonMinimumAge() : Boolean;
    function HasApertureModes() : Boolean;
    function HasAspectRatio() : Boolean;
    function HasAssemblyInstructions() : Boolean;
    function HasAssemblyRequired() : Boolean;
    function HasAudienceRating() : Boolean;
    function HasBackFinding() : Boolean;
    function HasBandMaterialType() : Boolean;
    function HasBatteriesIncluded() : Boolean;
    function HasBatteriesRequired() : Boolean;
    function HasBatteries() : Boolean;
    function HasBatteryDescription() : Boolean;
    function HasBatteryType() : Boolean;
    function HasBezelMaterialType() : Boolean;
    function HasBinding() : Boolean;
    function HasBrand() : Boolean;
    function HasCalendarType() : Boolean;
    function HasCaseDiameter() : Boolean;
    function HasCaseMaterialType() : Boolean;
    function HasCaseThickness() : Boolean;
    function HasCaseType() : Boolean;
    function HasCatalogNumber() : Boolean;
    function HasCDRWDescription() : Boolean;
    function HasChainType() : Boolean;
    function HasClaspType() : Boolean;
    function HasClothingSize() : Boolean;
    function HasColor() : Boolean;
    function HasCompatibility() : Boolean;
    function HasComputerHardwareType() : Boolean;
    function HasComputerPlatform() : Boolean;
    function HasConnectivity() : Boolean;
    function HasContinuousShootingSpeed() : Boolean;
    function HasCountry() : Boolean;
    function HasCountryOfOrigin() : Boolean;
    function HasCPUManufacturer() : Boolean;
    function HasCPUSpeed() : Boolean;
    function HasCPUType() : Boolean;
    function HasCuisine() : Boolean;
    function HasCustomizable() : Boolean;
    function HasDelayBetweenShots() : Boolean;
    function HasDeliveryOption() : Boolean;
    function HasDepartment() : Boolean;
    function HasDescription() : Boolean;
    function HasDeweyDecimalNumber() : Boolean;
    function HasDialColor() : Boolean;
    function HasDialWindowMaterialType() : Boolean;
    function HasDigitalZoom() : Boolean;
    function HasDisplaySize() : Boolean;
    function HasDrumSetPieceQuantity() : Boolean;
    function HasDVDLayers() : Boolean;
    function HasDVDRWDescription() : Boolean;
    function HasDVDSides() : Boolean;
    function HasDPCI() : Boolean;
    function HasEAN() : Boolean;
    function HasEdition() : Boolean;
    function HasESRBAgeRating() : Boolean;
    function HasExternalDisplaySupportDescription() : Boolean;
    function HasFabricType() : Boolean;
    function HasFaxNumber() : Boolean;
    function HasFirstIssueLeadTime() : Boolean;
    function HasFloppyDiskDriveDescription() : Boolean;
    function HasFixedShippingCharge() : Boolean;
    function HasGemType() : Boolean;
    function HasGraphicsCardInterface() : Boolean;
    function HasGraphicsDescription() : Boolean;
    function HasGraphicsMemorySize() : Boolean;
    function HasGuitarAttribute() : Boolean;
    function HasGuitarBridgeSystem() : Boolean;
    function HasGuitarPickThickness() : Boolean;
    function HasGuitarPickupConfiguration() : Boolean;
    function HasHardDiskCount() : Boolean;
    function HasHardDiskSize() : Boolean;
    function HasHasAutoFocus() : Boolean;
    function HasHasBurstMode() : Boolean;
    function HasHasInCameraEditing() : Boolean;
    function HasHasRedEyeReduction() : Boolean;
    function HasHasSelfTimer() : Boolean;
    function HasHasTripodMount() : Boolean;
    function HasHasVideoOut() : Boolean;
    function HasHasViewfinder() : Boolean;
    function HasHazardousMaterialType() : Boolean;
    function HasHoursOfOperation() : Boolean;
    function HasIncludedSoftware() : Boolean;
    function HasIncludesMp3Player() : Boolean;
    function HasIndications() : Boolean;
    function HasIngredients() : Boolean;
    function HasInstrumentKey() : Boolean;
    function HasIsAutographed() : Boolean;
    function HasISBN() : Boolean;
    function HasIsFragile() : Boolean;
    function HasIsLabCreated() : Boolean;
    function HasIsMemorabilia() : Boolean;
    function HasISOEquivalent() : Boolean;
    function HasIssuesPerYear() : Boolean;
    function HasItemDimensions() : Boolean;
    function HasKeyboardDescription() : Boolean;
    function Has_Label() : Boolean;
    function HasLanguages() : Boolean;
    function HasLegalDisclaimer() : Boolean;
    function HasLineVoltage() : Boolean;
    function HasListPrice() : Boolean;
    function HasMacroFocusRange() : Boolean;
    function HasMagazineType() : Boolean;
    function HasMalletHardness() : Boolean;
    function HasManufacturer() : Boolean;
    function HasManufacturerLaborWarrantyDescription() : Boolean;
    function HasManufacturerMaximumAge() : Boolean;
    function HasManufacturerMinimumAge() : Boolean;
    function HasManufacturerPartsWarrantyDescription() : Boolean;
    function HasMaterialType() : Boolean;
    function HasMaximumAperture() : Boolean;
    function HasMaximumColorDepth() : Boolean;
    function HasMaximumFocalLength() : Boolean;
    function HasMaximumHighResolutionImages() : Boolean;
    function HasMaximumHorizontalResolution() : Boolean;
    function HasMaximumLowResolutionImages() : Boolean;
    function HasMaximumResolution() : Boolean;
    function HasMaximumShutterSpeed() : Boolean;
    function HasMaximumVerticalResolution() : Boolean;
    function HasMaximumWeightRecommendation() : Boolean;
    function HasMemorySlotsAvailable() : Boolean;
    function HasMetalStamp() : Boolean;
    function HasMetalType() : Boolean;
    function HasMiniMovieDescription() : Boolean;
    function HasMinimumFocalLength() : Boolean;
    function HasMinimumShutterSpeed() : Boolean;
    function HasModel() : Boolean;
    function HasModelYear() : Boolean;
    function HasModemDescription() : Boolean;
    function HasMonitorSize() : Boolean;
    function HasMonitorViewableDiagonalSize() : Boolean;
    function HasMouseDescription() : Boolean;
    function HasMPN() : Boolean;
    function HasMusicalStyle() : Boolean;
    function HasNativeResolution() : Boolean;
    function HasNeighborhood() : Boolean;
    function HasNetworkInterfaceDescription() : Boolean;
    function HasNotebookDisplayTechnology() : Boolean;
    function HasNotebookPointingDeviceDescription() : Boolean;
    function HasNumberOfDiscs() : Boolean;
    function HasNumberOfIssues() : Boolean;
    function HasNumberOfItems() : Boolean;
    function HasNumberOfKeys() : Boolean;
    function HasNumberOfPages() : Boolean;
    function HasNumberOfPearls() : Boolean;
    function HasNumberOfRapidFireShots() : Boolean;
    function HasNumberOfStones() : Boolean;
    function HasNumberOfStrings() : Boolean;
    function HasNumberOfTracks() : Boolean;
    function HasOpticalZoom() : Boolean;
    function HasOriginalReleaseDate() : Boolean;
    function HasOutputWattage() : Boolean;
    function HasPackageDimensions() : Boolean;
    function HasPearlLustre() : Boolean;
    function HasPearlMinimumColor() : Boolean;
    function HasPearlShape() : Boolean;
    function HasPearlStringingMethod() : Boolean;
    function HasPearlSurfaceBlemishes() : Boolean;
    function HasPearlType() : Boolean;
    function HasPearlUniformity() : Boolean;
    function HasPhoneNumber() : Boolean;
    function HasPriceRating() : Boolean;
    function HasProcessorCount() : Boolean;
    function HasProductGroup() : Boolean;
    function HasPromotionalTag() : Boolean;
    function HasPOBoxShippingExcluded() : Boolean;
    function HasPublicationDate() : Boolean;
    function HasPublisher() : Boolean;
    function HasReadingLevel() : Boolean;
    function HasRecorderTrackCount() : Boolean;
    function HasRegionCode() : Boolean;
    function HasRegionOfOrigin() : Boolean;
    function HasReleaseDate() : Boolean;
    function HasRemovableMemory() : Boolean;
    function HasResolutionModes() : Boolean;
    function HasReturnPolicy() : Boolean;
    function HasRingSize() : Boolean;
    function HasSafetyWarning() : Boolean;
    function HasSalesRestriction() : Boolean;
    function HasSecondaryCacheSize() : Boolean;
    function HasSettingType() : Boolean;
    function HasSize() : Boolean;
    function HasSKU() : Boolean;
    function HasSoldInStores() : Boolean;
    function HasSizePerPearl() : Boolean;
    function HasSkillLevel() : Boolean;
    function HasSoundCardDescription() : Boolean;
    function HasSpeakerCount() : Boolean;
    function HasSpeakerDescription() : Boolean;
    function HasStoneClarity() : Boolean;
    function HasStoneColor() : Boolean;
    function HasStoneCut() : Boolean;
    function HasStoneShape() : Boolean;
    function HasStoneWeight() : Boolean;
    function HasStudio() : Boolean;
    function HasSubscriptionLength() : Boolean;
    function HasSystemBusSpeed() : Boolean;
    function HasSystemMemorySizeMax() : Boolean;
    function HasSystemMemorySize() : Boolean;
    function HasSystemMemoryType() : Boolean;
    function HasTellingPageIndicator() : Boolean;
    function HasTheatricalReleaseDate() : Boolean;
    function HasTitle() : Boolean;
    function HasTotalDiamondWeight() : Boolean;
    function HasTotalExternalBaysFree() : Boolean;
    function HasTotalFirewirePorts() : Boolean;
    function HasTotalGemWeight() : Boolean;
    function HasTotalInternalBaysFree() : Boolean;
    function HasTotalMetalWeight() : Boolean;
    function HasTotalNTSCPALPorts() : Boolean;
    function HasTotalParallelPorts() : Boolean;
    function HasTotalPCCardSlots() : Boolean;
    function HasTotalPCISlotsFree() : Boolean;
    function HasTotalSerialPorts() : Boolean;
    function HasTotalSVideoOutPorts() : Boolean;
    function HasTotalUSB2Ports() : Boolean;
    function HasTotalUSBPorts() : Boolean;
    function HasTotalVGAOutPorts() : Boolean;
    function HasUPC() : Boolean;
    function HasVariationDenomination() : Boolean;
    function HasVariationDescription() : Boolean;
    function HasVendorRebate() : Boolean;
    function HasWarranty() : Boolean;
    function HasWatchMovementType() : Boolean;
    function HasWebsiteBuyability() : Boolean;
    function HasWaterResistanceDepth() : Boolean;
    function HasWirelessMicrophoneFrequency() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Actor : MerchantItemAttributes_ActorArray read FActor write FActor;
    property Address : Address read FAddress write FAddress stored HasAddress;
    property AmazonMaximumAge : DecimalWithUnits read FAmazonMaximumAge write FAmazonMaximumAge stored HasAmazonMaximumAge;
    property AmazonMinimumAge : DecimalWithUnits read FAmazonMinimumAge write FAmazonMinimumAge stored HasAmazonMinimumAge;
    property ApertureModes : string read FApertureModes write FApertureModes stored HasApertureModes;
    property Artist : MerchantItemAttributes_ArtistArray read FArtist write FArtist;
    property AspectRatio : string read FAspectRatio write FAspectRatio stored HasAspectRatio;
    property AssemblyInstructions : string read FAssemblyInstructions write FAssemblyInstructions stored HasAssemblyInstructions;
    property AssemblyRequired : string read FAssemblyRequired write FAssemblyRequired stored HasAssemblyRequired;
    property AudienceRating : string read FAudienceRating write FAudienceRating stored HasAudienceRating;
    property AudioFormat : MerchantItemAttributes_AudioFormatArray read FAudioFormat write FAudioFormat;
    property Author : MerchantItemAttributes_AuthorArray read FAuthor write FAuthor;
    property BackFinding : string read FBackFinding write FBackFinding stored HasBackFinding;
    property BandMaterialType : string read FBandMaterialType write FBandMaterialType stored HasBandMaterialType;
    property BatteriesIncluded : string read FBatteriesIncluded write FBatteriesIncluded stored HasBatteriesIncluded;
    property BatteriesRequired : string read FBatteriesRequired write FBatteriesRequired stored HasBatteriesRequired;
    property Batteries : NonNegativeIntegerWithUnits read FBatteries write FBatteries stored HasBatteries;
    property BatteryDescription : string read FBatteryDescription write FBatteryDescription stored HasBatteryDescription;
    property BatteryType : string read FBatteryType write FBatteryType stored HasBatteryType;
    property BezelMaterialType : string read FBezelMaterialType write FBezelMaterialType stored HasBezelMaterialType;
    property Binding : string read FBinding write FBinding stored HasBinding;
    property Brand : string read FBrand write FBrand stored HasBrand;
    property CalendarType : string read FCalendarType write FCalendarType stored HasCalendarType;
    property CameraManualFeatures : MerchantItemAttributes_CameraManualFeaturesArray read FCameraManualFeatures write FCameraManualFeatures;
    property CaseDiameter : DecimalWithUnits read FCaseDiameter write FCaseDiameter stored HasCaseDiameter;
    property CaseMaterialType : string read FCaseMaterialType write FCaseMaterialType stored HasCaseMaterialType;
    property CaseThickness : DecimalWithUnits read FCaseThickness write FCaseThickness stored HasCaseThickness;
    property CaseType : string read FCaseType write FCaseType stored HasCaseType;
    property CatalogNumber : string read FCatalogNumber write FCatalogNumber stored HasCatalogNumber;
    property CDRWDescription : string read FCDRWDescription write FCDRWDescription stored HasCDRWDescription;
    property ChainType : string read FChainType write FChainType stored HasChainType;
    property ClaspType : string read FClaspType write FClaspType stored HasClaspType;
    property ClothingSize : string read FClothingSize write FClothingSize stored HasClothingSize;
    property Color : string read FColor write FColor stored HasColor;
    property Compatibility : string read FCompatibility write FCompatibility stored HasCompatibility;
    property ComputerHardwareType : string read FComputerHardwareType write FComputerHardwareType stored HasComputerHardwareType;
    property ComputerPlatform : string read FComputerPlatform write FComputerPlatform stored HasComputerPlatform;
    property Connectivity : string read FConnectivity write FConnectivity stored HasConnectivity;
    property ContinuousShootingSpeed : DecimalWithUnits read FContinuousShootingSpeed write FContinuousShootingSpeed stored HasContinuousShootingSpeed;
    property Country : string read FCountry write FCountry stored HasCountry;
    property CountryOfOrigin : string read FCountryOfOrigin write FCountryOfOrigin stored HasCountryOfOrigin;
    property CPUManufacturer : string read FCPUManufacturer write FCPUManufacturer stored HasCPUManufacturer;
    property CPUSpeed : DecimalWithUnits read FCPUSpeed write FCPUSpeed stored HasCPUSpeed;
    property CPUType : string read FCPUType write FCPUType stored HasCPUType;
    property Creator : MerchantItemAttributes_CreatorArray read FCreator write FCreator;
    property Cuisine : string read FCuisine write FCuisine stored HasCuisine;
    property Customizable : string read FCustomizable write FCustomizable stored HasCustomizable;
    property DelayBetweenShots : DecimalWithUnits read FDelayBetweenShots write FDelayBetweenShots stored HasDelayBetweenShots;
    property DeliveryOption : string read FDeliveryOption write FDeliveryOption stored HasDeliveryOption;
    property Department : string read FDepartment write FDepartment stored HasDepartment;
    property Description : string read FDescription write FDescription stored HasDescription;
    property DeweyDecimalNumber : string read FDeweyDecimalNumber write FDeweyDecimalNumber stored HasDeweyDecimalNumber;
    property DialColor : string read FDialColor write FDialColor stored HasDialColor;
    property DialWindowMaterialType : string read FDialWindowMaterialType write FDialWindowMaterialType stored HasDialWindowMaterialType;
    property DigitalZoom : DecimalWithUnits read FDigitalZoom write FDigitalZoom stored HasDigitalZoom;
    property Director : MerchantItemAttributes_DirectorArray read FDirector write FDirector;
    property DisplaySize : DecimalWithUnits read FDisplaySize write FDisplaySize stored HasDisplaySize;
    property DrumSetPieceQuantity : nonNegativeInteger read FDrumSetPieceQuantity write FDrumSetPieceQuantity stored HasDrumSetPieceQuantity;
    property DVDLayers : nonNegativeInteger read FDVDLayers write FDVDLayers stored HasDVDLayers;
    property DVDRWDescription : string read FDVDRWDescription write FDVDRWDescription stored HasDVDRWDescription;
    property DVDSides : nonNegativeInteger read FDVDSides write FDVDSides stored HasDVDSides;
    property DPCI : string read FDPCI write FDPCI stored HasDPCI;
    property EAN : string read FEAN write FEAN stored HasEAN;
    property Edition : string read FEdition write FEdition stored HasEdition;
    property ESRBAgeRating : string read FESRBAgeRating write FESRBAgeRating stored HasESRBAgeRating;
    property ExternalDisplaySupportDescription : string read FExternalDisplaySupportDescription write FExternalDisplaySupportDescription stored HasExternalDisplaySupportDescription;
    property FabricType : string read FFabricType write FFabricType stored HasFabricType;
    property FaxNumber : string read FFaxNumber write FFaxNumber stored HasFaxNumber;
    property Feature : MerchantItemAttributes_FeatureArray read FFeature write FFeature;
    property FirstIssueLeadTime : StringWithUnits read FFirstIssueLeadTime write FFirstIssueLeadTime stored HasFirstIssueLeadTime;
    property FloppyDiskDriveDescription : string read FFloppyDiskDriveDescription write FFloppyDiskDriveDescription stored HasFloppyDiskDriveDescription;
    property Format : MerchantItemAttributes_FormatArray read FFormat write FFormat;
    property FixedShippingCharge : Price read FFixedShippingCharge write FFixedShippingCharge stored HasFixedShippingCharge;
    property GemType : string read FGemType write FGemType stored HasGemType;
    property GraphicsCardInterface : string read FGraphicsCardInterface write FGraphicsCardInterface stored HasGraphicsCardInterface;
    property GraphicsDescription : string read FGraphicsDescription write FGraphicsDescription stored HasGraphicsDescription;
    property GraphicsMemorySize : DecimalWithUnits read FGraphicsMemorySize write FGraphicsMemorySize stored HasGraphicsMemorySize;
    property GuitarAttribute : string read FGuitarAttribute write FGuitarAttribute stored HasGuitarAttribute;
    property GuitarBridgeSystem : string read FGuitarBridgeSystem write FGuitarBridgeSystem stored HasGuitarBridgeSystem;
    property GuitarPickThickness : string read FGuitarPickThickness write FGuitarPickThickness stored HasGuitarPickThickness;
    property GuitarPickupConfiguration : string read FGuitarPickupConfiguration write FGuitarPickupConfiguration stored HasGuitarPickupConfiguration;
    property HardDiskCount : nonNegativeInteger read FHardDiskCount write FHardDiskCount stored HasHardDiskCount;
    property HardDiskSize : NonNegativeIntegerWithUnits read FHardDiskSize write FHardDiskSize stored HasHardDiskSize;
    property HasAutoFocus : boolean read FHasAutoFocus write FHasAutoFocus stored HasHasAutoFocus;
    property HasBurstMode : boolean read FHasBurstMode write FHasBurstMode stored HasHasBurstMode;
    property HasInCameraEditing : boolean read FHasInCameraEditing write FHasInCameraEditing stored HasHasInCameraEditing;
    property HasRedEyeReduction : boolean read FHasRedEyeReduction write FHasRedEyeReduction stored HasHasRedEyeReduction;
    property HasSelfTimer : boolean read FHasSelfTimer write FHasSelfTimer stored HasHasSelfTimer;
    property HasTripodMount : boolean read FHasTripodMount write FHasTripodMount stored HasHasTripodMount;
    property HasVideoOut : boolean read FHasVideoOut write FHasVideoOut stored HasHasVideoOut;
    property HasViewfinder : boolean read FHasViewfinder write FHasViewfinder stored HasHasViewfinder;
    property HazardousMaterialType : string read FHazardousMaterialType write FHazardousMaterialType stored HasHazardousMaterialType;
    property HoursOfOperation : string read FHoursOfOperation write FHoursOfOperation stored HasHoursOfOperation;
    property IncludedSoftware : string read FIncludedSoftware write FIncludedSoftware stored HasIncludedSoftware;
    property IncludesMp3Player : boolean read FIncludesMp3Player write FIncludesMp3Player stored HasIncludesMp3Player;
    property Indications : string read FIndications write FIndications stored HasIndications;
    property Ingredients : string read FIngredients write FIngredients stored HasIngredients;
    property InstrumentKey : string read FInstrumentKey write FInstrumentKey stored HasInstrumentKey;
    property IsAutographed : boolean read FIsAutographed write FIsAutographed stored HasIsAutographed;
    property ISBN : string read FISBN write FISBN stored HasISBN;
    property IsFragile : boolean read FIsFragile write FIsFragile stored HasIsFragile;
    property IsLabCreated : boolean read FIsLabCreated write FIsLabCreated stored HasIsLabCreated;
    property IsMemorabilia : boolean read FIsMemorabilia write FIsMemorabilia stored HasIsMemorabilia;
    property ISOEquivalent : NonNegativeIntegerWithUnits read FISOEquivalent write FISOEquivalent stored HasISOEquivalent;
    property IssuesPerYear : string read FIssuesPerYear write FIssuesPerYear stored HasIssuesPerYear;
    property ItemDimensions : MerchantItemAttributes_ItemDimensions_Type read FItemDimensions write FItemDimensions stored HasItemDimensions;
    property KeyboardDescription : string read FKeyboardDescription write FKeyboardDescription stored HasKeyboardDescription;
    property _Label : string read F_Label write F_Label stored Has_Label;
    property Languages : MerchantItemAttributes_Languages_Type read FLanguages write FLanguages stored HasLanguages;
    property LegalDisclaimer : string read FLegalDisclaimer write FLegalDisclaimer stored HasLegalDisclaimer;
    property LineVoltage : string read FLineVoltage write FLineVoltage stored HasLineVoltage;
    property ListPrice : Price read FListPrice write FListPrice stored HasListPrice;
    property MacroFocusRange : string read FMacroFocusRange write FMacroFocusRange stored HasMacroFocusRange;
    property MagazineType : string read FMagazineType write FMagazineType stored HasMagazineType;
    property MalletHardness : string read FMalletHardness write FMalletHardness stored HasMalletHardness;
    property Manufacturer : string read FManufacturer write FManufacturer stored HasManufacturer;
    property ManufacturerLaborWarrantyDescription : string read FManufacturerLaborWarrantyDescription write FManufacturerLaborWarrantyDescription stored HasManufacturerLaborWarrantyDescription;
    property ManufacturerMaximumAge : DecimalWithUnits read FManufacturerMaximumAge write FManufacturerMaximumAge stored HasManufacturerMaximumAge;
    property ManufacturerMinimumAge : DecimalWithUnits read FManufacturerMinimumAge write FManufacturerMinimumAge stored HasManufacturerMinimumAge;
    property ManufacturerPartsWarrantyDescription : string read FManufacturerPartsWarrantyDescription write FManufacturerPartsWarrantyDescription stored HasManufacturerPartsWarrantyDescription;
    property MaterialType : string read FMaterialType write FMaterialType stored HasMaterialType;
    property MaximumAperture : DecimalWithUnits read FMaximumAperture write FMaximumAperture stored HasMaximumAperture;
    property MaximumColorDepth : string read FMaximumColorDepth write FMaximumColorDepth stored HasMaximumColorDepth;
    property MaximumFocalLength : DecimalWithUnits read FMaximumFocalLength write FMaximumFocalLength stored HasMaximumFocalLength;
    property MaximumHighResolutionImages : NonNegativeIntegerWithUnits read FMaximumHighResolutionImages write FMaximumHighResolutionImages stored HasMaximumHighResolutionImages;
    property MaximumHorizontalResolution : NonNegativeIntegerWithUnits read FMaximumHorizontalResolution write FMaximumHorizontalResolution stored HasMaximumHorizontalResolution;
    property MaximumLowResolutionImages : string read FMaximumLowResolutionImages write FMaximumLowResolutionImages stored HasMaximumLowResolutionImages;
    property MaximumResolution : DecimalWithUnits read FMaximumResolution write FMaximumResolution stored HasMaximumResolution;
    property MaximumShutterSpeed : DecimalWithUnits read FMaximumShutterSpeed write FMaximumShutterSpeed stored HasMaximumShutterSpeed;
    property MaximumVerticalResolution : NonNegativeIntegerWithUnits read FMaximumVerticalResolution write FMaximumVerticalResolution stored HasMaximumVerticalResolution;
    property MaximumWeightRecommendation : DecimalWithUnits read FMaximumWeightRecommendation write FMaximumWeightRecommendation stored HasMaximumWeightRecommendation;
    property MemorySlotsAvailable : nonNegativeInteger read FMemorySlotsAvailable write FMemorySlotsAvailable stored HasMemorySlotsAvailable;
    property MetalStamp : string read FMetalStamp write FMetalStamp stored HasMetalStamp;
    property MetalType : string read FMetalType write FMetalType stored HasMetalType;
    property MiniMovieDescription : string read FMiniMovieDescription write FMiniMovieDescription stored HasMiniMovieDescription;
    property MinimumFocalLength : DecimalWithUnits read FMinimumFocalLength write FMinimumFocalLength stored HasMinimumFocalLength;
    property MinimumShutterSpeed : DecimalWithUnits read FMinimumShutterSpeed write FMinimumShutterSpeed stored HasMinimumShutterSpeed;
    property Model : string read FModel write FModel stored HasModel;
    property ModelYear : nonNegativeInteger read FModelYear write FModelYear stored HasModelYear;
    property ModemDescription : string read FModemDescription write FModemDescription stored HasModemDescription;
    property MonitorSize : DecimalWithUnits read FMonitorSize write FMonitorSize stored HasMonitorSize;
    property MonitorViewableDiagonalSize : DecimalWithUnits read FMonitorViewableDiagonalSize write FMonitorViewableDiagonalSize stored HasMonitorViewableDiagonalSize;
    property MouseDescription : string read FMouseDescription write FMouseDescription stored HasMouseDescription;
    property MPN : string read FMPN write FMPN stored HasMPN;
    property MusicalStyle : string read FMusicalStyle write FMusicalStyle stored HasMusicalStyle;
    property NativeResolution : string read FNativeResolution write FNativeResolution stored HasNativeResolution;
    property Neighborhood : string read FNeighborhood write FNeighborhood stored HasNeighborhood;
    property NetworkInterfaceDescription : string read FNetworkInterfaceDescription write FNetworkInterfaceDescription stored HasNetworkInterfaceDescription;
    property NotebookDisplayTechnology : string read FNotebookDisplayTechnology write FNotebookDisplayTechnology stored HasNotebookDisplayTechnology;
    property NotebookPointingDeviceDescription : string read FNotebookPointingDeviceDescription write FNotebookPointingDeviceDescription stored HasNotebookPointingDeviceDescription;
    property NumberOfDiscs : nonNegativeInteger read FNumberOfDiscs write FNumberOfDiscs stored HasNumberOfDiscs;
    property NumberOfIssues : nonNegativeInteger read FNumberOfIssues write FNumberOfIssues stored HasNumberOfIssues;
    property NumberOfItems : nonNegativeInteger read FNumberOfItems write FNumberOfItems stored HasNumberOfItems;
    property NumberOfKeys : nonNegativeInteger read FNumberOfKeys write FNumberOfKeys stored HasNumberOfKeys;
    property NumberOfPages : nonNegativeInteger read FNumberOfPages write FNumberOfPages stored HasNumberOfPages;
    property NumberOfPearls : nonNegativeInteger read FNumberOfPearls write FNumberOfPearls stored HasNumberOfPearls;
    property NumberOfRapidFireShots : nonNegativeInteger read FNumberOfRapidFireShots write FNumberOfRapidFireShots stored HasNumberOfRapidFireShots;
    property NumberOfStones : nonNegativeInteger read FNumberOfStones write FNumberOfStones stored HasNumberOfStones;
    property NumberOfStrings : nonNegativeInteger read FNumberOfStrings write FNumberOfStrings stored HasNumberOfStrings;
    property NumberOfTracks : nonNegativeInteger read FNumberOfTracks write FNumberOfTracks stored HasNumberOfTracks;
    property OpticalZoom : DecimalWithUnits read FOpticalZoom write FOpticalZoom stored HasOpticalZoom;
    property OriginalReleaseDate : string read FOriginalReleaseDate write FOriginalReleaseDate stored HasOriginalReleaseDate;
    property OutputWattage : nonNegativeInteger read FOutputWattage write FOutputWattage stored HasOutputWattage;
    property PackageDimensions : MerchantItemAttributes_PackageDimensions_Type read FPackageDimensions write FPackageDimensions stored HasPackageDimensions;
    property PearlLustre : string read FPearlLustre write FPearlLustre stored HasPearlLustre;
    property PearlMinimumColor : string read FPearlMinimumColor write FPearlMinimumColor stored HasPearlMinimumColor;
    property PearlShape : string read FPearlShape write FPearlShape stored HasPearlShape;
    property PearlStringingMethod : string read FPearlStringingMethod write FPearlStringingMethod stored HasPearlStringingMethod;
    property PearlSurfaceBlemishes : string read FPearlSurfaceBlemishes write FPearlSurfaceBlemishes stored HasPearlSurfaceBlemishes;
    property PearlType : string read FPearlType write FPearlType stored HasPearlType;
    property PearlUniformity : string read FPearlUniformity write FPearlUniformity stored HasPearlUniformity;
    property PhoneNumber : string read FPhoneNumber write FPhoneNumber stored HasPhoneNumber;
    property PhotoFlashType : MerchantItemAttributes_PhotoFlashTypeArray read FPhotoFlashType write FPhotoFlashType;
    property PictureFormat : MerchantItemAttributes_PictureFormatArray read FPictureFormat write FPictureFormat;
    property Platform : MerchantItemAttributes_PlatformArray read FPlatform write FPlatform;
    property PriceRating : nonNegativeInteger read FPriceRating write FPriceRating stored HasPriceRating;
    property ProcessorCount : nonNegativeInteger read FProcessorCount write FProcessorCount stored HasProcessorCount;
    property ProductGroup : string read FProductGroup write FProductGroup stored HasProductGroup;
    property PromotionalTag : string read FPromotionalTag write FPromotionalTag stored HasPromotionalTag;
    property POBoxShippingExcluded : string read FPOBoxShippingExcluded write FPOBoxShippingExcluded stored HasPOBoxShippingExcluded;
    property PublicationDate : string read FPublicationDate write FPublicationDate stored HasPublicationDate;
    property Publisher : string read FPublisher write FPublisher stored HasPublisher;
    property PurchasingChannel : MerchantItemAttributes_PurchasingChannelArray read FPurchasingChannel write FPurchasingChannel;
    property ReadingLevel : string read FReadingLevel write FReadingLevel stored HasReadingLevel;
    property RecorderTrackCount : nonNegativeInteger read FRecorderTrackCount write FRecorderTrackCount stored HasRecorderTrackCount;
    property RegionCode : string read FRegionCode write FRegionCode stored HasRegionCode;
    property RegionOfOrigin : string read FRegionOfOrigin write FRegionOfOrigin stored HasRegionOfOrigin;
    property ReleaseDate : string read FReleaseDate write FReleaseDate stored HasReleaseDate;
    property ReturnMethod : MerchantItemAttributes_ReturnMethodArray read FReturnMethod write FReturnMethod;
    property RemovableMemory : string read FRemovableMemory write FRemovableMemory stored HasRemovableMemory;
    property ResolutionModes : string read FResolutionModes write FResolutionModes stored HasResolutionModes;
    property ReturnPolicy : string read FReturnPolicy write FReturnPolicy stored HasReturnPolicy;
    property RingSize : string read FRingSize write FRingSize stored HasRingSize;
    property SafetyWarning : string read FSafetyWarning write FSafetyWarning stored HasSafetyWarning;
    property SalesRestriction : string read FSalesRestriction write FSalesRestriction stored HasSalesRestriction;
    property SecondaryCacheSize : NonNegativeIntegerWithUnits read FSecondaryCacheSize write FSecondaryCacheSize stored HasSecondaryCacheSize;
    property SettingType : string read FSettingType write FSettingType stored HasSettingType;
    property Size : string read FSize write FSize stored HasSize;
    property SKU : string read FSKU write FSKU stored HasSKU;
    property SoldInStores : string read FSoldInStores write FSoldInStores stored HasSoldInStores;
    property SizePerPearl : string read FSizePerPearl write FSizePerPearl stored HasSizePerPearl;
    property SkillLevel : string read FSkillLevel write FSkillLevel stored HasSkillLevel;
    property SoundCardDescription : string read FSoundCardDescription write FSoundCardDescription stored HasSoundCardDescription;
    property SpeakerCount : nonNegativeInteger read FSpeakerCount write FSpeakerCount stored HasSpeakerCount;
    property SpeakerDescription : string read FSpeakerDescription write FSpeakerDescription stored HasSpeakerDescription;
    property SpecialFeatures : MerchantItemAttributes_SpecialFeaturesArray read FSpecialFeatures write FSpecialFeatures;
    property StoneClarity : string read FStoneClarity write FStoneClarity stored HasStoneClarity;
    property StoneColor : string read FStoneColor write FStoneColor stored HasStoneColor;
    property StoneCut : string read FStoneCut write FStoneCut stored HasStoneCut;
    property StoneShape : string read FStoneShape write FStoneShape stored HasStoneShape;
    property StoneWeight : DecimalWithUnits read FStoneWeight write FStoneWeight stored HasStoneWeight;
    property Studio : string read FStudio write FStudio stored HasStudio;
    property SubscriptionLength : NonNegativeIntegerWithUnits read FSubscriptionLength write FSubscriptionLength stored HasSubscriptionLength;
    property SupportedImageType : MerchantItemAttributes_SupportedImageTypeArray read FSupportedImageType write FSupportedImageType;
    property SystemBusSpeed : DecimalWithUnits read FSystemBusSpeed write FSystemBusSpeed stored HasSystemBusSpeed;
    property SystemMemorySizeMax : DecimalWithUnits read FSystemMemorySizeMax write FSystemMemorySizeMax stored HasSystemMemorySizeMax;
    property SystemMemorySize : DecimalWithUnits read FSystemMemorySize write FSystemMemorySize stored HasSystemMemorySize;
    property SystemMemoryType : string read FSystemMemoryType write FSystemMemoryType stored HasSystemMemoryType;
    property TellingPageIndicator : string read FTellingPageIndicator write FTellingPageIndicator stored HasTellingPageIndicator;
    property TheatricalReleaseDate : string read FTheatricalReleaseDate write FTheatricalReleaseDate stored HasTheatricalReleaseDate;
    property Title : string read FTitle write FTitle stored HasTitle;
    property TotalDiamondWeight : DecimalWithUnits read FTotalDiamondWeight write FTotalDiamondWeight stored HasTotalDiamondWeight;
    property TotalExternalBaysFree : nonNegativeInteger read FTotalExternalBaysFree write FTotalExternalBaysFree stored HasTotalExternalBaysFree;
    property TotalFirewirePorts : nonNegativeInteger read FTotalFirewirePorts write FTotalFirewirePorts stored HasTotalFirewirePorts;
    property TotalGemWeight : DecimalWithUnits read FTotalGemWeight write FTotalGemWeight stored HasTotalGemWeight;
    property TotalInternalBaysFree : nonNegativeInteger read FTotalInternalBaysFree write FTotalInternalBaysFree stored HasTotalInternalBaysFree;
    property TotalMetalWeight : DecimalWithUnits read FTotalMetalWeight write FTotalMetalWeight stored HasTotalMetalWeight;
    property TotalNTSCPALPorts : nonNegativeInteger read FTotalNTSCPALPorts write FTotalNTSCPALPorts stored HasTotalNTSCPALPorts;
    property TotalParallelPorts : nonNegativeInteger read FTotalParallelPorts write FTotalParallelPorts stored HasTotalParallelPorts;
    property TotalPCCardSlots : nonNegativeInteger read FTotalPCCardSlots write FTotalPCCardSlots stored HasTotalPCCardSlots;
    property TotalPCISlotsFree : nonNegativeInteger read FTotalPCISlotsFree write FTotalPCISlotsFree stored HasTotalPCISlotsFree;
    property TotalSerialPorts : nonNegativeInteger read FTotalSerialPorts write FTotalSerialPorts stored HasTotalSerialPorts;
    property TotalSVideoOutPorts : nonNegativeInteger read FTotalSVideoOutPorts write FTotalSVideoOutPorts stored HasTotalSVideoOutPorts;
    property TotalUSB2Ports : nonNegativeInteger read FTotalUSB2Ports write FTotalUSB2Ports stored HasTotalUSB2Ports;
    property TotalUSBPorts : nonNegativeInteger read FTotalUSBPorts write FTotalUSBPorts stored HasTotalUSBPorts;
    property TotalVGAOutPorts : nonNegativeInteger read FTotalVGAOutPorts write FTotalVGAOutPorts stored HasTotalVGAOutPorts;
    property UPC : string read FUPC write FUPC stored HasUPC;
    property VariationDenomination : string read FVariationDenomination write FVariationDenomination stored HasVariationDenomination;
    property VariationDescription : string read FVariationDescription write FVariationDescription stored HasVariationDescription;
    property VendorRebate : MerchantItemAttributes_VendorRebate_Type read FVendorRebate write FVendorRebate stored HasVendorRebate;
    property Warranty : string read FWarranty write FWarranty stored HasWarranty;
    property WatchMovementType : string read FWatchMovementType write FWatchMovementType stored HasWatchMovementType;
    property WebsiteBuyability : string read FWebsiteBuyability write FWebsiteBuyability stored HasWebsiteBuyability;
    property WaterResistanceDepth : DecimalWithUnits read FWaterResistanceDepth write FWaterResistanceDepth stored HasWaterResistanceDepth;
    property WirelessMicrophoneFrequency : nonNegativeInteger read FWirelessMicrophoneFrequency write FWirelessMicrophoneFrequency stored HasWirelessMicrophoneFrequency;
  end;

  OfferSummary_Type = class(TBaseComplexRemotable)
  private
    FLowestNewPrice : Price;
    FLowestUsedPrice : Price;
    FLowestCollectiblePrice : Price;
    FLowestRefurbishedPrice : Price;
    FTotalNew : string;
    FTotalUsed : string;
    FTotalCollectible : string;
    FTotalRefurbished : string;
  private
    function HasLowestNewPrice() : Boolean;
    function HasLowestUsedPrice() : Boolean;
    function HasLowestCollectiblePrice() : Boolean;
    function HasLowestRefurbishedPrice() : Boolean;
    function HasTotalNew() : Boolean;
    function HasTotalUsed() : Boolean;
    function HasTotalCollectible() : Boolean;
    function HasTotalRefurbished() : Boolean;
  public
    destructor Destroy();override;
  published
    property LowestNewPrice : Price read FLowestNewPrice write FLowestNewPrice stored HasLowestNewPrice;
    property LowestUsedPrice : Price read FLowestUsedPrice write FLowestUsedPrice stored HasLowestUsedPrice;
    property LowestCollectiblePrice : Price read FLowestCollectiblePrice write FLowestCollectiblePrice stored HasLowestCollectiblePrice;
    property LowestRefurbishedPrice : Price read FLowestRefurbishedPrice write FLowestRefurbishedPrice stored HasLowestRefurbishedPrice;
    property TotalNew : string read FTotalNew write FTotalNew stored HasTotalNew;
    property TotalUsed : string read FTotalUsed write FTotalUsed stored HasTotalUsed;
    property TotalCollectible : string read FTotalCollectible write FTotalCollectible stored HasTotalCollectible;
    property TotalRefurbished : string read FTotalRefurbished write FTotalRefurbished stored HasTotalRefurbished;
  end;

  Offers_Type = class(TBaseComplexRemotable)
  private
    FTotalOffers : nonNegativeInteger;
    FTotalOfferPages : nonNegativeInteger;
    FOffer : Offers_OfferArray;
  private
    function HasTotalOffers() : Boolean;
    function HasTotalOfferPages() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property TotalOffers : nonNegativeInteger read FTotalOffers write FTotalOffers stored HasTotalOffers;
    property TotalOfferPages : nonNegativeInteger read FTotalOfferPages write FTotalOfferPages stored HasTotalOfferPages;
    property Offer : Offers_OfferArray read FOffer write FOffer;
  end;

  VariationSummary_Type = class(TBaseComplexRemotable)
  private
    FLowestPrice : Price;
    FHighestPrice : Price;
    FLowestSalePrice : Price;
    FHighestSalePrice : Price;
    FSingleMerchantId : string;
  private
    function HasLowestPrice() : Boolean;
    function HasHighestPrice() : Boolean;
    function HasLowestSalePrice() : Boolean;
    function HasHighestSalePrice() : Boolean;
    function HasSingleMerchantId() : Boolean;
  public
    destructor Destroy();override;
  published
    property LowestPrice : Price read FLowestPrice write FLowestPrice stored HasLowestPrice;
    property HighestPrice : Price read FHighestPrice write FHighestPrice stored HasHighestPrice;
    property LowestSalePrice : Price read FLowestSalePrice write FLowestSalePrice stored HasLowestSalePrice;
    property HighestSalePrice : Price read FHighestSalePrice write FHighestSalePrice stored HasHighestSalePrice;
    property SingleMerchantId : string read FSingleMerchantId write FSingleMerchantId stored HasSingleMerchantId;
  end;

  Variations_Type = class(TBaseComplexRemotable)
  private
    FTotalVariations : nonNegativeInteger;
    FTotalVariationPages : nonNegativeInteger;
    FVariationDimensions : VariationDimensions_Type;
    F_Item : Variations__ItemArray;
  private
    function HasTotalVariations() : Boolean;
    function HasTotalVariationPages() : Boolean;
    function HasVariationDimensions() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property TotalVariations : nonNegativeInteger read FTotalVariations write FTotalVariations stored HasTotalVariations;
    property TotalVariationPages : nonNegativeInteger read FTotalVariationPages write FTotalVariationPages stored HasTotalVariationPages;
    property VariationDimensions : VariationDimensions_Type read FVariationDimensions write FVariationDimensions stored HasVariationDimensions;
    property _Item : Variations__ItemArray read F_Item write F_Item;
  end;

  SearchInside_Type = class(TBaseComplexRemotable)
  private
    FTotalExcerpts : nonNegativeInteger;
    FExcerpt : SearchInside_Excerpt_Type;
  private
    function HasTotalExcerpts() : Boolean;
    function HasExcerpt() : Boolean;
  public
    destructor Destroy();override;
  published
    property TotalExcerpts : nonNegativeInteger read FTotalExcerpts write FTotalExcerpts stored HasTotalExcerpts;
    property Excerpt : SearchInside_Excerpt_Type read FExcerpt write FExcerpt stored HasExcerpt;
  end;

  Item_AlternateVersions_Type_AlternateVersion_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FTitle : string;
    FBinding : string;
  private
    function HasTitle() : Boolean;
    function HasBinding() : Boolean;
  published
    property ASIN : string read FASIN write FASIN;
    property Title : string read FTitle write FTitle stored HasTitle;
    property Binding : string read FBinding write FBinding stored HasBinding;
  end;

  Offer_Type = class(TBaseComplexRemotable)
  private
    FMerchant : Merchant_Type;
    FSeller : Seller_Type;
    FOfferAttributes : OfferAttributes_Type;
    FOfferListing : Offer_OfferListingArray;
    FLoyaltyPoints : LoyaltyPoints_Type;
    FPromotions : Promotions_Type;
  private
    function HasMerchant() : Boolean;
    function HasSeller() : Boolean;
    function HasOfferAttributes() : Boolean;
    function HasLoyaltyPoints() : Boolean;
    function HasPromotions() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Merchant : Merchant_Type read FMerchant write FMerchant stored HasMerchant;
    property Seller : Seller_Type read FSeller write FSeller stored HasSeller;
    property OfferAttributes : OfferAttributes_Type read FOfferAttributes write FOfferAttributes stored HasOfferAttributes;
    property OfferListing : Offer_OfferListingArray read FOfferListing write FOfferListing;
    property LoyaltyPoints : LoyaltyPoints_Type read FLoyaltyPoints write FLoyaltyPoints stored HasLoyaltyPoints;
    property Promotions : Promotions_Type read FPromotions write FPromotions stored HasPromotions;
  end;

  Merchant_Type = class(TBaseComplexRemotable)
  private
    FMerchantId : string;
    FName : string;
    FGlancePage : string;
    FAverageFeedbackRating : Extended;
    FTotalFeedback : nonNegativeInteger;
    FTotalFeedbackPages : nonNegativeInteger;
  private
    function HasName() : Boolean;
    function HasGlancePage() : Boolean;
    function HasAverageFeedbackRating() : Boolean;
    function HasTotalFeedback() : Boolean;
    function HasTotalFeedbackPages() : Boolean;
  published
    property MerchantId : string read FMerchantId write FMerchantId;
    property Name : string read FName write FName stored HasName;
    property GlancePage : string read FGlancePage write FGlancePage stored HasGlancePage;
    property AverageFeedbackRating : Extended read FAverageFeedbackRating write FAverageFeedbackRating stored HasAverageFeedbackRating;
    property TotalFeedback : nonNegativeInteger read FTotalFeedback write FTotalFeedback stored HasTotalFeedback;
    property TotalFeedbackPages : nonNegativeInteger read FTotalFeedbackPages write FTotalFeedbackPages stored HasTotalFeedbackPages;
  end;

  OfferAttributes_Type = class(TBaseComplexRemotable)
  private
    FCondition : string;
    FSubCondition : string;
    FConditionNote : string;
    FWillShipExpedited : boolean;
    FWillShipInternational : boolean;
  private
    function HasCondition() : Boolean;
    function HasSubCondition() : Boolean;
    function HasConditionNote() : Boolean;
    function HasWillShipExpedited() : Boolean;
    function HasWillShipInternational() : Boolean;
  published
    property Condition : string read FCondition write FCondition stored HasCondition;
    property SubCondition : string read FSubCondition write FSubCondition stored HasSubCondition;
    property ConditionNote : string read FConditionNote write FConditionNote stored HasConditionNote;
    property WillShipExpedited : boolean read FWillShipExpedited write FWillShipExpedited stored HasWillShipExpedited;
    property WillShipInternational : boolean read FWillShipInternational write FWillShipInternational stored HasWillShipInternational;
  end;

  OfferListing_Type = class(TBaseComplexRemotable)
  private
    FOfferListingId : string;
    FExchangeId : string;
    FPrice : Price;
    FSalePrice : Price;
    FAmountSaved : Price;
    FPercentageSaved : nonNegativeInteger;
    FAvailability : string;
    FAvailabilityAttributes : OfferListing_AvailabilityAttributes_Type;
    FQuantity : integer;
    FISPUStoreAddress : Address;
    FISPUStoreHours : string;
    FIsEligibleForSuperSaverShipping : boolean;
    FSalesRestriction : string;
    FShippingCharge : OfferListing_ShippingChargeArray;
  private
    function HasOfferListingId() : Boolean;
    function HasExchangeId() : Boolean;
    function HasPrice() : Boolean;
    function HasSalePrice() : Boolean;
    function HasAmountSaved() : Boolean;
    function HasPercentageSaved() : Boolean;
    function HasAvailability() : Boolean;
    function HasAvailabilityAttributes() : Boolean;
    function HasQuantity() : Boolean;
    function HasISPUStoreAddress() : Boolean;
    function HasISPUStoreHours() : Boolean;
    function HasIsEligibleForSuperSaverShipping() : Boolean;
    function HasSalesRestriction() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OfferListingId : string read FOfferListingId write FOfferListingId stored HasOfferListingId;
    property ExchangeId : string read FExchangeId write FExchangeId stored HasExchangeId;
    property Price : Price read FPrice write FPrice stored HasPrice;
    property SalePrice : Price read FSalePrice write FSalePrice stored HasSalePrice;
    property AmountSaved : Price read FAmountSaved write FAmountSaved stored HasAmountSaved;
    property PercentageSaved : nonNegativeInteger read FPercentageSaved write FPercentageSaved stored HasPercentageSaved;
    property Availability : string read FAvailability write FAvailability stored HasAvailability;
    property AvailabilityAttributes : OfferListing_AvailabilityAttributes_Type read FAvailabilityAttributes write FAvailabilityAttributes stored HasAvailabilityAttributes;
    property Quantity : integer read FQuantity write FQuantity stored HasQuantity;
    property ISPUStoreAddress : Address read FISPUStoreAddress write FISPUStoreAddress stored HasISPUStoreAddress;
    property ISPUStoreHours : string read FISPUStoreHours write FISPUStoreHours stored HasISPUStoreHours;
    property IsEligibleForSuperSaverShipping : boolean read FIsEligibleForSuperSaverShipping write FIsEligibleForSuperSaverShipping stored HasIsEligibleForSuperSaverShipping;
    property SalesRestriction : string read FSalesRestriction write FSalesRestriction stored HasSalesRestriction;
    property ShippingCharge : OfferListing_ShippingChargeArray read FShippingCharge write FShippingCharge;
  end;

  LoyaltyPoints_Type = class(TBaseComplexRemotable)
  private
    FPoints : nonNegativeInteger;
    FTypicalRedemptionValue : Price;
  private
    function HasPoints() : Boolean;
    function HasTypicalRedemptionValue() : Boolean;
  public
    destructor Destroy();override;
  published
    property Points : nonNegativeInteger read FPoints write FPoints stored HasPoints;
    property TypicalRedemptionValue : Price read FTypicalRedemptionValue write FTypicalRedemptionValue stored HasTypicalRedemptionValue;
  end;

  OfferListing_AvailabilityAttributes_Type = class(TBaseComplexRemotable)
  private
    FAvailabilityType : string;
    FIsPreorder : boolean;
    FMinimumHours : integer;
    FMaximumHours : integer;
  private
    function HasAvailabilityType() : Boolean;
    function HasIsPreorder() : Boolean;
    function HasMinimumHours() : Boolean;
    function HasMaximumHours() : Boolean;
  published
    property AvailabilityType : string read FAvailabilityType write FAvailabilityType stored HasAvailabilityType;
    property IsPreorder : boolean read FIsPreorder write FIsPreorder stored HasIsPreorder;
    property MinimumHours : integer read FMinimumHours write FMinimumHours stored HasMinimumHours;
    property MaximumHours : integer read FMaximumHours write FMaximumHours stored HasMaximumHours;
  end;

  Address = class(TBaseComplexRemotable)
  private
    FName : string;
    FAddress1 : string;
    FAddress2 : string;
    FAddress3 : string;
    FCity : string;
    FState : string;
    FPostalCode : string;
    FCountry : string;
  private
    function HasName() : Boolean;
    function HasAddress1() : Boolean;
    function HasAddress2() : Boolean;
    function HasAddress3() : Boolean;
    function HasCity() : Boolean;
    function HasState() : Boolean;
    function HasPostalCode() : Boolean;
    function HasCountry() : Boolean;
  published
    property Name : string read FName write FName stored HasName;
    property Address1 : string read FAddress1 write FAddress1 stored HasAddress1;
    property Address2 : string read FAddress2 write FAddress2 stored HasAddress2;
    property Address3 : string read FAddress3 write FAddress3 stored HasAddress3;
    property City : string read FCity write FCity stored HasCity;
    property State : string read FState write FState stored HasState;
    property PostalCode : string read FPostalCode write FPostalCode stored HasPostalCode;
    property Country : string read FCountry write FCountry stored HasCountry;
  end;

  OfferListing_ShippingCharge_Type = class(TBaseComplexRemotable)
  private
    FShippingType : string;
    FShippingPrice : Price;
  public
    destructor Destroy();override;
  published
    property ShippingType : string read FShippingType write FShippingType;
    property ShippingPrice : Price read FShippingPrice write FShippingPrice;
  end;

  EditorialReview_Type = class(TBaseComplexRemotable)
  private
    FSource : string;
    FContent : string;
  private
    function HasSource() : Boolean;
    function HasContent() : Boolean;
  published
    property Source : string read FSource write FSource stored HasSource;
    property Content : string read FContent write FContent stored HasContent;
  end;

  Collections_Collection_Type_CollectionSummary_Type = class(TBaseComplexRemotable)
  private
    FLowestListPrice : Price;
    FHighestListPrice : Price;
    FLowestSalePrice : Price;
    FHighestSalePrice : Price;
  private
    function HasLowestListPrice() : Boolean;
    function HasHighestListPrice() : Boolean;
    function HasLowestSalePrice() : Boolean;
    function HasHighestSalePrice() : Boolean;
  public
    destructor Destroy();override;
  published
    property LowestListPrice : Price read FLowestListPrice write FLowestListPrice stored HasLowestListPrice;
    property HighestListPrice : Price read FHighestListPrice write FHighestListPrice stored HasHighestListPrice;
    property LowestSalePrice : Price read FLowestSalePrice write FLowestSalePrice stored HasLowestSalePrice;
    property HighestSalePrice : Price read FHighestSalePrice write FHighestSalePrice stored HasHighestSalePrice;
  end;

  Collections_Collection_Type_CollectionParent_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FTitle : string;
  private
    function HasASIN() : Boolean;
    function HasTitle() : Boolean;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property Title : string read FTitle write FTitle stored HasTitle;
  end;

  Collections_Collection_Type_CollectionItem_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FTitle : string;
  private
    function HasASIN() : Boolean;
    function HasTitle() : Boolean;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property Title : string read FTitle write FTitle stored HasTitle;
  end;

  Collections_Collection_Type = class(TBaseComplexRemotable)
  private
    FCollectionSummary : Collections_Collection_Type_CollectionSummary_Type;
    FCollectionParent : Collections_Collection_Type_CollectionParent_Type;
    FCollectionItem : Collections_Collection_Type_CollectionItemArray;
  private
    function HasCollectionSummary() : Boolean;
    function HasCollectionParent() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CollectionSummary : Collections_Collection_Type_CollectionSummary_Type read FCollectionSummary write FCollectionSummary stored HasCollectionSummary;
    property CollectionParent : Collections_Collection_Type_CollectionParent_Type read FCollectionParent write FCollectionParent stored HasCollectionParent;
    property CollectionItem : Collections_Collection_Type_CollectionItemArray read FCollectionItem write FCollectionItem;
  end;

  Review_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FRating : Extended;
    FHelpfulVotes : nonNegativeInteger;
    FCustomerId : string;
    FReviewer : Reviewer_Type;
    FTotalVotes : nonNegativeInteger;
    FDate : string;
    FSummary : string;
    FContent : string;
  private
    function HasASIN() : Boolean;
    function HasRating() : Boolean;
    function HasHelpfulVotes() : Boolean;
    function HasCustomerId() : Boolean;
    function HasReviewer() : Boolean;
    function HasTotalVotes() : Boolean;
    function HasDate() : Boolean;
    function HasSummary() : Boolean;
    function HasContent() : Boolean;
  public
    destructor Destroy();override;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property Rating : Extended read FRating write FRating stored HasRating;
    property HelpfulVotes : nonNegativeInteger read FHelpfulVotes write FHelpfulVotes stored HasHelpfulVotes;
    property CustomerId : string read FCustomerId write FCustomerId stored HasCustomerId;
    property Reviewer : Reviewer_Type read FReviewer write FReviewer stored HasReviewer;
    property TotalVotes : nonNegativeInteger read FTotalVotes write FTotalVotes stored HasTotalVotes;
    property Date : string read FDate write FDate stored HasDate;
    property Summary : string read FSummary write FSummary stored HasSummary;
    property Content : string read FContent write FContent stored HasContent;
  end;

  Reviewer_Type = class(TBaseComplexRemotable)
  private
    FCustomerId : string;
    FName : string;
    FNickname : string;
    FLocation : string;
  private
    function HasCustomerId() : Boolean;
    function HasName() : Boolean;
    function HasNickname() : Boolean;
    function HasLocation() : Boolean;
  published
    property CustomerId : string read FCustomerId write FCustomerId stored HasCustomerId;
    property Name : string read FName write FName stored HasName;
    property Nickname : string read FNickname write FNickname stored HasNickname;
    property Location : string read FLocation write FLocation stored HasLocation;
  end;

  Tracks_Disc_Type_Track_Type = class(TComplexStringContentRemotable)
  private
    FNumber : positiveInteger;
  published
    property Number : positiveInteger read FNumber write FNumber;
  end;

  Tracks_Disc_Type = class(TBaseComplexRemotable)
  private
    FTrack : Tracks_Disc_Type_TrackArray;
    FNumber : positiveInteger;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Track : Tracks_Disc_Type_TrackArray read FTrack write FTrack;
    property Number : positiveInteger read FNumber write FNumber;
  end;

  SimilarProducts_SimilarProduct_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FTitle : string;
  private
    function HasASIN() : Boolean;
    function HasTitle() : Boolean;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property Title : string read FTitle write FTitle stored HasTitle;
  end;

  TopSellers_TopSeller_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FTitle : string;
  private
    function HasASIN() : Boolean;
    function HasTitle() : Boolean;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property Title : string read FTitle write FTitle stored HasTitle;
  end;

  NewReleases_NewRelease_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FTitle : string;
  private
    function HasASIN() : Boolean;
    function HasTitle() : Boolean;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property Title : string read FTitle write FTitle stored HasTitle;
  end;

  SimilarViewedProducts_SimilarViewedProduct_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FTitle : string;
  private
    function HasASIN() : Boolean;
    function HasTitle() : Boolean;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property Title : string read FTitle write FTitle stored HasTitle;
  end;

  OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FTitle : string;
  private
    function HasASIN() : Boolean;
    function HasTitle() : Boolean;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property Title : string read FTitle write FTitle stored HasTitle;
  end;

  Accessories_Accessory_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FTitle : string;
  private
    function HasASIN() : Boolean;
    function HasTitle() : Boolean;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property Title : string read FTitle write FTitle stored HasTitle;
  end;

  Promotion_Type = class(TBaseComplexRemotable)
  private
    FSummary : Promotion_Summary_Type;
    FDetails : Promotion_Details_Type;
  private
    function HasSummary() : Boolean;
    function HasDetails() : Boolean;
  public
    destructor Destroy();override;
  published
    property Summary : Promotion_Summary_Type read FSummary write FSummary stored HasSummary;
    property Details : Promotion_Details_Type read FDetails write FDetails stored HasDetails;
  end;

  Promotion_Summary_Type = class(TBaseComplexRemotable)
  private
    FPromotionId : string;
    FCategory : string;
    FStartDate : string;
    FEndDate : string;
    FEligibilityRequirementDescription : string;
    FBenefitDescription : string;
    FTermsAndConditions : string;
  private
    function HasCategory() : Boolean;
    function HasStartDate() : Boolean;
    function HasEndDate() : Boolean;
    function HasEligibilityRequirementDescription() : Boolean;
    function HasBenefitDescription() : Boolean;
    function HasTermsAndConditions() : Boolean;
  published
    property PromotionId : string read FPromotionId write FPromotionId;
    property Category : string read FCategory write FCategory stored HasCategory;
    property StartDate : string read FStartDate write FStartDate stored HasStartDate;
    property EndDate : string read FEndDate write FEndDate stored HasEndDate;
    property EligibilityRequirementDescription : string read FEligibilityRequirementDescription write FEligibilityRequirementDescription stored HasEligibilityRequirementDescription;
    property BenefitDescription : string read FBenefitDescription write FBenefitDescription stored HasBenefitDescription;
    property TermsAndConditions : string read FTermsAndConditions write FTermsAndConditions stored HasTermsAndConditions;
  end;

  PromotionItemApplicability = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FIsInBenefitSet : boolean;
    FIsInEligibilityRequirementSet : boolean;
  published
    property ASIN : string read FASIN write FASIN;
    property IsInBenefitSet : boolean read FIsInBenefitSet write FIsInBenefitSet;
    property IsInEligibilityRequirementSet : boolean read FIsInEligibilityRequirementSet write FIsInEligibilityRequirementSet;
  end;

  Promotion_Details_Type = class(TBaseComplexRemotable)
  private
    FMerchantId : string;
    FOwningMerchantId : string;
    FPromotionId : string;
    FPromotionCategory : string;
    FMerchantPromotionId : string;
    FGroupClaimCode : string;
    FCouponCombinationType : string;
    FStartDate : string;
    FEndDate : string;
    FTermsAndConditions : string;
    FEligibilityRequirements : PromotionEligibilityRequirements;
    FBenefits : PromotionBenefits;
    FItemApplicability : PromotionItemApplicability;
  private
    function HasMerchantPromotionId() : Boolean;
    function HasGroupClaimCode() : Boolean;
    function HasCouponCombinationType() : Boolean;
    function HasStartDate() : Boolean;
    function HasEndDate() : Boolean;
    function HasTermsAndConditions() : Boolean;
    function HasEligibilityRequirements() : Boolean;
    function HasBenefits() : Boolean;
    function HasItemApplicability() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MerchantId : string read FMerchantId write FMerchantId;
    property OwningMerchantId : string read FOwningMerchantId write FOwningMerchantId;
    property PromotionId : string read FPromotionId write FPromotionId;
    property PromotionCategory : string read FPromotionCategory write FPromotionCategory;
    property MerchantPromotionId : string read FMerchantPromotionId write FMerchantPromotionId stored HasMerchantPromotionId;
    property GroupClaimCode : string read FGroupClaimCode write FGroupClaimCode stored HasGroupClaimCode;
    property CouponCombinationType : string read FCouponCombinationType write FCouponCombinationType stored HasCouponCombinationType;
    property StartDate : string read FStartDate write FStartDate stored HasStartDate;
    property EndDate : string read FEndDate write FEndDate stored HasEndDate;
    property TermsAndConditions : string read FTermsAndConditions write FTermsAndConditions stored HasTermsAndConditions;
    property EligibilityRequirements : PromotionEligibilityRequirements read FEligibilityRequirements write FEligibilityRequirements stored HasEligibilityRequirements;
    property Benefits : PromotionBenefits read FBenefits write FBenefits stored HasBenefits;
    property ItemApplicability : PromotionItemApplicability read FItemApplicability write FItemApplicability stored HasItemApplicability;
  end;

  PromotionEligibilityRequirement = class(TBaseComplexRemotable)
  private
    FEligibilityRequirementType : string;
    FQuantity : integer;
    FCurrencyAmount : Price;
  private
    function HasQuantity() : Boolean;
    function HasCurrencyAmount() : Boolean;
  public
    destructor Destroy();override;
  published
    property EligibilityRequirementType : string read FEligibilityRequirementType write FEligibilityRequirementType;
    property Quantity : integer read FQuantity write FQuantity stored HasQuantity;
    property CurrencyAmount : Price read FCurrencyAmount write FCurrencyAmount stored HasCurrencyAmount;
  end;

  PromotionBenefit = class(TBaseComplexRemotable)
  private
    FBenefitType : string;
    FComponentType : string;
    FQuantity : integer;
    FPercentOff : Double;
    FFixedAmount : Price;
    FCeiling : Price;
  private
    function HasQuantity() : Boolean;
    function HasPercentOff() : Boolean;
    function HasFixedAmount() : Boolean;
    function HasCeiling() : Boolean;
  public
    destructor Destroy();override;
  published
    property BenefitType : string read FBenefitType write FBenefitType;
    property ComponentType : string read FComponentType write FComponentType;
    property Quantity : integer read FQuantity write FQuantity stored HasQuantity;
    property PercentOff : Double read FPercentOff write FPercentOff stored HasPercentOff;
    property FixedAmount : Price read FFixedAmount write FFixedAmount stored HasFixedAmount;
    property Ceiling : Price read FCeiling write FCeiling stored HasCeiling;
  end;

  BrowseNode_Type = class(TBaseComplexRemotable)
  private
    FBrowseNodeId : string;
    FName : string;
    FIsCategoryRoot : boolean;
    FChildren : BrowseNode_Children_Type;
    FAncestors : BrowseNode_Ancestors_Type;
    FTopSellers : TopSellers_Type;
    FNewReleases : NewReleases_Type;
  private
    function HasBrowseNodeId() : Boolean;
    function HasName() : Boolean;
    function HasIsCategoryRoot() : Boolean;
    function HasChildren() : Boolean;
    function HasAncestors() : Boolean;
    function HasTopSellers() : Boolean;
    function HasNewReleases() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property BrowseNodeId : string read FBrowseNodeId write FBrowseNodeId stored HasBrowseNodeId;
    property Name : string read FName write FName stored HasName;
    property IsCategoryRoot : boolean read FIsCategoryRoot write FIsCategoryRoot stored HasIsCategoryRoot;
    property Children : BrowseNode_Children_Type read FChildren write FChildren stored HasChildren;
    property Ancestors : BrowseNode_Ancestors_Type read FAncestors write FAncestors stored HasAncestors;
    property TopSellers : TopSellers_Type read FTopSellers write FTopSellers stored HasTopSellers;
    property NewReleases : NewReleases_Type read FNewReleases write FNewReleases stored HasNewReleases;
  end;

  ListmaniaLists_ListmaniaList_Type = class(TBaseComplexRemotable)
  private
    FListId : string;
    FListName : string;
  private
    function HasListName() : Boolean;
  published
    property ListId : string read FListId write FListId;
    property ListName : string read FListName write FListName stored HasListName;
  end;

  SearchInside_Excerpt_Type = class(TBaseComplexRemotable)
  private
    FChecksum : string;
    FPageType : string;
    FPageNumber : string;
    FSequenceNumber : string;
    FText : string;
  private
    function HasChecksum() : Boolean;
    function HasPageType() : Boolean;
    function HasPageNumber() : Boolean;
    function HasSequenceNumber() : Boolean;
    function HasText() : Boolean;
  published
    property Checksum : string read FChecksum write FChecksum stored HasChecksum;
    property PageType : string read FPageType write FPageType stored HasPageType;
    property PageNumber : string read FPageNumber write FPageNumber stored HasPageNumber;
    property SequenceNumber : string read FSequenceNumber write FSequenceNumber stored HasSequenceNumber;
    property Text : string read FText write FText stored HasText;
  end;

  CartItem = class(TBaseComplexRemotable)
  private
    FCartItemId : string;
    FASIN : string;
    FExchangeId : string;
    FMerchantId : string;
    FSellerId : string;
    FSellerNickname : string;
    FQuantity : string;
    FTitle : string;
    FProductGroup : string;
    FListOwner : string;
    FListType : string;
    FPrice : Price;
    FItemTotal : Price;
  private
    function HasASIN() : Boolean;
    function HasExchangeId() : Boolean;
    function HasMerchantId() : Boolean;
    function HasSellerId() : Boolean;
    function HasSellerNickname() : Boolean;
    function HasTitle() : Boolean;
    function HasProductGroup() : Boolean;
    function HasListOwner() : Boolean;
    function HasListType() : Boolean;
    function HasPrice() : Boolean;
    function HasItemTotal() : Boolean;
  public
    destructor Destroy();override;
  published
    property CartItemId : string read FCartItemId write FCartItemId;
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property ExchangeId : string read FExchangeId write FExchangeId stored HasExchangeId;
    property MerchantId : string read FMerchantId write FMerchantId stored HasMerchantId;
    property SellerId : string read FSellerId write FSellerId stored HasSellerId;
    property SellerNickname : string read FSellerNickname write FSellerNickname stored HasSellerNickname;
    property Quantity : string read FQuantity write FQuantity;
    property Title : string read FTitle write FTitle stored HasTitle;
    property ProductGroup : string read FProductGroup write FProductGroup stored HasProductGroup;
    property ListOwner : string read FListOwner write FListOwner stored HasListOwner;
    property ListType : string read FListType write FListType stored HasListType;
    property Price : Price read FPrice write FPrice stored HasPrice;
    property ItemTotal : Price read FItemTotal write FItemTotal stored HasItemTotal;
  end;

  Transaction_Totals_Type = class(TBaseComplexRemotable)
  private
    FTotal : Price;
    FSubtotal : Price;
    FTax : Price;
    FShippingCharge : Price;
    FPromotion : Price;
  public
    destructor Destroy();override;
  published
    property Total : Price read FTotal write FTotal;
    property Subtotal : Price read FSubtotal write FSubtotal;
    property Tax : Price read FTax write FTax;
    property ShippingCharge : Price read FShippingCharge write FShippingCharge;
    property Promotion : Price read FPromotion write FPromotion;
  end;

  TransactionItem_Type = class(TBaseComplexRemotable)
  private
    FTransactionItemId : string;
    FQuantity : string;
    FUnitPrice : Price;
    FTotalPrice : Price;
    FASIN : string;
    FChildTransactionItems : TransactionItem_ChildTransactionItems_Type;
  private
    function HasASIN() : Boolean;
    function HasChildTransactionItems() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property TransactionItemId : string read FTransactionItemId write FTransactionItemId;
    property Quantity : string read FQuantity write FQuantity;
    property UnitPrice : Price read FUnitPrice write FUnitPrice;
    property TotalPrice : Price read FTotalPrice write FTotalPrice;
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property ChildTransactionItems : TransactionItem_ChildTransactionItems_Type read FChildTransactionItems write FChildTransactionItems stored HasChildTransactionItems;
  end;

  Transaction_Shipments_Type_Shipment_Type_Packages_Type_Package_Type = class(TBaseComplexRemotable)
  private
    FTrackingNumber : string;
    FCarrierName : string;
  published
    property TrackingNumber : string read FTrackingNumber write FTrackingNumber;
    property CarrierName : string read FCarrierName write FCarrierName;
  end;

  Transaction_Shipments_Type_Shipment_Type = class(TBaseComplexRemotable)
  private
    FCondition : string;
    FDeliveryMethod : string;
    FShipmentItems : Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type;
    FPackages : Transaction_Shipments_Type_Shipment_Type_Packages_Type;
  private
    function HasShipmentItems() : Boolean;
    function HasPackages() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Condition : string read FCondition write FCondition;
    property DeliveryMethod : string read FDeliveryMethod write FDeliveryMethod;
    property ShipmentItems : Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type read FShipmentItems write FShipmentItems stored HasShipmentItems;
    property Packages : Transaction_Shipments_Type_Shipment_Type_Packages_Type read FPackages write FPackages stored HasPackages;
  end;

  Seller_Location_Type = class(TBaseComplexRemotable)
  private
    FUserDefinedLocation : string;
    FCity : string;
    FState : string;
    FCountry : string;
  private
    function HasUserDefinedLocation() : Boolean;
    function HasCity() : Boolean;
    function HasState() : Boolean;
    function HasCountry() : Boolean;
  published
    property UserDefinedLocation : string read FUserDefinedLocation write FUserDefinedLocation stored HasUserDefinedLocation;
    property City : string read FCity write FCity stored HasCity;
    property State : string read FState write FState stored HasState;
    property Country : string read FCountry write FCountry stored HasCountry;
  end;

  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type = class(TBaseComplexRemotable)
  private
    FCount : nonNegativeInteger;
    FPercentage : nonNegativeInteger;
    F_Type : string;
  private
    function HasCount() : Boolean;
    function HasPercentage() : Boolean;
  published
    property Count : nonNegativeInteger read FCount write FCount stored HasCount;
    property Percentage : nonNegativeInteger read FPercentage write FPercentage stored HasPercentage;
    property _Type : string read F_Type write F_Type;
  end;

  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type = class(TBaseComplexRemotable)
  private
    FSellerFeedbackRating : Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray;
    FPeriod : string;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property SellerFeedbackRating : Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray read FSellerFeedbackRating write FSellerFeedbackRating;
    property Period : string read FPeriod write FPeriod;
  end;

  SellerFeedback_Feedback_Type = class(TBaseComplexRemotable)
  private
    FRating : nonNegativeInteger;
    FComment : string;
    FDate : string;
    FRatedBy : string;
  private
    function HasRating() : Boolean;
    function HasComment() : Boolean;
    function HasDate() : Boolean;
    function HasRatedBy() : Boolean;
  published
    property Rating : nonNegativeInteger read FRating write FRating stored HasRating;
    property Comment : string read FComment write FComment stored HasComment;
    property Date : string read FDate write FDate stored HasDate;
    property RatedBy : string read FRatedBy write FRatedBy stored HasRatedBy;
  end;

  DecimalWithUnits = class(TComplexFloatExtendedContentRemotable)
  private
    FUnits : string;
  published
    property Units : string read FUnits write FUnits;
  end;

  NonNegativeIntegerWithUnits = class(TComplexInt32UContentRemotable)
  private
    FUnits : string;
  published
    property Units : string read FUnits write FUnits;
  end;

  ItemAttributes_Creator_Type = class(TComplexStringContentRemotable)
  private
    FRole : string;
  published
    property Role : string read FRole write FRole;
  end;

  StringWithUnits = class(TComplexStringContentRemotable)
  private
    FUnits : string;
  published
    property Units : string read FUnits write FUnits;
  end;

  ItemAttributes_ItemDimensions_Type = class(TBaseComplexRemotable)
  private
    FHeight : DecimalWithUnits;
    FLength : DecimalWithUnits;
    FWeight : DecimalWithUnits;
    FWidth : DecimalWithUnits;
  private
    function HasHeight() : Boolean;
    function HasLength() : Boolean;
    function HasWeight() : Boolean;
    function HasWidth() : Boolean;
  public
    destructor Destroy();override;
  published
    property Height : DecimalWithUnits read FHeight write FHeight stored HasHeight;
    property Length : DecimalWithUnits read FLength write FLength stored HasLength;
    property Weight : DecimalWithUnits read FWeight write FWeight stored HasWeight;
    property Width : DecimalWithUnits read FWidth write FWidth stored HasWidth;
  end;

  ItemAttributes_Languages_Type_Language_Type = class(TBaseComplexRemotable)
  private
    FName : string;
    F_Type : string;
    FAudioFormat : string;
  private
    function Has_Type() : Boolean;
    function HasAudioFormat() : Boolean;
  published
    property Name : string read FName write FName;
    property _Type : string read F_Type write F_Type stored Has_Type;
    property AudioFormat : string read FAudioFormat write FAudioFormat stored HasAudioFormat;
  end;

  ItemAttributes_PackageDimensions_Type = class(TBaseComplexRemotable)
  private
    FHeight : DecimalWithUnits;
    FLength : DecimalWithUnits;
    FWeight : DecimalWithUnits;
    FWidth : DecimalWithUnits;
  private
    function HasHeight() : Boolean;
    function HasLength() : Boolean;
    function HasWeight() : Boolean;
    function HasWidth() : Boolean;
  public
    destructor Destroy();override;
  published
    property Height : DecimalWithUnits read FHeight write FHeight stored HasHeight;
    property Length : DecimalWithUnits read FLength write FLength stored HasLength;
    property Weight : DecimalWithUnits read FWeight write FWeight stored HasWeight;
    property Width : DecimalWithUnits read FWidth write FWidth stored HasWidth;
  end;

  MerchantItemAttributes_Creator_Type = class(TComplexStringContentRemotable)
  private
    FRole : string;
  published
    property Role : string read FRole write FRole;
  end;

  MerchantItemAttributes_ItemDimensions_Type = class(TBaseComplexRemotable)
  private
    FHeight : DecimalWithUnits;
    FLength : DecimalWithUnits;
    FWeight : DecimalWithUnits;
    FWidth : DecimalWithUnits;
  private
    function HasHeight() : Boolean;
    function HasLength() : Boolean;
    function HasWeight() : Boolean;
    function HasWidth() : Boolean;
  public
    destructor Destroy();override;
  published
    property Height : DecimalWithUnits read FHeight write FHeight stored HasHeight;
    property Length : DecimalWithUnits read FLength write FLength stored HasLength;
    property Weight : DecimalWithUnits read FWeight write FWeight stored HasWeight;
    property Width : DecimalWithUnits read FWidth write FWidth stored HasWidth;
  end;

  MerchantItemAttributes_Languages_Type_Language_Type = class(TBaseComplexRemotable)
  private
    FName : string;
    F_Type : string;
    FAudioFormat : string;
  private
    function HasAudioFormat() : Boolean;
  published
    property Name : string read FName write FName;
    property _Type : string read F_Type write F_Type;
    property AudioFormat : string read FAudioFormat write FAudioFormat stored HasAudioFormat;
  end;

  MerchantItemAttributes_PackageDimensions_Type = class(TBaseComplexRemotable)
  private
    FHeight : DecimalWithUnits;
    FLength : DecimalWithUnits;
    FWeight : DecimalWithUnits;
    FWidth : DecimalWithUnits;
  private
    function HasHeight() : Boolean;
    function HasLength() : Boolean;
    function HasWeight() : Boolean;
    function HasWidth() : Boolean;
  public
    destructor Destroy();override;
  published
    property Height : DecimalWithUnits read FHeight write FHeight stored HasHeight;
    property Length : DecimalWithUnits read FLength write FLength stored HasLength;
    property Weight : DecimalWithUnits read FWeight write FWeight stored HasWeight;
    property Width : DecimalWithUnits read FWidth write FWidth stored HasWidth;
  end;

  MerchantItemAttributes_VendorRebate_Type = class(TBaseComplexRemotable)
  private
    F_Type : string;
    FStartDate : string;
    FEndDate : string;
  private
    function Has_Type() : Boolean;
    function HasStartDate() : Boolean;
    function HasEndDate() : Boolean;
  published
    property _Type : string read F_Type write F_Type stored Has_Type;
    property StartDate : string read FStartDate write FStartDate stored HasStartDate;
    property EndDate : string read FEndDate write FEndDate stored HasEndDate;
  end;

  Help_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): HelpRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : HelpRequest Read GetItem;Default;
  end;

  HelpResponse_InformationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Information_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Information_Type Read GetItem;Default;
  end;

  ItemSearch_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ItemSearchRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ItemSearchRequest Read GetItem;Default;
  end;

  ItemSearchResponse_ItemsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Items_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Items_Type Read GetItem;Default;
  end;

  ItemLookup_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ItemLookupRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ItemLookupRequest Read GetItem;Default;
  end;

  ItemLookupResponse_ItemsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Items_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Items_Type Read GetItem;Default;
  end;

  BrowseNodeLookup_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): BrowseNodeLookupRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : BrowseNodeLookupRequest Read GetItem;Default;
  end;

  BrowseNodeLookupResponse_BrowseNodesArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): BrowseNodes_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : BrowseNodes_Type Read GetItem;Default;
  end;

  ListSearch_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ListSearchRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ListSearchRequest Read GetItem;Default;
  end;

  ListSearchResponse_ListsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Lists_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Lists_Type Read GetItem;Default;
  end;

  ListLookup_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ListLookupRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ListLookupRequest Read GetItem;Default;
  end;

  ListLookupResponse_ListsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Lists_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Lists_Type Read GetItem;Default;
  end;

  CustomerContentSearch_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CustomerContentSearchRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CustomerContentSearchRequest Read GetItem;Default;
  end;

  CustomerContentSearchResponse_CustomersArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Customers_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Customers_Type Read GetItem;Default;
  end;

  CustomerContentLookup_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CustomerContentLookupRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CustomerContentLookupRequest Read GetItem;Default;
  end;

  CustomerContentLookupResponse_CustomersArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Customers_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Customers_Type Read GetItem;Default;
  end;

  SimilarityLookup_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SimilarityLookupRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SimilarityLookupRequest Read GetItem;Default;
  end;

  SimilarityLookupResponse_ItemsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Items_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Items_Type Read GetItem;Default;
  end;

  SellerLookup_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SellerLookupRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SellerLookupRequest Read GetItem;Default;
  end;

  SellerLookupResponse_SellersArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Sellers_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Sellers_Type Read GetItem;Default;
  end;

  CartGet_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartGetRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartGetRequest Read GetItem;Default;
  end;

  CartGetResponse_CartArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Cart_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Cart_Type Read GetItem;Default;
  end;

  CartAdd_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartAddRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartAddRequest Read GetItem;Default;
  end;

  CartAddResponse_CartArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Cart_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Cart_Type Read GetItem;Default;
  end;

  CartCreate_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartCreateRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartCreateRequest Read GetItem;Default;
  end;

  CartCreateResponse_CartArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Cart_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Cart_Type Read GetItem;Default;
  end;

  CartModify_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartModifyRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartModifyRequest Read GetItem;Default;
  end;

  CartModifyResponse_CartArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Cart_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Cart_Type Read GetItem;Default;
  end;

  CartClear_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartClearRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartClearRequest Read GetItem;Default;
  end;

  CartClearResponse_CartArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Cart_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Cart_Type Read GetItem;Default;
  end;

  TransactionLookup_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TransactionLookupRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TransactionLookupRequest Read GetItem;Default;
  end;

  TransactionLookupResponse_TransactionsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Transactions_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Transactions_Type Read GetItem;Default;
  end;

  SellerListingSearch_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SellerListingSearchRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SellerListingSearchRequest Read GetItem;Default;
  end;

  SellerListingSearchResponse_SellerListingsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SellerListings_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SellerListings_Type Read GetItem;Default;
  end;

  SellerListingLookup_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SellerListingLookupRequest;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SellerListingLookupRequest Read GetItem;Default;
  end;

  SellerListingLookupResponse_SellerListingsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SellerListings_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SellerListings_Type Read GetItem;Default;
  end;

  Bin_BinParameterArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Bin_BinParameter_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Bin_BinParameter_Type Read GetItem;Default;
  end;

  SearchBinSet_BinArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Bin_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Bin_Type Read GetItem;Default;
  end;

  SearchBinSets_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SearchBinSet_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SearchBinSet_Type Read GetItem;Default;
  end;

  HelpRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemSearchRequest_AudienceRatingArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of AudienceRating_Type;
  private
    function GetItem(AIndex: Integer): AudienceRating_Type;
    procedure SetItem(AIndex: Integer; const AValue: AudienceRating_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : AudienceRating_Type read GetItem write SetItem; default;
  end;

  ItemSearchRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemLookupRequest_ItemIdArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemLookupRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ListSearchRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ListLookupRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  CustomerContentSearchRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  CustomerContentLookupRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  SimilarityLookupRequest_ItemIdArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  SimilarityLookupRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  SellerLookupRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  SellerLookupRequest_SellerIdArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  CartGetRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  CartAddRequest_Items_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartAddRequest_Items_Type_Item_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartAddRequest_Items_Type_Item_Type Read GetItem;Default;
  end;

  CartAddRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  CartCreateRequest_Items_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartCreateRequest_Items_Type_Item_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartCreateRequest_Items_Type_Item_Type Read GetItem;Default;
  end;

  CartCreateRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  CartModifyRequest_Items_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartModifyRequest_Items_Type_Item_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartModifyRequest_Items_Type_Item_Type Read GetItem;Default;
  end;

  CartModifyRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  CartClearRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  TransactionLookupRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  TransactionLookupRequest_TransactionIdArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  SellerListingSearchRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  SellerListingLookupRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  BrowseNodeLookupRequest_BrowseNodeIdArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  BrowseNodeLookupRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  HTTPHeaders_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): HTTPHeaders_Header_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : HTTPHeaders_Header_Type Read GetItem;Default;
  end;

  Arguments_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Arguments_Argument_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Arguments_Argument_Type Read GetItem;Default;
  end;

  Errors_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Errors_Error_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Errors_Error_Type Read GetItem;Default;
  end;

  Information_OperationInformationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): OperationInformation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : OperationInformation_Type Read GetItem;Default;
  end;

  Information_ResponseGroupInformationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ResponseGroupInformation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ResponseGroupInformation_Type Read GetItem;Default;
  end;

  SearchResultsMap_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SearchResultsMap_SearchIndex_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SearchResultsMap_SearchIndex_Type Read GetItem;Default;
  end;

  Items__ItemArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Item_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Item_Type Read GetItem;Default;
  end;

  Lists_ListArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): List_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : List_Type Read GetItem;Default;
  end;

  Customers_CustomerArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Customer_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Customer_Type Read GetItem;Default;
  end;

  SimilarProducts_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SimilarProducts_SimilarProduct_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SimilarProducts_SimilarProduct_Type Read GetItem;Default;
  end;

  TopSellers_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TopSellers_TopSeller_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TopSellers_TopSeller_Type Read GetItem;Default;
  end;

  NewReleases_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): NewReleases_NewRelease_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : NewReleases_NewRelease_Type Read GetItem;Default;
  end;

  SimilarViewedProducts_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SimilarViewedProducts_SimilarViewedProduct_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SimilarViewedProducts_SimilarViewedProduct_Type Read GetItem;Default;
  end;

  OtherCategoriesSimilarProducts_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type Read GetItem;Default;
  end;

  Transactions_TransactionArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Transaction_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Transaction_Type Read GetItem;Default;
  end;

  Sellers_SellerArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Seller_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Seller_Type Read GetItem;Default;
  end;

  SellerListings_SellerListingArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SellerListing_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SellerListing_Type Read GetItem;Default;
  end;

  OperationInformation_RequiredParameters_Type = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  OperationInformation_AvailableParameters_Type = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  OperationInformation_DefaultResponseGroups_Type = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  OperationInformation_AvailableResponseGroups_Type = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ResponseGroupInformation_ValidOperations_Type = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ResponseGroupInformation_Elements_Type = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  List_ListItemArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ListItem_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ListItem_Type Read GetItem;Default;
  end;

  Customer_CustomerReviewsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CustomerReviews_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CustomerReviews_Type Read GetItem;Default;
  end;

  SearchResultsMap_SearchIndex_Type_ASINArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  Item_ImageSets_Type_ImageSetArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ImageSet_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ImageSet_Type Read GetItem;Default;
  end;

  Collections_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Collections_Collection_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Collections_Collection_Type Read GetItem;Default;
  end;

  Item_Subjects_Type = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  EditorialReviews_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): EditorialReview_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : EditorialReview_Type Read GetItem;Default;
  end;

  Accessories_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Accessories_Accessory_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Accessories_Accessory_Type Read GetItem;Default;
  end;

  Tracks_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Tracks_Disc_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Tracks_Disc_Type Read GetItem;Default;
  end;

  ListmaniaLists_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ListmaniaLists_ListmaniaList_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ListmaniaLists_ListmaniaList_Type Read GetItem;Default;
  end;

  Item_AlternateVersions_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Item_AlternateVersions_Type_AlternateVersion_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Item_AlternateVersions_Type_AlternateVersion_Type Read GetItem;Default;
  end;

  _Item_ImageSetsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Item_ImageSets_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Item_ImageSets_Type Read GetItem;Default;
  end;

  Offers_OfferArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Offer_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Offer_Type Read GetItem;Default;
  end;

  Promotions_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Promotion_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Promotion_Type Read GetItem;Default;
  end;

  Offer_OfferListingArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): OfferListing_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : OfferListing_Type Read GetItem;Default;
  end;

  OfferListing_ShippingChargeArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): OfferListing_ShippingCharge_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : OfferListing_ShippingCharge_Type Read GetItem;Default;
  end;

  VariationDimensions_Type = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  Variations__ItemArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Item_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Item_Type Read GetItem;Default;
  end;

  Collections_Collection_Type_CollectionItemArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Collections_Collection_Type_CollectionItem_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Collections_Collection_Type_CollectionItem_Type Read GetItem;Default;
  end;

  CustomerReviews_ReviewArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Review_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Review_Type Read GetItem;Default;
  end;

  Tracks_Disc_Type_TrackArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Tracks_Disc_Type_Track_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Tracks_Disc_Type_Track_Type Read GetItem;Default;
  end;

  PromotionEligibilityRequirements = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): PromotionEligibilityRequirement;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : PromotionEligibilityRequirement Read GetItem;Default;
  end;

  PromotionBenefits = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): PromotionBenefit;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : PromotionBenefit Read GetItem;Default;
  end;

  BrowseNodes_BrowseNodeArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): BrowseNode_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : BrowseNode_Type Read GetItem;Default;
  end;

  BrowseNode_Children_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): BrowseNode_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : BrowseNode_Type Read GetItem;Default;
  end;

  BrowseNode_Ancestors_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): BrowseNode_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : BrowseNode_Type Read GetItem;Default;
  end;

  CartItems_CartItemArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartItem;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartItem Read GetItem;Default;
  end;

  SavedForLaterItems_SavedForLaterItemArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartItem;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartItem Read GetItem;Default;
  end;

  Transaction_TransactionItems_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TransactionItem_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TransactionItem_Type Read GetItem;Default;
  end;

  Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  Transaction_Shipments_Type_Shipment_Type_Packages_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Transaction_Shipments_Type_Shipment_Type_Packages_Type_Package_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Transaction_Shipments_Type_Shipment_Type_Packages_Type_Package_Type Read GetItem;Default;
  end;

  Transaction_Shipments_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Transaction_Shipments_Type_Shipment_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Transaction_Shipments_Type_Shipment_Type Read GetItem;Default;
  end;

  TransactionItem_ChildTransactionItems_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TransactionItem_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TransactionItem_Type Read GetItem;Default;
  end;

  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type Read GetItem;Default;
  end;

  Seller_SellerFeedbackSummary_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type Read GetItem;Default;
  end;

  SellerFeedback_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SellerFeedback_Feedback_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SellerFeedback_Feedback_Type Read GetItem;Default;
  end;

  ItemAttributes_Languages_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ItemAttributes_Languages_Type_Language_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ItemAttributes_Languages_Type_Language_Type Read GetItem;Default;
  end;

  ItemAttributes_ActorArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_ArtistArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_AudioFormatArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_AuthorArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_CameraManualFeaturesArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_CompatibleDevicesArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_CreatorArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ItemAttributes_Creator_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ItemAttributes_Creator_Type Read GetItem;Default;
  end;

  ItemAttributes_DataLinkProtocolArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_DirectorArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_FeatureArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_FormatArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_FormFactorArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_PhotoFlashTypeArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_PictureFormatArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_PlatformArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_ReturnMethodArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_SpecialFeaturesArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ItemAttributes_SupportedImageTypeArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_Languages_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): MerchantItemAttributes_Languages_Type_Language_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : MerchantItemAttributes_Languages_Type_Language_Type Read GetItem;Default;
  end;

  MerchantItemAttributes_ActorArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_ArtistArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_AudioFormatArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_AuthorArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_CameraManualFeaturesArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_CreatorArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): MerchantItemAttributes_Creator_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : MerchantItemAttributes_Creator_Type Read GetItem;Default;
  end;

  MerchantItemAttributes_DirectorArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_FeatureArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_FormatArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_PhotoFlashTypeArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_PictureFormatArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_PlatformArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_PurchasingChannelArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_ReturnMethodArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_SpecialFeaturesArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  MerchantItemAttributes_SupportedImageTypeArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  AWSECommerceServicePortType = interface(IInvokable)
    ['{44FE21A3-034C-4AC2-81F3-73F0DD856713}']
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
  end;

  procedure Register_AWSECommerceService_ServiceMetadata();

Implementation
uses metadata_repository;

{ HelpRequest }

constructor HelpRequest.Create();
begin
  inherited Create();
  FResponseGroup := HelpRequest_ResponseGroupArray.Create();
end;

destructor HelpRequest.Destroy();
begin
  inherited Destroy();
end;

function HelpRequest.HasAbout() : Boolean;
begin
  Result := True;
end;

function HelpRequest.HasHelpType() : Boolean;
begin
  Result := True;
end;

{ Help_Type }

constructor Help_Type.Create();
begin
  inherited Create();
  FRequest := Help_RequestArray.Create();
end;

destructor Help_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function Help_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function Help_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function Help_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function Help_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function Help_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function Help_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ OperationRequest_Type }

constructor OperationRequest_Type.Create();
begin
  inherited Create();
  FHTTPHeaders := HTTPHeaders_Type.Create();
  FArguments := Arguments_Type.Create();
  FErrors := Errors_Type.Create();
end;

destructor OperationRequest_Type.Destroy();
begin
  inherited Destroy();
end;

function OperationRequest_Type.HasHTTPHeaders() : Boolean;
begin
  Result := True;
end;

function OperationRequest_Type.HasRequestId() : Boolean;
begin
  Result := True;
end;

function OperationRequest_Type.HasArguments() : Boolean;
begin
  Result := True;
end;

function OperationRequest_Type.HasErrors() : Boolean;
begin
  Result := True;
end;

function OperationRequest_Type.HasRequestProcessingTime() : Boolean;
begin
  Result := True;
end;

{ Information_Type }

constructor Information_Type.Create();
begin
  inherited Create();
  FOperationInformation := Information_OperationInformationArray.Create();
  FResponseGroupInformation := Information_ResponseGroupInformationArray.Create();
end;

destructor Information_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function Information_Type.HasRequest() : Boolean;
begin
  Result := True;
end;

{ HelpResponse_Type }

constructor HelpResponse_Type.Create();
begin
  inherited Create();
  FInformation := HelpResponse_InformationArray.Create();
end;

destructor HelpResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function HelpResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ ItemSearchRequest }

constructor ItemSearchRequest.Create();
begin
  inherited Create();
  FAudienceRating := ItemSearchRequest_AudienceRatingArray.Create();
  FResponseGroup := ItemSearchRequest_ResponseGroupArray.Create();
end;

destructor ItemSearchRequest.Destroy();
begin
  inherited Destroy();
end;

function ItemSearchRequest.HasActor() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasArtist() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasAvailability() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasAuthor() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasBrand() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasBrowseNode() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasCity() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasComposer() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasCondition() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasConductor() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasCount() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasCuisine() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasDeliveryMethod() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasDirector() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasFutureLaunchDate() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasISPUPostalCode() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasItemPage() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasKeywords() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasManufacturer() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasMaximumPrice() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasMerchantId() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasMinimumPrice() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasMusicLabel() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasNeighborhood() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasOrchestra() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasPostalCode() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasPower() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasPublisher() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasReviewSort() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasSearchIndex() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasSort() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasState() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasTextStream() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasTitle() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest.HasReleaseDate() : Boolean;
begin
  Result := True;
end;

{ ItemSearch_Type }

constructor ItemSearch_Type.Create();
begin
  inherited Create();
  FRequest := ItemSearch_RequestArray.Create();
end;

destructor ItemSearch_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function ItemSearch_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function ItemSearch_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function ItemSearch_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function ItemSearch_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function ItemSearch_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function ItemSearch_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function ItemSearch_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ Items_Type }

constructor Items_Type.Create();
begin
  inherited Create();
  FSearchResultsMap := SearchResultsMap_Type.Create();
  F_Item := Items__ItemArray.Create();
  FSearchBinSets := SearchBinSets_Type.Create();
end;

destructor Items_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  if Assigned(FCorrectedQuery) then
    FreeAndNil(FCorrectedQuery);
  inherited Destroy();
end;

function Items_Type.HasRequest() : Boolean;
begin
  Result := True;
end;

function Items_Type.HasCorrectedQuery() : Boolean;
begin
  Result := True;
end;

function Items_Type.HasQid() : Boolean;
begin
  Result := True;
end;

function Items_Type.HasTotalResults() : Boolean;
begin
  Result := True;
end;

function Items_Type.HasTotalPages() : Boolean;
begin
  Result := True;
end;

function Items_Type.HasSearchResultsMap() : Boolean;
begin
  Result := True;
end;

function Items_Type.HasSearchBinSets() : Boolean;
begin
  Result := True;
end;

{ ItemSearchResponse_Type }

constructor ItemSearchResponse_Type.Create();
begin
  inherited Create();
  FItems := ItemSearchResponse_ItemsArray.Create();
end;

destructor ItemSearchResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function ItemSearchResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ ItemLookupRequest }

constructor ItemLookupRequest.Create();
begin
  inherited Create();
  FItemId := ItemLookupRequest_ItemIdArray.Create();
  FResponseGroup := ItemLookupRequest_ResponseGroupArray.Create();
end;

destructor ItemLookupRequest.Destroy();
begin
  inherited Destroy();
end;

function ItemLookupRequest.HasCondition() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest.HasDeliveryMethod() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest.HasFutureLaunchDate() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest.HasIdType() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest.HasISPUPostalCode() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest.HasMerchantId() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest.HasOfferPage() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest.HasReviewPage() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest.HasReviewSort() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest.HasSearchIndex() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest.HasSearchInsideKeywords() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest.HasVariationPage() : Boolean;
begin
  Result := True;
end;

{ ItemLookup_Type }

constructor ItemLookup_Type.Create();
begin
  inherited Create();
  FRequest := ItemLookup_RequestArray.Create();
end;

destructor ItemLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function ItemLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function ItemLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function ItemLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function ItemLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function ItemLookup_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function ItemLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function ItemLookup_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ ItemLookupResponse_Type }

constructor ItemLookupResponse_Type.Create();
begin
  inherited Create();
  FItems := ItemLookupResponse_ItemsArray.Create();
end;

destructor ItemLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function ItemLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ BrowseNodeLookupRequest }

constructor BrowseNodeLookupRequest.Create();
begin
  inherited Create();
  FBrowseNodeId := BrowseNodeLookupRequest_BrowseNodeIdArray.Create();
  FResponseGroup := BrowseNodeLookupRequest_ResponseGroupArray.Create();
end;

destructor BrowseNodeLookupRequest.Destroy();
begin
  inherited Destroy();
end;

{ BrowseNodeLookup_Type }

constructor BrowseNodeLookup_Type.Create();
begin
  inherited Create();
  FRequest := BrowseNodeLookup_RequestArray.Create();
end;

destructor BrowseNodeLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function BrowseNodeLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function BrowseNodeLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function BrowseNodeLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function BrowseNodeLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function BrowseNodeLookup_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function BrowseNodeLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function BrowseNodeLookup_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ BrowseNodes_Type }

constructor BrowseNodes_Type.Create();
begin
  inherited Create();
  FBrowseNode := BrowseNodes_BrowseNodeArray.Create();
end;

destructor BrowseNodes_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function BrowseNodes_Type.HasRequest() : Boolean;
begin
  Result := True;
end;

{ BrowseNodeLookupResponse_Type }

constructor BrowseNodeLookupResponse_Type.Create();
begin
  inherited Create();
  FBrowseNodes := BrowseNodeLookupResponse_BrowseNodesArray.Create();
end;

destructor BrowseNodeLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function BrowseNodeLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ ListSearchRequest }

constructor ListSearchRequest.Create();
begin
  inherited Create();
  FResponseGroup := ListSearchRequest_ResponseGroupArray.Create();
end;

destructor ListSearchRequest.Destroy();
begin
  inherited Destroy();
end;

function ListSearchRequest.HasCity() : Boolean;
begin
  Result := True;
end;

function ListSearchRequest.HasEmail() : Boolean;
begin
  Result := True;
end;

function ListSearchRequest.HasFirstName() : Boolean;
begin
  Result := True;
end;

function ListSearchRequest.HasLastName() : Boolean;
begin
  Result := True;
end;

function ListSearchRequest.HasListPage() : Boolean;
begin
  Result := True;
end;

function ListSearchRequest.HasName() : Boolean;
begin
  Result := True;
end;

function ListSearchRequest.HasState() : Boolean;
begin
  Result := True;
end;

{ ListSearch_Type }

constructor ListSearch_Type.Create();
begin
  inherited Create();
  FRequest := ListSearch_RequestArray.Create();
end;

destructor ListSearch_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function ListSearch_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function ListSearch_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function ListSearch_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function ListSearch_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function ListSearch_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function ListSearch_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function ListSearch_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ Lists_Type }

constructor Lists_Type.Create();
begin
  inherited Create();
  FList := Lists_ListArray.Create();
end;

destructor Lists_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function Lists_Type.HasRequest() : Boolean;
begin
  Result := True;
end;

function Lists_Type.HasTotalResults() : Boolean;
begin
  Result := True;
end;

function Lists_Type.HasTotalPages() : Boolean;
begin
  Result := True;
end;

{ ListSearchResponse_Type }

constructor ListSearchResponse_Type.Create();
begin
  inherited Create();
  FLists := ListSearchResponse_ListsArray.Create();
end;

destructor ListSearchResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function ListSearchResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ ListLookupRequest }

constructor ListLookupRequest.Create();
begin
  inherited Create();
  FResponseGroup := ListLookupRequest_ResponseGroupArray.Create();
end;

destructor ListLookupRequest.Destroy();
begin
  inherited Destroy();
end;

function ListLookupRequest.HasCondition() : Boolean;
begin
  Result := True;
end;

function ListLookupRequest.HasDeliveryMethod() : Boolean;
begin
  Result := True;
end;

function ListLookupRequest.HasISPUPostalCode() : Boolean;
begin
  Result := True;
end;

function ListLookupRequest.HasListId() : Boolean;
begin
  Result := True;
end;

function ListLookupRequest.HasListType() : Boolean;
begin
  Result := True;
end;

function ListLookupRequest.HasMerchantId() : Boolean;
begin
  Result := True;
end;

function ListLookupRequest.HasProductGroup() : Boolean;
begin
  Result := True;
end;

function ListLookupRequest.HasProductPage() : Boolean;
begin
  Result := True;
end;

function ListLookupRequest.HasReviewSort() : Boolean;
begin
  Result := True;
end;

function ListLookupRequest.HasSort() : Boolean;
begin
  Result := True;
end;

{ ListLookup_Type }

constructor ListLookup_Type.Create();
begin
  inherited Create();
  FRequest := ListLookup_RequestArray.Create();
end;

destructor ListLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function ListLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function ListLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function ListLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function ListLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function ListLookup_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function ListLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function ListLookup_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ ListLookupResponse_Type }

constructor ListLookupResponse_Type.Create();
begin
  inherited Create();
  FLists := ListLookupResponse_ListsArray.Create();
end;

destructor ListLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function ListLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ CustomerContentSearchRequest }

constructor CustomerContentSearchRequest.Create();
begin
  inherited Create();
  FResponseGroup := CustomerContentSearchRequest_ResponseGroupArray.Create();
end;

destructor CustomerContentSearchRequest.Destroy();
begin
  inherited Destroy();
end;

function CustomerContentSearchRequest.HasCustomerPage() : Boolean;
begin
  Result := True;
end;

function CustomerContentSearchRequest.HasEmail() : Boolean;
begin
  Result := True;
end;

function CustomerContentSearchRequest.HasName() : Boolean;
begin
  Result := True;
end;

{ CustomerContentSearch_Type }

constructor CustomerContentSearch_Type.Create();
begin
  inherited Create();
  FRequest := CustomerContentSearch_RequestArray.Create();
end;

destructor CustomerContentSearch_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function CustomerContentSearch_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function CustomerContentSearch_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function CustomerContentSearch_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function CustomerContentSearch_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function CustomerContentSearch_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function CustomerContentSearch_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function CustomerContentSearch_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ Customers_Type }

constructor Customers_Type.Create();
begin
  inherited Create();
  FCustomer := Customers_CustomerArray.Create();
end;

destructor Customers_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function Customers_Type.HasRequest() : Boolean;
begin
  Result := True;
end;

function Customers_Type.HasTotalResults() : Boolean;
begin
  Result := True;
end;

function Customers_Type.HasTotalPages() : Boolean;
begin
  Result := True;
end;

{ CustomerContentSearchResponse_Type }

constructor CustomerContentSearchResponse_Type.Create();
begin
  inherited Create();
  FCustomers := CustomerContentSearchResponse_CustomersArray.Create();
end;

destructor CustomerContentSearchResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function CustomerContentSearchResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ CustomerContentLookupRequest }

constructor CustomerContentLookupRequest.Create();
begin
  inherited Create();
  FResponseGroup := CustomerContentLookupRequest_ResponseGroupArray.Create();
end;

destructor CustomerContentLookupRequest.Destroy();
begin
  inherited Destroy();
end;

function CustomerContentLookupRequest.HasCustomerId() : Boolean;
begin
  Result := True;
end;

function CustomerContentLookupRequest.HasReviewPage() : Boolean;
begin
  Result := True;
end;

{ CustomerContentLookup_Type }

constructor CustomerContentLookup_Type.Create();
begin
  inherited Create();
  FRequest := CustomerContentLookup_RequestArray.Create();
end;

destructor CustomerContentLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function CustomerContentLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function CustomerContentLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function CustomerContentLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function CustomerContentLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function CustomerContentLookup_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function CustomerContentLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function CustomerContentLookup_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ CustomerContentLookupResponse_Type }

constructor CustomerContentLookupResponse_Type.Create();
begin
  inherited Create();
  FCustomers := CustomerContentLookupResponse_CustomersArray.Create();
end;

destructor CustomerContentLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function CustomerContentLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ SimilarityLookupRequest }

constructor SimilarityLookupRequest.Create();
begin
  inherited Create();
  FItemId := SimilarityLookupRequest_ItemIdArray.Create();
  FResponseGroup := SimilarityLookupRequest_ResponseGroupArray.Create();
end;

destructor SimilarityLookupRequest.Destroy();
begin
  inherited Destroy();
end;

function SimilarityLookupRequest.HasCondition() : Boolean;
begin
  Result := True;
end;

function SimilarityLookupRequest.HasDeliveryMethod() : Boolean;
begin
  Result := True;
end;

function SimilarityLookupRequest.HasISPUPostalCode() : Boolean;
begin
  Result := True;
end;

function SimilarityLookupRequest.HasMerchantId() : Boolean;
begin
  Result := True;
end;

function SimilarityLookupRequest.HasReviewSort() : Boolean;
begin
  Result := True;
end;

function SimilarityLookupRequest.HasSimilarityType() : Boolean;
begin
  Result := True;
end;

{ SimilarityLookup_Type }

constructor SimilarityLookup_Type.Create();
begin
  inherited Create();
  FRequest := SimilarityLookup_RequestArray.Create();
end;

destructor SimilarityLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function SimilarityLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function SimilarityLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function SimilarityLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function SimilarityLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function SimilarityLookup_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function SimilarityLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function SimilarityLookup_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ SimilarityLookupResponse_Type }

constructor SimilarityLookupResponse_Type.Create();
begin
  inherited Create();
  FItems := SimilarityLookupResponse_ItemsArray.Create();
end;

destructor SimilarityLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function SimilarityLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ SellerLookupRequest }

constructor SellerLookupRequest.Create();
begin
  inherited Create();
  FResponseGroup := SellerLookupRequest_ResponseGroupArray.Create();
  FSellerId := SellerLookupRequest_SellerIdArray.Create();
end;

destructor SellerLookupRequest.Destroy();
begin
  inherited Destroy();
end;

function SellerLookupRequest.HasFeedbackPage() : Boolean;
begin
  Result := True;
end;

{ SellerLookup_Type }

constructor SellerLookup_Type.Create();
begin
  inherited Create();
  FRequest := SellerLookup_RequestArray.Create();
end;

destructor SellerLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function SellerLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function SellerLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function SellerLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function SellerLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function SellerLookup_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function SellerLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function SellerLookup_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ Sellers_Type }

constructor Sellers_Type.Create();
begin
  inherited Create();
  FSeller := Sellers_SellerArray.Create();
end;

destructor Sellers_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function Sellers_Type.HasRequest() : Boolean;
begin
  Result := True;
end;

function Sellers_Type.HasTotalResults() : Boolean;
begin
  Result := True;
end;

function Sellers_Type.HasTotalPages() : Boolean;
begin
  Result := True;
end;

{ SellerLookupResponse_Type }

constructor SellerLookupResponse_Type.Create();
begin
  inherited Create();
  FSellers := SellerLookupResponse_SellersArray.Create();
end;

destructor SellerLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function SellerLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ CartGetRequest }

constructor CartGetRequest.Create();
begin
  inherited Create();
  FResponseGroup := CartGetRequest_ResponseGroupArray.Create();
end;

destructor CartGetRequest.Destroy();
begin
  inherited Destroy();
end;

function CartGetRequest.HasCartId() : Boolean;
begin
  Result := True;
end;

function CartGetRequest.HasHMAC() : Boolean;
begin
  Result := True;
end;

function CartGetRequest.HasMergeCart() : Boolean;
begin
  Result := True;
end;

{ CartGet_Type }

constructor CartGet_Type.Create();
begin
  inherited Create();
  FRequest := CartGet_RequestArray.Create();
end;

destructor CartGet_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function CartGet_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function CartGet_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function CartGet_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function CartGet_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function CartGet_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function CartGet_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function CartGet_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ Cart_Type }

constructor Cart_Type.Create();
begin
  inherited Create();
  FSimilarProducts := SimilarProducts_Type.Create();
  FTopSellers := TopSellers_Type.Create();
  FNewReleases := NewReleases_Type.Create();
  FSimilarViewedProducts := SimilarViewedProducts_Type.Create();
  FOtherCategoriesSimilarProducts := OtherCategoriesSimilarProducts_Type.Create();
end;

destructor Cart_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  if Assigned(FSubTotal) then
    FreeAndNil(FSubTotal);
  if Assigned(FCartItems) then
    FreeAndNil(FCartItems);
  if Assigned(FSavedForLaterItems) then
    FreeAndNil(FSavedForLaterItems);
  inherited Destroy();
end;

function Cart_Type.HasRequest() : Boolean;
begin
  Result := True;
end;

function Cart_Type.HasPurchaseURL() : Boolean;
begin
  Result := True;
end;

function Cart_Type.HasSubTotal() : Boolean;
begin
  Result := True;
end;

function Cart_Type.HasCartItems() : Boolean;
begin
  Result := True;
end;

function Cart_Type.HasSavedForLaterItems() : Boolean;
begin
  Result := True;
end;

function Cart_Type.HasSimilarProducts() : Boolean;
begin
  Result := True;
end;

function Cart_Type.HasTopSellers() : Boolean;
begin
  Result := True;
end;

function Cart_Type.HasNewReleases() : Boolean;
begin
  Result := True;
end;

function Cart_Type.HasSimilarViewedProducts() : Boolean;
begin
  Result := True;
end;

function Cart_Type.HasOtherCategoriesSimilarProducts() : Boolean;
begin
  Result := True;
end;

{ CartGetResponse_Type }

constructor CartGetResponse_Type.Create();
begin
  inherited Create();
  FCart := CartGetResponse_CartArray.Create();
end;

destructor CartGetResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function CartGetResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ CartAddRequest }

constructor CartAddRequest.Create();
begin
  inherited Create();
  FItems := CartAddRequest_Items_Type.Create();
  FResponseGroup := CartAddRequest_ResponseGroupArray.Create();
end;

destructor CartAddRequest.Destroy();
begin
  inherited Destroy();
end;

function CartAddRequest.HasCartId() : Boolean;
begin
  Result := True;
end;

function CartAddRequest.HasHMAC() : Boolean;
begin
  Result := True;
end;

function CartAddRequest.HasMergeCart() : Boolean;
begin
  Result := True;
end;

function CartAddRequest.HasItems() : Boolean;
begin
  Result := True;
end;

{ CartAdd_Type }

constructor CartAdd_Type.Create();
begin
  inherited Create();
  FRequest := CartAdd_RequestArray.Create();
end;

destructor CartAdd_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function CartAdd_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function CartAdd_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function CartAdd_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function CartAdd_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function CartAdd_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function CartAdd_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function CartAdd_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ CartAddResponse_Type }

constructor CartAddResponse_Type.Create();
begin
  inherited Create();
  FCart := CartAddResponse_CartArray.Create();
end;

destructor CartAddResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function CartAddResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ CartCreateRequest }

constructor CartCreateRequest.Create();
begin
  inherited Create();
  FItems := CartCreateRequest_Items_Type.Create();
  FResponseGroup := CartCreateRequest_ResponseGroupArray.Create();
end;

destructor CartCreateRequest.Destroy();
begin
  inherited Destroy();
end;

function CartCreateRequest.HasMergeCart() : Boolean;
begin
  Result := True;
end;

function CartCreateRequest.HasItems() : Boolean;
begin
  Result := True;
end;

{ CartCreate_Type }

constructor CartCreate_Type.Create();
begin
  inherited Create();
  FRequest := CartCreate_RequestArray.Create();
end;

destructor CartCreate_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function CartCreate_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function CartCreate_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function CartCreate_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function CartCreate_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function CartCreate_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function CartCreate_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function CartCreate_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ CartCreateResponse_Type }

constructor CartCreateResponse_Type.Create();
begin
  inherited Create();
  FCart := CartCreateResponse_CartArray.Create();
end;

destructor CartCreateResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function CartCreateResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ CartModifyRequest }

constructor CartModifyRequest.Create();
begin
  inherited Create();
  FItems := CartModifyRequest_Items_Type.Create();
  FResponseGroup := CartModifyRequest_ResponseGroupArray.Create();
end;

destructor CartModifyRequest.Destroy();
begin
  inherited Destroy();
end;

function CartModifyRequest.HasCartId() : Boolean;
begin
  Result := True;
end;

function CartModifyRequest.HasHMAC() : Boolean;
begin
  Result := True;
end;

function CartModifyRequest.HasMergeCart() : Boolean;
begin
  Result := True;
end;

function CartModifyRequest.HasItems() : Boolean;
begin
  Result := True;
end;

{ CartModify_Type }

constructor CartModify_Type.Create();
begin
  inherited Create();
  FRequest := CartModify_RequestArray.Create();
end;

destructor CartModify_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function CartModify_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function CartModify_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function CartModify_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function CartModify_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function CartModify_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function CartModify_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function CartModify_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ CartModifyResponse_Type }

constructor CartModifyResponse_Type.Create();
begin
  inherited Create();
  FCart := CartModifyResponse_CartArray.Create();
end;

destructor CartModifyResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function CartModifyResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ CartClearRequest }

constructor CartClearRequest.Create();
begin
  inherited Create();
  FResponseGroup := CartClearRequest_ResponseGroupArray.Create();
end;

destructor CartClearRequest.Destroy();
begin
  inherited Destroy();
end;

function CartClearRequest.HasCartId() : Boolean;
begin
  Result := True;
end;

function CartClearRequest.HasHMAC() : Boolean;
begin
  Result := True;
end;

function CartClearRequest.HasMergeCart() : Boolean;
begin
  Result := True;
end;

{ CartClear_Type }

constructor CartClear_Type.Create();
begin
  inherited Create();
  FRequest := CartClear_RequestArray.Create();
end;

destructor CartClear_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function CartClear_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function CartClear_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function CartClear_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function CartClear_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function CartClear_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function CartClear_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function CartClear_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ CartClearResponse_Type }

constructor CartClearResponse_Type.Create();
begin
  inherited Create();
  FCart := CartClearResponse_CartArray.Create();
end;

destructor CartClearResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function CartClearResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ TransactionLookupRequest }

constructor TransactionLookupRequest.Create();
begin
  inherited Create();
  FResponseGroup := TransactionLookupRequest_ResponseGroupArray.Create();
  FTransactionId := TransactionLookupRequest_TransactionIdArray.Create();
end;

destructor TransactionLookupRequest.Destroy();
begin
  inherited Destroy();
end;

{ TransactionLookup_Type }

constructor TransactionLookup_Type.Create();
begin
  inherited Create();
  FRequest := TransactionLookup_RequestArray.Create();
end;

destructor TransactionLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function TransactionLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function TransactionLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function TransactionLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function TransactionLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function TransactionLookup_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function TransactionLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function TransactionLookup_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ Transactions_Type }

constructor Transactions_Type.Create();
begin
  inherited Create();
  FTransaction := Transactions_TransactionArray.Create();
end;

destructor Transactions_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function Transactions_Type.HasRequest() : Boolean;
begin
  Result := True;
end;

function Transactions_Type.HasTotalResults() : Boolean;
begin
  Result := True;
end;

function Transactions_Type.HasTotalPages() : Boolean;
begin
  Result := True;
end;

{ TransactionLookupResponse_Type }

constructor TransactionLookupResponse_Type.Create();
begin
  inherited Create();
  FTransactions := TransactionLookupResponse_TransactionsArray.Create();
end;

destructor TransactionLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function TransactionLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ SellerListingSearchRequest }

constructor SellerListingSearchRequest.Create();
begin
  inherited Create();
  FResponseGroup := SellerListingSearchRequest_ResponseGroupArray.Create();
end;

destructor SellerListingSearchRequest.Destroy();
begin
  inherited Destroy();
end;

function SellerListingSearchRequest.HasKeywords() : Boolean;
begin
  Result := True;
end;

function SellerListingSearchRequest.HasListingPage() : Boolean;
begin
  Result := True;
end;

function SellerListingSearchRequest.HasOfferStatus() : Boolean;
begin
  Result := True;
end;

function SellerListingSearchRequest.HasSort() : Boolean;
begin
  Result := True;
end;

function SellerListingSearchRequest.HasTitle() : Boolean;
begin
  Result := True;
end;

{ SellerListingSearch_Type }

constructor SellerListingSearch_Type.Create();
begin
  inherited Create();
  FRequest := SellerListingSearch_RequestArray.Create();
end;

destructor SellerListingSearch_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function SellerListingSearch_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function SellerListingSearch_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function SellerListingSearch_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function SellerListingSearch_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function SellerListingSearch_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function SellerListingSearch_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function SellerListingSearch_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ SellerListings_Type }

constructor SellerListings_Type.Create();
begin
  inherited Create();
  FSellerListing := SellerListings_SellerListingArray.Create();
end;

destructor SellerListings_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function SellerListings_Type.HasRequest() : Boolean;
begin
  Result := True;
end;

function SellerListings_Type.HasTotalResults() : Boolean;
begin
  Result := True;
end;

function SellerListings_Type.HasTotalPages() : Boolean;
begin
  Result := True;
end;

{ SellerListingSearchResponse_Type }

constructor SellerListingSearchResponse_Type.Create();
begin
  inherited Create();
  FSellerListings := SellerListingSearchResponse_SellerListingsArray.Create();
end;

destructor SellerListingSearchResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function SellerListingSearchResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ SellerListingLookupRequest }

constructor SellerListingLookupRequest.Create();
begin
  inherited Create();
  FResponseGroup := SellerListingLookupRequest_ResponseGroupArray.Create();
end;

destructor SellerListingLookupRequest.Destroy();
begin
  inherited Destroy();
end;

function SellerListingLookupRequest.HasSellerId() : Boolean;
begin
  Result := True;
end;

{ SellerListingLookup_Type }

constructor SellerListingLookup_Type.Create();
begin
  inherited Create();
  FRequest := SellerListingLookup_RequestArray.Create();
end;

destructor SellerListingLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  inherited Destroy();
end;

function SellerListingLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := True;
end;

function SellerListingLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := True;
end;

function SellerListingLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := True;
end;

function SellerListingLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function SellerListingLookup_Type.HasValidate() : Boolean;
begin
  Result := True;
end;

function SellerListingLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := True;
end;

function SellerListingLookup_Type.HasShared() : Boolean;
begin
  Result := True;
end;

{ SellerListingLookupResponse_Type }

constructor SellerListingLookupResponse_Type.Create();
begin
  inherited Create();
  FSellerListings := SellerListingLookupResponse_SellerListingsArray.Create();
end;

destructor SellerListingLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  inherited Destroy();
end;

function SellerListingLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

{ MultiOperationType }

destructor MultiOperationType.Destroy();
begin
  if Assigned(FHelp) then
    FreeAndNil(FHelp);
  if Assigned(FItemSearch) then
    FreeAndNil(FItemSearch);
  if Assigned(FItemLookup) then
    FreeAndNil(FItemLookup);
  if Assigned(FListSearch) then
    FreeAndNil(FListSearch);
  if Assigned(FListLookup) then
    FreeAndNil(FListLookup);
  if Assigned(FCustomerContentSearch) then
    FreeAndNil(FCustomerContentSearch);
  if Assigned(FCustomerContentLookup) then
    FreeAndNil(FCustomerContentLookup);
  if Assigned(FSimilarityLookup) then
    FreeAndNil(FSimilarityLookup);
  if Assigned(FSellerLookup) then
    FreeAndNil(FSellerLookup);
  if Assigned(FCartGet) then
    FreeAndNil(FCartGet);
  if Assigned(FCartAdd) then
    FreeAndNil(FCartAdd);
  if Assigned(FCartCreate) then
    FreeAndNil(FCartCreate);
  if Assigned(FCartModify) then
    FreeAndNil(FCartModify);
  if Assigned(FCartClear) then
    FreeAndNil(FCartClear);
  if Assigned(FTransactionLookup) then
    FreeAndNil(FTransactionLookup);
  if Assigned(FSellerListingSearch) then
    FreeAndNil(FSellerListingSearch);
  if Assigned(FSellerListingLookup) then
    FreeAndNil(FSellerListingLookup);
  if Assigned(FBrowseNodeLookup) then
    FreeAndNil(FBrowseNodeLookup);
  inherited Destroy();
end;

function MultiOperationType.HasHelp() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasItemSearch() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasItemLookup() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasListSearch() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasListLookup() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasCustomerContentSearch() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasCustomerContentLookup() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasSimilarityLookup() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasSellerLookup() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasCartGet() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasCartAdd() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasCartCreate() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasCartModify() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasCartClear() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasTransactionLookup() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasSellerListingSearch() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasSellerListingLookup() : Boolean;
begin
  Result := True;
end;

function MultiOperationType.HasBrowseNodeLookup() : Boolean;
begin
  Result := True;
end;

{ MultiOperationResponse }

destructor MultiOperationResponse.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FHelpResponse) then
    FreeAndNil(FHelpResponse);
  if Assigned(FItemSearchResponse) then
    FreeAndNil(FItemSearchResponse);
  if Assigned(FItemLookupResponse) then
    FreeAndNil(FItemLookupResponse);
  if Assigned(FListSearchResponse) then
    FreeAndNil(FListSearchResponse);
  if Assigned(FListLookupResponse) then
    FreeAndNil(FListLookupResponse);
  if Assigned(FCustomerContentSearchResponse) then
    FreeAndNil(FCustomerContentSearchResponse);
  if Assigned(FCustomerContentLookupResponse) then
    FreeAndNil(FCustomerContentLookupResponse);
  if Assigned(FSimilarityLookupResponse) then
    FreeAndNil(FSimilarityLookupResponse);
  if Assigned(FSellerLookupResponse) then
    FreeAndNil(FSellerLookupResponse);
  if Assigned(FCartGetResponse) then
    FreeAndNil(FCartGetResponse);
  if Assigned(FCartAddResponse) then
    FreeAndNil(FCartAddResponse);
  if Assigned(FCartCreateResponse) then
    FreeAndNil(FCartCreateResponse);
  if Assigned(FCartModifyResponse) then
    FreeAndNil(FCartModifyResponse);
  if Assigned(FCartClearResponse) then
    FreeAndNil(FCartClearResponse);
  if Assigned(FTransactionLookupResponse) then
    FreeAndNil(FTransactionLookupResponse);
  if Assigned(FSellerListingSearchResponse) then
    FreeAndNil(FSellerListingSearchResponse);
  if Assigned(FSellerListingLookupResponse) then
    FreeAndNil(FSellerListingLookupResponse);
  if Assigned(FBrowseNodeLookupResponse) then
    FreeAndNil(FBrowseNodeLookupResponse);
  inherited Destroy();
end;

function MultiOperationResponse.HasOperationRequest() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasHelpResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasItemSearchResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasItemLookupResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasListSearchResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasListLookupResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasCustomerContentSearchResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasCustomerContentLookupResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasSimilarityLookupResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasSellerLookupResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasCartGetResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasCartAddResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasCartCreateResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasCartModifyResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasCartClearResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasTransactionLookupResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasSellerListingSearchResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasSellerListingLookupResponse() : Boolean;
begin
  Result := True;
end;

function MultiOperationResponse.HasBrowseNodeLookupResponse() : Boolean;
begin
  Result := True;
end;

{ Bin_Type }

constructor Bin_Type.Create();
begin
  inherited Create();
  FBinParameter := Bin_BinParameterArray.Create();
end;

destructor Bin_Type.Destroy();
begin
  inherited Destroy();
end;

{ SearchBinSet_Type }

constructor SearchBinSet_Type.Create();
begin
  inherited Create();
  FBin := SearchBinSet_BinArray.Create();
end;

destructor SearchBinSet_Type.Destroy();
begin
  inherited Destroy();
end;

{ CartAddRequest_Items_Type_Item_Type }

function CartAddRequest_Items_Type_Item_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function CartAddRequest_Items_Type_Item_Type.HasOfferListingId() : Boolean;
begin
  Result := True;
end;

function CartAddRequest_Items_Type_Item_Type.HasQuantity() : Boolean;
begin
  Result := True;
end;

function CartAddRequest_Items_Type_Item_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function CartAddRequest_Items_Type_Item_Type.HasListItemId() : Boolean;
begin
  Result := True;
end;

{ CartCreateRequest_Items_Type_Item_Type }

function CartCreateRequest_Items_Type_Item_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function CartCreateRequest_Items_Type_Item_Type.HasOfferListingId() : Boolean;
begin
  Result := True;
end;

function CartCreateRequest_Items_Type_Item_Type.HasQuantity() : Boolean;
begin
  Result := True;
end;

function CartCreateRequest_Items_Type_Item_Type.HasAssociateTag() : Boolean;
begin
  Result := True;
end;

function CartCreateRequest_Items_Type_Item_Type.HasListItemId() : Boolean;
begin
  Result := True;
end;

{ CartModifyRequest_Items_Type_Item_Type }

function CartModifyRequest_Items_Type_Item_Type.HasAction() : Boolean;
begin
  Result := True;
end;

function CartModifyRequest_Items_Type_Item_Type.HasCartItemId() : Boolean;
begin
  Result := True;
end;

function CartModifyRequest_Items_Type_Item_Type.HasQuantity() : Boolean;
begin
  Result := True;
end;

{ Request_Type }

constructor Request_Type.Create();
begin
  inherited Create();
  FErrors := Errors_Type.Create();
end;

destructor Request_Type.Destroy();
begin
  if Assigned(FHelpRequest) then
    FreeAndNil(FHelpRequest);
  if Assigned(FBrowseNodeLookupRequest) then
    FreeAndNil(FBrowseNodeLookupRequest);
  if Assigned(FItemSearchRequest) then
    FreeAndNil(FItemSearchRequest);
  if Assigned(FItemLookupRequest) then
    FreeAndNil(FItemLookupRequest);
  if Assigned(FListSearchRequest) then
    FreeAndNil(FListSearchRequest);
  if Assigned(FListLookupRequest) then
    FreeAndNil(FListLookupRequest);
  if Assigned(FCustomerContentSearchRequest) then
    FreeAndNil(FCustomerContentSearchRequest);
  if Assigned(FCustomerContentLookupRequest) then
    FreeAndNil(FCustomerContentLookupRequest);
  if Assigned(FSimilarityLookupRequest) then
    FreeAndNil(FSimilarityLookupRequest);
  if Assigned(FCartGetRequest) then
    FreeAndNil(FCartGetRequest);
  if Assigned(FCartAddRequest) then
    FreeAndNil(FCartAddRequest);
  if Assigned(FCartCreateRequest) then
    FreeAndNil(FCartCreateRequest);
  if Assigned(FCartModifyRequest) then
    FreeAndNil(FCartModifyRequest);
  if Assigned(FCartClearRequest) then
    FreeAndNil(FCartClearRequest);
  if Assigned(FTransactionLookupRequest) then
    FreeAndNil(FTransactionLookupRequest);
  if Assigned(FSellerListingSearchRequest) then
    FreeAndNil(FSellerListingSearchRequest);
  if Assigned(FSellerListingLookupRequest) then
    FreeAndNil(FSellerListingLookupRequest);
  if Assigned(FSellerLookupRequest) then
    FreeAndNil(FSellerLookupRequest);
  inherited Destroy();
end;

function Request_Type.HasIsValid() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasHelpRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasBrowseNodeLookupRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasItemSearchRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasItemLookupRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasListSearchRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasListLookupRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasCustomerContentSearchRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasCustomerContentLookupRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasSimilarityLookupRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasCartGetRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasCartAddRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasCartCreateRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasCartModifyRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasCartClearRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasTransactionLookupRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasSellerListingSearchRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasSellerListingLookupRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasSellerLookupRequest() : Boolean;
begin
  Result := True;
end;

function Request_Type.HasErrors() : Boolean;
begin
  Result := True;
end;

{ OperationInformation_Type }

constructor OperationInformation_Type.Create();
begin
  inherited Create();
  FRequiredParameters := OperationInformation_RequiredParameters_Type.Create();
  FAvailableParameters := OperationInformation_AvailableParameters_Type.Create();
  FDefaultResponseGroups := OperationInformation_DefaultResponseGroups_Type.Create();
  FAvailableResponseGroups := OperationInformation_AvailableResponseGroups_Type.Create();
end;

destructor OperationInformation_Type.Destroy();
begin
  inherited Destroy();
end;

function OperationInformation_Type.HasName() : Boolean;
begin
  Result := True;
end;

function OperationInformation_Type.HasDescription() : Boolean;
begin
  Result := True;
end;

function OperationInformation_Type.HasRequiredParameters() : Boolean;
begin
  Result := True;
end;

function OperationInformation_Type.HasAvailableParameters() : Boolean;
begin
  Result := True;
end;

function OperationInformation_Type.HasDefaultResponseGroups() : Boolean;
begin
  Result := True;
end;

function OperationInformation_Type.HasAvailableResponseGroups() : Boolean;
begin
  Result := True;
end;

{ ResponseGroupInformation_Type }

constructor ResponseGroupInformation_Type.Create();
begin
  inherited Create();
  FValidOperations := ResponseGroupInformation_ValidOperations_Type.Create();
  FElements := ResponseGroupInformation_Elements_Type.Create();
end;

destructor ResponseGroupInformation_Type.Destroy();
begin
  inherited Destroy();
end;

function ResponseGroupInformation_Type.HasName() : Boolean;
begin
  Result := True;
end;

function ResponseGroupInformation_Type.HasCreationDate() : Boolean;
begin
  Result := True;
end;

function ResponseGroupInformation_Type.HasValidOperations() : Boolean;
begin
  Result := True;
end;

function ResponseGroupInformation_Type.HasElements() : Boolean;
begin
  Result := True;
end;

{ CorrectedQuery_Type }

function CorrectedQuery_Type.HasKeywords() : Boolean;
begin
  Result := True;
end;

function CorrectedQuery_Type.HasMessage() : Boolean;
begin
  Result := True;
end;

{ Item_Type }

constructor Item_Type.Create();
begin
  inherited Create();
  FErrors := Errors_Type.Create();
  FImageSets := _Item_ImageSetsArray.Create();
  FCollections := Collections_Type.Create();
  FSubjects := Item_Subjects_Type.Create();
  FEditorialReviews := EditorialReviews_Type.Create();
  FSimilarProducts := SimilarProducts_Type.Create();
  FAccessories := Accessories_Type.Create();
  FTracks := Tracks_Type.Create();
  FListmaniaLists := ListmaniaLists_Type.Create();
  FAlternateVersions := Item_AlternateVersions_Type.Create();
end;

destructor Item_Type.Destroy();
begin
  if Assigned(FSmallImage) then
    FreeAndNil(FSmallImage);
  if Assigned(FMediumImage) then
    FreeAndNil(FMediumImage);
  if Assigned(FLargeImage) then
    FreeAndNil(FLargeImage);
  if Assigned(FItemAttributes) then
    FreeAndNil(FItemAttributes);
  if Assigned(FMerchantItemAttributes) then
    FreeAndNil(FMerchantItemAttributes);
  if Assigned(FOfferSummary) then
    FreeAndNil(FOfferSummary);
  if Assigned(FOffers) then
    FreeAndNil(FOffers);
  if Assigned(FVariationSummary) then
    FreeAndNil(FVariationSummary);
  if Assigned(FVariations) then
    FreeAndNil(FVariations);
  if Assigned(FCustomerReviews) then
    FreeAndNil(FCustomerReviews);
  if Assigned(FBrowseNodes) then
    FreeAndNil(FBrowseNodes);
  if Assigned(FSearchInside) then
    FreeAndNil(FSearchInside);
  inherited Destroy();
end;

function Item_Type.HasParentASIN() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasErrors() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasDetailPageURL() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasSalesRank() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasSmallImage() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasMediumImage() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasLargeImage() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasItemAttributes() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasMerchantItemAttributes() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasCollections() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasSubjects() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasOfferSummary() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasOffers() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasVariationSummary() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasVariations() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasCustomerReviews() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasEditorialReviews() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasSimilarProducts() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasAccessories() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasTracks() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasBrowseNodes() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasListmaniaLists() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasSearchInside() : Boolean;
begin
  Result := True;
end;

function Item_Type.HasAlternateVersions() : Boolean;
begin
  Result := True;
end;

{ List_Type }

constructor List_Type.Create();
begin
  inherited Create();
  FListItem := List_ListItemArray.Create();
end;

destructor List_Type.Destroy();
begin
  if Assigned(FImage) then
    FreeAndNil(FImage);
  inherited Destroy();
end;

function List_Type.HasListURL() : Boolean;
begin
  Result := True;
end;

function List_Type.HasRegistryNumber() : Boolean;
begin
  Result := True;
end;

function List_Type.HasListName() : Boolean;
begin
  Result := True;
end;

function List_Type.HasListType() : Boolean;
begin
  Result := True;
end;

function List_Type.HasTotalItems() : Boolean;
begin
  Result := True;
end;

function List_Type.HasTotalPages() : Boolean;
begin
  Result := True;
end;

function List_Type.HasDateCreated() : Boolean;
begin
  Result := True;
end;

function List_Type.HasOccasionDate() : Boolean;
begin
  Result := True;
end;

function List_Type.HasCustomerName() : Boolean;
begin
  Result := True;
end;

function List_Type.HasPartnerName() : Boolean;
begin
  Result := True;
end;

function List_Type.HasAdditionalName() : Boolean;
begin
  Result := True;
end;

function List_Type.HasComment() : Boolean;
begin
  Result := True;
end;

function List_Type.HasImage() : Boolean;
begin
  Result := True;
end;

function List_Type.HasAverageRating() : Boolean;
begin
  Result := True;
end;

function List_Type.HasTotalVotes() : Boolean;
begin
  Result := True;
end;

function List_Type.HasTotalTimesRead() : Boolean;
begin
  Result := True;
end;

{ Customer_Type }

constructor Customer_Type.Create();
begin
  inherited Create();
  FCustomerReviews := Customer_CustomerReviewsArray.Create();
end;

destructor Customer_Type.Destroy();
begin
  if Assigned(FLocation) then
    FreeAndNil(FLocation);
  inherited Destroy();
end;

function Customer_Type.HasNickname() : Boolean;
begin
  Result := True;
end;

function Customer_Type.HasBirthday() : Boolean;
begin
  Result := True;
end;

function Customer_Type.HasWishListId() : Boolean;
begin
  Result := True;
end;

function Customer_Type.HasLocation() : Boolean;
begin
  Result := True;
end;

{ Price }

function Price.HasAmount() : Boolean;
begin
  Result := True;
end;

function Price.HasCurrencyCode() : Boolean;
begin
  Result := True;
end;

{ CartItems_Type }

constructor CartItems_Type.Create();
begin
  inherited Create();
  FCartItem := CartItems_CartItemArray.Create();
end;

destructor CartItems_Type.Destroy();
begin
  if Assigned(FSubTotal) then
    FreeAndNil(FSubTotal);
  inherited Destroy();
end;

function CartItems_Type.HasSubTotal() : Boolean;
begin
  Result := True;
end;

{ SavedForLaterItems_Type }

constructor SavedForLaterItems_Type.Create();
begin
  inherited Create();
  FSavedForLaterItem := SavedForLaterItems_SavedForLaterItemArray.Create();
end;

destructor SavedForLaterItems_Type.Destroy();
begin
  if Assigned(FSubTotal) then
    FreeAndNil(FSubTotal);
  inherited Destroy();
end;

function SavedForLaterItems_Type.HasSubTotal() : Boolean;
begin
  Result := True;
end;

{ Transaction_Type }

constructor Transaction_Type.Create();
begin
  inherited Create();
  FTransactionItems := Transaction_TransactionItems_Type.Create();
  FShipments := Transaction_Shipments_Type.Create();
end;

destructor Transaction_Type.Destroy();
begin
  if Assigned(FTotals) then
    FreeAndNil(FTotals);
  inherited Destroy();
end;

function Transaction_Type.HasSellerName() : Boolean;
begin
  Result := True;
end;

function Transaction_Type.HasPayingCustomerId() : Boolean;
begin
  Result := True;
end;

function Transaction_Type.HasOrderingCustomerId() : Boolean;
begin
  Result := True;
end;

function Transaction_Type.HasTotals() : Boolean;
begin
  Result := True;
end;

function Transaction_Type.HasTransactionItems() : Boolean;
begin
  Result := True;
end;

function Transaction_Type.HasShipments() : Boolean;
begin
  Result := True;
end;

{ Seller_Type }

constructor Seller_Type.Create();
begin
  inherited Create();
  FSellerFeedbackSummary := Seller_SellerFeedbackSummary_Type.Create();
  FSellerFeedback := SellerFeedback_Type.Create();
end;

destructor Seller_Type.Destroy();
begin
  if Assigned(FLocation) then
    FreeAndNil(FLocation);
  inherited Destroy();
end;

function Seller_Type.HasSellerName() : Boolean;
begin
  Result := True;
end;

function Seller_Type.HasSellerLegalName() : Boolean;
begin
  Result := True;
end;

function Seller_Type.HasNickname() : Boolean;
begin
  Result := True;
end;

function Seller_Type.HasGlancePage() : Boolean;
begin
  Result := True;
end;

function Seller_Type.HasAbout() : Boolean;
begin
  Result := True;
end;

function Seller_Type.HasMoreAbout() : Boolean;
begin
  Result := True;
end;

function Seller_Type.HasLocation() : Boolean;
begin
  Result := True;
end;

function Seller_Type.HasAverageFeedbackRating() : Boolean;
begin
  Result := True;
end;

function Seller_Type.HasTotalFeedback() : Boolean;
begin
  Result := True;
end;

function Seller_Type.HasTotalFeedbackPages() : Boolean;
begin
  Result := True;
end;

function Seller_Type.HasSellerFeedbackSummary() : Boolean;
begin
  Result := True;
end;

function Seller_Type.HasSellerFeedback() : Boolean;
begin
  Result := True;
end;

{ SellerListing_Type }

destructor SellerListing_Type.Destroy();
begin
  if Assigned(FPrice) then
    FreeAndNil(FPrice);
  if Assigned(FSeller) then
    FreeAndNil(FSeller);
  inherited Destroy();
end;

function SellerListing_Type.HasExchangeId() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasListingId() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasSKU() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasUPC() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasEAN() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasWillShipExpedited() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasWillShipInternational() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasTitle() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasPrice() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasStartDate() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasEndDate() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasStatus() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasQuantity() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasCondition() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasSubCondition() : Boolean;
begin
  Result := True;
end;

function SellerListing_Type.HasSeller() : Boolean;
begin
  Result := True;
end;

{ Image }

destructor Image.Destroy();
begin
  if Assigned(FHeight) then
    FreeAndNil(FHeight);
  if Assigned(FWidth) then
    FreeAndNil(FWidth);
  inherited Destroy();
end;

function Image.HasIsVerified() : Boolean;
begin
  Result := True;
end;

{ ListItem_Type }

destructor ListItem_Type.Destroy();
begin
  if Assigned(F_Item) then
    FreeAndNil(F_Item);
  inherited Destroy();
end;

function ListItem_Type.HasListItemId() : Boolean;
begin
  Result := True;
end;

function ListItem_Type.HasDateAdded() : Boolean;
begin
  Result := True;
end;

function ListItem_Type.HasComment() : Boolean;
begin
  Result := True;
end;

function ListItem_Type.HasQuantityDesired() : Boolean;
begin
  Result := True;
end;

function ListItem_Type.HasQuantityReceived() : Boolean;
begin
  Result := True;
end;

function ListItem_Type.Has_Item() : Boolean;
begin
  Result := True;
end;

{ Customer_Location_Type }

function Customer_Location_Type.HasUserDefinedLocation() : Boolean;
begin
  Result := True;
end;

function Customer_Location_Type.HasCity() : Boolean;
begin
  Result := True;
end;

function Customer_Location_Type.HasState() : Boolean;
begin
  Result := True;
end;

function Customer_Location_Type.HasCountry() : Boolean;
begin
  Result := True;
end;

{ CustomerReviews_Type }

constructor CustomerReviews_Type.Create();
begin
  inherited Create();
  FReview := CustomerReviews_ReviewArray.Create();
end;

destructor CustomerReviews_Type.Destroy();
begin
  inherited Destroy();
end;

function CustomerReviews_Type.HasAverageRating() : Boolean;
begin
  Result := True;
end;

function CustomerReviews_Type.HasTotalReviews() : Boolean;
begin
  Result := True;
end;

function CustomerReviews_Type.HasTotalReviewPages() : Boolean;
begin
  Result := True;
end;

{ SearchResultsMap_SearchIndex_Type }

constructor SearchResultsMap_SearchIndex_Type.Create();
begin
  inherited Create();
  FASIN := SearchResultsMap_SearchIndex_Type_ASINArray.Create();
end;

destructor SearchResultsMap_SearchIndex_Type.Destroy();
begin
  if Assigned(FCorrectedQuery) then
    FreeAndNil(FCorrectedQuery);
  inherited Destroy();
end;

function SearchResultsMap_SearchIndex_Type.HasResults() : Boolean;
begin
  Result := True;
end;

function SearchResultsMap_SearchIndex_Type.HasPages() : Boolean;
begin
  Result := True;
end;

function SearchResultsMap_SearchIndex_Type.HasCorrectedQuery() : Boolean;
begin
  Result := True;
end;

{ ImageSet_Type }

destructor ImageSet_Type.Destroy();
begin
  if Assigned(FSwatchImage) then
    FreeAndNil(FSwatchImage);
  if Assigned(FSmallImage) then
    FreeAndNil(FSmallImage);
  if Assigned(FMediumImage) then
    FreeAndNil(FMediumImage);
  if Assigned(FLargeImage) then
    FreeAndNil(FLargeImage);
  inherited Destroy();
end;

function ImageSet_Type.HasSwatchImage() : Boolean;
begin
  Result := True;
end;

function ImageSet_Type.HasSmallImage() : Boolean;
begin
  Result := True;
end;

function ImageSet_Type.HasMediumImage() : Boolean;
begin
  Result := True;
end;

function ImageSet_Type.HasLargeImage() : Boolean;
begin
  Result := True;
end;

{ Item_ImageSets_Type }

constructor Item_ImageSets_Type.Create();
begin
  inherited Create();
  FImageSet := Item_ImageSets_Type_ImageSetArray.Create();
end;

destructor Item_ImageSets_Type.Destroy();
begin
  inherited Destroy();
end;

function Item_ImageSets_Type.HasMerchantId() : Boolean;
begin
  Result := True;
end;

{ ItemAttributes_Type }

constructor ItemAttributes_Type.Create();
begin
  inherited Create();
  FActor := ItemAttributes_ActorArray.Create();
  FArtist := ItemAttributes_ArtistArray.Create();
  FAudioFormat := ItemAttributes_AudioFormatArray.Create();
  FAuthor := ItemAttributes_AuthorArray.Create();
  FCameraManualFeatures := ItemAttributes_CameraManualFeaturesArray.Create();
  FCompatibleDevices := ItemAttributes_CompatibleDevicesArray.Create();
  FCreator := ItemAttributes_CreatorArray.Create();
  FDataLinkProtocol := ItemAttributes_DataLinkProtocolArray.Create();
  FDirector := ItemAttributes_DirectorArray.Create();
  FFeature := ItemAttributes_FeatureArray.Create();
  FFormat := ItemAttributes_FormatArray.Create();
  FFormFactor := ItemAttributes_FormFactorArray.Create();
  FLanguages := ItemAttributes_Languages_Type.Create();
  FPhotoFlashType := ItemAttributes_PhotoFlashTypeArray.Create();
  FPictureFormat := ItemAttributes_PictureFormatArray.Create();
  FPlatform := ItemAttributes_PlatformArray.Create();
  FReturnMethod := ItemAttributes_ReturnMethodArray.Create();
  FSpecialFeatures := ItemAttributes_SpecialFeaturesArray.Create();
  FSupportedImageType := ItemAttributes_SupportedImageTypeArray.Create();
end;

destructor ItemAttributes_Type.Destroy();
begin
  if Assigned(FAddress) then
    FreeAndNil(FAddress);
  if Assigned(FAmazonMaximumAge) then
    FreeAndNil(FAmazonMaximumAge);
  if Assigned(FAmazonMinimumAge) then
    FreeAndNil(FAmazonMinimumAge);
  if Assigned(FBatteries) then
    FreeAndNil(FBatteries);
  if Assigned(FCaseDiameter) then
    FreeAndNil(FCaseDiameter);
  if Assigned(FCaseThickness) then
    FreeAndNil(FCaseThickness);
  if Assigned(FContinuousShootingSpeed) then
    FreeAndNil(FContinuousShootingSpeed);
  if Assigned(FCPUSpeed) then
    FreeAndNil(FCPUSpeed);
  if Assigned(FDelayBetweenShots) then
    FreeAndNil(FDelayBetweenShots);
  if Assigned(FDigitalZoom) then
    FreeAndNil(FDigitalZoom);
  if Assigned(FDisplaySize) then
    FreeAndNil(FDisplaySize);
  if Assigned(FFirstIssueLeadTime) then
    FreeAndNil(FFirstIssueLeadTime);
  if Assigned(FGraphicsMemorySize) then
    FreeAndNil(FGraphicsMemorySize);
  if Assigned(FHardDiskSize) then
    FreeAndNil(FHardDiskSize);
  if Assigned(FISOEquivalent) then
    FreeAndNil(FISOEquivalent);
  if Assigned(FItemDimensions) then
    FreeAndNil(FItemDimensions);
  if Assigned(FListPrice) then
    FreeAndNil(FListPrice);
  if Assigned(FManufacturerMaximumAge) then
    FreeAndNil(FManufacturerMaximumAge);
  if Assigned(FManufacturerMinimumAge) then
    FreeAndNil(FManufacturerMinimumAge);
  if Assigned(FMaximumAperture) then
    FreeAndNil(FMaximumAperture);
  if Assigned(FMaximumFocalLength) then
    FreeAndNil(FMaximumFocalLength);
  if Assigned(FMaximumHighResolutionImages) then
    FreeAndNil(FMaximumHighResolutionImages);
  if Assigned(FMaximumHorizontalResolution) then
    FreeAndNil(FMaximumHorizontalResolution);
  if Assigned(FMaximumResolution) then
    FreeAndNil(FMaximumResolution);
  if Assigned(FMaximumShutterSpeed) then
    FreeAndNil(FMaximumShutterSpeed);
  if Assigned(FMaximumVerticalResolution) then
    FreeAndNil(FMaximumVerticalResolution);
  if Assigned(FMaximumWeightRecommendation) then
    FreeAndNil(FMaximumWeightRecommendation);
  if Assigned(FMinimumFocalLength) then
    FreeAndNil(FMinimumFocalLength);
  if Assigned(FMinimumShutterSpeed) then
    FreeAndNil(FMinimumShutterSpeed);
  if Assigned(FMonitorSize) then
    FreeAndNil(FMonitorSize);
  if Assigned(FMonitorViewableDiagonalSize) then
    FreeAndNil(FMonitorViewableDiagonalSize);
  if Assigned(FOpticalSensorResolution) then
    FreeAndNil(FOpticalSensorResolution);
  if Assigned(FOpticalZoom) then
    FreeAndNil(FOpticalZoom);
  if Assigned(FPackageDimensions) then
    FreeAndNil(FPackageDimensions);
  if Assigned(FRunningTime) then
    FreeAndNil(FRunningTime);
  if Assigned(FSecondaryCacheSize) then
    FreeAndNil(FSecondaryCacheSize);
  if Assigned(FStoneWeight) then
    FreeAndNil(FStoneWeight);
  if Assigned(FSubscriptionLength) then
    FreeAndNil(FSubscriptionLength);
  if Assigned(FSystemBusSpeed) then
    FreeAndNil(FSystemBusSpeed);
  if Assigned(FSystemMemorySizeMax) then
    FreeAndNil(FSystemMemorySizeMax);
  if Assigned(FSystemMemorySize) then
    FreeAndNil(FSystemMemorySize);
  if Assigned(FTotalDiamondWeight) then
    FreeAndNil(FTotalDiamondWeight);
  if Assigned(FTotalGemWeight) then
    FreeAndNil(FTotalGemWeight);
  if Assigned(FTotalMetalWeight) then
    FreeAndNil(FTotalMetalWeight);
  if Assigned(FWaterResistanceDepth) then
    FreeAndNil(FWaterResistanceDepth);
  if Assigned(FWEEETaxValue) then
    FreeAndNil(FWEEETaxValue);
  inherited Destroy();
end;

function ItemAttributes_Type.HasAddress() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasAmazonMaximumAge() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasAmazonMinimumAge() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasAnalogVideoFormat() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasApertureModes() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasAspectRatio() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasAssemblyInstructions() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasAssemblyRequired() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasAudienceRating() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasBackFinding() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasBandMaterialType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasBatteriesIncluded() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasBatteriesRequired() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasBatteries() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasBatteryDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasBatteryType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasBezelMaterialType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasBinding() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasBrand() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCalendarType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCaseDiameter() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCaseMaterialType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCaseThickness() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCaseType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCatalogNumber() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCDRWDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasChainType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCEROAgeRating() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasClaspType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasClothingSize() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasClubType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasColor() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCompatibility() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasComputerHardwareType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasComputerPlatform() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasConnectivity() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasContinuousShootingSpeed() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCountry() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCPUManufacturer() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCPUSpeed() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCPUType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasCuisine() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDeliveryOption() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDelayBetweenShots() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDepartment() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDeweyDecimalNumber() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDialColor() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDialWindowMaterialType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDigitalZoom() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDisplayColorSupport() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDisplaySize() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDrumSetPieceQuantity() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDVDLayers() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDVDRWDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDVDSides() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasDPCI() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasEAN() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasEdition() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasESRBAgeRating() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasExternalDisplaySupportDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasFabricType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasFaxNumber() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasFilmColorType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasFirstIssueLeadTime() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasFloppyDiskDriveDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasGemType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasGenre() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasGraphicsCardInterface() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasGraphicsDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasGraphicsMemorySize() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasGuitarAttribute() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasGuitarBridgeSystem() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasGuitarPickThickness() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasGuitarPickupConfiguration() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHandOrientation() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHardDiskCount() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHardDiskSize() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHardDiskInterface() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHardwarePlatform() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHasAutoFocus() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHasBurstMode() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHasInCameraEditing() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHasRedEyeReduction() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHasSelfTimer() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHasTripodMount() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHasVideoOut() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHasViewfinder() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHazardousMaterialType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasHoursOfOperation() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasIncludedSoftware() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasIncludesMp3Player() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasIngredients() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasInstrumentKey() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasIsAdultProduct() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasIsAutographed() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasISBN() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasIsFragile() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasIsLabCreated() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasIsMemorabilia() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasISOEquivalent() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasIsPreannounce() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasIssuesPerYear() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasItemDimensions() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasKeyboardDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.Has_Label() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasLanguages() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasLegalDisclaimer() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasLensType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasLineVoltage() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasListPrice() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMacroFocusRange() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMagazineType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMalletHardness() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasManufacturer() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasManufacturerLaborWarrantyDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasManufacturerMaximumAge() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasManufacturerMinimumAge() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasManufacturerPartsWarrantyDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMaterialType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMaximumAperture() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMaximumColorDepth() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMaximumFocalLength() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMaximumHighResolutionImages() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMaximumHorizontalResolution() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMaximumLowResolutionImages() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMaximumResolution() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMaximumShutterSpeed() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMaximumVerticalResolution() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMaximumWeightRecommendation() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMediaType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMemorySlotsAvailable() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMetalStamp() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMetalType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMiniMovieDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMinimumFocalLength() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMinimumShutterSpeed() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasModel() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasModelYear() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasModemDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMonitorSize() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMonitorViewableDiagonalSize() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMouseDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMPN() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasMusicalStyle() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNativeResolution() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNeighborhood() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNetworkInterfaceDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNotebookDisplayTechnology() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNotebookPointingDeviceDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNumberOfDiscs() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNumberOfIssues() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNumberOfItems() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNumberOfKeys() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNumberOfPages() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNumberOfPearls() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNumberOfRapidFireShots() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNumberOfStones() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNumberOfStrings() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasNumberOfTracks() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasOperatingSystem() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasOpticalSensorResolution() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasOpticalZoom() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasOriginalReleaseDate() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasOutputWattage() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPackageDimensions() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPackageQuantity() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPearlLustre() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPearlMinimumColor() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPearlShape() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPearlStringingMethod() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPearlSurfaceBlemishes() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPearlType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPearlUniformity() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPhoneNumber() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPriceRating() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasProcessorCount() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasProductGroup() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasProductSiteLaunchDate() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasProductTypeName() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasProductTypeSubcategory() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPromotionalTag() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPublicationDate() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPublisher() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasPOBoxShippingExcluded() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasReadingLevel() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasRecorderTrackCount() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasRegionCode() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasRegionOfOrigin() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasReturnPolicy() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasReleaseDate() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasRemovableMemory() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasRemovableStorage() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasRequiredVoltageRange() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasResolutionModes() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasRingSize() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasRunningTime() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasScentName() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSecondaryCacheSize() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSettingType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasShaftMaterialType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSize() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSizePerPearl() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSkillLevel() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSKU() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSoldInStores() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSoundCardDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSpeakerCount() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSpeakerDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasStoneClarity() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasStoneColor() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasStoneCut() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasStoneShape() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasStoneWeight() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasStudio() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasStyle() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSubscriptionLength() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSupportedMediaSize() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSystemBusSpeed() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSystemMemorySizeMax() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSystemMemorySize() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasSystemMemoryType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTellingPageIndicator() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTheatricalReleaseDate() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTitle() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalDiamondWeight() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalExternalBaysFree() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalFirewirePorts() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalGemWeight() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalInternalBaysFree() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalMetalWeight() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalNTSCPALPorts() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalParallelPorts() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalPCCardSlots() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalPCISlotsFree() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalSerialPorts() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalSVideoOutPorts() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalUSB2Ports() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalUSBPorts() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasTotalVGAOutPorts() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasUPC() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasVariationDenomination() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasVariationDescription() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasWarranty() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasWatchMovementType() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasWaterResistanceDepth() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasWEEETaxValue() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Type.HasWirelessMicrophoneFrequency() : Boolean;
begin
  Result := True;
end;

{ MerchantItemAttributes_Type }

constructor MerchantItemAttributes_Type.Create();
begin
  inherited Create();
  FActor := MerchantItemAttributes_ActorArray.Create();
  FArtist := MerchantItemAttributes_ArtistArray.Create();
  FAudioFormat := MerchantItemAttributes_AudioFormatArray.Create();
  FAuthor := MerchantItemAttributes_AuthorArray.Create();
  FCameraManualFeatures := MerchantItemAttributes_CameraManualFeaturesArray.Create();
  FCreator := MerchantItemAttributes_CreatorArray.Create();
  FDirector := MerchantItemAttributes_DirectorArray.Create();
  FFeature := MerchantItemAttributes_FeatureArray.Create();
  FFormat := MerchantItemAttributes_FormatArray.Create();
  FLanguages := MerchantItemAttributes_Languages_Type.Create();
  FPhotoFlashType := MerchantItemAttributes_PhotoFlashTypeArray.Create();
  FPictureFormat := MerchantItemAttributes_PictureFormatArray.Create();
  FPlatform := MerchantItemAttributes_PlatformArray.Create();
  FPurchasingChannel := MerchantItemAttributes_PurchasingChannelArray.Create();
  FReturnMethod := MerchantItemAttributes_ReturnMethodArray.Create();
  FSpecialFeatures := MerchantItemAttributes_SpecialFeaturesArray.Create();
  FSupportedImageType := MerchantItemAttributes_SupportedImageTypeArray.Create();
end;

destructor MerchantItemAttributes_Type.Destroy();
begin
  if Assigned(FAddress) then
    FreeAndNil(FAddress);
  if Assigned(FAmazonMaximumAge) then
    FreeAndNil(FAmazonMaximumAge);
  if Assigned(FAmazonMinimumAge) then
    FreeAndNil(FAmazonMinimumAge);
  if Assigned(FBatteries) then
    FreeAndNil(FBatteries);
  if Assigned(FCaseDiameter) then
    FreeAndNil(FCaseDiameter);
  if Assigned(FCaseThickness) then
    FreeAndNil(FCaseThickness);
  if Assigned(FContinuousShootingSpeed) then
    FreeAndNil(FContinuousShootingSpeed);
  if Assigned(FCPUSpeed) then
    FreeAndNil(FCPUSpeed);
  if Assigned(FDelayBetweenShots) then
    FreeAndNil(FDelayBetweenShots);
  if Assigned(FDigitalZoom) then
    FreeAndNil(FDigitalZoom);
  if Assigned(FDisplaySize) then
    FreeAndNil(FDisplaySize);
  if Assigned(FFirstIssueLeadTime) then
    FreeAndNil(FFirstIssueLeadTime);
  if Assigned(FFixedShippingCharge) then
    FreeAndNil(FFixedShippingCharge);
  if Assigned(FGraphicsMemorySize) then
    FreeAndNil(FGraphicsMemorySize);
  if Assigned(FHardDiskSize) then
    FreeAndNil(FHardDiskSize);
  if Assigned(FISOEquivalent) then
    FreeAndNil(FISOEquivalent);
  if Assigned(FItemDimensions) then
    FreeAndNil(FItemDimensions);
  if Assigned(FListPrice) then
    FreeAndNil(FListPrice);
  if Assigned(FManufacturerMaximumAge) then
    FreeAndNil(FManufacturerMaximumAge);
  if Assigned(FManufacturerMinimumAge) then
    FreeAndNil(FManufacturerMinimumAge);
  if Assigned(FMaximumAperture) then
    FreeAndNil(FMaximumAperture);
  if Assigned(FMaximumFocalLength) then
    FreeAndNil(FMaximumFocalLength);
  if Assigned(FMaximumHighResolutionImages) then
    FreeAndNil(FMaximumHighResolutionImages);
  if Assigned(FMaximumHorizontalResolution) then
    FreeAndNil(FMaximumHorizontalResolution);
  if Assigned(FMaximumResolution) then
    FreeAndNil(FMaximumResolution);
  if Assigned(FMaximumShutterSpeed) then
    FreeAndNil(FMaximumShutterSpeed);
  if Assigned(FMaximumVerticalResolution) then
    FreeAndNil(FMaximumVerticalResolution);
  if Assigned(FMaximumWeightRecommendation) then
    FreeAndNil(FMaximumWeightRecommendation);
  if Assigned(FMinimumFocalLength) then
    FreeAndNil(FMinimumFocalLength);
  if Assigned(FMinimumShutterSpeed) then
    FreeAndNil(FMinimumShutterSpeed);
  if Assigned(FMonitorSize) then
    FreeAndNil(FMonitorSize);
  if Assigned(FMonitorViewableDiagonalSize) then
    FreeAndNil(FMonitorViewableDiagonalSize);
  if Assigned(FOpticalZoom) then
    FreeAndNil(FOpticalZoom);
  if Assigned(FPackageDimensions) then
    FreeAndNil(FPackageDimensions);
  if Assigned(FSecondaryCacheSize) then
    FreeAndNil(FSecondaryCacheSize);
  if Assigned(FStoneWeight) then
    FreeAndNil(FStoneWeight);
  if Assigned(FSubscriptionLength) then
    FreeAndNil(FSubscriptionLength);
  if Assigned(FSystemBusSpeed) then
    FreeAndNil(FSystemBusSpeed);
  if Assigned(FSystemMemorySizeMax) then
    FreeAndNil(FSystemMemorySizeMax);
  if Assigned(FSystemMemorySize) then
    FreeAndNil(FSystemMemorySize);
  if Assigned(FTotalDiamondWeight) then
    FreeAndNil(FTotalDiamondWeight);
  if Assigned(FTotalGemWeight) then
    FreeAndNil(FTotalGemWeight);
  if Assigned(FTotalMetalWeight) then
    FreeAndNil(FTotalMetalWeight);
  if Assigned(FVendorRebate) then
    FreeAndNil(FVendorRebate);
  if Assigned(FWaterResistanceDepth) then
    FreeAndNil(FWaterResistanceDepth);
  inherited Destroy();
end;

function MerchantItemAttributes_Type.HasAddress() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasAmazonMaximumAge() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasAmazonMinimumAge() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasApertureModes() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasAspectRatio() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasAssemblyInstructions() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasAssemblyRequired() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasAudienceRating() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasBackFinding() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasBandMaterialType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasBatteriesIncluded() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasBatteriesRequired() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasBatteries() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasBatteryDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasBatteryType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasBezelMaterialType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasBinding() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasBrand() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCalendarType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCaseDiameter() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCaseMaterialType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCaseThickness() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCaseType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCatalogNumber() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCDRWDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasChainType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasClaspType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasClothingSize() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasColor() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCompatibility() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasComputerHardwareType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasComputerPlatform() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasConnectivity() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasContinuousShootingSpeed() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCountry() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCountryOfOrigin() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCPUManufacturer() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCPUSpeed() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCPUType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCuisine() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasCustomizable() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDelayBetweenShots() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDeliveryOption() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDepartment() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDeweyDecimalNumber() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDialColor() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDialWindowMaterialType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDigitalZoom() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDisplaySize() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDrumSetPieceQuantity() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDVDLayers() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDVDRWDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDVDSides() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasDPCI() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasEAN() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasEdition() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasESRBAgeRating() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasExternalDisplaySupportDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasFabricType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasFaxNumber() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasFirstIssueLeadTime() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasFloppyDiskDriveDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasFixedShippingCharge() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasGemType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasGraphicsCardInterface() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasGraphicsDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasGraphicsMemorySize() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasGuitarAttribute() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasGuitarBridgeSystem() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasGuitarPickThickness() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasGuitarPickupConfiguration() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasHardDiskCount() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasHardDiskSize() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasHasAutoFocus() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasHasBurstMode() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasHasInCameraEditing() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasHasRedEyeReduction() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasHasSelfTimer() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasHasTripodMount() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasHasVideoOut() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasHasViewfinder() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasHazardousMaterialType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasHoursOfOperation() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasIncludedSoftware() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasIncludesMp3Player() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasIndications() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasIngredients() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasInstrumentKey() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasIsAutographed() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasISBN() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasIsFragile() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasIsLabCreated() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasIsMemorabilia() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasISOEquivalent() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasIssuesPerYear() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasItemDimensions() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasKeyboardDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.Has_Label() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasLanguages() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasLegalDisclaimer() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasLineVoltage() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasListPrice() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMacroFocusRange() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMagazineType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMalletHardness() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasManufacturer() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasManufacturerLaborWarrantyDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasManufacturerMaximumAge() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasManufacturerMinimumAge() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasManufacturerPartsWarrantyDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMaterialType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMaximumAperture() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMaximumColorDepth() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMaximumFocalLength() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMaximumHighResolutionImages() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMaximumHorizontalResolution() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMaximumLowResolutionImages() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMaximumResolution() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMaximumShutterSpeed() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMaximumVerticalResolution() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMaximumWeightRecommendation() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMemorySlotsAvailable() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMetalStamp() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMetalType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMiniMovieDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMinimumFocalLength() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMinimumShutterSpeed() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasModel() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasModelYear() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasModemDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMonitorSize() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMonitorViewableDiagonalSize() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMouseDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMPN() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasMusicalStyle() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNativeResolution() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNeighborhood() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNetworkInterfaceDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNotebookDisplayTechnology() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNotebookPointingDeviceDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNumberOfDiscs() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNumberOfIssues() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNumberOfItems() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNumberOfKeys() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNumberOfPages() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNumberOfPearls() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNumberOfRapidFireShots() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNumberOfStones() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNumberOfStrings() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasNumberOfTracks() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasOpticalZoom() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasOriginalReleaseDate() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasOutputWattage() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPackageDimensions() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPearlLustre() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPearlMinimumColor() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPearlShape() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPearlStringingMethod() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPearlSurfaceBlemishes() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPearlType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPearlUniformity() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPhoneNumber() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPriceRating() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasProcessorCount() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasProductGroup() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPromotionalTag() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPOBoxShippingExcluded() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPublicationDate() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasPublisher() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasReadingLevel() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasRecorderTrackCount() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasRegionCode() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasRegionOfOrigin() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasReleaseDate() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasRemovableMemory() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasResolutionModes() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasReturnPolicy() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasRingSize() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSafetyWarning() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSalesRestriction() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSecondaryCacheSize() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSettingType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSize() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSKU() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSoldInStores() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSizePerPearl() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSkillLevel() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSoundCardDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSpeakerCount() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSpeakerDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasStoneClarity() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasStoneColor() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasStoneCut() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasStoneShape() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasStoneWeight() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasStudio() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSubscriptionLength() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSystemBusSpeed() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSystemMemorySizeMax() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSystemMemorySize() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasSystemMemoryType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTellingPageIndicator() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTheatricalReleaseDate() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTitle() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalDiamondWeight() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalExternalBaysFree() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalFirewirePorts() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalGemWeight() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalInternalBaysFree() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalMetalWeight() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalNTSCPALPorts() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalParallelPorts() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalPCCardSlots() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalPCISlotsFree() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalSerialPorts() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalSVideoOutPorts() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalUSB2Ports() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalUSBPorts() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasTotalVGAOutPorts() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasUPC() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasVariationDenomination() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasVariationDescription() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasVendorRebate() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasWarranty() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasWatchMovementType() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasWebsiteBuyability() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasWaterResistanceDepth() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_Type.HasWirelessMicrophoneFrequency() : Boolean;
begin
  Result := True;
end;

{ OfferSummary_Type }

destructor OfferSummary_Type.Destroy();
begin
  if Assigned(FLowestNewPrice) then
    FreeAndNil(FLowestNewPrice);
  if Assigned(FLowestUsedPrice) then
    FreeAndNil(FLowestUsedPrice);
  if Assigned(FLowestCollectiblePrice) then
    FreeAndNil(FLowestCollectiblePrice);
  if Assigned(FLowestRefurbishedPrice) then
    FreeAndNil(FLowestRefurbishedPrice);
  inherited Destroy();
end;

function OfferSummary_Type.HasLowestNewPrice() : Boolean;
begin
  Result := True;
end;

function OfferSummary_Type.HasLowestUsedPrice() : Boolean;
begin
  Result := True;
end;

function OfferSummary_Type.HasLowestCollectiblePrice() : Boolean;
begin
  Result := True;
end;

function OfferSummary_Type.HasLowestRefurbishedPrice() : Boolean;
begin
  Result := True;
end;

function OfferSummary_Type.HasTotalNew() : Boolean;
begin
  Result := True;
end;

function OfferSummary_Type.HasTotalUsed() : Boolean;
begin
  Result := True;
end;

function OfferSummary_Type.HasTotalCollectible() : Boolean;
begin
  Result := True;
end;

function OfferSummary_Type.HasTotalRefurbished() : Boolean;
begin
  Result := True;
end;

{ Offers_Type }

constructor Offers_Type.Create();
begin
  inherited Create();
  FOffer := Offers_OfferArray.Create();
end;

destructor Offers_Type.Destroy();
begin
  inherited Destroy();
end;

function Offers_Type.HasTotalOffers() : Boolean;
begin
  Result := True;
end;

function Offers_Type.HasTotalOfferPages() : Boolean;
begin
  Result := True;
end;

{ VariationSummary_Type }

destructor VariationSummary_Type.Destroy();
begin
  if Assigned(FLowestPrice) then
    FreeAndNil(FLowestPrice);
  if Assigned(FHighestPrice) then
    FreeAndNil(FHighestPrice);
  if Assigned(FLowestSalePrice) then
    FreeAndNil(FLowestSalePrice);
  if Assigned(FHighestSalePrice) then
    FreeAndNil(FHighestSalePrice);
  inherited Destroy();
end;

function VariationSummary_Type.HasLowestPrice() : Boolean;
begin
  Result := True;
end;

function VariationSummary_Type.HasHighestPrice() : Boolean;
begin
  Result := True;
end;

function VariationSummary_Type.HasLowestSalePrice() : Boolean;
begin
  Result := True;
end;

function VariationSummary_Type.HasHighestSalePrice() : Boolean;
begin
  Result := True;
end;

function VariationSummary_Type.HasSingleMerchantId() : Boolean;
begin
  Result := True;
end;

{ Variations_Type }

constructor Variations_Type.Create();
begin
  inherited Create();
  FVariationDimensions := VariationDimensions_Type.Create();
  F_Item := Variations__ItemArray.Create();
end;

destructor Variations_Type.Destroy();
begin
  inherited Destroy();
end;

function Variations_Type.HasTotalVariations() : Boolean;
begin
  Result := True;
end;

function Variations_Type.HasTotalVariationPages() : Boolean;
begin
  Result := True;
end;

function Variations_Type.HasVariationDimensions() : Boolean;
begin
  Result := True;
end;

{ SearchInside_Type }

destructor SearchInside_Type.Destroy();
begin
  if Assigned(FExcerpt) then
    FreeAndNil(FExcerpt);
  inherited Destroy();
end;

function SearchInside_Type.HasTotalExcerpts() : Boolean;
begin
  Result := True;
end;

function SearchInside_Type.HasExcerpt() : Boolean;
begin
  Result := True;
end;

{ Item_AlternateVersions_Type_AlternateVersion_Type }

function Item_AlternateVersions_Type_AlternateVersion_Type.HasTitle() : Boolean;
begin
  Result := True;
end;

function Item_AlternateVersions_Type_AlternateVersion_Type.HasBinding() : Boolean;
begin
  Result := True;
end;

{ Offer_Type }

constructor Offer_Type.Create();
begin
  inherited Create();
  FOfferListing := Offer_OfferListingArray.Create();
  FPromotions := Promotions_Type.Create();
end;

destructor Offer_Type.Destroy();
begin
  if Assigned(FMerchant) then
    FreeAndNil(FMerchant);
  if Assigned(FSeller) then
    FreeAndNil(FSeller);
  if Assigned(FOfferAttributes) then
    FreeAndNil(FOfferAttributes);
  if Assigned(FLoyaltyPoints) then
    FreeAndNil(FLoyaltyPoints);
  inherited Destroy();
end;

function Offer_Type.HasMerchant() : Boolean;
begin
  Result := True;
end;

function Offer_Type.HasSeller() : Boolean;
begin
  Result := True;
end;

function Offer_Type.HasOfferAttributes() : Boolean;
begin
  Result := True;
end;

function Offer_Type.HasLoyaltyPoints() : Boolean;
begin
  Result := True;
end;

function Offer_Type.HasPromotions() : Boolean;
begin
  Result := True;
end;

{ Merchant_Type }

function Merchant_Type.HasName() : Boolean;
begin
  Result := True;
end;

function Merchant_Type.HasGlancePage() : Boolean;
begin
  Result := True;
end;

function Merchant_Type.HasAverageFeedbackRating() : Boolean;
begin
  Result := True;
end;

function Merchant_Type.HasTotalFeedback() : Boolean;
begin
  Result := True;
end;

function Merchant_Type.HasTotalFeedbackPages() : Boolean;
begin
  Result := True;
end;

{ OfferAttributes_Type }

function OfferAttributes_Type.HasCondition() : Boolean;
begin
  Result := True;
end;

function OfferAttributes_Type.HasSubCondition() : Boolean;
begin
  Result := True;
end;

function OfferAttributes_Type.HasConditionNote() : Boolean;
begin
  Result := True;
end;

function OfferAttributes_Type.HasWillShipExpedited() : Boolean;
begin
  Result := True;
end;

function OfferAttributes_Type.HasWillShipInternational() : Boolean;
begin
  Result := True;
end;

{ OfferListing_Type }

constructor OfferListing_Type.Create();
begin
  inherited Create();
  FShippingCharge := OfferListing_ShippingChargeArray.Create();
end;

destructor OfferListing_Type.Destroy();
begin
  if Assigned(FPrice) then
    FreeAndNil(FPrice);
  if Assigned(FSalePrice) then
    FreeAndNil(FSalePrice);
  if Assigned(FAmountSaved) then
    FreeAndNil(FAmountSaved);
  if Assigned(FAvailabilityAttributes) then
    FreeAndNil(FAvailabilityAttributes);
  if Assigned(FISPUStoreAddress) then
    FreeAndNil(FISPUStoreAddress);
  inherited Destroy();
end;

function OfferListing_Type.HasOfferListingId() : Boolean;
begin
  Result := True;
end;

function OfferListing_Type.HasExchangeId() : Boolean;
begin
  Result := True;
end;

function OfferListing_Type.HasPrice() : Boolean;
begin
  Result := True;
end;

function OfferListing_Type.HasSalePrice() : Boolean;
begin
  Result := True;
end;

function OfferListing_Type.HasAmountSaved() : Boolean;
begin
  Result := True;
end;

function OfferListing_Type.HasPercentageSaved() : Boolean;
begin
  Result := True;
end;

function OfferListing_Type.HasAvailability() : Boolean;
begin
  Result := True;
end;

function OfferListing_Type.HasAvailabilityAttributes() : Boolean;
begin
  Result := True;
end;

function OfferListing_Type.HasQuantity() : Boolean;
begin
  Result := True;
end;

function OfferListing_Type.HasISPUStoreAddress() : Boolean;
begin
  Result := True;
end;

function OfferListing_Type.HasISPUStoreHours() : Boolean;
begin
  Result := True;
end;

function OfferListing_Type.HasIsEligibleForSuperSaverShipping() : Boolean;
begin
  Result := True;
end;

function OfferListing_Type.HasSalesRestriction() : Boolean;
begin
  Result := True;
end;

{ LoyaltyPoints_Type }

destructor LoyaltyPoints_Type.Destroy();
begin
  if Assigned(FTypicalRedemptionValue) then
    FreeAndNil(FTypicalRedemptionValue);
  inherited Destroy();
end;

function LoyaltyPoints_Type.HasPoints() : Boolean;
begin
  Result := True;
end;

function LoyaltyPoints_Type.HasTypicalRedemptionValue() : Boolean;
begin
  Result := True;
end;

{ OfferListing_AvailabilityAttributes_Type }

function OfferListing_AvailabilityAttributes_Type.HasAvailabilityType() : Boolean;
begin
  Result := True;
end;

function OfferListing_AvailabilityAttributes_Type.HasIsPreorder() : Boolean;
begin
  Result := True;
end;

function OfferListing_AvailabilityAttributes_Type.HasMinimumHours() : Boolean;
begin
  Result := True;
end;

function OfferListing_AvailabilityAttributes_Type.HasMaximumHours() : Boolean;
begin
  Result := True;
end;

{ Address }

function Address.HasName() : Boolean;
begin
  Result := True;
end;

function Address.HasAddress1() : Boolean;
begin
  Result := True;
end;

function Address.HasAddress2() : Boolean;
begin
  Result := True;
end;

function Address.HasAddress3() : Boolean;
begin
  Result := True;
end;

function Address.HasCity() : Boolean;
begin
  Result := True;
end;

function Address.HasState() : Boolean;
begin
  Result := True;
end;

function Address.HasPostalCode() : Boolean;
begin
  Result := True;
end;

function Address.HasCountry() : Boolean;
begin
  Result := True;
end;

{ OfferListing_ShippingCharge_Type }

destructor OfferListing_ShippingCharge_Type.Destroy();
begin
  if Assigned(FShippingPrice) then
    FreeAndNil(FShippingPrice);
  inherited Destroy();
end;

{ EditorialReview_Type }

function EditorialReview_Type.HasSource() : Boolean;
begin
  Result := True;
end;

function EditorialReview_Type.HasContent() : Boolean;
begin
  Result := True;
end;

{ Collections_Collection_Type_CollectionSummary_Type }

destructor Collections_Collection_Type_CollectionSummary_Type.Destroy();
begin
  if Assigned(FLowestListPrice) then
    FreeAndNil(FLowestListPrice);
  if Assigned(FHighestListPrice) then
    FreeAndNil(FHighestListPrice);
  if Assigned(FLowestSalePrice) then
    FreeAndNil(FLowestSalePrice);
  if Assigned(FHighestSalePrice) then
    FreeAndNil(FHighestSalePrice);
  inherited Destroy();
end;

function Collections_Collection_Type_CollectionSummary_Type.HasLowestListPrice() : Boolean;
begin
  Result := True;
end;

function Collections_Collection_Type_CollectionSummary_Type.HasHighestListPrice() : Boolean;
begin
  Result := True;
end;

function Collections_Collection_Type_CollectionSummary_Type.HasLowestSalePrice() : Boolean;
begin
  Result := True;
end;

function Collections_Collection_Type_CollectionSummary_Type.HasHighestSalePrice() : Boolean;
begin
  Result := True;
end;

{ Collections_Collection_Type_CollectionParent_Type }

function Collections_Collection_Type_CollectionParent_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function Collections_Collection_Type_CollectionParent_Type.HasTitle() : Boolean;
begin
  Result := True;
end;

{ Collections_Collection_Type_CollectionItem_Type }

function Collections_Collection_Type_CollectionItem_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function Collections_Collection_Type_CollectionItem_Type.HasTitle() : Boolean;
begin
  Result := True;
end;

{ Collections_Collection_Type }

constructor Collections_Collection_Type.Create();
begin
  inherited Create();
  FCollectionItem := Collections_Collection_Type_CollectionItemArray.Create();
end;

destructor Collections_Collection_Type.Destroy();
begin
  if Assigned(FCollectionSummary) then
    FreeAndNil(FCollectionSummary);
  if Assigned(FCollectionParent) then
    FreeAndNil(FCollectionParent);
  inherited Destroy();
end;

function Collections_Collection_Type.HasCollectionSummary() : Boolean;
begin
  Result := True;
end;

function Collections_Collection_Type.HasCollectionParent() : Boolean;
begin
  Result := True;
end;

{ Review_Type }

destructor Review_Type.Destroy();
begin
  if Assigned(FReviewer) then
    FreeAndNil(FReviewer);
  inherited Destroy();
end;

function Review_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function Review_Type.HasRating() : Boolean;
begin
  Result := True;
end;

function Review_Type.HasHelpfulVotes() : Boolean;
begin
  Result := True;
end;

function Review_Type.HasCustomerId() : Boolean;
begin
  Result := True;
end;

function Review_Type.HasReviewer() : Boolean;
begin
  Result := True;
end;

function Review_Type.HasTotalVotes() : Boolean;
begin
  Result := True;
end;

function Review_Type.HasDate() : Boolean;
begin
  Result := True;
end;

function Review_Type.HasSummary() : Boolean;
begin
  Result := True;
end;

function Review_Type.HasContent() : Boolean;
begin
  Result := True;
end;

{ Reviewer_Type }

function Reviewer_Type.HasCustomerId() : Boolean;
begin
  Result := True;
end;

function Reviewer_Type.HasName() : Boolean;
begin
  Result := True;
end;

function Reviewer_Type.HasNickname() : Boolean;
begin
  Result := True;
end;

function Reviewer_Type.HasLocation() : Boolean;
begin
  Result := True;
end;

{ Tracks_Disc_Type }

constructor Tracks_Disc_Type.Create();
begin
  inherited Create();
  FTrack := Tracks_Disc_Type_TrackArray.Create();
end;

destructor Tracks_Disc_Type.Destroy();
begin
  inherited Destroy();
end;

{ SimilarProducts_SimilarProduct_Type }

function SimilarProducts_SimilarProduct_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function SimilarProducts_SimilarProduct_Type.HasTitle() : Boolean;
begin
  Result := True;
end;

{ TopSellers_TopSeller_Type }

function TopSellers_TopSeller_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function TopSellers_TopSeller_Type.HasTitle() : Boolean;
begin
  Result := True;
end;

{ NewReleases_NewRelease_Type }

function NewReleases_NewRelease_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function NewReleases_NewRelease_Type.HasTitle() : Boolean;
begin
  Result := True;
end;

{ SimilarViewedProducts_SimilarViewedProduct_Type }

function SimilarViewedProducts_SimilarViewedProduct_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function SimilarViewedProducts_SimilarViewedProduct_Type.HasTitle() : Boolean;
begin
  Result := True;
end;

{ OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type }

function OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type.HasTitle() : Boolean;
begin
  Result := True;
end;

{ Accessories_Accessory_Type }

function Accessories_Accessory_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function Accessories_Accessory_Type.HasTitle() : Boolean;
begin
  Result := True;
end;

{ Promotion_Type }

destructor Promotion_Type.Destroy();
begin
  if Assigned(FSummary) then
    FreeAndNil(FSummary);
  if Assigned(FDetails) then
    FreeAndNil(FDetails);
  inherited Destroy();
end;

function Promotion_Type.HasSummary() : Boolean;
begin
  Result := True;
end;

function Promotion_Type.HasDetails() : Boolean;
begin
  Result := True;
end;

{ Promotion_Summary_Type }

function Promotion_Summary_Type.HasCategory() : Boolean;
begin
  Result := True;
end;

function Promotion_Summary_Type.HasStartDate() : Boolean;
begin
  Result := True;
end;

function Promotion_Summary_Type.HasEndDate() : Boolean;
begin
  Result := True;
end;

function Promotion_Summary_Type.HasEligibilityRequirementDescription() : Boolean;
begin
  Result := True;
end;

function Promotion_Summary_Type.HasBenefitDescription() : Boolean;
begin
  Result := True;
end;

function Promotion_Summary_Type.HasTermsAndConditions() : Boolean;
begin
  Result := True;
end;

{ Promotion_Details_Type }

constructor Promotion_Details_Type.Create();
begin
  inherited Create();
  FEligibilityRequirements := PromotionEligibilityRequirements.Create();
  FBenefits := PromotionBenefits.Create();
end;

destructor Promotion_Details_Type.Destroy();
begin
  if Assigned(FItemApplicability) then
    FreeAndNil(FItemApplicability);
  inherited Destroy();
end;

function Promotion_Details_Type.HasMerchantPromotionId() : Boolean;
begin
  Result := True;
end;

function Promotion_Details_Type.HasGroupClaimCode() : Boolean;
begin
  Result := True;
end;

function Promotion_Details_Type.HasCouponCombinationType() : Boolean;
begin
  Result := True;
end;

function Promotion_Details_Type.HasStartDate() : Boolean;
begin
  Result := True;
end;

function Promotion_Details_Type.HasEndDate() : Boolean;
begin
  Result := True;
end;

function Promotion_Details_Type.HasTermsAndConditions() : Boolean;
begin
  Result := True;
end;

function Promotion_Details_Type.HasEligibilityRequirements() : Boolean;
begin
  Result := True;
end;

function Promotion_Details_Type.HasBenefits() : Boolean;
begin
  Result := True;
end;

function Promotion_Details_Type.HasItemApplicability() : Boolean;
begin
  Result := True;
end;

{ PromotionEligibilityRequirement }

destructor PromotionEligibilityRequirement.Destroy();
begin
  if Assigned(FCurrencyAmount) then
    FreeAndNil(FCurrencyAmount);
  inherited Destroy();
end;

function PromotionEligibilityRequirement.HasQuantity() : Boolean;
begin
  Result := True;
end;

function PromotionEligibilityRequirement.HasCurrencyAmount() : Boolean;
begin
  Result := True;
end;

{ PromotionBenefit }

destructor PromotionBenefit.Destroy();
begin
  if Assigned(FFixedAmount) then
    FreeAndNil(FFixedAmount);
  if Assigned(FCeiling) then
    FreeAndNil(FCeiling);
  inherited Destroy();
end;

function PromotionBenefit.HasQuantity() : Boolean;
begin
  Result := True;
end;

function PromotionBenefit.HasPercentOff() : Boolean;
begin
  Result := True;
end;

function PromotionBenefit.HasFixedAmount() : Boolean;
begin
  Result := True;
end;

function PromotionBenefit.HasCeiling() : Boolean;
begin
  Result := True;
end;

{ BrowseNode_Type }

constructor BrowseNode_Type.Create();
begin
  inherited Create();
  FChildren := BrowseNode_Children_Type.Create();
  FAncestors := BrowseNode_Ancestors_Type.Create();
  FTopSellers := TopSellers_Type.Create();
  FNewReleases := NewReleases_Type.Create();
end;

destructor BrowseNode_Type.Destroy();
begin
  inherited Destroy();
end;

function BrowseNode_Type.HasBrowseNodeId() : Boolean;
begin
  Result := True;
end;

function BrowseNode_Type.HasName() : Boolean;
begin
  Result := True;
end;

function BrowseNode_Type.HasIsCategoryRoot() : Boolean;
begin
  Result := True;
end;

function BrowseNode_Type.HasChildren() : Boolean;
begin
  Result := True;
end;

function BrowseNode_Type.HasAncestors() : Boolean;
begin
  Result := True;
end;

function BrowseNode_Type.HasTopSellers() : Boolean;
begin
  Result := True;
end;

function BrowseNode_Type.HasNewReleases() : Boolean;
begin
  Result := True;
end;

{ ListmaniaLists_ListmaniaList_Type }

function ListmaniaLists_ListmaniaList_Type.HasListName() : Boolean;
begin
  Result := True;
end;

{ SearchInside_Excerpt_Type }

function SearchInside_Excerpt_Type.HasChecksum() : Boolean;
begin
  Result := True;
end;

function SearchInside_Excerpt_Type.HasPageType() : Boolean;
begin
  Result := True;
end;

function SearchInside_Excerpt_Type.HasPageNumber() : Boolean;
begin
  Result := True;
end;

function SearchInside_Excerpt_Type.HasSequenceNumber() : Boolean;
begin
  Result := True;
end;

function SearchInside_Excerpt_Type.HasText() : Boolean;
begin
  Result := True;
end;

{ CartItem }

destructor CartItem.Destroy();
begin
  if Assigned(FPrice) then
    FreeAndNil(FPrice);
  if Assigned(FItemTotal) then
    FreeAndNil(FItemTotal);
  inherited Destroy();
end;

function CartItem.HasASIN() : Boolean;
begin
  Result := True;
end;

function CartItem.HasExchangeId() : Boolean;
begin
  Result := True;
end;

function CartItem.HasMerchantId() : Boolean;
begin
  Result := True;
end;

function CartItem.HasSellerId() : Boolean;
begin
  Result := True;
end;

function CartItem.HasSellerNickname() : Boolean;
begin
  Result := True;
end;

function CartItem.HasTitle() : Boolean;
begin
  Result := True;
end;

function CartItem.HasProductGroup() : Boolean;
begin
  Result := True;
end;

function CartItem.HasListOwner() : Boolean;
begin
  Result := True;
end;

function CartItem.HasListType() : Boolean;
begin
  Result := True;
end;

function CartItem.HasPrice() : Boolean;
begin
  Result := True;
end;

function CartItem.HasItemTotal() : Boolean;
begin
  Result := True;
end;

{ Transaction_Totals_Type }

destructor Transaction_Totals_Type.Destroy();
begin
  if Assigned(FTotal) then
    FreeAndNil(FTotal);
  if Assigned(FSubtotal) then
    FreeAndNil(FSubtotal);
  if Assigned(FTax) then
    FreeAndNil(FTax);
  if Assigned(FShippingCharge) then
    FreeAndNil(FShippingCharge);
  if Assigned(FPromotion) then
    FreeAndNil(FPromotion);
  inherited Destroy();
end;

{ TransactionItem_Type }

constructor TransactionItem_Type.Create();
begin
  inherited Create();
  FChildTransactionItems := TransactionItem_ChildTransactionItems_Type.Create();
end;

destructor TransactionItem_Type.Destroy();
begin
  if Assigned(FUnitPrice) then
    FreeAndNil(FUnitPrice);
  if Assigned(FTotalPrice) then
    FreeAndNil(FTotalPrice);
  inherited Destroy();
end;

function TransactionItem_Type.HasASIN() : Boolean;
begin
  Result := True;
end;

function TransactionItem_Type.HasChildTransactionItems() : Boolean;
begin
  Result := True;
end;

{ Transaction_Shipments_Type_Shipment_Type }

constructor Transaction_Shipments_Type_Shipment_Type.Create();
begin
  inherited Create();
  FShipmentItems := Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type.Create();
  FPackages := Transaction_Shipments_Type_Shipment_Type_Packages_Type.Create();
end;

destructor Transaction_Shipments_Type_Shipment_Type.Destroy();
begin
  inherited Destroy();
end;

function Transaction_Shipments_Type_Shipment_Type.HasShipmentItems() : Boolean;
begin
  Result := True;
end;

function Transaction_Shipments_Type_Shipment_Type.HasPackages() : Boolean;
begin
  Result := True;
end;

{ Seller_Location_Type }

function Seller_Location_Type.HasUserDefinedLocation() : Boolean;
begin
  Result := True;
end;

function Seller_Location_Type.HasCity() : Boolean;
begin
  Result := True;
end;

function Seller_Location_Type.HasState() : Boolean;
begin
  Result := True;
end;

function Seller_Location_Type.HasCountry() : Boolean;
begin
  Result := True;
end;

{ Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type }

function Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type.HasCount() : Boolean;
begin
  Result := True;
end;

function Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type.HasPercentage() : Boolean;
begin
  Result := True;
end;

{ Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type }

constructor Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type.Create();
begin
  inherited Create();
  FSellerFeedbackRating := Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray.Create();
end;

destructor Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type.Destroy();
begin
  inherited Destroy();
end;

{ SellerFeedback_Feedback_Type }

function SellerFeedback_Feedback_Type.HasRating() : Boolean;
begin
  Result := True;
end;

function SellerFeedback_Feedback_Type.HasComment() : Boolean;
begin
  Result := True;
end;

function SellerFeedback_Feedback_Type.HasDate() : Boolean;
begin
  Result := True;
end;

function SellerFeedback_Feedback_Type.HasRatedBy() : Boolean;
begin
  Result := True;
end;

{ ItemAttributes_ItemDimensions_Type }

destructor ItemAttributes_ItemDimensions_Type.Destroy();
begin
  if Assigned(FHeight) then
    FreeAndNil(FHeight);
  if Assigned(FLength) then
    FreeAndNil(FLength);
  if Assigned(FWeight) then
    FreeAndNil(FWeight);
  if Assigned(FWidth) then
    FreeAndNil(FWidth);
  inherited Destroy();
end;

function ItemAttributes_ItemDimensions_Type.HasHeight() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_ItemDimensions_Type.HasLength() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_ItemDimensions_Type.HasWeight() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_ItemDimensions_Type.HasWidth() : Boolean;
begin
  Result := True;
end;

{ ItemAttributes_Languages_Type_Language_Type }

function ItemAttributes_Languages_Type_Language_Type.Has_Type() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_Languages_Type_Language_Type.HasAudioFormat() : Boolean;
begin
  Result := True;
end;

{ ItemAttributes_PackageDimensions_Type }

destructor ItemAttributes_PackageDimensions_Type.Destroy();
begin
  if Assigned(FHeight) then
    FreeAndNil(FHeight);
  if Assigned(FLength) then
    FreeAndNil(FLength);
  if Assigned(FWeight) then
    FreeAndNil(FWeight);
  if Assigned(FWidth) then
    FreeAndNil(FWidth);
  inherited Destroy();
end;

function ItemAttributes_PackageDimensions_Type.HasHeight() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_PackageDimensions_Type.HasLength() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_PackageDimensions_Type.HasWeight() : Boolean;
begin
  Result := True;
end;

function ItemAttributes_PackageDimensions_Type.HasWidth() : Boolean;
begin
  Result := True;
end;

{ MerchantItemAttributes_ItemDimensions_Type }

destructor MerchantItemAttributes_ItemDimensions_Type.Destroy();
begin
  if Assigned(FHeight) then
    FreeAndNil(FHeight);
  if Assigned(FLength) then
    FreeAndNil(FLength);
  if Assigned(FWeight) then
    FreeAndNil(FWeight);
  if Assigned(FWidth) then
    FreeAndNil(FWidth);
  inherited Destroy();
end;

function MerchantItemAttributes_ItemDimensions_Type.HasHeight() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_ItemDimensions_Type.HasLength() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_ItemDimensions_Type.HasWeight() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_ItemDimensions_Type.HasWidth() : Boolean;
begin
  Result := True;
end;

{ MerchantItemAttributes_Languages_Type_Language_Type }

function MerchantItemAttributes_Languages_Type_Language_Type.HasAudioFormat() : Boolean;
begin
  Result := True;
end;

{ MerchantItemAttributes_PackageDimensions_Type }

destructor MerchantItemAttributes_PackageDimensions_Type.Destroy();
begin
  if Assigned(FHeight) then
    FreeAndNil(FHeight);
  if Assigned(FLength) then
    FreeAndNil(FLength);
  if Assigned(FWeight) then
    FreeAndNil(FWeight);
  if Assigned(FWidth) then
    FreeAndNil(FWidth);
  inherited Destroy();
end;

function MerchantItemAttributes_PackageDimensions_Type.HasHeight() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_PackageDimensions_Type.HasLength() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_PackageDimensions_Type.HasWeight() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_PackageDimensions_Type.HasWidth() : Boolean;
begin
  Result := True;
end;

{ MerchantItemAttributes_VendorRebate_Type }

function MerchantItemAttributes_VendorRebate_Type.Has_Type() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_VendorRebate_Type.HasStartDate() : Boolean;
begin
  Result := True;
end;

function MerchantItemAttributes_VendorRebate_Type.HasEndDate() : Boolean;
begin
  Result := True;
end;

{ Help_RequestArray }

function Help_RequestArray.GetItem(AIndex: Integer): HelpRequest;
begin
  Result := Inherited GetItem(AIndex) As HelpRequest;
end;

class function Help_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= HelpRequest;
end;

{ HelpResponse_InformationArray }

function HelpResponse_InformationArray.GetItem(AIndex: Integer): Information_Type;
begin
  Result := Inherited GetItem(AIndex) As Information_Type;
end;

class function HelpResponse_InformationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Information_Type;
end;

{ ItemSearch_RequestArray }

function ItemSearch_RequestArray.GetItem(AIndex: Integer): ItemSearchRequest;
begin
  Result := Inherited GetItem(AIndex) As ItemSearchRequest;
end;

class function ItemSearch_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ItemSearchRequest;
end;

{ ItemSearchResponse_ItemsArray }

function ItemSearchResponse_ItemsArray.GetItem(AIndex: Integer): Items_Type;
begin
  Result := Inherited GetItem(AIndex) As Items_Type;
end;

class function ItemSearchResponse_ItemsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Items_Type;
end;

{ ItemLookup_RequestArray }

function ItemLookup_RequestArray.GetItem(AIndex: Integer): ItemLookupRequest;
begin
  Result := Inherited GetItem(AIndex) As ItemLookupRequest;
end;

class function ItemLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ItemLookupRequest;
end;

{ ItemLookupResponse_ItemsArray }

function ItemLookupResponse_ItemsArray.GetItem(AIndex: Integer): Items_Type;
begin
  Result := Inherited GetItem(AIndex) As Items_Type;
end;

class function ItemLookupResponse_ItemsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Items_Type;
end;

{ BrowseNodeLookup_RequestArray }

function BrowseNodeLookup_RequestArray.GetItem(AIndex: Integer): BrowseNodeLookupRequest;
begin
  Result := Inherited GetItem(AIndex) As BrowseNodeLookupRequest;
end;

class function BrowseNodeLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= BrowseNodeLookupRequest;
end;

{ BrowseNodeLookupResponse_BrowseNodesArray }

function BrowseNodeLookupResponse_BrowseNodesArray.GetItem(AIndex: Integer): BrowseNodes_Type;
begin
  Result := Inherited GetItem(AIndex) As BrowseNodes_Type;
end;

class function BrowseNodeLookupResponse_BrowseNodesArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= BrowseNodes_Type;
end;

{ ListSearch_RequestArray }

function ListSearch_RequestArray.GetItem(AIndex: Integer): ListSearchRequest;
begin
  Result := Inherited GetItem(AIndex) As ListSearchRequest;
end;

class function ListSearch_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ListSearchRequest;
end;

{ ListSearchResponse_ListsArray }

function ListSearchResponse_ListsArray.GetItem(AIndex: Integer): Lists_Type;
begin
  Result := Inherited GetItem(AIndex) As Lists_Type;
end;

class function ListSearchResponse_ListsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Lists_Type;
end;

{ ListLookup_RequestArray }

function ListLookup_RequestArray.GetItem(AIndex: Integer): ListLookupRequest;
begin
  Result := Inherited GetItem(AIndex) As ListLookupRequest;
end;

class function ListLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ListLookupRequest;
end;

{ ListLookupResponse_ListsArray }

function ListLookupResponse_ListsArray.GetItem(AIndex: Integer): Lists_Type;
begin
  Result := Inherited GetItem(AIndex) As Lists_Type;
end;

class function ListLookupResponse_ListsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Lists_Type;
end;

{ CustomerContentSearch_RequestArray }

function CustomerContentSearch_RequestArray.GetItem(AIndex: Integer): CustomerContentSearchRequest;
begin
  Result := Inherited GetItem(AIndex) As CustomerContentSearchRequest;
end;

class function CustomerContentSearch_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CustomerContentSearchRequest;
end;

{ CustomerContentSearchResponse_CustomersArray }

function CustomerContentSearchResponse_CustomersArray.GetItem(AIndex: Integer): Customers_Type;
begin
  Result := Inherited GetItem(AIndex) As Customers_Type;
end;

class function CustomerContentSearchResponse_CustomersArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Customers_Type;
end;

{ CustomerContentLookup_RequestArray }

function CustomerContentLookup_RequestArray.GetItem(AIndex: Integer): CustomerContentLookupRequest;
begin
  Result := Inherited GetItem(AIndex) As CustomerContentLookupRequest;
end;

class function CustomerContentLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CustomerContentLookupRequest;
end;

{ CustomerContentLookupResponse_CustomersArray }

function CustomerContentLookupResponse_CustomersArray.GetItem(AIndex: Integer): Customers_Type;
begin
  Result := Inherited GetItem(AIndex) As Customers_Type;
end;

class function CustomerContentLookupResponse_CustomersArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Customers_Type;
end;

{ SimilarityLookup_RequestArray }

function SimilarityLookup_RequestArray.GetItem(AIndex: Integer): SimilarityLookupRequest;
begin
  Result := Inherited GetItem(AIndex) As SimilarityLookupRequest;
end;

class function SimilarityLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SimilarityLookupRequest;
end;

{ SimilarityLookupResponse_ItemsArray }

function SimilarityLookupResponse_ItemsArray.GetItem(AIndex: Integer): Items_Type;
begin
  Result := Inherited GetItem(AIndex) As Items_Type;
end;

class function SimilarityLookupResponse_ItemsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Items_Type;
end;

{ SellerLookup_RequestArray }

function SellerLookup_RequestArray.GetItem(AIndex: Integer): SellerLookupRequest;
begin
  Result := Inherited GetItem(AIndex) As SellerLookupRequest;
end;

class function SellerLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SellerLookupRequest;
end;

{ SellerLookupResponse_SellersArray }

function SellerLookupResponse_SellersArray.GetItem(AIndex: Integer): Sellers_Type;
begin
  Result := Inherited GetItem(AIndex) As Sellers_Type;
end;

class function SellerLookupResponse_SellersArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Sellers_Type;
end;

{ CartGet_RequestArray }

function CartGet_RequestArray.GetItem(AIndex: Integer): CartGetRequest;
begin
  Result := Inherited GetItem(AIndex) As CartGetRequest;
end;

class function CartGet_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartGetRequest;
end;

{ CartGetResponse_CartArray }

function CartGetResponse_CartArray.GetItem(AIndex: Integer): Cart_Type;
begin
  Result := Inherited GetItem(AIndex) As Cart_Type;
end;

class function CartGetResponse_CartArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Cart_Type;
end;

{ CartAdd_RequestArray }

function CartAdd_RequestArray.GetItem(AIndex: Integer): CartAddRequest;
begin
  Result := Inherited GetItem(AIndex) As CartAddRequest;
end;

class function CartAdd_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartAddRequest;
end;

{ CartAddResponse_CartArray }

function CartAddResponse_CartArray.GetItem(AIndex: Integer): Cart_Type;
begin
  Result := Inherited GetItem(AIndex) As Cart_Type;
end;

class function CartAddResponse_CartArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Cart_Type;
end;

{ CartCreate_RequestArray }

function CartCreate_RequestArray.GetItem(AIndex: Integer): CartCreateRequest;
begin
  Result := Inherited GetItem(AIndex) As CartCreateRequest;
end;

class function CartCreate_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartCreateRequest;
end;

{ CartCreateResponse_CartArray }

function CartCreateResponse_CartArray.GetItem(AIndex: Integer): Cart_Type;
begin
  Result := Inherited GetItem(AIndex) As Cart_Type;
end;

class function CartCreateResponse_CartArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Cart_Type;
end;

{ CartModify_RequestArray }

function CartModify_RequestArray.GetItem(AIndex: Integer): CartModifyRequest;
begin
  Result := Inherited GetItem(AIndex) As CartModifyRequest;
end;

class function CartModify_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartModifyRequest;
end;

{ CartModifyResponse_CartArray }

function CartModifyResponse_CartArray.GetItem(AIndex: Integer): Cart_Type;
begin
  Result := Inherited GetItem(AIndex) As Cart_Type;
end;

class function CartModifyResponse_CartArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Cart_Type;
end;

{ CartClear_RequestArray }

function CartClear_RequestArray.GetItem(AIndex: Integer): CartClearRequest;
begin
  Result := Inherited GetItem(AIndex) As CartClearRequest;
end;

class function CartClear_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartClearRequest;
end;

{ CartClearResponse_CartArray }

function CartClearResponse_CartArray.GetItem(AIndex: Integer): Cart_Type;
begin
  Result := Inherited GetItem(AIndex) As Cart_Type;
end;

class function CartClearResponse_CartArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Cart_Type;
end;

{ TransactionLookup_RequestArray }

function TransactionLookup_RequestArray.GetItem(AIndex: Integer): TransactionLookupRequest;
begin
  Result := Inherited GetItem(AIndex) As TransactionLookupRequest;
end;

class function TransactionLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TransactionLookupRequest;
end;

{ TransactionLookupResponse_TransactionsArray }

function TransactionLookupResponse_TransactionsArray.GetItem(AIndex: Integer): Transactions_Type;
begin
  Result := Inherited GetItem(AIndex) As Transactions_Type;
end;

class function TransactionLookupResponse_TransactionsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Transactions_Type;
end;

{ SellerListingSearch_RequestArray }

function SellerListingSearch_RequestArray.GetItem(AIndex: Integer): SellerListingSearchRequest;
begin
  Result := Inherited GetItem(AIndex) As SellerListingSearchRequest;
end;

class function SellerListingSearch_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SellerListingSearchRequest;
end;

{ SellerListingSearchResponse_SellerListingsArray }

function SellerListingSearchResponse_SellerListingsArray.GetItem(AIndex: Integer): SellerListings_Type;
begin
  Result := Inherited GetItem(AIndex) As SellerListings_Type;
end;

class function SellerListingSearchResponse_SellerListingsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SellerListings_Type;
end;

{ SellerListingLookup_RequestArray }

function SellerListingLookup_RequestArray.GetItem(AIndex: Integer): SellerListingLookupRequest;
begin
  Result := Inherited GetItem(AIndex) As SellerListingLookupRequest;
end;

class function SellerListingLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SellerListingLookupRequest;
end;

{ SellerListingLookupResponse_SellerListingsArray }

function SellerListingLookupResponse_SellerListingsArray.GetItem(AIndex: Integer): SellerListings_Type;
begin
  Result := Inherited GetItem(AIndex) As SellerListings_Type;
end;

class function SellerListingLookupResponse_SellerListingsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SellerListings_Type;
end;

{ Bin_BinParameterArray }

function Bin_BinParameterArray.GetItem(AIndex: Integer): Bin_BinParameter_Type;
begin
  Result := Inherited GetItem(AIndex) As Bin_BinParameter_Type;
end;

class function Bin_BinParameterArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Bin_BinParameter_Type;
end;

{ SearchBinSet_BinArray }

function SearchBinSet_BinArray.GetItem(AIndex: Integer): Bin_Type;
begin
  Result := Inherited GetItem(AIndex) As Bin_Type;
end;

class function SearchBinSet_BinArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Bin_Type;
end;

{ SearchBinSets_Type }

function SearchBinSets_Type.GetItem(AIndex: Integer): SearchBinSet_Type;
begin
  Result := Inherited GetItem(AIndex) As SearchBinSet_Type;
end;

class function SearchBinSets_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SearchBinSet_Type;
end;

{ HelpRequest_ResponseGroupArray }

function HelpRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure HelpRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function HelpRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure HelpRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure HelpRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function HelpRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure HelpRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemSearchRequest_AudienceRatingArray }

function ItemSearchRequest_AudienceRatingArray.GetItem(AIndex: Integer): AudienceRating_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemSearchRequest_AudienceRatingArray.SetItem(AIndex: Integer;const AValue: AudienceRating_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemSearchRequest_AudienceRatingArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemSearchRequest_AudienceRatingArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('AudienceRating',TypeInfo(AudienceRating_Type),FData[AIndex]);
end;

procedure ItemSearchRequest_AudienceRatingArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'AudienceRating';
  AStore.Get(TypeInfo(AudienceRating_Type),sName,FData[AIndex]);
end;

class function ItemSearchRequest_AudienceRatingArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(AudienceRating_Type);
end;

procedure ItemSearchRequest_AudienceRatingArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemSearchRequest_ResponseGroupArray }

function ItemSearchRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemSearchRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemSearchRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemSearchRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure ItemSearchRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemSearchRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemSearchRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemLookupRequest_ItemIdArray }

function ItemLookupRequest_ItemIdArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemLookupRequest_ItemIdArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemLookupRequest_ItemIdArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemLookupRequest_ItemIdArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ItemId',TypeInfo(string),FData[AIndex]);
end;

procedure ItemLookupRequest_ItemIdArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ItemId';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemLookupRequest_ItemIdArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemLookupRequest_ItemIdArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemLookupRequest_ResponseGroupArray }

function ItemLookupRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemLookupRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemLookupRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemLookupRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure ItemLookupRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemLookupRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemLookupRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ListSearchRequest_ResponseGroupArray }

function ListSearchRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ListSearchRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ListSearchRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ListSearchRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure ListSearchRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ListSearchRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ListSearchRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ListLookupRequest_ResponseGroupArray }

function ListLookupRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ListLookupRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ListLookupRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ListLookupRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure ListLookupRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ListLookupRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ListLookupRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ CustomerContentSearchRequest_ResponseGroupArray }

function CustomerContentSearchRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure CustomerContentSearchRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function CustomerContentSearchRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure CustomerContentSearchRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure CustomerContentSearchRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function CustomerContentSearchRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure CustomerContentSearchRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ CustomerContentLookupRequest_ResponseGroupArray }

function CustomerContentLookupRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure CustomerContentLookupRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function CustomerContentLookupRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure CustomerContentLookupRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure CustomerContentLookupRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function CustomerContentLookupRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure CustomerContentLookupRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ SimilarityLookupRequest_ItemIdArray }

function SimilarityLookupRequest_ItemIdArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure SimilarityLookupRequest_ItemIdArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function SimilarityLookupRequest_ItemIdArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure SimilarityLookupRequest_ItemIdArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ItemId',TypeInfo(string),FData[AIndex]);
end;

procedure SimilarityLookupRequest_ItemIdArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ItemId';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function SimilarityLookupRequest_ItemIdArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure SimilarityLookupRequest_ItemIdArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ SimilarityLookupRequest_ResponseGroupArray }

function SimilarityLookupRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure SimilarityLookupRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function SimilarityLookupRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure SimilarityLookupRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure SimilarityLookupRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function SimilarityLookupRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure SimilarityLookupRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ SellerLookupRequest_ResponseGroupArray }

function SellerLookupRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure SellerLookupRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function SellerLookupRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure SellerLookupRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure SellerLookupRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function SellerLookupRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure SellerLookupRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ SellerLookupRequest_SellerIdArray }

function SellerLookupRequest_SellerIdArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure SellerLookupRequest_SellerIdArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function SellerLookupRequest_SellerIdArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure SellerLookupRequest_SellerIdArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('SellerId',TypeInfo(string),FData[AIndex]);
end;

procedure SellerLookupRequest_SellerIdArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'SellerId';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function SellerLookupRequest_SellerIdArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure SellerLookupRequest_SellerIdArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ CartGetRequest_ResponseGroupArray }

function CartGetRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure CartGetRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function CartGetRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure CartGetRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure CartGetRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function CartGetRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure CartGetRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ CartAddRequest_Items_Type }

function CartAddRequest_Items_Type.GetItem(AIndex: Integer): CartAddRequest_Items_Type_Item_Type;
begin
  Result := Inherited GetItem(AIndex) As CartAddRequest_Items_Type_Item_Type;
end;

class function CartAddRequest_Items_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartAddRequest_Items_Type_Item_Type;
end;

{ CartAddRequest_ResponseGroupArray }

function CartAddRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure CartAddRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function CartAddRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure CartAddRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure CartAddRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function CartAddRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure CartAddRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ CartCreateRequest_Items_Type }

function CartCreateRequest_Items_Type.GetItem(AIndex: Integer): CartCreateRequest_Items_Type_Item_Type;
begin
  Result := Inherited GetItem(AIndex) As CartCreateRequest_Items_Type_Item_Type;
end;

class function CartCreateRequest_Items_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartCreateRequest_Items_Type_Item_Type;
end;

{ CartCreateRequest_ResponseGroupArray }

function CartCreateRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure CartCreateRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function CartCreateRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure CartCreateRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure CartCreateRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function CartCreateRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure CartCreateRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ CartModifyRequest_Items_Type }

function CartModifyRequest_Items_Type.GetItem(AIndex: Integer): CartModifyRequest_Items_Type_Item_Type;
begin
  Result := Inherited GetItem(AIndex) As CartModifyRequest_Items_Type_Item_Type;
end;

class function CartModifyRequest_Items_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartModifyRequest_Items_Type_Item_Type;
end;

{ CartModifyRequest_ResponseGroupArray }

function CartModifyRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure CartModifyRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function CartModifyRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure CartModifyRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure CartModifyRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function CartModifyRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure CartModifyRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ CartClearRequest_ResponseGroupArray }

function CartClearRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure CartClearRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function CartClearRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure CartClearRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure CartClearRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function CartClearRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure CartClearRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TransactionLookupRequest_ResponseGroupArray }

function TransactionLookupRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TransactionLookupRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TransactionLookupRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TransactionLookupRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure TransactionLookupRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function TransactionLookupRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure TransactionLookupRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TransactionLookupRequest_TransactionIdArray }

function TransactionLookupRequest_TransactionIdArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TransactionLookupRequest_TransactionIdArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TransactionLookupRequest_TransactionIdArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TransactionLookupRequest_TransactionIdArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('TransactionId',TypeInfo(string),FData[AIndex]);
end;

procedure TransactionLookupRequest_TransactionIdArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'TransactionId';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function TransactionLookupRequest_TransactionIdArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure TransactionLookupRequest_TransactionIdArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ SellerListingSearchRequest_ResponseGroupArray }

function SellerListingSearchRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure SellerListingSearchRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function SellerListingSearchRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure SellerListingSearchRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure SellerListingSearchRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function SellerListingSearchRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure SellerListingSearchRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ SellerListingLookupRequest_ResponseGroupArray }

function SellerListingLookupRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure SellerListingLookupRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function SellerListingLookupRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure SellerListingLookupRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure SellerListingLookupRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function SellerListingLookupRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure SellerListingLookupRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ BrowseNodeLookupRequest_BrowseNodeIdArray }

function BrowseNodeLookupRequest_BrowseNodeIdArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure BrowseNodeLookupRequest_BrowseNodeIdArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function BrowseNodeLookupRequest_BrowseNodeIdArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure BrowseNodeLookupRequest_BrowseNodeIdArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('BrowseNodeId',TypeInfo(string),FData[AIndex]);
end;

procedure BrowseNodeLookupRequest_BrowseNodeIdArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'BrowseNodeId';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function BrowseNodeLookupRequest_BrowseNodeIdArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure BrowseNodeLookupRequest_BrowseNodeIdArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ BrowseNodeLookupRequest_ResponseGroupArray }

function BrowseNodeLookupRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure BrowseNodeLookupRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function BrowseNodeLookupRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure BrowseNodeLookupRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure BrowseNodeLookupRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function BrowseNodeLookupRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure BrowseNodeLookupRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ HTTPHeaders_Type }

function HTTPHeaders_Type.GetItem(AIndex: Integer): HTTPHeaders_Header_Type;
begin
  Result := Inherited GetItem(AIndex) As HTTPHeaders_Header_Type;
end;

class function HTTPHeaders_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= HTTPHeaders_Header_Type;
end;

{ Arguments_Type }

function Arguments_Type.GetItem(AIndex: Integer): Arguments_Argument_Type;
begin
  Result := Inherited GetItem(AIndex) As Arguments_Argument_Type;
end;

class function Arguments_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Arguments_Argument_Type;
end;

{ Errors_Type }

function Errors_Type.GetItem(AIndex: Integer): Errors_Error_Type;
begin
  Result := Inherited GetItem(AIndex) As Errors_Error_Type;
end;

class function Errors_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Errors_Error_Type;
end;

{ Information_OperationInformationArray }

function Information_OperationInformationArray.GetItem(AIndex: Integer): OperationInformation_Type;
begin
  Result := Inherited GetItem(AIndex) As OperationInformation_Type;
end;

class function Information_OperationInformationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= OperationInformation_Type;
end;

{ Information_ResponseGroupInformationArray }

function Information_ResponseGroupInformationArray.GetItem(AIndex: Integer): ResponseGroupInformation_Type;
begin
  Result := Inherited GetItem(AIndex) As ResponseGroupInformation_Type;
end;

class function Information_ResponseGroupInformationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ResponseGroupInformation_Type;
end;

{ SearchResultsMap_Type }

function SearchResultsMap_Type.GetItem(AIndex: Integer): SearchResultsMap_SearchIndex_Type;
begin
  Result := Inherited GetItem(AIndex) As SearchResultsMap_SearchIndex_Type;
end;

class function SearchResultsMap_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SearchResultsMap_SearchIndex_Type;
end;

{ Items__ItemArray }

function Items__ItemArray.GetItem(AIndex: Integer): Item_Type;
begin
  Result := Inherited GetItem(AIndex) As Item_Type;
end;

class function Items__ItemArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Item_Type;
end;

{ Lists_ListArray }

function Lists_ListArray.GetItem(AIndex: Integer): List_Type;
begin
  Result := Inherited GetItem(AIndex) As List_Type;
end;

class function Lists_ListArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= List_Type;
end;

{ Customers_CustomerArray }

function Customers_CustomerArray.GetItem(AIndex: Integer): Customer_Type;
begin
  Result := Inherited GetItem(AIndex) As Customer_Type;
end;

class function Customers_CustomerArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Customer_Type;
end;

{ SimilarProducts_Type }

function SimilarProducts_Type.GetItem(AIndex: Integer): SimilarProducts_SimilarProduct_Type;
begin
  Result := Inherited GetItem(AIndex) As SimilarProducts_SimilarProduct_Type;
end;

class function SimilarProducts_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SimilarProducts_SimilarProduct_Type;
end;

{ TopSellers_Type }

function TopSellers_Type.GetItem(AIndex: Integer): TopSellers_TopSeller_Type;
begin
  Result := Inherited GetItem(AIndex) As TopSellers_TopSeller_Type;
end;

class function TopSellers_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TopSellers_TopSeller_Type;
end;

{ NewReleases_Type }

function NewReleases_Type.GetItem(AIndex: Integer): NewReleases_NewRelease_Type;
begin
  Result := Inherited GetItem(AIndex) As NewReleases_NewRelease_Type;
end;

class function NewReleases_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= NewReleases_NewRelease_Type;
end;

{ SimilarViewedProducts_Type }

function SimilarViewedProducts_Type.GetItem(AIndex: Integer): SimilarViewedProducts_SimilarViewedProduct_Type;
begin
  Result := Inherited GetItem(AIndex) As SimilarViewedProducts_SimilarViewedProduct_Type;
end;

class function SimilarViewedProducts_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SimilarViewedProducts_SimilarViewedProduct_Type;
end;

{ OtherCategoriesSimilarProducts_Type }

function OtherCategoriesSimilarProducts_Type.GetItem(AIndex: Integer): OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type;
begin
  Result := Inherited GetItem(AIndex) As OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type;
end;

class function OtherCategoriesSimilarProducts_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type;
end;

{ Transactions_TransactionArray }

function Transactions_TransactionArray.GetItem(AIndex: Integer): Transaction_Type;
begin
  Result := Inherited GetItem(AIndex) As Transaction_Type;
end;

class function Transactions_TransactionArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Transaction_Type;
end;

{ Sellers_SellerArray }

function Sellers_SellerArray.GetItem(AIndex: Integer): Seller_Type;
begin
  Result := Inherited GetItem(AIndex) As Seller_Type;
end;

class function Sellers_SellerArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Seller_Type;
end;

{ SellerListings_SellerListingArray }

function SellerListings_SellerListingArray.GetItem(AIndex: Integer): SellerListing_Type;
begin
  Result := Inherited GetItem(AIndex) As SellerListing_Type;
end;

class function SellerListings_SellerListingArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SellerListing_Type;
end;

{ OperationInformation_RequiredParameters_Type }

function OperationInformation_RequiredParameters_Type.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure OperationInformation_RequiredParameters_Type.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function OperationInformation_RequiredParameters_Type.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure OperationInformation_RequiredParameters_Type.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Parameter',TypeInfo(string),FData[AIndex]);
end;

procedure OperationInformation_RequiredParameters_Type.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Parameter';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function OperationInformation_RequiredParameters_Type.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure OperationInformation_RequiredParameters_Type.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ OperationInformation_AvailableParameters_Type }

function OperationInformation_AvailableParameters_Type.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure OperationInformation_AvailableParameters_Type.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function OperationInformation_AvailableParameters_Type.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure OperationInformation_AvailableParameters_Type.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Parameter',TypeInfo(string),FData[AIndex]);
end;

procedure OperationInformation_AvailableParameters_Type.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Parameter';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function OperationInformation_AvailableParameters_Type.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure OperationInformation_AvailableParameters_Type.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ OperationInformation_DefaultResponseGroups_Type }

function OperationInformation_DefaultResponseGroups_Type.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure OperationInformation_DefaultResponseGroups_Type.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function OperationInformation_DefaultResponseGroups_Type.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure OperationInformation_DefaultResponseGroups_Type.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure OperationInformation_DefaultResponseGroups_Type.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function OperationInformation_DefaultResponseGroups_Type.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure OperationInformation_DefaultResponseGroups_Type.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ OperationInformation_AvailableResponseGroups_Type }

function OperationInformation_AvailableResponseGroups_Type.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure OperationInformation_AvailableResponseGroups_Type.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function OperationInformation_AvailableResponseGroups_Type.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure OperationInformation_AvailableResponseGroups_Type.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure OperationInformation_AvailableResponseGroups_Type.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function OperationInformation_AvailableResponseGroups_Type.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure OperationInformation_AvailableResponseGroups_Type.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ResponseGroupInformation_ValidOperations_Type }

function ResponseGroupInformation_ValidOperations_Type.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ResponseGroupInformation_ValidOperations_Type.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ResponseGroupInformation_ValidOperations_Type.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ResponseGroupInformation_ValidOperations_Type.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Operation',TypeInfo(string),FData[AIndex]);
end;

procedure ResponseGroupInformation_ValidOperations_Type.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Operation';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ResponseGroupInformation_ValidOperations_Type.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ResponseGroupInformation_ValidOperations_Type.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ResponseGroupInformation_Elements_Type }

function ResponseGroupInformation_Elements_Type.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ResponseGroupInformation_Elements_Type.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ResponseGroupInformation_Elements_Type.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ResponseGroupInformation_Elements_Type.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Element',TypeInfo(string),FData[AIndex]);
end;

procedure ResponseGroupInformation_Elements_Type.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Element';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ResponseGroupInformation_Elements_Type.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ResponseGroupInformation_Elements_Type.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ List_ListItemArray }

function List_ListItemArray.GetItem(AIndex: Integer): ListItem_Type;
begin
  Result := Inherited GetItem(AIndex) As ListItem_Type;
end;

class function List_ListItemArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ListItem_Type;
end;

{ Customer_CustomerReviewsArray }

function Customer_CustomerReviewsArray.GetItem(AIndex: Integer): CustomerReviews_Type;
begin
  Result := Inherited GetItem(AIndex) As CustomerReviews_Type;
end;

class function Customer_CustomerReviewsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CustomerReviews_Type;
end;

{ SearchResultsMap_SearchIndex_Type_ASINArray }

function SearchResultsMap_SearchIndex_Type_ASINArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure SearchResultsMap_SearchIndex_Type_ASINArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function SearchResultsMap_SearchIndex_Type_ASINArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure SearchResultsMap_SearchIndex_Type_ASINArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ASIN',TypeInfo(string),FData[AIndex]);
end;

procedure SearchResultsMap_SearchIndex_Type_ASINArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ASIN';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function SearchResultsMap_SearchIndex_Type_ASINArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure SearchResultsMap_SearchIndex_Type_ASINArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ Item_ImageSets_Type_ImageSetArray }

function Item_ImageSets_Type_ImageSetArray.GetItem(AIndex: Integer): ImageSet_Type;
begin
  Result := Inherited GetItem(AIndex) As ImageSet_Type;
end;

class function Item_ImageSets_Type_ImageSetArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ImageSet_Type;
end;

{ Collections_Type }

function Collections_Type.GetItem(AIndex: Integer): Collections_Collection_Type;
begin
  Result := Inherited GetItem(AIndex) As Collections_Collection_Type;
end;

class function Collections_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Collections_Collection_Type;
end;

{ Item_Subjects_Type }

function Item_Subjects_Type.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure Item_Subjects_Type.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function Item_Subjects_Type.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure Item_Subjects_Type.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Subject',TypeInfo(string),FData[AIndex]);
end;

procedure Item_Subjects_Type.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Subject';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function Item_Subjects_Type.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure Item_Subjects_Type.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ EditorialReviews_Type }

function EditorialReviews_Type.GetItem(AIndex: Integer): EditorialReview_Type;
begin
  Result := Inherited GetItem(AIndex) As EditorialReview_Type;
end;

class function EditorialReviews_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= EditorialReview_Type;
end;

{ Accessories_Type }

function Accessories_Type.GetItem(AIndex: Integer): Accessories_Accessory_Type;
begin
  Result := Inherited GetItem(AIndex) As Accessories_Accessory_Type;
end;

class function Accessories_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Accessories_Accessory_Type;
end;

{ Tracks_Type }

function Tracks_Type.GetItem(AIndex: Integer): Tracks_Disc_Type;
begin
  Result := Inherited GetItem(AIndex) As Tracks_Disc_Type;
end;

class function Tracks_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Tracks_Disc_Type;
end;

{ ListmaniaLists_Type }

function ListmaniaLists_Type.GetItem(AIndex: Integer): ListmaniaLists_ListmaniaList_Type;
begin
  Result := Inherited GetItem(AIndex) As ListmaniaLists_ListmaniaList_Type;
end;

class function ListmaniaLists_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ListmaniaLists_ListmaniaList_Type;
end;

{ Item_AlternateVersions_Type }

function Item_AlternateVersions_Type.GetItem(AIndex: Integer): Item_AlternateVersions_Type_AlternateVersion_Type;
begin
  Result := Inherited GetItem(AIndex) As Item_AlternateVersions_Type_AlternateVersion_Type;
end;

class function Item_AlternateVersions_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Item_AlternateVersions_Type_AlternateVersion_Type;
end;

{ _Item_ImageSetsArray }

function _Item_ImageSetsArray.GetItem(AIndex: Integer): Item_ImageSets_Type;
begin
  Result := Inherited GetItem(AIndex) As Item_ImageSets_Type;
end;

class function _Item_ImageSetsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Item_ImageSets_Type;
end;

{ Offers_OfferArray }

function Offers_OfferArray.GetItem(AIndex: Integer): Offer_Type;
begin
  Result := Inherited GetItem(AIndex) As Offer_Type;
end;

class function Offers_OfferArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Offer_Type;
end;

{ Promotions_Type }

function Promotions_Type.GetItem(AIndex: Integer): Promotion_Type;
begin
  Result := Inherited GetItem(AIndex) As Promotion_Type;
end;

class function Promotions_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Promotion_Type;
end;

{ Offer_OfferListingArray }

function Offer_OfferListingArray.GetItem(AIndex: Integer): OfferListing_Type;
begin
  Result := Inherited GetItem(AIndex) As OfferListing_Type;
end;

class function Offer_OfferListingArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= OfferListing_Type;
end;

{ OfferListing_ShippingChargeArray }

function OfferListing_ShippingChargeArray.GetItem(AIndex: Integer): OfferListing_ShippingCharge_Type;
begin
  Result := Inherited GetItem(AIndex) As OfferListing_ShippingCharge_Type;
end;

class function OfferListing_ShippingChargeArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= OfferListing_ShippingCharge_Type;
end;

{ VariationDimensions_Type }

function VariationDimensions_Type.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure VariationDimensions_Type.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function VariationDimensions_Type.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure VariationDimensions_Type.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('VariationDimension',TypeInfo(string),FData[AIndex]);
end;

procedure VariationDimensions_Type.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'VariationDimension';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function VariationDimensions_Type.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure VariationDimensions_Type.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ Variations__ItemArray }

function Variations__ItemArray.GetItem(AIndex: Integer): Item_Type;
begin
  Result := Inherited GetItem(AIndex) As Item_Type;
end;

class function Variations__ItemArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Item_Type;
end;

{ Collections_Collection_Type_CollectionItemArray }

function Collections_Collection_Type_CollectionItemArray.GetItem(AIndex: Integer): Collections_Collection_Type_CollectionItem_Type;
begin
  Result := Inherited GetItem(AIndex) As Collections_Collection_Type_CollectionItem_Type;
end;

class function Collections_Collection_Type_CollectionItemArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Collections_Collection_Type_CollectionItem_Type;
end;

{ CustomerReviews_ReviewArray }

function CustomerReviews_ReviewArray.GetItem(AIndex: Integer): Review_Type;
begin
  Result := Inherited GetItem(AIndex) As Review_Type;
end;

class function CustomerReviews_ReviewArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Review_Type;
end;

{ Tracks_Disc_Type_TrackArray }

function Tracks_Disc_Type_TrackArray.GetItem(AIndex: Integer): Tracks_Disc_Type_Track_Type;
begin
  Result := Inherited GetItem(AIndex) As Tracks_Disc_Type_Track_Type;
end;

class function Tracks_Disc_Type_TrackArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Tracks_Disc_Type_Track_Type;
end;

{ PromotionEligibilityRequirements }

function PromotionEligibilityRequirements.GetItem(AIndex: Integer): PromotionEligibilityRequirement;
begin
  Result := Inherited GetItem(AIndex) As PromotionEligibilityRequirement;
end;

class function PromotionEligibilityRequirements.GetItemClass(): TBaseRemotableClass;
begin
  Result:= PromotionEligibilityRequirement;
end;

{ PromotionBenefits }

function PromotionBenefits.GetItem(AIndex: Integer): PromotionBenefit;
begin
  Result := Inherited GetItem(AIndex) As PromotionBenefit;
end;

class function PromotionBenefits.GetItemClass(): TBaseRemotableClass;
begin
  Result:= PromotionBenefit;
end;

{ BrowseNodes_BrowseNodeArray }

function BrowseNodes_BrowseNodeArray.GetItem(AIndex: Integer): BrowseNode_Type;
begin
  Result := Inherited GetItem(AIndex) As BrowseNode_Type;
end;

class function BrowseNodes_BrowseNodeArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= BrowseNode_Type;
end;

{ BrowseNode_Children_Type }

function BrowseNode_Children_Type.GetItem(AIndex: Integer): BrowseNode_Type;
begin
  Result := Inherited GetItem(AIndex) As BrowseNode_Type;
end;

class function BrowseNode_Children_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= BrowseNode_Type;
end;

{ BrowseNode_Ancestors_Type }

function BrowseNode_Ancestors_Type.GetItem(AIndex: Integer): BrowseNode_Type;
begin
  Result := Inherited GetItem(AIndex) As BrowseNode_Type;
end;

class function BrowseNode_Ancestors_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= BrowseNode_Type;
end;

{ CartItems_CartItemArray }

function CartItems_CartItemArray.GetItem(AIndex: Integer): CartItem;
begin
  Result := Inherited GetItem(AIndex) As CartItem;
end;

class function CartItems_CartItemArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartItem;
end;

{ SavedForLaterItems_SavedForLaterItemArray }

function SavedForLaterItems_SavedForLaterItemArray.GetItem(AIndex: Integer): CartItem;
begin
  Result := Inherited GetItem(AIndex) As CartItem;
end;

class function SavedForLaterItems_SavedForLaterItemArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartItem;
end;

{ Transaction_TransactionItems_Type }

function Transaction_TransactionItems_Type.GetItem(AIndex: Integer): TransactionItem_Type;
begin
  Result := Inherited GetItem(AIndex) As TransactionItem_Type;
end;

class function Transaction_TransactionItems_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TransactionItem_Type;
end;

{ Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type }

function Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('TransactionItemId',TypeInfo(string),FData[AIndex]);
end;

procedure Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'TransactionItemId';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ Transaction_Shipments_Type_Shipment_Type_Packages_Type }

function Transaction_Shipments_Type_Shipment_Type_Packages_Type.GetItem(AIndex: Integer): Transaction_Shipments_Type_Shipment_Type_Packages_Type_Package_Type;
begin
  Result := Inherited GetItem(AIndex) As Transaction_Shipments_Type_Shipment_Type_Packages_Type_Package_Type;
end;

class function Transaction_Shipments_Type_Shipment_Type_Packages_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Transaction_Shipments_Type_Shipment_Type_Packages_Type_Package_Type;
end;

{ Transaction_Shipments_Type }

function Transaction_Shipments_Type.GetItem(AIndex: Integer): Transaction_Shipments_Type_Shipment_Type;
begin
  Result := Inherited GetItem(AIndex) As Transaction_Shipments_Type_Shipment_Type;
end;

class function Transaction_Shipments_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Transaction_Shipments_Type_Shipment_Type;
end;

{ TransactionItem_ChildTransactionItems_Type }

function TransactionItem_ChildTransactionItems_Type.GetItem(AIndex: Integer): TransactionItem_Type;
begin
  Result := Inherited GetItem(AIndex) As TransactionItem_Type;
end;

class function TransactionItem_ChildTransactionItems_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TransactionItem_Type;
end;

{ Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray }

function Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray.GetItem(AIndex: Integer): Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type;
begin
  Result := Inherited GetItem(AIndex) As Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type;
end;

class function Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type;
end;

{ Seller_SellerFeedbackSummary_Type }

function Seller_SellerFeedbackSummary_Type.GetItem(AIndex: Integer): Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type;
begin
  Result := Inherited GetItem(AIndex) As Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type;
end;

class function Seller_SellerFeedbackSummary_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type;
end;

{ SellerFeedback_Type }

function SellerFeedback_Type.GetItem(AIndex: Integer): SellerFeedback_Feedback_Type;
begin
  Result := Inherited GetItem(AIndex) As SellerFeedback_Feedback_Type;
end;

class function SellerFeedback_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SellerFeedback_Feedback_Type;
end;

{ ItemAttributes_Languages_Type }

function ItemAttributes_Languages_Type.GetItem(AIndex: Integer): ItemAttributes_Languages_Type_Language_Type;
begin
  Result := Inherited GetItem(AIndex) As ItemAttributes_Languages_Type_Language_Type;
end;

class function ItemAttributes_Languages_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ItemAttributes_Languages_Type_Language_Type;
end;

{ ItemAttributes_ActorArray }

function ItemAttributes_ActorArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_ActorArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_ActorArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_ActorArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Actor',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_ActorArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Actor';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_ActorArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_ActorArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_ArtistArray }

function ItemAttributes_ArtistArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_ArtistArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_ArtistArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_ArtistArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Artist',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_ArtistArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Artist';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_ArtistArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_ArtistArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_AudioFormatArray }

function ItemAttributes_AudioFormatArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_AudioFormatArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_AudioFormatArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_AudioFormatArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('AudioFormat',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_AudioFormatArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'AudioFormat';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_AudioFormatArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_AudioFormatArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_AuthorArray }

function ItemAttributes_AuthorArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_AuthorArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_AuthorArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_AuthorArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Author',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_AuthorArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Author';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_AuthorArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_AuthorArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_CameraManualFeaturesArray }

function ItemAttributes_CameraManualFeaturesArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_CameraManualFeaturesArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_CameraManualFeaturesArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_CameraManualFeaturesArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('CameraManualFeatures',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_CameraManualFeaturesArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'CameraManualFeatures';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_CameraManualFeaturesArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_CameraManualFeaturesArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_CompatibleDevicesArray }

function ItemAttributes_CompatibleDevicesArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_CompatibleDevicesArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_CompatibleDevicesArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_CompatibleDevicesArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('CompatibleDevices',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_CompatibleDevicesArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'CompatibleDevices';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_CompatibleDevicesArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_CompatibleDevicesArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_CreatorArray }

function ItemAttributes_CreatorArray.GetItem(AIndex: Integer): ItemAttributes_Creator_Type;
begin
  Result := Inherited GetItem(AIndex) As ItemAttributes_Creator_Type;
end;

class function ItemAttributes_CreatorArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ItemAttributes_Creator_Type;
end;

{ ItemAttributes_DataLinkProtocolArray }

function ItemAttributes_DataLinkProtocolArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_DataLinkProtocolArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_DataLinkProtocolArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_DataLinkProtocolArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('DataLinkProtocol',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_DataLinkProtocolArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'DataLinkProtocol';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_DataLinkProtocolArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_DataLinkProtocolArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_DirectorArray }

function ItemAttributes_DirectorArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_DirectorArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_DirectorArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_DirectorArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Director',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_DirectorArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Director';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_DirectorArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_DirectorArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_FeatureArray }

function ItemAttributes_FeatureArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_FeatureArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_FeatureArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_FeatureArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Feature',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_FeatureArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Feature';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_FeatureArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_FeatureArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_FormatArray }

function ItemAttributes_FormatArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_FormatArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_FormatArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_FormatArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Format',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_FormatArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Format';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_FormatArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_FormatArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_FormFactorArray }

function ItemAttributes_FormFactorArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_FormFactorArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_FormFactorArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_FormFactorArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('FormFactor',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_FormFactorArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'FormFactor';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_FormFactorArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_FormFactorArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_PhotoFlashTypeArray }

function ItemAttributes_PhotoFlashTypeArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_PhotoFlashTypeArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_PhotoFlashTypeArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_PhotoFlashTypeArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('PhotoFlashType',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_PhotoFlashTypeArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'PhotoFlashType';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_PhotoFlashTypeArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_PhotoFlashTypeArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_PictureFormatArray }

function ItemAttributes_PictureFormatArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_PictureFormatArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_PictureFormatArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_PictureFormatArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('PictureFormat',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_PictureFormatArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'PictureFormat';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_PictureFormatArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_PictureFormatArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_PlatformArray }

function ItemAttributes_PlatformArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_PlatformArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_PlatformArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_PlatformArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Platform',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_PlatformArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Platform';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_PlatformArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_PlatformArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_ReturnMethodArray }

function ItemAttributes_ReturnMethodArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_ReturnMethodArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_ReturnMethodArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_ReturnMethodArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ReturnMethod',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_ReturnMethodArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ReturnMethod';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_ReturnMethodArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_ReturnMethodArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_SpecialFeaturesArray }

function ItemAttributes_SpecialFeaturesArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_SpecialFeaturesArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_SpecialFeaturesArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_SpecialFeaturesArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('SpecialFeatures',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_SpecialFeaturesArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'SpecialFeatures';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_SpecialFeaturesArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_SpecialFeaturesArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_SupportedImageTypeArray }

function ItemAttributes_SupportedImageTypeArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_SupportedImageTypeArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_SupportedImageTypeArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_SupportedImageTypeArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('SupportedImageType',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_SupportedImageTypeArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'SupportedImageType';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_SupportedImageTypeArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_SupportedImageTypeArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_Languages_Type }

function MerchantItemAttributes_Languages_Type.GetItem(AIndex: Integer): MerchantItemAttributes_Languages_Type_Language_Type;
begin
  Result := Inherited GetItem(AIndex) As MerchantItemAttributes_Languages_Type_Language_Type;
end;

class function MerchantItemAttributes_Languages_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= MerchantItemAttributes_Languages_Type_Language_Type;
end;

{ MerchantItemAttributes_ActorArray }

function MerchantItemAttributes_ActorArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_ActorArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_ActorArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_ActorArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Actor',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_ActorArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Actor';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_ActorArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_ActorArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_ArtistArray }

function MerchantItemAttributes_ArtistArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_ArtistArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_ArtistArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_ArtistArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Artist',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_ArtistArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Artist';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_ArtistArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_ArtistArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_AudioFormatArray }

function MerchantItemAttributes_AudioFormatArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_AudioFormatArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_AudioFormatArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_AudioFormatArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('AudioFormat',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_AudioFormatArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'AudioFormat';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_AudioFormatArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_AudioFormatArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_AuthorArray }

function MerchantItemAttributes_AuthorArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_AuthorArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_AuthorArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_AuthorArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Author',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_AuthorArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Author';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_AuthorArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_AuthorArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_CameraManualFeaturesArray }

function MerchantItemAttributes_CameraManualFeaturesArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_CameraManualFeaturesArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_CameraManualFeaturesArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_CameraManualFeaturesArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('CameraManualFeatures',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_CameraManualFeaturesArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'CameraManualFeatures';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_CameraManualFeaturesArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_CameraManualFeaturesArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_CreatorArray }

function MerchantItemAttributes_CreatorArray.GetItem(AIndex: Integer): MerchantItemAttributes_Creator_Type;
begin
  Result := Inherited GetItem(AIndex) As MerchantItemAttributes_Creator_Type;
end;

class function MerchantItemAttributes_CreatorArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= MerchantItemAttributes_Creator_Type;
end;

{ MerchantItemAttributes_DirectorArray }

function MerchantItemAttributes_DirectorArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_DirectorArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_DirectorArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_DirectorArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Director',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_DirectorArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Director';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_DirectorArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_DirectorArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_FeatureArray }

function MerchantItemAttributes_FeatureArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_FeatureArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_FeatureArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_FeatureArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Feature',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_FeatureArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Feature';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_FeatureArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_FeatureArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_FormatArray }

function MerchantItemAttributes_FormatArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_FormatArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_FormatArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_FormatArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Format',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_FormatArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Format';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_FormatArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_FormatArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_PhotoFlashTypeArray }

function MerchantItemAttributes_PhotoFlashTypeArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_PhotoFlashTypeArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_PhotoFlashTypeArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_PhotoFlashTypeArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('PhotoFlashType',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_PhotoFlashTypeArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'PhotoFlashType';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_PhotoFlashTypeArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_PhotoFlashTypeArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_PictureFormatArray }

function MerchantItemAttributes_PictureFormatArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_PictureFormatArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_PictureFormatArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_PictureFormatArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('PictureFormat',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_PictureFormatArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'PictureFormat';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_PictureFormatArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_PictureFormatArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_PlatformArray }

function MerchantItemAttributes_PlatformArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_PlatformArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_PlatformArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_PlatformArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Platform',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_PlatformArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Platform';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_PlatformArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_PlatformArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_PurchasingChannelArray }

function MerchantItemAttributes_PurchasingChannelArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_PurchasingChannelArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_PurchasingChannelArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_PurchasingChannelArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('PurchasingChannel',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_PurchasingChannelArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'PurchasingChannel';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_PurchasingChannelArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_PurchasingChannelArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_ReturnMethodArray }

function MerchantItemAttributes_ReturnMethodArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_ReturnMethodArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_ReturnMethodArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_ReturnMethodArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ReturnMethod',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_ReturnMethodArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ReturnMethod';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_ReturnMethodArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_ReturnMethodArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_SpecialFeaturesArray }

function MerchantItemAttributes_SpecialFeaturesArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_SpecialFeaturesArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_SpecialFeaturesArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_SpecialFeaturesArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('SpecialFeatures',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_SpecialFeaturesArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'SpecialFeatures';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_SpecialFeaturesArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_SpecialFeaturesArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ MerchantItemAttributes_SupportedImageTypeArray }

function MerchantItemAttributes_SupportedImageTypeArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure MerchantItemAttributes_SupportedImageTypeArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function MerchantItemAttributes_SupportedImageTypeArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure MerchantItemAttributes_SupportedImageTypeArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('SupportedImageType',TypeInfo(string),FData[AIndex]);
end;

procedure MerchantItemAttributes_SupportedImageTypeArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'SupportedImageType';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function MerchantItemAttributes_SupportedImageTypeArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure MerchantItemAttributes_SupportedImageTypeArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;


procedure Register_AWSECommerceService_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'TRANSPORT_Address',
    'http://soap.amazon.com/onca/soap?Service=AWSECommerceService'
  );
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'FORMAT_Style',
    'document'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'Help',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'Help',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'Help',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'ItemSearch',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'ItemSearch',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'ItemSearch',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'ItemLookup',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'ItemLookup',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'ItemLookup',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'BrowseNodeLookup',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'BrowseNodeLookup',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'BrowseNodeLookup',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'ListSearch',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'ListSearch',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'ListSearch',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'ListLookup',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'ListLookup',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'ListLookup',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CustomerContentSearch',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CustomerContentSearch',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CustomerContentSearch',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CustomerContentLookup',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CustomerContentLookup',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CustomerContentLookup',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'SimilarityLookup',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'SimilarityLookup',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'SimilarityLookup',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'SellerLookup',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'SellerLookup',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'SellerLookup',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartGet',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartGet',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartGet',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartAdd',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartAdd',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartAdd',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartCreate',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartCreate',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartCreate',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartModify',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartModify',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartModify',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartClear',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartClear',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'CartClear',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'TransactionLookup',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'TransactionLookup',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'TransactionLookup',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'SellerListingSearch',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'SellerListingSearch',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'SellerListingSearch',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'SellerListingLookup',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'SellerListingLookup',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'SellerListingLookup',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'MultiOperation',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'MultiOperation',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'MultiOperation',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
end;


initialization
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(HelpRequest_HelpType_Type),'HelpRequest_HelpType_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemSearchRequest_Availability_Type),'ItemSearchRequest_Availability_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(AudienceRating_Type),'AudienceRating');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(AudienceRating_Type)].RegisterExternalPropertyName('AudienceRatingPG_13','PG-13');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(AudienceRating_Type)].RegisterExternalPropertyName('AudienceRatingNC_17','NC-17');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(AudienceRating_Type)].RegisterExternalPropertyName('AudienceRating_6','6');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(AudienceRating_Type)].RegisterExternalPropertyName('AudienceRating_12','12');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(AudienceRating_Type)].RegisterExternalPropertyName('AudienceRating_16','16');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(AudienceRating_Type)].RegisterExternalPropertyName('AudienceRating_18','18');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Condition_Type),'Condition');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(DeliveryMethod_Type),'DeliveryMethod');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemLookupRequest_IdType_Type),'ItemLookupRequest_IdType_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListSearchRequest_ListType_Type),'ListSearchRequest_ListType_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListLookupRequest_ListType_Type),'ListLookupRequest_ListType_Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ListLookupRequest_ListType_Type)].RegisterExternalPropertyName('ListLookupRequest_ListType_TypeWishList','WishList');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ListLookupRequest_ListType_Type)].RegisterExternalPropertyName('ListLookupRequest_ListType_TypeWeddingRegistry','WeddingRegistry');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarityLookupRequest_SimilarityType_Type),'SimilarityLookupRequest_SimilarityType_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModifyRequest_Items_Type_Item_Type_Action_Type),'CartModifyRequest_Items_Type_Item_Type_Action_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingSearchRequest_OfferStatus_Type),'SellerListingSearchRequest_OfferStatus_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingLookupRequest_IdType_Type),'SellerListingLookupRequest_IdType_Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerListingLookupRequest_IdType_Type)].RegisterExternalPropertyName('SellerListingLookupRequest_IdType_TypeASIN','ASIN');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerListingLookupRequest_IdType_Type)].RegisterExternalPropertyName('SellerListingLookupRequest_IdType_TypeSKU','SKU');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(List_ListType_Type),'List_ListType_Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(List_ListType_Type)].RegisterExternalPropertyName('List_ListType_TypeWishList','WishList');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(List_ListType_Type)].RegisterExternalPropertyName('List_ListType_TypeWeddingRegistry','WeddingRegistry');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(List_ListType_Type)].RegisterExternalPropertyName('List_ListType_TypeBabyRegistry','BabyRegistry');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(List_ListType_Type)].RegisterExternalPropertyName('List_ListType_TypeListmania','Listmania');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(HelpRequest),'HelpRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Help_Type),'Help');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OperationRequest_Type),'OperationRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Information_Type),'Information');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(HelpResponse_Type),'HelpResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemSearchRequest),'ItemSearchRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemSearch_Type),'ItemSearch');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Items_Type),'Items');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemSearchResponse_Type),'ItemSearchResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemLookupRequest),'ItemLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemLookup_Type),'ItemLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemLookupResponse_Type),'ItemLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodeLookupRequest),'BrowseNodeLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodeLookup_Type),'BrowseNodeLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodes_Type),'BrowseNodes');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodeLookupResponse_Type),'BrowseNodeLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListSearchRequest),'ListSearchRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListSearch_Type),'ListSearch');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Lists_Type),'Lists');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListSearchResponse_Type),'ListSearchResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListLookupRequest),'ListLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListLookup_Type),'ListLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListLookupResponse_Type),'ListLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentSearchRequest),'CustomerContentSearchRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentSearch_Type),'CustomerContentSearch');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Customers_Type),'Customers');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentSearchResponse_Type),'CustomerContentSearchResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentLookupRequest),'CustomerContentLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentLookup_Type),'CustomerContentLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentLookupResponse_Type),'CustomerContentLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarityLookupRequest),'SimilarityLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarityLookup_Type),'SimilarityLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarityLookupResponse_Type),'SimilarityLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerLookupRequest),'SellerLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerLookup_Type),'SellerLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Sellers_Type),'Sellers');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerLookupResponse_Type),'SellerLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartGetRequest),'CartGetRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartGet_Type),'CartGet');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Cart_Type),'Cart');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartGetResponse_Type),'CartGetResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddRequest),'CartAddRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAdd_Type),'CartAdd');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddResponse_Type),'CartAddResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreateRequest),'CartCreateRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreate_Type),'CartCreate');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreateResponse_Type),'CartCreateResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModifyRequest),'CartModifyRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModify_Type),'CartModify');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModifyResponse_Type),'CartModifyResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartClearRequest),'CartClearRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartClear_Type),'CartClear');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartClearResponse_Type),'CartClearResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionLookupRequest),'TransactionLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionLookup_Type),'TransactionLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transactions_Type),'Transactions');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionLookupResponse_Type),'TransactionLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingSearchRequest),'SellerListingSearchRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingSearch_Type),'SellerListingSearch');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListings_Type),'SellerListings');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingSearchResponse_Type),'SellerListingSearchResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingLookupRequest),'SellerListingLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingLookup_Type),'SellerListingLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingLookupResponse_Type),'SellerListingLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MultiOperationType),'MultiOperation');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MultiOperationResponse),'MultiOperationResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Bin_BinParameter_Type),'Bin_BinParameter_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Bin_Type),'Bin');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchBinSet_Type),'SearchBinSet');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddRequest_Items_Type_Item_Type),'CartAddRequest_Items_Type_Item_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreateRequest_Items_Type_Item_Type),'CartCreateRequest_Items_Type_Item_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModifyRequest_Items_Type_Item_Type),'CartModifyRequest_Items_Type_Item_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Request_Type),'Request');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Arguments_Argument_Type),'Arguments_Argument_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(HTTPHeaders_Header_Type),'HTTPHeaders_Header_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Errors_Error_Type),'Errors_Error_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OperationInformation_Type),'OperationInformation');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ResponseGroupInformation_Type),'ResponseGroupInformation');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CorrectedQuery_Type),'CorrectedQuery');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Item_Type),'Item');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(List_Type),'List');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Customer_Type),'Customer');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Price),'Price');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartItems_Type),'CartItems');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SavedForLaterItems_Type),'SavedForLaterItems');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transaction_Type),'Transaction');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Seller_Type),'Seller');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListing_Type),'SellerListing');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Image),'Image');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListItem_Type),'ListItem');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Customer_Location_Type),'Customer_Location_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerReviews_Type),'CustomerReviews');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchResultsMap_SearchIndex_Type),'SearchResultsMap_SearchIndex_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ImageSet_Type),'ImageSet');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Item_ImageSets_Type),'Item_ImageSets_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_Type),'ItemAttributes');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_Type),'MerchantItemAttributes');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OfferSummary_Type),'OfferSummary');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Offers_Type),'Offers');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(VariationSummary_Type),'VariationSummary');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Variations_Type),'Variations');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchInside_Type),'SearchInside');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Item_AlternateVersions_Type_AlternateVersion_Type),'Item_AlternateVersions_Type_AlternateVersion_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Offer_Type),'Offer');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Merchant_Type),'Merchant');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OfferAttributes_Type),'OfferAttributes');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OfferListing_Type),'OfferListing');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(LoyaltyPoints_Type),'LoyaltyPoints');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OfferListing_AvailabilityAttributes_Type),'OfferListing_AvailabilityAttributes_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Address),'Address');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OfferListing_ShippingCharge_Type),'OfferListing_ShippingCharge_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(EditorialReview_Type),'EditorialReview');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Collections_Collection_Type_CollectionSummary_Type),'Collections_Collection_Type_CollectionSummary_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Collections_Collection_Type_CollectionParent_Type),'Collections_Collection_Type_CollectionParent_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Collections_Collection_Type_CollectionItem_Type),'Collections_Collection_Type_CollectionItem_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Collections_Collection_Type),'Collections_Collection_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Review_Type),'Review');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Reviewer_Type),'Reviewer');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tracks_Disc_Type_Track_Type),'Tracks_Disc_Type_Track_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tracks_Disc_Type),'Tracks_Disc_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarProducts_SimilarProduct_Type),'SimilarProducts_SimilarProduct_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TopSellers_TopSeller_Type),'TopSellers_TopSeller_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(NewReleases_NewRelease_Type),'NewReleases_NewRelease_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarViewedProducts_SimilarViewedProduct_Type),'SimilarViewedProducts_SimilarViewedProduct_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type),'OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Accessories_Accessory_Type),'Accessories_Accessory_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Promotion_Type),'Promotion');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Promotion_Summary_Type),'Promotion_Summary_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(PromotionItemApplicability),'PromotionItemApplicability');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Promotion_Details_Type),'Promotion_Details_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(PromotionEligibilityRequirement),'PromotionEligibilityRequirement');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(PromotionBenefit),'PromotionBenefit');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNode_Type),'BrowseNode');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListmaniaLists_ListmaniaList_Type),'ListmaniaLists_ListmaniaList_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchInside_Excerpt_Type),'SearchInside_Excerpt_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartItem),'CartItem');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transaction_Totals_Type),'Transaction_Totals_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionItem_Type),'TransactionItem');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transaction_Shipments_Type_Shipment_Type_Packages_Type_Package_Type),'Transaction_Shipments_Type_Shipment_Type_Packages_Type_Package_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transaction_Shipments_Type_Shipment_Type),'Transaction_Shipments_Type_Shipment_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Seller_Location_Type),'Seller_Location_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type),'Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type),'Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerFeedback_Feedback_Type),'SellerFeedback_Feedback_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(DecimalWithUnits),'DecimalWithUnits');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(NonNegativeIntegerWithUnits),'NonNegativeIntegerWithUnits');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_Creator_Type),'ItemAttributes_Creator_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(StringWithUnits),'StringWithUnits');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_ItemDimensions_Type),'ItemAttributes_ItemDimensions_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_Languages_Type_Language_Type),'ItemAttributes_Languages_Type_Language_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_PackageDimensions_Type),'ItemAttributes_PackageDimensions_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_Creator_Type),'MerchantItemAttributes_Creator_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_ItemDimensions_Type),'MerchantItemAttributes_ItemDimensions_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_Languages_Type_Language_Type),'MerchantItemAttributes_Languages_Type_Language_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_PackageDimensions_Type),'MerchantItemAttributes_PackageDimensions_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_VendorRebate_Type),'MerchantItemAttributes_VendorRebate_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Help_RequestArray),'Help_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Help_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(HelpResponse_InformationArray),'HelpResponse_InformationArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(HelpResponse_InformationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemSearch_RequestArray),'ItemSearch_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemSearch_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemSearchResponse_ItemsArray),'ItemSearchResponse_ItemsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemSearchResponse_ItemsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemLookup_RequestArray),'ItemLookup_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemLookup_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemLookupResponse_ItemsArray),'ItemLookupResponse_ItemsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemLookupResponse_ItemsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodeLookup_RequestArray),'BrowseNodeLookup_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(BrowseNodeLookup_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodeLookupResponse_BrowseNodesArray),'BrowseNodeLookupResponse_BrowseNodesArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(BrowseNodeLookupResponse_BrowseNodesArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListSearch_RequestArray),'ListSearch_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ListSearch_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListSearchResponse_ListsArray),'ListSearchResponse_ListsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ListSearchResponse_ListsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListLookup_RequestArray),'ListLookup_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ListLookup_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListLookupResponse_ListsArray),'ListLookupResponse_ListsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ListLookupResponse_ListsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentSearch_RequestArray),'CustomerContentSearch_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CustomerContentSearch_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentSearchResponse_CustomersArray),'CustomerContentSearchResponse_CustomersArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CustomerContentSearchResponse_CustomersArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentLookup_RequestArray),'CustomerContentLookup_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CustomerContentLookup_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentLookupResponse_CustomersArray),'CustomerContentLookupResponse_CustomersArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CustomerContentLookupResponse_CustomersArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarityLookup_RequestArray),'SimilarityLookup_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SimilarityLookup_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarityLookupResponse_ItemsArray),'SimilarityLookupResponse_ItemsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SimilarityLookupResponse_ItemsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerLookup_RequestArray),'SellerLookup_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerLookup_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerLookupResponse_SellersArray),'SellerLookupResponse_SellersArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerLookupResponse_SellersArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartGet_RequestArray),'CartGet_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartGet_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartGetResponse_CartArray),'CartGetResponse_CartArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartGetResponse_CartArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAdd_RequestArray),'CartAdd_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartAdd_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddResponse_CartArray),'CartAddResponse_CartArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartAddResponse_CartArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreate_RequestArray),'CartCreate_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartCreate_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreateResponse_CartArray),'CartCreateResponse_CartArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartCreateResponse_CartArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModify_RequestArray),'CartModify_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartModify_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModifyResponse_CartArray),'CartModifyResponse_CartArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartModifyResponse_CartArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartClear_RequestArray),'CartClear_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartClear_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartClearResponse_CartArray),'CartClearResponse_CartArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartClearResponse_CartArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionLookup_RequestArray),'TransactionLookup_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TransactionLookup_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionLookupResponse_TransactionsArray),'TransactionLookupResponse_TransactionsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TransactionLookupResponse_TransactionsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingSearch_RequestArray),'SellerListingSearch_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerListingSearch_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingSearchResponse_SellerListingsArray),'SellerListingSearchResponse_SellerListingsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerListingSearchResponse_SellerListingsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingLookup_RequestArray),'SellerListingLookup_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerListingLookup_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingLookupResponse_SellerListingsArray),'SellerListingLookupResponse_SellerListingsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerListingLookupResponse_SellerListingsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Bin_BinParameterArray),'Bin_BinParameterArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Bin_BinParameterArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchBinSet_BinArray),'SearchBinSet_BinArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SearchBinSet_BinArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchBinSets_Type),'SearchBinSets');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(HelpRequest_ResponseGroupArray),'HelpRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(HelpRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemSearchRequest_AudienceRatingArray),'ItemSearchRequest_AudienceRatingArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemSearchRequest_AudienceRatingArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemSearchRequest_ResponseGroupArray),'ItemSearchRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemSearchRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemLookupRequest_ItemIdArray),'ItemLookupRequest_ItemIdArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemLookupRequest_ItemIdArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemLookupRequest_ResponseGroupArray),'ItemLookupRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemLookupRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListSearchRequest_ResponseGroupArray),'ListSearchRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ListSearchRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListLookupRequest_ResponseGroupArray),'ListLookupRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ListLookupRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentSearchRequest_ResponseGroupArray),'CustomerContentSearchRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CustomerContentSearchRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentLookupRequest_ResponseGroupArray),'CustomerContentLookupRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CustomerContentLookupRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarityLookupRequest_ItemIdArray),'SimilarityLookupRequest_ItemIdArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SimilarityLookupRequest_ItemIdArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarityLookupRequest_ResponseGroupArray),'SimilarityLookupRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SimilarityLookupRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerLookupRequest_ResponseGroupArray),'SellerLookupRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerLookupRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerLookupRequest_SellerIdArray),'SellerLookupRequest_SellerIdArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerLookupRequest_SellerIdArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartGetRequest_ResponseGroupArray),'CartGetRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartGetRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddRequest_Items_Type),'CartAddRequest_Items_Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartAddRequest_Items_Type)].RegisterExternalPropertyName(sARRAY_ITEM,'Item');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddRequest_ResponseGroupArray),'CartAddRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartAddRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreateRequest_Items_Type),'CartCreateRequest_Items_Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartCreateRequest_Items_Type)].RegisterExternalPropertyName(sARRAY_ITEM,'Item');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreateRequest_ResponseGroupArray),'CartCreateRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartCreateRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModifyRequest_Items_Type),'CartModifyRequest_Items_Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartModifyRequest_Items_Type)].RegisterExternalPropertyName(sARRAY_ITEM,'Item');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModifyRequest_ResponseGroupArray),'CartModifyRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartModifyRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartClearRequest_ResponseGroupArray),'CartClearRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartClearRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionLookupRequest_ResponseGroupArray),'TransactionLookupRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TransactionLookupRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionLookupRequest_TransactionIdArray),'TransactionLookupRequest_TransactionIdArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TransactionLookupRequest_TransactionIdArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingSearchRequest_ResponseGroupArray),'SellerListingSearchRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerListingSearchRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingLookupRequest_ResponseGroupArray),'SellerListingLookupRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerListingLookupRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodeLookupRequest_BrowseNodeIdArray),'BrowseNodeLookupRequest_BrowseNodeIdArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(BrowseNodeLookupRequest_BrowseNodeIdArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodeLookupRequest_ResponseGroupArray),'BrowseNodeLookupRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(BrowseNodeLookupRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(HTTPHeaders_Type),'HTTPHeaders');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Arguments_Type),'Arguments');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Errors_Type),'Errors');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Information_OperationInformationArray),'Information_OperationInformationArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Information_OperationInformationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Information_ResponseGroupInformationArray),'Information_ResponseGroupInformationArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Information_ResponseGroupInformationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchResultsMap_Type),'SearchResultsMap');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Items__ItemArray),'Items__ItemArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Items__ItemArray)].RegisterExternalPropertyName(sARRAY_ITEM,'Item');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Items__ItemArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Lists_ListArray),'Lists_ListArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Lists_ListArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Customers_CustomerArray),'Customers_CustomerArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Customers_CustomerArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarProducts_Type),'SimilarProducts');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TopSellers_Type),'TopSellers');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(NewReleases_Type),'NewReleases');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarViewedProducts_Type),'SimilarViewedProducts');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OtherCategoriesSimilarProducts_Type),'OtherCategoriesSimilarProducts');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transactions_TransactionArray),'Transactions_TransactionArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Transactions_TransactionArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Sellers_SellerArray),'Sellers_SellerArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Sellers_SellerArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListings_SellerListingArray),'SellerListings_SellerListingArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerListings_SellerListingArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OperationInformation_RequiredParameters_Type),'OperationInformation_RequiredParameters_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OperationInformation_AvailableParameters_Type),'OperationInformation_AvailableParameters_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OperationInformation_DefaultResponseGroups_Type),'OperationInformation_DefaultResponseGroups_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OperationInformation_AvailableResponseGroups_Type),'OperationInformation_AvailableResponseGroups_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ResponseGroupInformation_ValidOperations_Type),'ResponseGroupInformation_ValidOperations_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ResponseGroupInformation_Elements_Type),'ResponseGroupInformation_Elements_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(List_ListItemArray),'List_ListItemArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(List_ListItemArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Customer_CustomerReviewsArray),'Customer_CustomerReviewsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Customer_CustomerReviewsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchResultsMap_SearchIndex_Type_ASINArray),'SearchResultsMap_SearchIndex_Type_ASINArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SearchResultsMap_SearchIndex_Type_ASINArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Item_ImageSets_Type_ImageSetArray),'Item_ImageSets_Type_ImageSetArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Item_ImageSets_Type_ImageSetArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Collections_Type),'Collections');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Item_Subjects_Type),'Item_Subjects_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(EditorialReviews_Type),'EditorialReviews');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Accessories_Type),'Accessories');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tracks_Type),'Tracks');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListmaniaLists_Type),'ListmaniaLists');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Item_AlternateVersions_Type),'Item_AlternateVersions_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(_Item_ImageSetsArray),'_Item_ImageSetsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(_Item_ImageSetsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Offers_OfferArray),'Offers_OfferArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Offers_OfferArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Promotions_Type),'Promotions');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Offer_OfferListingArray),'Offer_OfferListingArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Offer_OfferListingArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OfferListing_ShippingChargeArray),'OfferListing_ShippingChargeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(OfferListing_ShippingChargeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(VariationDimensions_Type),'VariationDimensions');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Variations__ItemArray),'Variations__ItemArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Variations__ItemArray)].RegisterExternalPropertyName(sARRAY_ITEM,'Item');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Variations__ItemArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Collections_Collection_Type_CollectionItemArray),'Collections_Collection_Type_CollectionItemArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Collections_Collection_Type_CollectionItemArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerReviews_ReviewArray),'CustomerReviews_ReviewArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CustomerReviews_ReviewArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tracks_Disc_Type_TrackArray),'Tracks_Disc_Type_TrackArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Tracks_Disc_Type_TrackArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(PromotionEligibilityRequirements),'PromotionEligibilityRequirements');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(PromotionBenefits),'PromotionBenefits');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodes_BrowseNodeArray),'BrowseNodes_BrowseNodeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(BrowseNodes_BrowseNodeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNode_Children_Type),'BrowseNode_Children_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNode_Ancestors_Type),'BrowseNode_Ancestors_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartItems_CartItemArray),'CartItems_CartItemArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartItems_CartItemArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SavedForLaterItems_SavedForLaterItemArray),'SavedForLaterItems_SavedForLaterItemArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SavedForLaterItems_SavedForLaterItemArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transaction_TransactionItems_Type),'Transaction_TransactionItems_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type),'Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transaction_Shipments_Type_Shipment_Type_Packages_Type),'Transaction_Shipments_Type_Shipment_Type_Packages_Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Transaction_Shipments_Type_Shipment_Type_Packages_Type)].RegisterExternalPropertyName(sARRAY_ITEM,'Package');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transaction_Shipments_Type),'Transaction_Shipments_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionItem_ChildTransactionItems_Type),'TransactionItem_ChildTransactionItems_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray),'Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Seller_SellerFeedbackSummary_Type),'Seller_SellerFeedbackSummary_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerFeedback_Type),'SellerFeedback');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_Languages_Type),'ItemAttributes_Languages_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_ActorArray),'ItemAttributes_ActorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_ActorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_ArtistArray),'ItemAttributes_ArtistArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_ArtistArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_AudioFormatArray),'ItemAttributes_AudioFormatArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_AudioFormatArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_AuthorArray),'ItemAttributes_AuthorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_AuthorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_CameraManualFeaturesArray),'ItemAttributes_CameraManualFeaturesArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_CameraManualFeaturesArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_CompatibleDevicesArray),'ItemAttributes_CompatibleDevicesArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_CompatibleDevicesArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_CreatorArray),'ItemAttributes_CreatorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_CreatorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_DataLinkProtocolArray),'ItemAttributes_DataLinkProtocolArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_DataLinkProtocolArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_DirectorArray),'ItemAttributes_DirectorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_DirectorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_FeatureArray),'ItemAttributes_FeatureArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_FeatureArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_FormatArray),'ItemAttributes_FormatArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_FormatArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_FormFactorArray),'ItemAttributes_FormFactorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_FormFactorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_PhotoFlashTypeArray),'ItemAttributes_PhotoFlashTypeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_PhotoFlashTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_PictureFormatArray),'ItemAttributes_PictureFormatArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_PictureFormatArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_PlatformArray),'ItemAttributes_PlatformArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_PlatformArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_ReturnMethodArray),'ItemAttributes_ReturnMethodArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_ReturnMethodArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_SpecialFeaturesArray),'ItemAttributes_SpecialFeaturesArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_SpecialFeaturesArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_SupportedImageTypeArray),'ItemAttributes_SupportedImageTypeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_SupportedImageTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_Languages_Type),'MerchantItemAttributes_Languages_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_ActorArray),'MerchantItemAttributes_ActorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_ActorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_ArtistArray),'MerchantItemAttributes_ArtistArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_ArtistArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_AudioFormatArray),'MerchantItemAttributes_AudioFormatArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_AudioFormatArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_AuthorArray),'MerchantItemAttributes_AuthorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_AuthorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_CameraManualFeaturesArray),'MerchantItemAttributes_CameraManualFeaturesArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_CameraManualFeaturesArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_CreatorArray),'MerchantItemAttributes_CreatorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_CreatorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_DirectorArray),'MerchantItemAttributes_DirectorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_DirectorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_FeatureArray),'MerchantItemAttributes_FeatureArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_FeatureArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_FormatArray),'MerchantItemAttributes_FormatArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_FormatArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_PhotoFlashTypeArray),'MerchantItemAttributes_PhotoFlashTypeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_PhotoFlashTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_PictureFormatArray),'MerchantItemAttributes_PictureFormatArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_PictureFormatArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_PlatformArray),'MerchantItemAttributes_PlatformArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_PlatformArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_PurchasingChannelArray),'MerchantItemAttributes_PurchasingChannelArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_PurchasingChannelArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_ReturnMethodArray),'MerchantItemAttributes_ReturnMethodArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_ReturnMethodArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_SpecialFeaturesArray),'MerchantItemAttributes_SpecialFeaturesArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_SpecialFeaturesArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_SupportedImageTypeArray),'MerchantItemAttributes_SupportedImageTypeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_SupportedImageTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);

  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Items_Type)].RegisterExternalPropertyName('_Item','Item');
  SearchBinSet_Type.RegisterAttributeProperty('NarrowBy');
  Arguments_Argument_Type.RegisterAttributeProperty('Name');
  Arguments_Argument_Type.RegisterAttributeProperty('Value');
  HTTPHeaders_Header_Type.RegisterAttributeProperty('Name');
  HTTPHeaders_Header_Type.RegisterAttributeProperty('Value');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ListItem_Type)].RegisterExternalPropertyName('_Item','Item');
  ImageSet_Type.RegisterAttributeProperty('Category');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_Type)].RegisterExternalPropertyName('_Label','Label');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_Type)].RegisterExternalPropertyName('_Label','Label');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Variations_Type)].RegisterExternalPropertyName('_Item','Item');
  Tracks_Disc_Type_Track_Type.RegisterAttributeProperty('Number');
  Tracks_Disc_Type.RegisterAttributeProperty('Number');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type)].RegisterExternalPropertyName('_Type','Type');
  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type.RegisterAttributeProperty('_Type');
  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type.RegisterAttributeProperty('Period');
  DecimalWithUnits.RegisterAttributeProperty('Units');
  NonNegativeIntegerWithUnits.RegisterAttributeProperty('Units');
  ItemAttributes_Creator_Type.RegisterAttributeProperty('Role');
  StringWithUnits.RegisterAttributeProperty('Units');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_Languages_Type_Language_Type)].RegisterExternalPropertyName('_Type','Type');
  MerchantItemAttributes_Creator_Type.RegisterAttributeProperty('Role');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_Languages_Type_Language_Type)].RegisterExternalPropertyName('_Type','Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_VendorRebate_Type)].RegisterExternalPropertyName('_Type','Type');

End.
