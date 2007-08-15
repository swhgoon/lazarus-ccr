{
This unit has been produced by ws_helper.
  Input unit name : "AWSECommerceService".
  This unit name  : "AWSECommerceService".
  Date            : "12/08/2007 22:38:54".
}
unit AWSECommerceService;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'http://webservices.amazon.com/AWSECommerceService/2007-07-16';
  sUNIT_NAME = 'AWSECommerceService';

type

  Help_RequestArray = class;
  Help_Type = class;
  HelpResponse_InformationArray = class;
  HelpResponse_Type = class;
  ItemSearch_RequestArray = class;
  ItemSearch_Type = class;
  ItemSearchResponse_ItemsArray = class;
  ItemSearchResponse_Type = class;
  ItemLookup_RequestArray = class;
  ItemLookup_Type = class;
  ItemLookupResponse_ItemsArray = class;
  ItemLookupResponse_Type = class;
  BrowseNodeLookup_RequestArray = class;
  BrowseNodeLookup_Type = class;
  BrowseNodeLookupResponse_BrowseNodesArray = class;
  BrowseNodeLookupResponse_Type = class;
  ListSearch_RequestArray = class;
  ListSearch_Type = class;
  ListSearchResponse_ListsArray = class;
  ListSearchResponse_Type = class;
  ListLookup_RequestArray = class;
  ListLookup_Type = class;
  ListLookupResponse_ListsArray = class;
  ListLookupResponse_Type = class;
  CustomerContentSearch_RequestArray = class;
  CustomerContentSearch_Type = class;
  CustomerContentSearchResponse_CustomersArray = class;
  CustomerContentSearchResponse_Type = class;
  CustomerContentLookup_RequestArray = class;
  CustomerContentLookup_Type = class;
  CustomerContentLookupResponse_CustomersArray = class;
  CustomerContentLookupResponse_Type = class;
  SimilarityLookup_RequestArray = class;
  SimilarityLookup_Type = class;
  SimilarityLookupResponse_ItemsArray = class;
  SimilarityLookupResponse_Type = class;
  SellerLookup_RequestArray = class;
  SellerLookup_Type = class;
  SellerLookupResponse_SellersArray = class;
  SellerLookupResponse_Type = class;
  CartGet_RequestArray = class;
  CartGet_Type = class;
  CartGetResponse_CartArray = class;
  CartGetResponse_Type = class;
  CartAdd_RequestArray = class;
  CartAdd_Type = class;
  CartAddResponse_CartArray = class;
  CartAddResponse_Type = class;
  CartCreate_RequestArray = class;
  CartCreate_Type = class;
  CartCreateResponse_CartArray = class;
  CartCreateResponse_Type = class;
  CartModify_RequestArray = class;
  CartModify_Type = class;
  CartModifyResponse_CartArray = class;
  CartModifyResponse_Type = class;
  CartClear_RequestArray = class;
  CartClear_Type = class;
  CartClearResponse_CartArray = class;
  CartClearResponse_Type = class;
  TransactionLookup_RequestArray = class;
  TransactionLookup_Type = class;
  TransactionLookupResponse_TransactionsArray = class;
  TransactionLookupResponse_Type = class;
  SellerListingSearch_RequestArray = class;
  SellerListingSearch_Type = class;
  SellerListingSearchResponse_SellerListingsArray = class;
  SellerListingSearchResponse_Type = class;
  SellerListingLookup_RequestArray = class;
  SellerListingLookup_Type = class;
  SellerListingLookupResponse_SellerListingsArray = class;
  SellerListingLookupResponse_Type = class;
  TagLookup_RequestArray = class;
  TagLookup_Type = class;
  TagLookupResponse_TagsArray = class;
  TagLookupResponse_Type = class;
  MultiOperation_Type = class;
  MultiOperationResponse = class;
  Bin_BinParameter_Type = class;
  Bin_BinParameterArray = class;
  Bin_Type = class;
  SearchBinSet_BinArray = class;
  SearchBinSet_Type = class;
  SearchBinSets_Type = class;
  HelpRequest_ResponseGroupArray = class;
  HelpRequest_Type = class;
  ItemSearchRequest_AudienceRatingArray = class;
  ItemSearchRequest_ResponseGroupArray = class;
  ItemSearchRequest_Type = class;
  ItemLookupRequest_ItemIdArray = class;
  ItemLookupRequest_ResponseGroupArray = class;
  ItemLookupRequest_Type = class;
  ListSearchRequest_ResponseGroupArray = class;
  ListSearchRequest_Type = class;
  ListLookupRequest_ResponseGroupArray = class;
  ListLookupRequest_Type = class;
  CustomerContentSearchRequest_ResponseGroupArray = class;
  CustomerContentSearchRequest_Type = class;
  CustomerContentLookupRequest_ResponseGroupArray = class;
  CustomerContentLookupRequest_Type = class;
  SimilarityLookupRequest_ItemIdArray = class;
  SimilarityLookupRequest_ResponseGroupArray = class;
  SimilarityLookupRequest_Type = class;
  SellerLookupRequest_ResponseGroupArray = class;
  SellerLookupRequest_SellerIdArray = class;
  SellerLookupRequest_Type = class;
  CartGetRequest_ResponseGroupArray = class;
  CartGetRequest_Type = class;
  CartAddRequest_Items_Type_Item_Type_MetaData_Type = class;
  CartAddRequest_Items_Type_Item_Type_MetaDataArray = class;
  CartAddRequest_Items_Type_Item_Type = class;
  CartAddRequest_Items_Type = class;
  CartAddRequest_ResponseGroupArray = class;
  CartAddRequest_Type = class;
  CartCreateRequest_Items_Type_Item_Type_MetaData_Type = class;
  CartCreateRequest_Items_Type_Item_Type_MetaDataArray = class;
  CartCreateRequest_Items_Type_Item_Type = class;
  CartCreateRequest_Items_Type = class;
  CartCreateRequest_ResponseGroupArray = class;
  CartCreateRequest_Type = class;
  CartModifyRequest_Items_Type_Item_Type = class;
  CartModifyRequest_Items_Type = class;
  CartModifyRequest_ResponseGroupArray = class;
  CartModifyRequest_Type = class;
  CartClearRequest_ResponseGroupArray = class;
  CartClearRequest_Type = class;
  TransactionLookupRequest_ResponseGroupArray = class;
  TransactionLookupRequest_TransactionIdArray = class;
  TransactionLookupRequest_Type = class;
  SellerListingSearchRequest_ResponseGroupArray = class;
  SellerListingSearchRequest_Type = class;
  SellerListingLookupRequest_ResponseGroupArray = class;
  SellerListingLookupRequest_Type = class;
  TagLookupRequest_TagNameArray = class;
  TagLookupRequest_ResponseGroupArray = class;
  TagLookupRequest_Type = class;
  BrowseNodeLookupRequest_BrowseNodeIdArray = class;
  BrowseNodeLookupRequest_ResponseGroupArray = class;
  BrowseNodeLookupRequest_Type = class;
  OperationRequest_Type = class;
  Request_Type = class;
  Arguments_Argument_Type = class;
  Arguments_Type = class;
  HTTPHeaders_Header_Type = class;
  HTTPHeaders_Type = class;
  Errors_Error_Type = class;
  Errors_Type = class;
  Information_OperationInformationArray = class;
  Information_ResponseGroupInformationArray = class;
  Information_Type = class;
  Items__ItemArray = class;
  Items_Type = class;
  CorrectedQuery_Type = class;
  Lists_ListArray = class;
  Lists_Type = class;
  Customers_CustomerArray = class;
  Customers_Type = class;
  Cart_Type = class;
  Transactions_TransactionArray = class;
  Transactions_Type = class;
  Sellers_SellerArray = class;
  Sellers_Type = class;
  SellerListings_SellerListingArray = class;
  SellerListings_Type = class;
  OperationInformation_RequiredParameters_Type = class;
  OperationInformation_AvailableParameters_Type = class;
  OperationInformation_DefaultResponseGroups_Type = class;
  OperationInformation_AvailableResponseGroups_Type = class;
  OperationInformation_Type = class;
  ResponseGroupInformation_ValidOperations_Type = class;
  ResponseGroupInformation_Elements_Type = class;
  ResponseGroupInformation_Type = class;
  List_ListItemArray = class;
  List_Type = class;
  ListItem_Type = class;
  Customer_Location_Type = class;
  Customer_CustomerReviewsArray = class;
  Customer_Type = class;
  SearchResultsMap_SearchIndex_Type_ASINArray = class;
  SearchResultsMap_SearchIndex_Type = class;
  SearchResultsMap_Type = class;
  Item_ImageSets_Type_ImageSetArray = class;
  Item_ImageSets_Type = class;
  Item_Subjects_Type = class;
  Item_AlternateVersions_Type_AlternateVersion_Type = class;
  Item_AlternateVersions_Type = class;
  _Item_ImageSetsArray = class;
  Item_Type = class;
  Tags_TagArray = class;
  Tags_Type = class;
  Tag_TaggedItemsArray = class;
  Tag_TaggedListmaniaListsArray = class;
  Tag_TaggedGuidesArray = class;
  Tag_Type = class;
  TaggedItems_Type = class;
  TaggedListmaniaLists_Type = class;
  TaggedGuides_Type = class;
  Guide_Type = class;
  Tagging_Type = class;
  OfferSummary_Type = class;
  Offers_OfferArray = class;
  Offers_Type = class;
  Offer_OfferListingArray = class;
  Offer_Type = class;
  OfferAttributes_Type = class;
  Merchant_Type = class;
  OfferListing_AvailabilityAttributes_Type = class;
  OfferListing_ShippingCharge_Type = class;
  OfferListing_ShippingChargeArray = class;
  OfferListing_Type = class;
  LoyaltyPoints_Type = class;
  VariationSummary_Type = class;
  Variations__ItemArray = class;
  Variations_Type = class;
  VariationDimensions_Type = class;
  EditorialReviews_Type = class;
  Collections_Collection_Type_CollectionSummary_Type = class;
  Collections_Collection_Type_CollectionParent_Type = class;
  Collections_Collection_Type_CollectionItem_Type = class;
  Collections_Collection_Type_CollectionItemArray = class;
  Collections_Collection_Type = class;
  Collections_Type = class;
  EditorialReview_Type = class;
  CustomerReviews_ReviewArray = class;
  CustomerReviews_Type = class;
  Review_Type = class;
  Reviewer_Type = class;
  Tracks_Disc_Type_Track_Type = class;
  Tracks_Disc_Type_TrackArray = class;
  Tracks_Disc_Type = class;
  Tracks_Type = class;
  SimilarProducts_SimilarProduct_Type = class;
  SimilarProducts_Type = class;
  TopSellers_TopSeller_Type = class;
  TopSellers_Type = class;
  NewReleases_NewRelease_Type = class;
  NewReleases_Type = class;
  SimilarViewedProducts_SimilarViewedProduct_Type = class;
  SimilarViewedProducts_Type = class;
  OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type = class;
  OtherCategoriesSimilarProducts_Type = class;
  Accessories_Accessory_Type = class;
  Accessories_Type = class;
  Promotions_Type = class;
  Promotion_Summary_Type = class;
  Promotion_Details_Type = class;
  Promotion_Type = class;
  PromotionEligibilityRequirements_Type = class;
  PromotionBenefits_Type = class;
  PromotionBenefit_Type = class;
  PromotionEligibilityRequirement_Type = class;
  PromotionItemApplicability_Type = class;
  BrowseNodes_BrowseNodeArray = class;
  BrowseNodes_Type = class;
  Property_Type = class;
  BrowseNode_Properties_Type = class;
  BrowseNode_Children_Type = class;
  BrowseNode_Ancestors_Type = class;
  BrowseNode_Type = class;
  ListmaniaLists_ListmaniaList_Type = class;
  ListmaniaLists_Type = class;
  SearchInside_Excerpt_Type = class;
  SearchInside_Type = class;
  CartItems_CartItemArray = class;
  CartItems_Type = class;
  SavedForLaterItems_SavedForLaterItemArray = class;
  SavedForLaterItems_Type = class;
  CartItem_MetaData_Type_KeyValuePair_Type = class;
  CartItem_MetaData_Type = class;
  CartItem_Type = class;
  Transaction_Totals_Type = class;
  Transaction_TransactionItems_Type = class;
  Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type = class;
  Transaction_Shipments_Type_Shipment_Type_Packages_Type_Package_Type = class;
  Transaction_Shipments_Type_Shipment_Type_Packages_Type = class;
  Transaction_Shipments_Type_Shipment_Type = class;
  Transaction_Shipments_Type = class;
  Transaction_Type = class;
  TransactionItem_ChildTransactionItems_Type = class;
  TransactionItem_Type = class;
  Seller_Location_Type = class;
  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type = class;
  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray = class;
  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type = class;
  Seller_SellerFeedbackSummary_Type = class;
  Seller_Type = class;
  SellerFeedback_Feedback_Type = class;
  SellerFeedback_Type = class;
  Address_Type = class;
  SellerListing_Type = class;
  Price_Type = class;
  ImageSet_Type = class;
  Image_Type = class;
  ItemAttributes_Creator_Type = class;
  ItemAttributes_ItemDimensions_Type = class;
  ItemAttributes_Languages_Type_Language_Type = class;
  ItemAttributes_Languages_Type = class;
  ItemAttributes_PackageDimensions_Type = class;
  ItemAttributes_ActorArray = class;
  ItemAttributes_AgeArray = class;
  ItemAttributes_ArtistArray = class;
  ItemAttributes_AudioFormatArray = class;
  ItemAttributes_AuthorArray = class;
  ItemAttributes_CameraManualFeaturesArray = class;
  ItemAttributes_CategoryArray = class;
  ItemAttributes_CategoryBinArray = class;
  ItemAttributes_CharacterArray = class;
  ItemAttributes_CompatibleDevicesArray = class;
  ItemAttributes_CreatorArray = class;
  ItemAttributes_DataLinkProtocolArray = class;
  ItemAttributes_DirectorArray = class;
  ItemAttributes_EducationalFocusArray = class;
  ItemAttributes_EthnicityArray = class;
  ItemAttributes_FeatureArray = class;
  ItemAttributes_FormatArray = class;
  ItemAttributes_FormFactorArray = class;
  ItemAttributes_GemTypeSetElementArray = class;
  ItemAttributes_GenderArray = class;
  ItemAttributes_IngredientsSetElementArray = class;
  ItemAttributes_InterestArray = class;
  ItemAttributes_LanguageNameArray = class;
  ItemAttributes_MaterialTypeSetElementArray = class;
  ItemAttributes_PantLengthArray = class;
  ItemAttributes_PantSizeArray = class;
  ItemAttributes_PhotoFlashTypeArray = class;
  ItemAttributes_PictureFormatArray = class;
  ItemAttributes_PlatformArray = class;
  ItemAttributes_PrimaryColorArray = class;
  ItemAttributes_ReturnMethodArray = class;
  ItemAttributes_ShoeSizeArray = class;
  ItemAttributes_SpecialFeaturesArray = class;
  ItemAttributes_SupportedImageTypeArray = class;
  ItemAttributes_TargetBrandArray = class;
  ItemAttributes_Type = class;
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
  MerchantItemAttributes_Type = class;
  NonNegativeIntegerWithUnits_Type = class;
  DecimalWithUnits_Type = class;
  StringWithUnits_Type = class;

  HelpRequest_HelpType_Type = ( 
    Operation
    ,ResponseGroup
  );

  ItemSearchRequest_Availability_Type = ( 
    Available
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
    ListLookupRequest_ListType_Type_WishList
    ,Listmania
    ,ListLookupRequest_ListType_Type_WeddingRegistry
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
    ,SellerListingLookupRequest_IdType_Type_ASIN
    ,SellerListingLookupRequest_IdType_Type_SKU
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

  AudienceRating_Type = ( 
    G
    ,PG
    ,AudienceRating_PG_13
    ,R
    ,AudienceRating_NC_17
    ,NR
    ,Unrated
    ,AudienceRating__6
    ,AudienceRating__12
    ,AudienceRating__16
    ,AudienceRating__18
    ,FamilyViewing
  );

  List_ListType_Type = ( 
    List_ListType_Type_WishList
    ,List_ListType_Type_WeddingRegistry
    ,List_ListType_Type_BabyRegistry
    ,List_ListType_Type_Listmania
  );

  Tag_TagType_Type = ( 
    Tag_TagType_Type_Item
    ,ListmaniaList
    ,Guide
  );

  positiveIntegerOrAll_Type = type string;

  Help_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FShared : HelpRequest_Type;
    FRequest : Help_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MarketplaceDomain : string read FMarketplaceDomain write FMarketplaceDomain stored HasMarketplaceDomain;
    property AWSAccessKeyId : string read FAWSAccessKeyId write FAWSAccessKeyId stored HasAWSAccessKeyId;
    property SubscriptionId : string read FSubscriptionId write FSubscriptionId stored HasSubscriptionId;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property Validate : string read FValidate write FValidate stored HasValidate;
    property Shared : HelpRequest_Type read FShared write FShared stored HasShared;
    property Request : Help_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  HelpResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FInformation : HelpResponse_InformationArray;
  private
    function HasOperationRequest() : Boolean;
    function HasInformation() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Information : HelpResponse_InformationArray read FInformation write FInformation stored HasInformation;
  end;

  ItemSearch_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FXMLEscaping : string;
    FValidate : string;
    FShared : ItemSearchRequest_Type;
    FRequest : ItemSearch_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasValidate() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : ItemSearchRequest_Type read FShared write FShared stored HasShared;
    property Request : ItemSearch_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  ItemSearchResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FItems : ItemSearchResponse_ItemsArray;
  private
    function HasOperationRequest() : Boolean;
    function HasItems() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Items : ItemSearchResponse_ItemsArray read FItems write FItems stored HasItems;
  end;

  ItemLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : ItemLookupRequest_Type;
    FRequest : ItemLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : ItemLookupRequest_Type read FShared write FShared stored HasShared;
    property Request : ItemLookup_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  ItemLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FItems : ItemLookupResponse_ItemsArray;
  private
    function HasOperationRequest() : Boolean;
    function HasItems() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Items : ItemLookupResponse_ItemsArray read FItems write FItems stored HasItems;
  end;

  BrowseNodeLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : BrowseNodeLookupRequest_Type;
    FRequest : BrowseNodeLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : BrowseNodeLookupRequest_Type read FShared write FShared stored HasShared;
    property Request : BrowseNodeLookup_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  BrowseNodeLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FBrowseNodes : BrowseNodeLookupResponse_BrowseNodesArray;
  private
    function HasOperationRequest() : Boolean;
    function HasBrowseNodes() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property BrowseNodes : BrowseNodeLookupResponse_BrowseNodesArray read FBrowseNodes write FBrowseNodes stored HasBrowseNodes;
  end;

  ListSearch_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : ListSearchRequest_Type;
    FRequest : ListSearch_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : ListSearchRequest_Type read FShared write FShared stored HasShared;
    property Request : ListSearch_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  ListSearchResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FLists : ListSearchResponse_ListsArray;
  private
    function HasOperationRequest() : Boolean;
    function HasLists() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Lists : ListSearchResponse_ListsArray read FLists write FLists stored HasLists;
  end;

  ListLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : ListLookupRequest_Type;
    FRequest : ListLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : ListLookupRequest_Type read FShared write FShared stored HasShared;
    property Request : ListLookup_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  ListLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FLists : ListLookupResponse_ListsArray;
  private
    function HasOperationRequest() : Boolean;
    function HasLists() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Lists : ListLookupResponse_ListsArray read FLists write FLists stored HasLists;
  end;

  CustomerContentSearch_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CustomerContentSearchRequest_Type;
    FRequest : CustomerContentSearch_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : CustomerContentSearchRequest_Type read FShared write FShared stored HasShared;
    property Request : CustomerContentSearch_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  CustomerContentSearchResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCustomers : CustomerContentSearchResponse_CustomersArray;
  private
    function HasOperationRequest() : Boolean;
    function HasCustomers() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Customers : CustomerContentSearchResponse_CustomersArray read FCustomers write FCustomers stored HasCustomers;
  end;

  CustomerContentLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CustomerContentLookupRequest_Type;
    FRequest : CustomerContentLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : CustomerContentLookupRequest_Type read FShared write FShared stored HasShared;
    property Request : CustomerContentLookup_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  CustomerContentLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCustomers : CustomerContentLookupResponse_CustomersArray;
  private
    function HasOperationRequest() : Boolean;
    function HasCustomers() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Customers : CustomerContentLookupResponse_CustomersArray read FCustomers write FCustomers stored HasCustomers;
  end;

  SimilarityLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : SimilarityLookupRequest_Type;
    FRequest : SimilarityLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : SimilarityLookupRequest_Type read FShared write FShared stored HasShared;
    property Request : SimilarityLookup_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  SimilarityLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FItems : SimilarityLookupResponse_ItemsArray;
  private
    function HasOperationRequest() : Boolean;
    function HasItems() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Items : SimilarityLookupResponse_ItemsArray read FItems write FItems stored HasItems;
  end;

  SellerLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : SellerLookupRequest_Type;
    FRequest : SellerLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : SellerLookupRequest_Type read FShared write FShared stored HasShared;
    property Request : SellerLookup_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  SellerLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FSellers : SellerLookupResponse_SellersArray;
  private
    function HasOperationRequest() : Boolean;
    function HasSellers() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Sellers : SellerLookupResponse_SellersArray read FSellers write FSellers stored HasSellers;
  end;

  CartGet_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CartGetRequest_Type;
    FRequest : CartGet_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : CartGetRequest_Type read FShared write FShared stored HasShared;
    property Request : CartGet_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  CartGetResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCart : CartGetResponse_CartArray;
  private
    function HasOperationRequest() : Boolean;
    function HasCart() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Cart : CartGetResponse_CartArray read FCart write FCart stored HasCart;
  end;

  CartAdd_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CartAddRequest_Type;
    FRequest : CartAdd_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : CartAddRequest_Type read FShared write FShared stored HasShared;
    property Request : CartAdd_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  CartAddResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCart : CartAddResponse_CartArray;
  private
    function HasOperationRequest() : Boolean;
    function HasCart() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Cart : CartAddResponse_CartArray read FCart write FCart stored HasCart;
  end;

  CartCreate_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CartCreateRequest_Type;
    FRequest : CartCreate_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : CartCreateRequest_Type read FShared write FShared stored HasShared;
    property Request : CartCreate_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  CartCreateResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCart : CartCreateResponse_CartArray;
  private
    function HasOperationRequest() : Boolean;
    function HasCart() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Cart : CartCreateResponse_CartArray read FCart write FCart stored HasCart;
  end;

  CartModify_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CartModifyRequest_Type;
    FRequest : CartModify_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : CartModifyRequest_Type read FShared write FShared stored HasShared;
    property Request : CartModify_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  CartModifyResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCart : CartModifyResponse_CartArray;
  private
    function HasOperationRequest() : Boolean;
    function HasCart() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Cart : CartModifyResponse_CartArray read FCart write FCart stored HasCart;
  end;

  CartClear_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : CartClearRequest_Type;
    FRequest : CartClear_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : CartClearRequest_Type read FShared write FShared stored HasShared;
    property Request : CartClear_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  CartClearResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FCart : CartClearResponse_CartArray;
  private
    function HasOperationRequest() : Boolean;
    function HasCart() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Cart : CartClearResponse_CartArray read FCart write FCart stored HasCart;
  end;

  TransactionLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : TransactionLookupRequest_Type;
    FRequest : TransactionLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : TransactionLookupRequest_Type read FShared write FShared stored HasShared;
    property Request : TransactionLookup_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  TransactionLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FTransactions : TransactionLookupResponse_TransactionsArray;
  private
    function HasOperationRequest() : Boolean;
    function HasTransactions() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Transactions : TransactionLookupResponse_TransactionsArray read FTransactions write FTransactions stored HasTransactions;
  end;

  SellerListingSearch_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : SellerListingSearchRequest_Type;
    FRequest : SellerListingSearch_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : SellerListingSearchRequest_Type read FShared write FShared stored HasShared;
    property Request : SellerListingSearch_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  SellerListingSearchResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FSellerListings : SellerListingSearchResponse_SellerListingsArray;
  private
    function HasOperationRequest() : Boolean;
    function HasSellerListings() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property SellerListings : SellerListingSearchResponse_SellerListingsArray read FSellerListings write FSellerListings stored HasSellerListings;
  end;

  SellerListingLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : SellerListingLookupRequest_Type;
    FRequest : SellerListingLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : SellerListingLookupRequest_Type read FShared write FShared stored HasShared;
    property Request : SellerListingLookup_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  SellerListingLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FSellerListings : SellerListingLookupResponse_SellerListingsArray;
  private
    function HasOperationRequest() : Boolean;
    function HasSellerListings() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property SellerListings : SellerListingLookupResponse_SellerListingsArray read FSellerListings write FSellerListings stored HasSellerListings;
  end;

  TagLookup_Type = class(TBaseComplexRemotable)
  private
    FMarketplaceDomain : string;
    FAWSAccessKeyId : string;
    FSubscriptionId : string;
    FAssociateTag : string;
    FValidate : string;
    FXMLEscaping : string;
    FShared : TagLookupRequest_Type;
    FRequest : TagLookup_RequestArray;
  private
    function HasMarketplaceDomain() : Boolean;
    function HasAWSAccessKeyId() : Boolean;
    function HasSubscriptionId() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasValidate() : Boolean;
    function HasXMLEscaping() : Boolean;
    function HasShared() : Boolean;
    function HasRequest() : Boolean;
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
    property Shared : TagLookupRequest_Type read FShared write FShared stored HasShared;
    property Request : TagLookup_RequestArray read FRequest write FRequest stored HasRequest;
  end;

  TagLookupResponse_Type = class(TBaseComplexRemotable)
  private
    FOperationRequest : OperationRequest_Type;
    FTags : TagLookupResponse_TagsArray;
  private
    function HasOperationRequest() : Boolean;
    function HasTags() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OperationRequest : OperationRequest_Type read FOperationRequest write FOperationRequest stored HasOperationRequest;
    property Tags : TagLookupResponse_TagsArray read FTags write FTags stored HasTags;
  end;

  MultiOperation_Type = class(TBaseComplexRemotable)
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
    FTagLookup : TagLookup_Type;
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
    function HasTagLookup() : Boolean;
    function HasBrowseNodeLookup() : Boolean;
  public
    constructor Create();override;
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
    property TagLookup : TagLookup_Type read FTagLookup write FTagLookup stored HasTagLookup;
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
    FTagLookupResponse : TagLookupResponse_Type;
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
    function HasTagLookupResponse() : Boolean;
    function HasBrowseNodeLookupResponse() : Boolean;
  public
    constructor Create();override;
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
    property TagLookupResponse : TagLookupResponse_Type read FTagLookupResponse write FTagLookupResponse stored HasTagLookupResponse;
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
  private
    function HasBinParameter() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property BinName : string read FBinName write FBinName;
    property BinItemCount : positiveInteger read FBinItemCount write FBinItemCount;
    property BinParameter : Bin_BinParameterArray read FBinParameter write FBinParameter stored HasBinParameter;
  end;

  SearchBinSet_Type = class(TBaseComplexRemotable)
  private
    FBin : SearchBinSet_BinArray;
    FNarrowBy : string;
  private
    function HasBin() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Bin : SearchBinSet_BinArray read FBin write FBin stored HasBin;
    property NarrowBy : string read FNarrowBy write FNarrowBy;
  end;

  HelpRequest_Type = class(TBaseComplexRemotable)
  private
    FAbout : string;
    FHelpType : HelpRequest_HelpType_Type;
    FResponseGroup : HelpRequest_ResponseGroupArray;
  private
    function HasAbout() : Boolean;
    function HasHelpType() : Boolean;
    function HasResponseGroup() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property About : string read FAbout write FAbout stored HasAbout;
    property HelpType : HelpRequest_HelpType_Type read FHelpType write FHelpType stored HasHelpType;
    property ResponseGroup : HelpRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
  end;

  ItemSearchRequest_Type = class(TBaseComplexRemotable)
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
    FTagPage : positiveInteger;
    FTagsPerPage : positiveInteger;
    FTagSort : string;
    FTextStream : string;
    FTitle : string;
    FReleaseDate : string;
  private
    function HasActor() : Boolean;
    function HasArtist() : Boolean;
    function HasAvailability() : Boolean;
    function HasAudienceRating() : Boolean;
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
    function HasResponseGroup() : Boolean;
    function HasReviewSort() : Boolean;
    function HasSearchIndex() : Boolean;
    function HasSort() : Boolean;
    function HasState() : Boolean;
    function HasTagPage() : Boolean;
    function HasTagsPerPage() : Boolean;
    function HasTagSort() : Boolean;
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
    property AudienceRating : ItemSearchRequest_AudienceRatingArray read FAudienceRating write FAudienceRating stored HasAudienceRating;
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
    property ResponseGroup : ItemSearchRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
    property ReviewSort : string read FReviewSort write FReviewSort stored HasReviewSort;
    property SearchIndex : string read FSearchIndex write FSearchIndex stored HasSearchIndex;
    property Sort : string read FSort write FSort stored HasSort;
    property State : string read FState write FState stored HasState;
    property TagPage : positiveInteger read FTagPage write FTagPage stored HasTagPage;
    property TagsPerPage : positiveInteger read FTagsPerPage write FTagsPerPage stored HasTagsPerPage;
    property TagSort : string read FTagSort write FTagSort stored HasTagSort;
    property TextStream : string read FTextStream write FTextStream stored HasTextStream;
    property Title : string read FTitle write FTitle stored HasTitle;
    property ReleaseDate : string read FReleaseDate write FReleaseDate stored HasReleaseDate;
  end;

  ItemLookupRequest_Type = class(TBaseComplexRemotable)
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
    FTagPage : positiveInteger;
    FTagsPerPage : positiveInteger;
    FTagSort : string;
    FVariationPage : positiveIntegerOrAll_Type;
  private
    function HasCondition() : Boolean;
    function HasDeliveryMethod() : Boolean;
    function HasFutureLaunchDate() : Boolean;
    function HasIdType() : Boolean;
    function HasISPUPostalCode() : Boolean;
    function HasMerchantId() : Boolean;
    function HasOfferPage() : Boolean;
    function HasItemId() : Boolean;
    function HasResponseGroup() : Boolean;
    function HasReviewPage() : Boolean;
    function HasReviewSort() : Boolean;
    function HasSearchIndex() : Boolean;
    function HasSearchInsideKeywords() : Boolean;
    function HasTagPage() : Boolean;
    function HasTagsPerPage() : Boolean;
    function HasTagSort() : Boolean;
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
    property ItemId : ItemLookupRequest_ItemIdArray read FItemId write FItemId stored HasItemId;
    property ResponseGroup : ItemLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
    property ReviewPage : positiveInteger read FReviewPage write FReviewPage stored HasReviewPage;
    property ReviewSort : string read FReviewSort write FReviewSort stored HasReviewSort;
    property SearchIndex : string read FSearchIndex write FSearchIndex stored HasSearchIndex;
    property SearchInsideKeywords : string read FSearchInsideKeywords write FSearchInsideKeywords stored HasSearchInsideKeywords;
    property TagPage : positiveInteger read FTagPage write FTagPage stored HasTagPage;
    property TagsPerPage : positiveInteger read FTagsPerPage write FTagsPerPage stored HasTagsPerPage;
    property TagSort : string read FTagSort write FTagSort stored HasTagSort;
    property VariationPage : positiveIntegerOrAll_Type read FVariationPage write FVariationPage stored HasVariationPage;
  end;

  ListSearchRequest_Type = class(TBaseComplexRemotable)
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
    function HasResponseGroup() : Boolean;
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
    property ResponseGroup : ListSearchRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
    property State : string read FState write FState stored HasState;
  end;

  ListLookupRequest_Type = class(TBaseComplexRemotable)
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
    function HasResponseGroup() : Boolean;
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
    property ResponseGroup : ListLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
    property ReviewSort : string read FReviewSort write FReviewSort stored HasReviewSort;
    property Sort : string read FSort write FSort stored HasSort;
  end;

  CustomerContentSearchRequest_Type = class(TBaseComplexRemotable)
  private
    FCustomerPage : positiveInteger;
    FEmail : string;
    FName : string;
    FResponseGroup : CustomerContentSearchRequest_ResponseGroupArray;
  private
    function HasCustomerPage() : Boolean;
    function HasEmail() : Boolean;
    function HasName() : Boolean;
    function HasResponseGroup() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CustomerPage : positiveInteger read FCustomerPage write FCustomerPage stored HasCustomerPage;
    property Email : string read FEmail write FEmail stored HasEmail;
    property Name : string read FName write FName stored HasName;
    property ResponseGroup : CustomerContentSearchRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
  end;

  CustomerContentLookupRequest_Type = class(TBaseComplexRemotable)
  private
    FCustomerId : string;
    FResponseGroup : CustomerContentLookupRequest_ResponseGroupArray;
    FReviewPage : positiveInteger;
    FTagPage : positiveInteger;
    FTagsPerPage : positiveInteger;
    FTagSort : string;
  private
    function HasCustomerId() : Boolean;
    function HasResponseGroup() : Boolean;
    function HasReviewPage() : Boolean;
    function HasTagPage() : Boolean;
    function HasTagsPerPage() : Boolean;
    function HasTagSort() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CustomerId : string read FCustomerId write FCustomerId stored HasCustomerId;
    property ResponseGroup : CustomerContentLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
    property ReviewPage : positiveInteger read FReviewPage write FReviewPage stored HasReviewPage;
    property TagPage : positiveInteger read FTagPage write FTagPage stored HasTagPage;
    property TagsPerPage : positiveInteger read FTagsPerPage write FTagsPerPage stored HasTagsPerPage;
    property TagSort : string read FTagSort write FTagSort stored HasTagSort;
  end;

  SimilarityLookupRequest_Type = class(TBaseComplexRemotable)
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
    function HasItemId() : Boolean;
    function HasISPUPostalCode() : Boolean;
    function HasMerchantId() : Boolean;
    function HasResponseGroup() : Boolean;
    function HasReviewSort() : Boolean;
    function HasSimilarityType() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Condition : Condition_Type read FCondition write FCondition stored HasCondition;
    property DeliveryMethod : DeliveryMethod_Type read FDeliveryMethod write FDeliveryMethod stored HasDeliveryMethod;
    property ItemId : SimilarityLookupRequest_ItemIdArray read FItemId write FItemId stored HasItemId;
    property ISPUPostalCode : string read FISPUPostalCode write FISPUPostalCode stored HasISPUPostalCode;
    property MerchantId : string read FMerchantId write FMerchantId stored HasMerchantId;
    property ResponseGroup : SimilarityLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
    property ReviewSort : string read FReviewSort write FReviewSort stored HasReviewSort;
    property SimilarityType : SimilarityLookupRequest_SimilarityType_Type read FSimilarityType write FSimilarityType stored HasSimilarityType;
  end;

  SellerLookupRequest_Type = class(TBaseComplexRemotable)
  private
    FResponseGroup : SellerLookupRequest_ResponseGroupArray;
    FSellerId : SellerLookupRequest_SellerIdArray;
    FFeedbackPage : positiveInteger;
  private
    function HasResponseGroup() : Boolean;
    function HasSellerId() : Boolean;
    function HasFeedbackPage() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property ResponseGroup : SellerLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
    property SellerId : SellerLookupRequest_SellerIdArray read FSellerId write FSellerId stored HasSellerId;
    property FeedbackPage : positiveInteger read FFeedbackPage write FFeedbackPage stored HasFeedbackPage;
  end;

  CartGetRequest_Type = class(TBaseComplexRemotable)
  private
    FCartId : string;
    FHMAC : string;
    FMergeCart : string;
    FResponseGroup : CartGetRequest_ResponseGroupArray;
  private
    function HasCartId() : Boolean;
    function HasHMAC() : Boolean;
    function HasMergeCart() : Boolean;
    function HasResponseGroup() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CartId : string read FCartId write FCartId stored HasCartId;
    property HMAC : string read FHMAC write FHMAC stored HasHMAC;
    property MergeCart : string read FMergeCart write FMergeCart stored HasMergeCart;
    property ResponseGroup : CartGetRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
  end;

  CartAddRequest_Items_Type_Item_Type_MetaData_Type = class(TBaseComplexRemotable)
  private
    FKey : string;
    FValue : string;
  private
    function HasKey() : Boolean;
    function HasValue() : Boolean;
  published
    property Key : string read FKey write FKey stored HasKey;
    property Value : string read FValue write FValue stored HasValue;
  end;

  CartAddRequest_Items_Type_Item_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FOfferListingId : string;
    FQuantity : positiveInteger;
    FAssociateTag : string;
    FListItemId : string;
    FMetaData : CartAddRequest_Items_Type_Item_Type_MetaDataArray;
  private
    function HasASIN() : Boolean;
    function HasOfferListingId() : Boolean;
    function HasQuantity() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasListItemId() : Boolean;
    function HasMetaData() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property OfferListingId : string read FOfferListingId write FOfferListingId stored HasOfferListingId;
    property Quantity : positiveInteger read FQuantity write FQuantity stored HasQuantity;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property ListItemId : string read FListItemId write FListItemId stored HasListItemId;
    property MetaData : CartAddRequest_Items_Type_Item_Type_MetaDataArray read FMetaData write FMetaData stored HasMetaData;
  end;

  CartAddRequest_Type = class(TBaseComplexRemotable)
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
    function HasResponseGroup() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CartId : string read FCartId write FCartId stored HasCartId;
    property HMAC : string read FHMAC write FHMAC stored HasHMAC;
    property MergeCart : string read FMergeCart write FMergeCart stored HasMergeCart;
    property Items : CartAddRequest_Items_Type read FItems write FItems stored HasItems;
    property ResponseGroup : CartAddRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
  end;

  CartCreateRequest_Items_Type_Item_Type_MetaData_Type = class(TBaseComplexRemotable)
  private
    FKey : string;
    FValue : string;
  private
    function HasKey() : Boolean;
    function HasValue() : Boolean;
  published
    property Key : string read FKey write FKey stored HasKey;
    property Value : string read FValue write FValue stored HasValue;
  end;

  CartCreateRequest_Items_Type_Item_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FOfferListingId : string;
    FQuantity : positiveInteger;
    FAssociateTag : string;
    FListItemId : string;
    FMetaData : CartCreateRequest_Items_Type_Item_Type_MetaDataArray;
  private
    function HasASIN() : Boolean;
    function HasOfferListingId() : Boolean;
    function HasQuantity() : Boolean;
    function HasAssociateTag() : Boolean;
    function HasListItemId() : Boolean;
    function HasMetaData() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property OfferListingId : string read FOfferListingId write FOfferListingId stored HasOfferListingId;
    property Quantity : positiveInteger read FQuantity write FQuantity stored HasQuantity;
    property AssociateTag : string read FAssociateTag write FAssociateTag stored HasAssociateTag;
    property ListItemId : string read FListItemId write FListItemId stored HasListItemId;
    property MetaData : CartCreateRequest_Items_Type_Item_Type_MetaDataArray read FMetaData write FMetaData stored HasMetaData;
  end;

  CartCreateRequest_Type = class(TBaseComplexRemotable)
  private
    FMergeCart : string;
    FItems : CartCreateRequest_Items_Type;
    FResponseGroup : CartCreateRequest_ResponseGroupArray;
  private
    function HasMergeCart() : Boolean;
    function HasItems() : Boolean;
    function HasResponseGroup() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MergeCart : string read FMergeCart write FMergeCart stored HasMergeCart;
    property Items : CartCreateRequest_Items_Type read FItems write FItems stored HasItems;
    property ResponseGroup : CartCreateRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
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

  CartModifyRequest_Type = class(TBaseComplexRemotable)
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
    function HasResponseGroup() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CartId : string read FCartId write FCartId stored HasCartId;
    property HMAC : string read FHMAC write FHMAC stored HasHMAC;
    property MergeCart : string read FMergeCart write FMergeCart stored HasMergeCart;
    property Items : CartModifyRequest_Items_Type read FItems write FItems stored HasItems;
    property ResponseGroup : CartModifyRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
  end;

  CartClearRequest_Type = class(TBaseComplexRemotable)
  private
    FCartId : string;
    FHMAC : string;
    FMergeCart : string;
    FResponseGroup : CartClearRequest_ResponseGroupArray;
  private
    function HasCartId() : Boolean;
    function HasHMAC() : Boolean;
    function HasMergeCart() : Boolean;
    function HasResponseGroup() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CartId : string read FCartId write FCartId stored HasCartId;
    property HMAC : string read FHMAC write FHMAC stored HasHMAC;
    property MergeCart : string read FMergeCart write FMergeCart stored HasMergeCart;
    property ResponseGroup : CartClearRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
  end;

  TransactionLookupRequest_Type = class(TBaseComplexRemotable)
  private
    FResponseGroup : TransactionLookupRequest_ResponseGroupArray;
    FTransactionId : TransactionLookupRequest_TransactionIdArray;
  private
    function HasResponseGroup() : Boolean;
    function HasTransactionId() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property ResponseGroup : TransactionLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
    property TransactionId : TransactionLookupRequest_TransactionIdArray read FTransactionId write FTransactionId stored HasTransactionId;
  end;

  SellerListingSearchRequest_Type = class(TBaseComplexRemotable)
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
    function HasResponseGroup() : Boolean;
    function HasSort() : Boolean;
    function HasTitle() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Keywords : string read FKeywords write FKeywords stored HasKeywords;
    property ListingPage : positiveInteger read FListingPage write FListingPage stored HasListingPage;
    property OfferStatus : SellerListingSearchRequest_OfferStatus_Type read FOfferStatus write FOfferStatus stored HasOfferStatus;
    property ResponseGroup : SellerListingSearchRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
    property SellerId : string read FSellerId write FSellerId;
    property Sort : string read FSort write FSort stored HasSort;
    property Title : string read FTitle write FTitle stored HasTitle;
  end;

  SellerListingLookupRequest_Type = class(TBaseComplexRemotable)
  private
    FId : string;
    FSellerId : string;
    FIdType : SellerListingLookupRequest_IdType_Type;
    FResponseGroup : SellerListingLookupRequest_ResponseGroupArray;
  private
    function HasSellerId() : Boolean;
    function HasResponseGroup() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Id : string read FId write FId;
    property SellerId : string read FSellerId write FSellerId stored HasSellerId;
    property IdType : SellerListingLookupRequest_IdType_Type read FIdType write FIdType;
    property ResponseGroup : SellerListingLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
  end;

  TagLookupRequest_Type = class(TBaseComplexRemotable)
  private
    FTagName : TagLookupRequest_TagNameArray;
    FCustomerId : string;
    FTagPage : positiveInteger;
    FCount : positiveInteger;
    FTagSort : string;
    FResponseGroup : TagLookupRequest_ResponseGroupArray;
  private
    function HasTagName() : Boolean;
    function HasCustomerId() : Boolean;
    function HasTagPage() : Boolean;
    function HasCount() : Boolean;
    function HasTagSort() : Boolean;
    function HasResponseGroup() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property TagName : TagLookupRequest_TagNameArray read FTagName write FTagName stored HasTagName;
    property CustomerId : string read FCustomerId write FCustomerId stored HasCustomerId;
    property TagPage : positiveInteger read FTagPage write FTagPage stored HasTagPage;
    property Count : positiveInteger read FCount write FCount stored HasCount;
    property TagSort : string read FTagSort write FTagSort stored HasTagSort;
    property ResponseGroup : TagLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
  end;

  BrowseNodeLookupRequest_Type = class(TBaseComplexRemotable)
  private
    FBrowseNodeId : BrowseNodeLookupRequest_BrowseNodeIdArray;
    FResponseGroup : BrowseNodeLookupRequest_ResponseGroupArray;
  private
    function HasBrowseNodeId() : Boolean;
    function HasResponseGroup() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property BrowseNodeId : BrowseNodeLookupRequest_BrowseNodeIdArray read FBrowseNodeId write FBrowseNodeId stored HasBrowseNodeId;
    property ResponseGroup : BrowseNodeLookupRequest_ResponseGroupArray read FResponseGroup write FResponseGroup stored HasResponseGroup;
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

  Request_Type = class(TBaseComplexRemotable)
  private
    FIsValid : string;
    FHelpRequest : HelpRequest_Type;
    FBrowseNodeLookupRequest : BrowseNodeLookupRequest_Type;
    FItemSearchRequest : ItemSearchRequest_Type;
    FItemLookupRequest : ItemLookupRequest_Type;
    FListSearchRequest : ListSearchRequest_Type;
    FListLookupRequest : ListLookupRequest_Type;
    FCustomerContentSearchRequest : CustomerContentSearchRequest_Type;
    FCustomerContentLookupRequest : CustomerContentLookupRequest_Type;
    FSimilarityLookupRequest : SimilarityLookupRequest_Type;
    FCartGetRequest : CartGetRequest_Type;
    FCartAddRequest : CartAddRequest_Type;
    FCartCreateRequest : CartCreateRequest_Type;
    FCartModifyRequest : CartModifyRequest_Type;
    FCartClearRequest : CartClearRequest_Type;
    FTransactionLookupRequest : TransactionLookupRequest_Type;
    FSellerListingSearchRequest : SellerListingSearchRequest_Type;
    FSellerListingLookupRequest : SellerListingLookupRequest_Type;
    FSellerLookupRequest : SellerLookupRequest_Type;
    FTagLookupRequest : TagLookupRequest_Type;
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
    function HasTagLookupRequest() : Boolean;
    function HasErrors() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property IsValid : string read FIsValid write FIsValid stored HasIsValid;
    property HelpRequest : HelpRequest_Type read FHelpRequest write FHelpRequest stored HasHelpRequest;
    property BrowseNodeLookupRequest : BrowseNodeLookupRequest_Type read FBrowseNodeLookupRequest write FBrowseNodeLookupRequest stored HasBrowseNodeLookupRequest;
    property ItemSearchRequest : ItemSearchRequest_Type read FItemSearchRequest write FItemSearchRequest stored HasItemSearchRequest;
    property ItemLookupRequest : ItemLookupRequest_Type read FItemLookupRequest write FItemLookupRequest stored HasItemLookupRequest;
    property ListSearchRequest : ListSearchRequest_Type read FListSearchRequest write FListSearchRequest stored HasListSearchRequest;
    property ListLookupRequest : ListLookupRequest_Type read FListLookupRequest write FListLookupRequest stored HasListLookupRequest;
    property CustomerContentSearchRequest : CustomerContentSearchRequest_Type read FCustomerContentSearchRequest write FCustomerContentSearchRequest stored HasCustomerContentSearchRequest;
    property CustomerContentLookupRequest : CustomerContentLookupRequest_Type read FCustomerContentLookupRequest write FCustomerContentLookupRequest stored HasCustomerContentLookupRequest;
    property SimilarityLookupRequest : SimilarityLookupRequest_Type read FSimilarityLookupRequest write FSimilarityLookupRequest stored HasSimilarityLookupRequest;
    property CartGetRequest : CartGetRequest_Type read FCartGetRequest write FCartGetRequest stored HasCartGetRequest;
    property CartAddRequest : CartAddRequest_Type read FCartAddRequest write FCartAddRequest stored HasCartAddRequest;
    property CartCreateRequest : CartCreateRequest_Type read FCartCreateRequest write FCartCreateRequest stored HasCartCreateRequest;
    property CartModifyRequest : CartModifyRequest_Type read FCartModifyRequest write FCartModifyRequest stored HasCartModifyRequest;
    property CartClearRequest : CartClearRequest_Type read FCartClearRequest write FCartClearRequest stored HasCartClearRequest;
    property TransactionLookupRequest : TransactionLookupRequest_Type read FTransactionLookupRequest write FTransactionLookupRequest stored HasTransactionLookupRequest;
    property SellerListingSearchRequest : SellerListingSearchRequest_Type read FSellerListingSearchRequest write FSellerListingSearchRequest stored HasSellerListingSearchRequest;
    property SellerListingLookupRequest : SellerListingLookupRequest_Type read FSellerListingLookupRequest write FSellerListingLookupRequest stored HasSellerListingLookupRequest;
    property SellerLookupRequest : SellerLookupRequest_Type read FSellerLookupRequest write FSellerLookupRequest stored HasSellerLookupRequest;
    property TagLookupRequest : TagLookupRequest_Type read FTagLookupRequest write FTagLookupRequest stored HasTagLookupRequest;
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

  Information_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FOperationInformation : Information_OperationInformationArray;
    FResponseGroupInformation : Information_ResponseGroupInformationArray;
  private
    function HasRequest() : Boolean;
    function HasOperationInformation() : Boolean;
    function HasResponseGroupInformation() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property OperationInformation : Information_OperationInformationArray read FOperationInformation write FOperationInformation stored HasOperationInformation;
    property ResponseGroupInformation : Information_ResponseGroupInformationArray read FResponseGroupInformation write FResponseGroupInformation stored HasResponseGroupInformation;
  end;

  Items_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FCorrectedQuery : CorrectedQuery_Type;
    FQid : string;
    FEngineQuery : string;
    FTotalResults : nonNegativeInteger;
    FTotalPages : nonNegativeInteger;
    FSearchResultsMap : SearchResultsMap_Type;
    F_Item : Items__ItemArray;
    FSearchBinSets : SearchBinSets_Type;
  private
    function HasRequest() : Boolean;
    function HasCorrectedQuery() : Boolean;
    function HasQid() : Boolean;
    function HasEngineQuery() : Boolean;
    function HasTotalResults() : Boolean;
    function HasTotalPages() : Boolean;
    function HasSearchResultsMap() : Boolean;
    function Has_Item() : Boolean;
    function HasSearchBinSets() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property CorrectedQuery : CorrectedQuery_Type read FCorrectedQuery write FCorrectedQuery stored HasCorrectedQuery;
    property Qid : string read FQid write FQid stored HasQid;
    property EngineQuery : string read FEngineQuery write FEngineQuery stored HasEngineQuery;
    property TotalResults : nonNegativeInteger read FTotalResults write FTotalResults stored HasTotalResults;
    property TotalPages : nonNegativeInteger read FTotalPages write FTotalPages stored HasTotalPages;
    property SearchResultsMap : SearchResultsMap_Type read FSearchResultsMap write FSearchResultsMap stored HasSearchResultsMap;
    property _Item : Items__ItemArray read F_Item write F_Item stored Has_Item;
    property SearchBinSets : SearchBinSets_Type read FSearchBinSets write FSearchBinSets stored HasSearchBinSets;
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
    function HasList() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property TotalResults : nonNegativeInteger read FTotalResults write FTotalResults stored HasTotalResults;
    property TotalPages : nonNegativeInteger read FTotalPages write FTotalPages stored HasTotalPages;
    property List : Lists_ListArray read FList write FList stored HasList;
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
    function HasCustomer() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property TotalResults : nonNegativeInteger read FTotalResults write FTotalResults stored HasTotalResults;
    property TotalPages : nonNegativeInteger read FTotalPages write FTotalPages stored HasTotalPages;
    property Customer : Customers_CustomerArray read FCustomer write FCustomer stored HasCustomer;
  end;

  Cart_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FCartId : string;
    FHMAC : string;
    FURLEncodedHMAC : string;
    FPurchaseURL : string;
    FSubTotal : Price_Type;
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
    property SubTotal : Price_Type read FSubTotal write FSubTotal stored HasSubTotal;
    property CartItems : CartItems_Type read FCartItems write FCartItems stored HasCartItems;
    property SavedForLaterItems : SavedForLaterItems_Type read FSavedForLaterItems write FSavedForLaterItems stored HasSavedForLaterItems;
    property SimilarProducts : SimilarProducts_Type read FSimilarProducts write FSimilarProducts stored HasSimilarProducts;
    property TopSellers : TopSellers_Type read FTopSellers write FTopSellers stored HasTopSellers;
    property NewReleases : NewReleases_Type read FNewReleases write FNewReleases stored HasNewReleases;
    property SimilarViewedProducts : SimilarViewedProducts_Type read FSimilarViewedProducts write FSimilarViewedProducts stored HasSimilarViewedProducts;
    property OtherCategoriesSimilarProducts : OtherCategoriesSimilarProducts_Type read FOtherCategoriesSimilarProducts write FOtherCategoriesSimilarProducts stored HasOtherCategoriesSimilarProducts;
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
    function HasSeller() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property TotalResults : nonNegativeInteger read FTotalResults write FTotalResults stored HasTotalResults;
    property TotalPages : nonNegativeInteger read FTotalPages write FTotalPages stored HasTotalPages;
    property Seller : Sellers_SellerArray read FSeller write FSeller stored HasSeller;
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
    function HasSellerListing() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property TotalResults : nonNegativeInteger read FTotalResults write FTotalResults stored HasTotalResults;
    property TotalPages : nonNegativeInteger read FTotalPages write FTotalPages stored HasTotalPages;
    property SellerListing : SellerListings_SellerListingArray read FSellerListing write FSellerListing stored HasSellerListing;
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
    FImage : Image_Type;
    FAverageRating : Extended;
    FTotalVotes : nonNegativeInteger;
    FTotalTimesRead : nonNegativeInteger;
    FTags : Tags_Type;
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
    function HasTags() : Boolean;
    function HasListItem() : Boolean;
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
    property Image : Image_Type read FImage write FImage stored HasImage;
    property AverageRating : Extended read FAverageRating write FAverageRating stored HasAverageRating;
    property TotalVotes : nonNegativeInteger read FTotalVotes write FTotalVotes stored HasTotalVotes;
    property TotalTimesRead : nonNegativeInteger read FTotalTimesRead write FTotalTimesRead stored HasTotalTimesRead;
    property Tags : Tags_Type read FTags write FTags stored HasTags;
    property ListItem : List_ListItemArray read FListItem write FListItem stored HasListItem;
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
    constructor Create();override;
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

  Customer_Type = class(TBaseComplexRemotable)
  private
    FCustomerId : string;
    FNickname : string;
    FBirthday : string;
    FWishListId : string;
    FLocation : Customer_Location_Type;
    FCustomerReviews : Customer_CustomerReviewsArray;
    FTags : Tags_Type;
  private
    function HasNickname() : Boolean;
    function HasBirthday() : Boolean;
    function HasWishListId() : Boolean;
    function HasLocation() : Boolean;
    function HasCustomerReviews() : Boolean;
    function HasTags() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CustomerId : string read FCustomerId write FCustomerId;
    property Nickname : string read FNickname write FNickname stored HasNickname;
    property Birthday : string read FBirthday write FBirthday stored HasBirthday;
    property WishListId : string read FWishListId write FWishListId stored HasWishListId;
    property Location : Customer_Location_Type read FLocation write FLocation stored HasLocation;
    property CustomerReviews : Customer_CustomerReviewsArray read FCustomerReviews write FCustomerReviews stored HasCustomerReviews;
    property Tags : Tags_Type read FTags write FTags stored HasTags;
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

  Item_ImageSets_Type = class(TBaseComplexRemotable)
  private
    FMerchantId : string;
    FImageSet : Item_ImageSets_Type_ImageSetArray;
  private
    function HasMerchantId() : Boolean;
    function HasImageSet() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property MerchantId : string read FMerchantId write FMerchantId stored HasMerchantId;
    property ImageSet : Item_ImageSets_Type_ImageSetArray read FImageSet write FImageSet stored HasImageSet;
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

  Item_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FParentASIN : string;
    FErrors : Errors_Type;
    FDetailPageURL : string;
    FSalesRank : string;
    FSmallImage : Image_Type;
    FMediumImage : Image_Type;
    FLargeImage : Image_Type;
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
    FTags : Tags_Type;
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
    function HasImageSets() : Boolean;
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
    function HasTags() : Boolean;
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
    property SmallImage : Image_Type read FSmallImage write FSmallImage stored HasSmallImage;
    property MediumImage : Image_Type read FMediumImage write FMediumImage stored HasMediumImage;
    property LargeImage : Image_Type read FLargeImage write FLargeImage stored HasLargeImage;
    property ImageSets : _Item_ImageSetsArray read FImageSets write FImageSets stored HasImageSets;
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
    property Tags : Tags_Type read FTags write FTags stored HasTags;
    property ListmaniaLists : ListmaniaLists_Type read FListmaniaLists write FListmaniaLists stored HasListmaniaLists;
    property SearchInside : SearchInside_Type read FSearchInside write FSearchInside stored HasSearchInside;
    property AlternateVersions : Item_AlternateVersions_Type read FAlternateVersions write FAlternateVersions stored HasAlternateVersions;
  end;

  Tags_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FDistinctTags : string;
    FDistinctItems : string;
    FDistinctUsers : string;
    FTotalUsages : string;
    FFirstTagging : Tagging_Type;
    FLastTagging : Tagging_Type;
    FTag : Tags_TagArray;
  private
    function HasRequest() : Boolean;
    function HasDistinctTags() : Boolean;
    function HasDistinctItems() : Boolean;
    function HasDistinctUsers() : Boolean;
    function HasTotalUsages() : Boolean;
    function HasFirstTagging() : Boolean;
    function HasLastTagging() : Boolean;
    function HasTag() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property DistinctTags : string read FDistinctTags write FDistinctTags stored HasDistinctTags;
    property DistinctItems : string read FDistinctItems write FDistinctItems stored HasDistinctItems;
    property DistinctUsers : string read FDistinctUsers write FDistinctUsers stored HasDistinctUsers;
    property TotalUsages : string read FTotalUsages write FTotalUsages stored HasTotalUsages;
    property FirstTagging : Tagging_Type read FFirstTagging write FFirstTagging stored HasFirstTagging;
    property LastTagging : Tagging_Type read FLastTagging write FLastTagging stored HasLastTagging;
    property Tag : Tags_TagArray read FTag write FTag stored HasTag;
  end;

  Tag_Type = class(TBaseComplexRemotable)
  private
    FName : string;
    FTagType : Tag_TagType_Type;
    FDistinctItems : string;
    FDistinctUsers : string;
    FTotalUsages : string;
    FFirstTagging : Tagging_Type;
    FLastTagging : Tagging_Type;
    FTaggedItems : Tag_TaggedItemsArray;
    FTaggedListmaniaLists : Tag_TaggedListmaniaListsArray;
    FTaggedGuides : Tag_TaggedGuidesArray;
  private
    function HasName() : Boolean;
    function HasTagType() : Boolean;
    function HasDistinctItems() : Boolean;
    function HasDistinctUsers() : Boolean;
    function HasTotalUsages() : Boolean;
    function HasFirstTagging() : Boolean;
    function HasLastTagging() : Boolean;
    function HasTaggedItems() : Boolean;
    function HasTaggedListmaniaLists() : Boolean;
    function HasTaggedGuides() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Name : string read FName write FName stored HasName;
    property TagType : Tag_TagType_Type read FTagType write FTagType stored HasTagType;
    property DistinctItems : string read FDistinctItems write FDistinctItems stored HasDistinctItems;
    property DistinctUsers : string read FDistinctUsers write FDistinctUsers stored HasDistinctUsers;
    property TotalUsages : string read FTotalUsages write FTotalUsages stored HasTotalUsages;
    property FirstTagging : Tagging_Type read FFirstTagging write FFirstTagging stored HasFirstTagging;
    property LastTagging : Tagging_Type read FLastTagging write FLastTagging stored HasLastTagging;
    property TaggedItems : Tag_TaggedItemsArray read FTaggedItems write FTaggedItems stored HasTaggedItems;
    property TaggedListmaniaLists : Tag_TaggedListmaniaListsArray read FTaggedListmaniaLists write FTaggedListmaniaLists stored HasTaggedListmaniaLists;
    property TaggedGuides : Tag_TaggedGuidesArray read FTaggedGuides write FTaggedGuides stored HasTaggedGuides;
  end;

  TaggedItems_Type = class(TBaseComplexRemotable)
  private
    F_Item : Item_Type;
    FDistinctUsers : string;
    FTotalUsages : string;
    FFirstTagging : Tagging_Type;
    FLastTagging : Tagging_Type;
  private
    function Has_Item() : Boolean;
    function HasDistinctUsers() : Boolean;
    function HasTotalUsages() : Boolean;
    function HasFirstTagging() : Boolean;
    function HasLastTagging() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property _Item : Item_Type read F_Item write F_Item stored Has_Item;
    property DistinctUsers : string read FDistinctUsers write FDistinctUsers stored HasDistinctUsers;
    property TotalUsages : string read FTotalUsages write FTotalUsages stored HasTotalUsages;
    property FirstTagging : Tagging_Type read FFirstTagging write FFirstTagging stored HasFirstTagging;
    property LastTagging : Tagging_Type read FLastTagging write FLastTagging stored HasLastTagging;
  end;

  TaggedListmaniaLists_Type = class(TBaseComplexRemotable)
  private
    FList : List_Type;
    FDistinctUsers : string;
    FTotalUsages : string;
    FFirstTagging : Tagging_Type;
    FLastTagging : Tagging_Type;
  private
    function HasList() : Boolean;
    function HasDistinctUsers() : Boolean;
    function HasTotalUsages() : Boolean;
    function HasFirstTagging() : Boolean;
    function HasLastTagging() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property List : List_Type read FList write FList stored HasList;
    property DistinctUsers : string read FDistinctUsers write FDistinctUsers stored HasDistinctUsers;
    property TotalUsages : string read FTotalUsages write FTotalUsages stored HasTotalUsages;
    property FirstTagging : Tagging_Type read FFirstTagging write FFirstTagging stored HasFirstTagging;
    property LastTagging : Tagging_Type read FLastTagging write FLastTagging stored HasLastTagging;
  end;

  TaggedGuides_Type = class(TBaseComplexRemotable)
  private
    FGuide : Guide_Type;
    FDistinctUsers : string;
    FTotalUsages : string;
    FFirstTagging : Tagging_Type;
    FLastTagging : Tagging_Type;
  private
    function HasGuide() : Boolean;
    function HasDistinctUsers() : Boolean;
    function HasTotalUsages() : Boolean;
    function HasFirstTagging() : Boolean;
    function HasLastTagging() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Guide : Guide_Type read FGuide write FGuide stored HasGuide;
    property DistinctUsers : string read FDistinctUsers write FDistinctUsers stored HasDistinctUsers;
    property TotalUsages : string read FTotalUsages write FTotalUsages stored HasTotalUsages;
    property FirstTagging : Tagging_Type read FFirstTagging write FFirstTagging stored HasFirstTagging;
    property LastTagging : Tagging_Type read FLastTagging write FLastTagging stored HasLastTagging;
  end;

  Guide_Type = class(TBaseComplexRemotable)
  private
    FGuideId : string;
  private
    function HasGuideId() : Boolean;
  published
    property GuideId : string read FGuideId write FGuideId stored HasGuideId;
  end;

  Tagging_Type = class(TBaseComplexRemotable)
  private
    FName : string;
    FEntityId : string;
    FUserId : string;
    FTime : string;
  private
    function HasName() : Boolean;
    function HasEntityId() : Boolean;
    function HasUserId() : Boolean;
    function HasTime() : Boolean;
  published
    property Name : string read FName write FName stored HasName;
    property EntityId : string read FEntityId write FEntityId stored HasEntityId;
    property UserId : string read FUserId write FUserId stored HasUserId;
    property Time : string read FTime write FTime stored HasTime;
  end;

  OfferSummary_Type = class(TBaseComplexRemotable)
  private
    FLowestNewPrice : Price_Type;
    FLowestUsedPrice : Price_Type;
    FLowestCollectiblePrice : Price_Type;
    FLowestRefurbishedPrice : Price_Type;
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
    constructor Create();override;
    destructor Destroy();override;
  published
    property LowestNewPrice : Price_Type read FLowestNewPrice write FLowestNewPrice stored HasLowestNewPrice;
    property LowestUsedPrice : Price_Type read FLowestUsedPrice write FLowestUsedPrice stored HasLowestUsedPrice;
    property LowestCollectiblePrice : Price_Type read FLowestCollectiblePrice write FLowestCollectiblePrice stored HasLowestCollectiblePrice;
    property LowestRefurbishedPrice : Price_Type read FLowestRefurbishedPrice write FLowestRefurbishedPrice stored HasLowestRefurbishedPrice;
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
    function HasOffer() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property TotalOffers : nonNegativeInteger read FTotalOffers write FTotalOffers stored HasTotalOffers;
    property TotalOfferPages : nonNegativeInteger read FTotalOfferPages write FTotalOfferPages stored HasTotalOfferPages;
    property Offer : Offers_OfferArray read FOffer write FOffer stored HasOffer;
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
    function HasOfferListing() : Boolean;
    function HasLoyaltyPoints() : Boolean;
    function HasPromotions() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Merchant : Merchant_Type read FMerchant write FMerchant stored HasMerchant;
    property Seller : Seller_Type read FSeller write FSeller stored HasSeller;
    property OfferAttributes : OfferAttributes_Type read FOfferAttributes write FOfferAttributes stored HasOfferAttributes;
    property OfferListing : Offer_OfferListingArray read FOfferListing write FOfferListing stored HasOfferListing;
    property LoyaltyPoints : LoyaltyPoints_Type read FLoyaltyPoints write FLoyaltyPoints stored HasLoyaltyPoints;
    property Promotions : Promotions_Type read FPromotions write FPromotions stored HasPromotions;
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

  OfferListing_ShippingCharge_Type = class(TBaseComplexRemotable)
  private
    FShippingType : string;
    FShippingPrice : Price_Type;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property ShippingType : string read FShippingType write FShippingType;
    property ShippingPrice : Price_Type read FShippingPrice write FShippingPrice;
  end;

  OfferListing_Type = class(TBaseComplexRemotable)
  private
    FOfferListingId : string;
    FExchangeId : string;
    FPrice : Price_Type;
    FSalePrice : Price_Type;
    FAmountSaved : Price_Type;
    FPercentageSaved : nonNegativeInteger;
    FAvailability : string;
    FAvailabilityAttributes : OfferListing_AvailabilityAttributes_Type;
    FQuantity : integer;
    FISPUStoreAddress : Address_Type;
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
    function HasShippingCharge() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property OfferListingId : string read FOfferListingId write FOfferListingId stored HasOfferListingId;
    property ExchangeId : string read FExchangeId write FExchangeId stored HasExchangeId;
    property Price : Price_Type read FPrice write FPrice stored HasPrice;
    property SalePrice : Price_Type read FSalePrice write FSalePrice stored HasSalePrice;
    property AmountSaved : Price_Type read FAmountSaved write FAmountSaved stored HasAmountSaved;
    property PercentageSaved : nonNegativeInteger read FPercentageSaved write FPercentageSaved stored HasPercentageSaved;
    property Availability : string read FAvailability write FAvailability stored HasAvailability;
    property AvailabilityAttributes : OfferListing_AvailabilityAttributes_Type read FAvailabilityAttributes write FAvailabilityAttributes stored HasAvailabilityAttributes;
    property Quantity : integer read FQuantity write FQuantity stored HasQuantity;
    property ISPUStoreAddress : Address_Type read FISPUStoreAddress write FISPUStoreAddress stored HasISPUStoreAddress;
    property ISPUStoreHours : string read FISPUStoreHours write FISPUStoreHours stored HasISPUStoreHours;
    property IsEligibleForSuperSaverShipping : boolean read FIsEligibleForSuperSaverShipping write FIsEligibleForSuperSaverShipping stored HasIsEligibleForSuperSaverShipping;
    property SalesRestriction : string read FSalesRestriction write FSalesRestriction stored HasSalesRestriction;
    property ShippingCharge : OfferListing_ShippingChargeArray read FShippingCharge write FShippingCharge stored HasShippingCharge;
  end;

  LoyaltyPoints_Type = class(TBaseComplexRemotable)
  private
    FPoints : nonNegativeInteger;
    FTypicalRedemptionValue : Price_Type;
  private
    function HasPoints() : Boolean;
    function HasTypicalRedemptionValue() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Points : nonNegativeInteger read FPoints write FPoints stored HasPoints;
    property TypicalRedemptionValue : Price_Type read FTypicalRedemptionValue write FTypicalRedemptionValue stored HasTypicalRedemptionValue;
  end;

  VariationSummary_Type = class(TBaseComplexRemotable)
  private
    FLowestPrice : Price_Type;
    FHighestPrice : Price_Type;
    FLowestSalePrice : Price_Type;
    FHighestSalePrice : Price_Type;
    FSingleMerchantId : string;
  private
    function HasLowestPrice() : Boolean;
    function HasHighestPrice() : Boolean;
    function HasLowestSalePrice() : Boolean;
    function HasHighestSalePrice() : Boolean;
    function HasSingleMerchantId() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property LowestPrice : Price_Type read FLowestPrice write FLowestPrice stored HasLowestPrice;
    property HighestPrice : Price_Type read FHighestPrice write FHighestPrice stored HasHighestPrice;
    property LowestSalePrice : Price_Type read FLowestSalePrice write FLowestSalePrice stored HasLowestSalePrice;
    property HighestSalePrice : Price_Type read FHighestSalePrice write FHighestSalePrice stored HasHighestSalePrice;
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
    function Has_Item() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property TotalVariations : nonNegativeInteger read FTotalVariations write FTotalVariations stored HasTotalVariations;
    property TotalVariationPages : nonNegativeInteger read FTotalVariationPages write FTotalVariationPages stored HasTotalVariationPages;
    property VariationDimensions : VariationDimensions_Type read FVariationDimensions write FVariationDimensions stored HasVariationDimensions;
    property _Item : Variations__ItemArray read F_Item write F_Item stored Has_Item;
  end;

  Collections_Collection_Type_CollectionSummary_Type = class(TBaseComplexRemotable)
  private
    FLowestListPrice : Price_Type;
    FHighestListPrice : Price_Type;
    FLowestSalePrice : Price_Type;
    FHighestSalePrice : Price_Type;
  private
    function HasLowestListPrice() : Boolean;
    function HasHighestListPrice() : Boolean;
    function HasLowestSalePrice() : Boolean;
    function HasHighestSalePrice() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property LowestListPrice : Price_Type read FLowestListPrice write FLowestListPrice stored HasLowestListPrice;
    property HighestListPrice : Price_Type read FHighestListPrice write FHighestListPrice stored HasHighestListPrice;
    property LowestSalePrice : Price_Type read FLowestSalePrice write FLowestSalePrice stored HasLowestSalePrice;
    property HighestSalePrice : Price_Type read FHighestSalePrice write FHighestSalePrice stored HasHighestSalePrice;
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
    function HasCollectionItem() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property CollectionSummary : Collections_Collection_Type_CollectionSummary_Type read FCollectionSummary write FCollectionSummary stored HasCollectionSummary;
    property CollectionParent : Collections_Collection_Type_CollectionParent_Type read FCollectionParent write FCollectionParent stored HasCollectionParent;
    property CollectionItem : Collections_Collection_Type_CollectionItemArray read FCollectionItem write FCollectionItem stored HasCollectionItem;
  end;

  EditorialReview_Type = class(TBaseComplexRemotable)
  private
    FSource : string;
    FContent : string;
    FIsLinkSuppressed : boolean;
  private
    function HasSource() : Boolean;
    function HasContent() : Boolean;
    function HasIsLinkSuppressed() : Boolean;
  published
    property Source : string read FSource write FSource stored HasSource;
    property Content : string read FContent write FContent stored HasContent;
    property IsLinkSuppressed : boolean read FIsLinkSuppressed write FIsLinkSuppressed stored HasIsLinkSuppressed;
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
    function HasReview() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property AverageRating : Extended read FAverageRating write FAverageRating stored HasAverageRating;
    property TotalReviews : nonNegativeInteger read FTotalReviews write FTotalReviews stored HasTotalReviews;
    property TotalReviewPages : nonNegativeInteger read FTotalReviewPages write FTotalReviewPages stored HasTotalReviewPages;
    property Review : CustomerReviews_ReviewArray read FReview write FReview stored HasReview;
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
    constructor Create();override;
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
    FEligibilityRequirements : PromotionEligibilityRequirements_Type;
    FBenefits : PromotionBenefits_Type;
    FItemApplicability : PromotionItemApplicability_Type;
    FMerchandisingMessage : string;
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
    function HasMerchandisingMessage() : Boolean;
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
    property EligibilityRequirements : PromotionEligibilityRequirements_Type read FEligibilityRequirements write FEligibilityRequirements stored HasEligibilityRequirements;
    property Benefits : PromotionBenefits_Type read FBenefits write FBenefits stored HasBenefits;
    property ItemApplicability : PromotionItemApplicability_Type read FItemApplicability write FItemApplicability stored HasItemApplicability;
    property MerchandisingMessage : string read FMerchandisingMessage write FMerchandisingMessage stored HasMerchandisingMessage;
  end;

  Promotion_Type = class(TBaseComplexRemotable)
  private
    FSummary : Promotion_Summary_Type;
    FDetails : Promotion_Details_Type;
  private
    function HasSummary() : Boolean;
    function HasDetails() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Summary : Promotion_Summary_Type read FSummary write FSummary stored HasSummary;
    property Details : Promotion_Details_Type read FDetails write FDetails stored HasDetails;
  end;

  PromotionBenefit_Type = class(TBaseComplexRemotable)
  private
    FBenefitType : string;
    FComponentType : string;
    FQuantity : integer;
    FPercentOff : Double;
    FFixedAmount : Price_Type;
    FCeiling : Price_Type;
  private
    function HasQuantity() : Boolean;
    function HasPercentOff() : Boolean;
    function HasFixedAmount() : Boolean;
    function HasCeiling() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property BenefitType : string read FBenefitType write FBenefitType;
    property ComponentType : string read FComponentType write FComponentType;
    property Quantity : integer read FQuantity write FQuantity stored HasQuantity;
    property PercentOff : Double read FPercentOff write FPercentOff stored HasPercentOff;
    property FixedAmount : Price_Type read FFixedAmount write FFixedAmount stored HasFixedAmount;
    property Ceiling : Price_Type read FCeiling write FCeiling stored HasCeiling;
  end;

  PromotionEligibilityRequirement_Type = class(TBaseComplexRemotable)
  private
    FEligibilityRequirementType : string;
    FQuantity : integer;
    FCurrencyAmount : Price_Type;
  private
    function HasQuantity() : Boolean;
    function HasCurrencyAmount() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property EligibilityRequirementType : string read FEligibilityRequirementType write FEligibilityRequirementType;
    property Quantity : integer read FQuantity write FQuantity stored HasQuantity;
    property CurrencyAmount : Price_Type read FCurrencyAmount write FCurrencyAmount stored HasCurrencyAmount;
  end;

  PromotionItemApplicability_Type = class(TBaseComplexRemotable)
  private
    FASIN : string;
    FIsInBenefitSet : boolean;
    FIsInEligibilityRequirementSet : boolean;
  published
    property ASIN : string read FASIN write FASIN;
    property IsInBenefitSet : boolean read FIsInBenefitSet write FIsInBenefitSet;
    property IsInEligibilityRequirementSet : boolean read FIsInEligibilityRequirementSet write FIsInEligibilityRequirementSet;
  end;

  BrowseNodes_Type = class(TBaseComplexRemotable)
  private
    FRequest : Request_Type;
    FBrowseNode : BrowseNodes_BrowseNodeArray;
  private
    function HasRequest() : Boolean;
    function HasBrowseNode() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Request : Request_Type read FRequest write FRequest stored HasRequest;
    property BrowseNode : BrowseNodes_BrowseNodeArray read FBrowseNode write FBrowseNode stored HasBrowseNode;
  end;

  Property_Type = class(TBaseComplexRemotable)
  private
    FName : string;
    FValue : string;
  private
    function HasName() : Boolean;
    function HasValue() : Boolean;
  published
    property Name : string read FName write FName stored HasName;
    property Value : string read FValue write FValue stored HasValue;
  end;

  BrowseNode_Type = class(TBaseComplexRemotable)
  private
    FBrowseNodeId : string;
    FName : string;
    FIsCategoryRoot : boolean;
    FProperties : BrowseNode_Properties_Type;
    FChildren : BrowseNode_Children_Type;
    FAncestors : BrowseNode_Ancestors_Type;
    FTopSellers : TopSellers_Type;
    FNewReleases : NewReleases_Type;
  private
    function HasBrowseNodeId() : Boolean;
    function HasName() : Boolean;
    function HasIsCategoryRoot() : Boolean;
    function HasProperties() : Boolean;
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
    property Properties : BrowseNode_Properties_Type read FProperties write FProperties stored HasProperties;
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

  SearchInside_Type = class(TBaseComplexRemotable)
  private
    FTotalExcerpts : nonNegativeInteger;
    FExcerpt : SearchInside_Excerpt_Type;
  private
    function HasTotalExcerpts() : Boolean;
    function HasExcerpt() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property TotalExcerpts : nonNegativeInteger read FTotalExcerpts write FTotalExcerpts stored HasTotalExcerpts;
    property Excerpt : SearchInside_Excerpt_Type read FExcerpt write FExcerpt stored HasExcerpt;
  end;

  CartItems_Type = class(TBaseComplexRemotable)
  private
    FSubTotal : Price_Type;
    FCartItem : CartItems_CartItemArray;
  private
    function HasSubTotal() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property SubTotal : Price_Type read FSubTotal write FSubTotal stored HasSubTotal;
    property CartItem : CartItems_CartItemArray read FCartItem write FCartItem;
  end;

  SavedForLaterItems_Type = class(TBaseComplexRemotable)
  private
    FSubTotal : Price_Type;
    FSavedForLaterItem : SavedForLaterItems_SavedForLaterItemArray;
  private
    function HasSubTotal() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property SubTotal : Price_Type read FSubTotal write FSubTotal stored HasSubTotal;
    property SavedForLaterItem : SavedForLaterItems_SavedForLaterItemArray read FSavedForLaterItem write FSavedForLaterItem;
  end;

  CartItem_MetaData_Type_KeyValuePair_Type = class(TBaseComplexRemotable)
  private
    FKey : string;
    FValue : string;
  published
    property Key : string read FKey write FKey;
    property Value : string read FValue write FValue;
  end;

  CartItem_Type = class(TBaseComplexRemotable)
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
    FMetaData : CartItem_MetaData_Type;
    FPrice : Price_Type;
    FItemTotal : Price_Type;
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
    function HasMetaData() : Boolean;
    function HasPrice() : Boolean;
    function HasItemTotal() : Boolean;
  public
    constructor Create();override;
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
    property MetaData : CartItem_MetaData_Type read FMetaData write FMetaData stored HasMetaData;
    property Price : Price_Type read FPrice write FPrice stored HasPrice;
    property ItemTotal : Price_Type read FItemTotal write FItemTotal stored HasItemTotal;
  end;

  Transaction_Totals_Type = class(TBaseComplexRemotable)
  private
    FTotal : Price_Type;
    FSubtotal : Price_Type;
    FTax : Price_Type;
    FShippingCharge : Price_Type;
    FPromotion : Price_Type;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Total : Price_Type read FTotal write FTotal;
    property Subtotal : Price_Type read FSubtotal write FSubtotal;
    property Tax : Price_Type read FTax write FTax;
    property ShippingCharge : Price_Type read FShippingCharge write FShippingCharge;
    property Promotion : Price_Type read FPromotion write FPromotion;
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

  TransactionItem_Type = class(TBaseComplexRemotable)
  private
    FTransactionItemId : string;
    FQuantity : string;
    FUnitPrice : Price_Type;
    FTotalPrice : Price_Type;
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
    property UnitPrice : Price_Type read FUnitPrice write FUnitPrice;
    property TotalPrice : Price_Type read FTotalPrice write FTotalPrice;
    property ASIN : string read FASIN write FASIN stored HasASIN;
    property ChildTransactionItems : TransactionItem_ChildTransactionItems_Type read FChildTransactionItems write FChildTransactionItems stored HasChildTransactionItems;
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
  private
    function HasSellerFeedbackRating() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property SellerFeedbackRating : Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray read FSellerFeedbackRating write FSellerFeedbackRating stored HasSellerFeedbackRating;
    property Period : string read FPeriod write FPeriod;
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

  Address_Type = class(TBaseComplexRemotable)
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
    FPrice : Price_Type;
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
    constructor Create();override;
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
    property Price : Price_Type read FPrice write FPrice stored HasPrice;
    property StartDate : string read FStartDate write FStartDate stored HasStartDate;
    property EndDate : string read FEndDate write FEndDate stored HasEndDate;
    property Status : string read FStatus write FStatus stored HasStatus;
    property Quantity : string read FQuantity write FQuantity stored HasQuantity;
    property Condition : string read FCondition write FCondition stored HasCondition;
    property SubCondition : string read FSubCondition write FSubCondition stored HasSubCondition;
    property Seller : Seller_Type read FSeller write FSeller stored HasSeller;
  end;

  Price_Type = class(TBaseComplexRemotable)
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

  ImageSet_Type = class(TBaseComplexRemotable)
  private
    FSwatchImage : Image_Type;
    FSmallImage : Image_Type;
    FThumbnailImage : Image_Type;
    FTinyImage : Image_Type;
    FMediumImage : Image_Type;
    FLargeImage : Image_Type;
    FCategory : string;
  private
    function HasSwatchImage() : Boolean;
    function HasSmallImage() : Boolean;
    function HasThumbnailImage() : Boolean;
    function HasTinyImage() : Boolean;
    function HasMediumImage() : Boolean;
    function HasLargeImage() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property SwatchImage : Image_Type read FSwatchImage write FSwatchImage stored HasSwatchImage;
    property SmallImage : Image_Type read FSmallImage write FSmallImage stored HasSmallImage;
    property ThumbnailImage : Image_Type read FThumbnailImage write FThumbnailImage stored HasThumbnailImage;
    property TinyImage : Image_Type read FTinyImage write FTinyImage stored HasTinyImage;
    property MediumImage : Image_Type read FMediumImage write FMediumImage stored HasMediumImage;
    property LargeImage : Image_Type read FLargeImage write FLargeImage stored HasLargeImage;
    property Category : string read FCategory write FCategory;
  end;

  Image_Type = class(TBaseComplexRemotable)
  private
    FURL : string;
    FHeight : DecimalWithUnits_Type;
    FWidth : DecimalWithUnits_Type;
    FIsVerified : string;
  private
    function HasIsVerified() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property URL : string read FURL write FURL;
    property Height : DecimalWithUnits_Type read FHeight write FHeight;
    property Width : DecimalWithUnits_Type read FWidth write FWidth;
    property IsVerified : string read FIsVerified write FIsVerified stored HasIsVerified;
  end;

  ItemAttributes_Creator_Type = class(TComplexStringContentRemotable)
  private
    FRole : string;
  published
    property Role : string read FRole write FRole;
  end;

  ItemAttributes_ItemDimensions_Type = class(TBaseComplexRemotable)
  private
    FHeight : DecimalWithUnits_Type;
    FLength : DecimalWithUnits_Type;
    FWeight : DecimalWithUnits_Type;
    FWidth : DecimalWithUnits_Type;
  private
    function HasHeight() : Boolean;
    function HasLength() : Boolean;
    function HasWeight() : Boolean;
    function HasWidth() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Height : DecimalWithUnits_Type read FHeight write FHeight stored HasHeight;
    property Length : DecimalWithUnits_Type read FLength write FLength stored HasLength;
    property Weight : DecimalWithUnits_Type read FWeight write FWeight stored HasWeight;
    property Width : DecimalWithUnits_Type read FWidth write FWidth stored HasWidth;
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
    FHeight : DecimalWithUnits_Type;
    FLength : DecimalWithUnits_Type;
    FWeight : DecimalWithUnits_Type;
    FWidth : DecimalWithUnits_Type;
  private
    function HasHeight() : Boolean;
    function HasLength() : Boolean;
    function HasWeight() : Boolean;
    function HasWidth() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Height : DecimalWithUnits_Type read FHeight write FHeight stored HasHeight;
    property Length : DecimalWithUnits_Type read FLength write FLength stored HasLength;
    property Weight : DecimalWithUnits_Type read FWeight write FWeight stored HasWeight;
    property Width : DecimalWithUnits_Type read FWidth write FWidth stored HasWidth;
  end;

  ItemAttributes_Type = class(TBaseComplexRemotable)
  private
    FActor : ItemAttributes_ActorArray;
    FAddress : Address_Type;
    FAge : ItemAttributes_AgeArray;
    FAmazonMaximumAge : DecimalWithUnits_Type;
    FAmazonMinimumAge : DecimalWithUnits_Type;
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
    FBatteries : NonNegativeIntegerWithUnits_Type;
    FBatteryDescription : string;
    FBatteryType : string;
    FBezelMaterialType : string;
    FBinding : string;
    FBrand : string;
    FCalendarType : string;
    FCameraManualFeatures : ItemAttributes_CameraManualFeaturesArray;
    FCaseDiameter : DecimalWithUnits_Type;
    FCaseMaterialType : string;
    FCaseThickness : DecimalWithUnits_Type;
    FCaseType : string;
    FCatalogNumber : string;
    FCategory : ItemAttributes_CategoryArray;
    FCategoryBin : ItemAttributes_CategoryBinArray;
    FCDRWDescription : string;
    FChainType : string;
    FCharacter : ItemAttributes_CharacterArray;
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
    FContinuousShootingSpeed : DecimalWithUnits_Type;
    FCountry : string;
    FCPUManufacturer : string;
    FCPUSpeed : DecimalWithUnits_Type;
    FCPUType : string;
    FCreator : ItemAttributes_CreatorArray;
    FCuisine : string;
    FDataLinkProtocol : ItemAttributes_DataLinkProtocolArray;
    FDeliveryOption : string;
    FDelayBetweenShots : DecimalWithUnits_Type;
    FDepartment : string;
    FDeweyDecimalNumber : string;
    FDialColor : string;
    FDialWindowMaterialType : string;
    FDigitalZoom : DecimalWithUnits_Type;
    FDirector : ItemAttributes_DirectorArray;
    FDisplayColorSupport : string;
    FDisplaySize : DecimalWithUnits_Type;
    FDrumSetPieceQuantity : nonNegativeInteger;
    FDVDLayers : nonNegativeInteger;
    FDVDRWDescription : string;
    FDVDSides : nonNegativeInteger;
    FDPCI : string;
    FEAN : string;
    FEdition : string;
    FEducationalFocus : ItemAttributes_EducationalFocusArray;
    FEthnicity : ItemAttributes_EthnicityArray;
    FESRBAgeRating : string;
    FExternalDisplaySupportDescription : string;
    FFabricType : string;
    FFaxNumber : string;
    FFeature : ItemAttributes_FeatureArray;
    FFilmColorType : string;
    FFirstIssueLeadTime : StringWithUnits_Type;
    FFlavorName : string;
    FFloppyDiskDriveDescription : string;
    FFormat : ItemAttributes_FormatArray;
    FFormFactor : ItemAttributes_FormFactorArray;
    FGemType : string;
    FGemTypeSetElement : ItemAttributes_GemTypeSetElementArray;
    FGender : ItemAttributes_GenderArray;
    FGenre : string;
    FGLProductGroup : string;
    FGolfClubFlex : string;
    FGolfClubLoft : string;
    FGraphicsCardInterface : string;
    FGraphicsDescription : string;
    FGraphicsMemorySize : DecimalWithUnits_Type;
    FGuitarAttribute : string;
    FGuitarBridgeSystem : string;
    FGuitarPickThickness : string;
    FGuitarPickupConfiguration : string;
    FHandOrientation : string;
    FHardDiskCount : nonNegativeInteger;
    FHardDiskSize : DecimalWithUnits_Type;
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
    FIngredientsSetElement : ItemAttributes_IngredientsSetElementArray;
    FInstrumentKey : string;
    FInterest : ItemAttributes_InterestArray;
    FIsAdultProduct : boolean;
    FIsAutographed : boolean;
    FISBN : string;
    FIsFragile : boolean;
    FIsLabCreated : boolean;
    FIsMemorabilia : boolean;
    FISOEquivalent : NonNegativeIntegerWithUnits_Type;
    FIsPreannounce : boolean;
    FIssuesPerYear : string;
    FItemDimensions : ItemAttributes_ItemDimensions_Type;
    FKeyboardDescription : string;
    F_Label : string;
    FLanguageName : ItemAttributes_LanguageNameArray;
    FLanguages : ItemAttributes_Languages_Type;
    FLegalDisclaimer : string;
    FLensType : string;
    FLineVoltage : string;
    FListPrice : Price_Type;
    FMacroFocusRange : string;
    FMagazineType : string;
    FMalletHardness : string;
    FManufacturer : string;
    FManufacturerLaborWarrantyDescription : string;
    FManufacturerMaximumAge : DecimalWithUnits_Type;
    FManufacturerMinimumAge : DecimalWithUnits_Type;
    FManufacturerPartsWarrantyDescription : string;
    FMaterialType : string;
    FMaterialTypeSetElement : ItemAttributes_MaterialTypeSetElementArray;
    FMaximumAperture : DecimalWithUnits_Type;
    FMaximumColorDepth : string;
    FMaximumFocalLength : DecimalWithUnits_Type;
    FMaximumHighResolutionImages : NonNegativeIntegerWithUnits_Type;
    FMaximumHorizontalResolution : NonNegativeIntegerWithUnits_Type;
    FMaximumLowResolutionImages : string;
    FMaximumResolution : DecimalWithUnits_Type;
    FMaximumShutterSpeed : DecimalWithUnits_Type;
    FMaximumVerticalResolution : NonNegativeIntegerWithUnits_Type;
    FMaximumWeightRecommendation : DecimalWithUnits_Type;
    FMediaType : string;
    FMemorySlotsAvailable : string;
    FMetalStamp : string;
    FMetalType : string;
    FMiniMovieDescription : string;
    FMinimumFocalLength : DecimalWithUnits_Type;
    FMinimumShutterSpeed : DecimalWithUnits_Type;
    FModel : string;
    FModelYear : nonNegativeInteger;
    FModemDescription : string;
    FMonitorSize : DecimalWithUnits_Type;
    FMonitorViewableDiagonalSize : DecimalWithUnits_Type;
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
    FOpticalSensorResolution : DecimalWithUnits_Type;
    FOpticalZoom : DecimalWithUnits_Type;
    FOriginalReleaseDate : string;
    FOutputWattage : nonNegativeInteger;
    FPackageDimensions : ItemAttributes_PackageDimensions_Type;
    FPackageQuantity : nonNegativeInteger;
    FPantLength : ItemAttributes_PantLengthArray;
    FPantSize : ItemAttributes_PantSizeArray;
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
    FPrimaryColor : ItemAttributes_PrimaryColorArray;
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
    FRunningTime : DecimalWithUnits_Type;
    FScentName : string;
    FSecondaryCacheSize : NonNegativeIntegerWithUnits_Type;
    FSettingType : string;
    FShaftMaterialType : string;
    FShoeSize : ItemAttributes_ShoeSizeArray;
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
    FStoneWeight : DecimalWithUnits_Type;
    FStudio : string;
    FStyle : string;
    FSubscriptionLength : NonNegativeIntegerWithUnits_Type;
    FSupportedImageType : ItemAttributes_SupportedImageTypeArray;
    FSupportedMediaSize : string;
    FSystemBusSpeed : DecimalWithUnits_Type;
    FSystemMemorySizeMax : DecimalWithUnits_Type;
    FSystemMemorySize : DecimalWithUnits_Type;
    FSystemMemoryType : string;
    FTargetBrand : ItemAttributes_TargetBrandArray;
    FTellingPageIndicator : string;
    FTheatricalReleaseDate : string;
    FTitle : string;
    FTotalDiamondWeight : DecimalWithUnits_Type;
    FTotalExternalBaysFree : nonNegativeInteger;
    FTotalFirewirePorts : nonNegativeInteger;
    FTotalGemWeight : DecimalWithUnits_Type;
    FTotalInternalBaysFree : nonNegativeInteger;
    FTotalMetalWeight : DecimalWithUnits_Type;
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
    FWaterResistanceDepth : DecimalWithUnits_Type;
    FWEEETaxValue : Price_Type;
    FWirelessMicrophoneFrequency : nonNegativeInteger;
  private
    function HasActor() : Boolean;
    function HasAddress() : Boolean;
    function HasAge() : Boolean;
    function HasAmazonMaximumAge() : Boolean;
    function HasAmazonMinimumAge() : Boolean;
    function HasAnalogVideoFormat() : Boolean;
    function HasApertureModes() : Boolean;
    function HasArtist() : Boolean;
    function HasAspectRatio() : Boolean;
    function HasAssemblyInstructions() : Boolean;
    function HasAssemblyRequired() : Boolean;
    function HasAudienceRating() : Boolean;
    function HasAudioFormat() : Boolean;
    function HasAuthor() : Boolean;
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
    function HasCameraManualFeatures() : Boolean;
    function HasCaseDiameter() : Boolean;
    function HasCaseMaterialType() : Boolean;
    function HasCaseThickness() : Boolean;
    function HasCaseType() : Boolean;
    function HasCatalogNumber() : Boolean;
    function HasCategory() : Boolean;
    function HasCategoryBin() : Boolean;
    function HasCDRWDescription() : Boolean;
    function HasChainType() : Boolean;
    function HasCharacter() : Boolean;
    function HasCEROAgeRating() : Boolean;
    function HasClaspType() : Boolean;
    function HasClothingSize() : Boolean;
    function HasClubType() : Boolean;
    function HasColor() : Boolean;
    function HasCompatibility() : Boolean;
    function HasCompatibleDevices() : Boolean;
    function HasComputerHardwareType() : Boolean;
    function HasComputerPlatform() : Boolean;
    function HasConnectivity() : Boolean;
    function HasContinuousShootingSpeed() : Boolean;
    function HasCountry() : Boolean;
    function HasCPUManufacturer() : Boolean;
    function HasCPUSpeed() : Boolean;
    function HasCPUType() : Boolean;
    function HasCreator() : Boolean;
    function HasCuisine() : Boolean;
    function HasDataLinkProtocol() : Boolean;
    function HasDeliveryOption() : Boolean;
    function HasDelayBetweenShots() : Boolean;
    function HasDepartment() : Boolean;
    function HasDeweyDecimalNumber() : Boolean;
    function HasDialColor() : Boolean;
    function HasDialWindowMaterialType() : Boolean;
    function HasDigitalZoom() : Boolean;
    function HasDirector() : Boolean;
    function HasDisplayColorSupport() : Boolean;
    function HasDisplaySize() : Boolean;
    function HasDrumSetPieceQuantity() : Boolean;
    function HasDVDLayers() : Boolean;
    function HasDVDRWDescription() : Boolean;
    function HasDVDSides() : Boolean;
    function HasDPCI() : Boolean;
    function HasEAN() : Boolean;
    function HasEdition() : Boolean;
    function HasEducationalFocus() : Boolean;
    function HasEthnicity() : Boolean;
    function HasESRBAgeRating() : Boolean;
    function HasExternalDisplaySupportDescription() : Boolean;
    function HasFabricType() : Boolean;
    function HasFaxNumber() : Boolean;
    function HasFeature() : Boolean;
    function HasFilmColorType() : Boolean;
    function HasFirstIssueLeadTime() : Boolean;
    function HasFlavorName() : Boolean;
    function HasFloppyDiskDriveDescription() : Boolean;
    function HasFormat() : Boolean;
    function HasFormFactor() : Boolean;
    function HasGemType() : Boolean;
    function HasGemTypeSetElement() : Boolean;
    function HasGender() : Boolean;
    function HasGenre() : Boolean;
    function HasGLProductGroup() : Boolean;
    function HasGolfClubFlex() : Boolean;
    function HasGolfClubLoft() : Boolean;
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
    function HasIngredientsSetElement() : Boolean;
    function HasInstrumentKey() : Boolean;
    function HasInterest() : Boolean;
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
    function HasLanguageName() : Boolean;
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
    function HasMaterialTypeSetElement() : Boolean;
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
    function HasPantLength() : Boolean;
    function HasPantSize() : Boolean;
    function HasPearlLustre() : Boolean;
    function HasPearlMinimumColor() : Boolean;
    function HasPearlShape() : Boolean;
    function HasPearlStringingMethod() : Boolean;
    function HasPearlSurfaceBlemishes() : Boolean;
    function HasPearlType() : Boolean;
    function HasPearlUniformity() : Boolean;
    function HasPhoneNumber() : Boolean;
    function HasPhotoFlashType() : Boolean;
    function HasPictureFormat() : Boolean;
    function HasPlatform() : Boolean;
    function HasPriceRating() : Boolean;
    function HasPrimaryColor() : Boolean;
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
    function HasReturnMethod() : Boolean;
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
    function HasShoeSize() : Boolean;
    function HasSize() : Boolean;
    function HasSizePerPearl() : Boolean;
    function HasSkillLevel() : Boolean;
    function HasSKU() : Boolean;
    function HasSoldInStores() : Boolean;
    function HasSoundCardDescription() : Boolean;
    function HasSpeakerCount() : Boolean;
    function HasSpeakerDescription() : Boolean;
    function HasSpecialFeatures() : Boolean;
    function HasStoneClarity() : Boolean;
    function HasStoneColor() : Boolean;
    function HasStoneCut() : Boolean;
    function HasStoneShape() : Boolean;
    function HasStoneWeight() : Boolean;
    function HasStudio() : Boolean;
    function HasStyle() : Boolean;
    function HasSubscriptionLength() : Boolean;
    function HasSupportedImageType() : Boolean;
    function HasSupportedMediaSize() : Boolean;
    function HasSystemBusSpeed() : Boolean;
    function HasSystemMemorySizeMax() : Boolean;
    function HasSystemMemorySize() : Boolean;
    function HasSystemMemoryType() : Boolean;
    function HasTargetBrand() : Boolean;
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
    property Actor : ItemAttributes_ActorArray read FActor write FActor stored HasActor;
    property Address : Address_Type read FAddress write FAddress stored HasAddress;
    property Age : ItemAttributes_AgeArray read FAge write FAge stored HasAge;
    property AmazonMaximumAge : DecimalWithUnits_Type read FAmazonMaximumAge write FAmazonMaximumAge stored HasAmazonMaximumAge;
    property AmazonMinimumAge : DecimalWithUnits_Type read FAmazonMinimumAge write FAmazonMinimumAge stored HasAmazonMinimumAge;
    property AnalogVideoFormat : string read FAnalogVideoFormat write FAnalogVideoFormat stored HasAnalogVideoFormat;
    property ApertureModes : string read FApertureModes write FApertureModes stored HasApertureModes;
    property Artist : ItemAttributes_ArtistArray read FArtist write FArtist stored HasArtist;
    property AspectRatio : string read FAspectRatio write FAspectRatio stored HasAspectRatio;
    property AssemblyInstructions : string read FAssemblyInstructions write FAssemblyInstructions stored HasAssemblyInstructions;
    property AssemblyRequired : string read FAssemblyRequired write FAssemblyRequired stored HasAssemblyRequired;
    property AudienceRating : string read FAudienceRating write FAudienceRating stored HasAudienceRating;
    property AudioFormat : ItemAttributes_AudioFormatArray read FAudioFormat write FAudioFormat stored HasAudioFormat;
    property Author : ItemAttributes_AuthorArray read FAuthor write FAuthor stored HasAuthor;
    property BackFinding : string read FBackFinding write FBackFinding stored HasBackFinding;
    property BandMaterialType : string read FBandMaterialType write FBandMaterialType stored HasBandMaterialType;
    property BatteriesIncluded : string read FBatteriesIncluded write FBatteriesIncluded stored HasBatteriesIncluded;
    property BatteriesRequired : string read FBatteriesRequired write FBatteriesRequired stored HasBatteriesRequired;
    property Batteries : NonNegativeIntegerWithUnits_Type read FBatteries write FBatteries stored HasBatteries;
    property BatteryDescription : string read FBatteryDescription write FBatteryDescription stored HasBatteryDescription;
    property BatteryType : string read FBatteryType write FBatteryType stored HasBatteryType;
    property BezelMaterialType : string read FBezelMaterialType write FBezelMaterialType stored HasBezelMaterialType;
    property Binding : string read FBinding write FBinding stored HasBinding;
    property Brand : string read FBrand write FBrand stored HasBrand;
    property CalendarType : string read FCalendarType write FCalendarType stored HasCalendarType;
    property CameraManualFeatures : ItemAttributes_CameraManualFeaturesArray read FCameraManualFeatures write FCameraManualFeatures stored HasCameraManualFeatures;
    property CaseDiameter : DecimalWithUnits_Type read FCaseDiameter write FCaseDiameter stored HasCaseDiameter;
    property CaseMaterialType : string read FCaseMaterialType write FCaseMaterialType stored HasCaseMaterialType;
    property CaseThickness : DecimalWithUnits_Type read FCaseThickness write FCaseThickness stored HasCaseThickness;
    property CaseType : string read FCaseType write FCaseType stored HasCaseType;
    property CatalogNumber : string read FCatalogNumber write FCatalogNumber stored HasCatalogNumber;
    property Category : ItemAttributes_CategoryArray read FCategory write FCategory stored HasCategory;
    property CategoryBin : ItemAttributes_CategoryBinArray read FCategoryBin write FCategoryBin stored HasCategoryBin;
    property CDRWDescription : string read FCDRWDescription write FCDRWDescription stored HasCDRWDescription;
    property ChainType : string read FChainType write FChainType stored HasChainType;
    property Character : ItemAttributes_CharacterArray read FCharacter write FCharacter stored HasCharacter;
    property CEROAgeRating : string read FCEROAgeRating write FCEROAgeRating stored HasCEROAgeRating;
    property ClaspType : string read FClaspType write FClaspType stored HasClaspType;
    property ClothingSize : string read FClothingSize write FClothingSize stored HasClothingSize;
    property ClubType : string read FClubType write FClubType stored HasClubType;
    property Color : string read FColor write FColor stored HasColor;
    property Compatibility : string read FCompatibility write FCompatibility stored HasCompatibility;
    property CompatibleDevices : ItemAttributes_CompatibleDevicesArray read FCompatibleDevices write FCompatibleDevices stored HasCompatibleDevices;
    property ComputerHardwareType : string read FComputerHardwareType write FComputerHardwareType stored HasComputerHardwareType;
    property ComputerPlatform : string read FComputerPlatform write FComputerPlatform stored HasComputerPlatform;
    property Connectivity : string read FConnectivity write FConnectivity stored HasConnectivity;
    property ContinuousShootingSpeed : DecimalWithUnits_Type read FContinuousShootingSpeed write FContinuousShootingSpeed stored HasContinuousShootingSpeed;
    property Country : string read FCountry write FCountry stored HasCountry;
    property CPUManufacturer : string read FCPUManufacturer write FCPUManufacturer stored HasCPUManufacturer;
    property CPUSpeed : DecimalWithUnits_Type read FCPUSpeed write FCPUSpeed stored HasCPUSpeed;
    property CPUType : string read FCPUType write FCPUType stored HasCPUType;
    property Creator : ItemAttributes_CreatorArray read FCreator write FCreator stored HasCreator;
    property Cuisine : string read FCuisine write FCuisine stored HasCuisine;
    property DataLinkProtocol : ItemAttributes_DataLinkProtocolArray read FDataLinkProtocol write FDataLinkProtocol stored HasDataLinkProtocol;
    property DeliveryOption : string read FDeliveryOption write FDeliveryOption stored HasDeliveryOption;
    property DelayBetweenShots : DecimalWithUnits_Type read FDelayBetweenShots write FDelayBetweenShots stored HasDelayBetweenShots;
    property Department : string read FDepartment write FDepartment stored HasDepartment;
    property DeweyDecimalNumber : string read FDeweyDecimalNumber write FDeweyDecimalNumber stored HasDeweyDecimalNumber;
    property DialColor : string read FDialColor write FDialColor stored HasDialColor;
    property DialWindowMaterialType : string read FDialWindowMaterialType write FDialWindowMaterialType stored HasDialWindowMaterialType;
    property DigitalZoom : DecimalWithUnits_Type read FDigitalZoom write FDigitalZoom stored HasDigitalZoom;
    property Director : ItemAttributes_DirectorArray read FDirector write FDirector stored HasDirector;
    property DisplayColorSupport : string read FDisplayColorSupport write FDisplayColorSupport stored HasDisplayColorSupport;
    property DisplaySize : DecimalWithUnits_Type read FDisplaySize write FDisplaySize stored HasDisplaySize;
    property DrumSetPieceQuantity : nonNegativeInteger read FDrumSetPieceQuantity write FDrumSetPieceQuantity stored HasDrumSetPieceQuantity;
    property DVDLayers : nonNegativeInteger read FDVDLayers write FDVDLayers stored HasDVDLayers;
    property DVDRWDescription : string read FDVDRWDescription write FDVDRWDescription stored HasDVDRWDescription;
    property DVDSides : nonNegativeInteger read FDVDSides write FDVDSides stored HasDVDSides;
    property DPCI : string read FDPCI write FDPCI stored HasDPCI;
    property EAN : string read FEAN write FEAN stored HasEAN;
    property Edition : string read FEdition write FEdition stored HasEdition;
    property EducationalFocus : ItemAttributes_EducationalFocusArray read FEducationalFocus write FEducationalFocus stored HasEducationalFocus;
    property Ethnicity : ItemAttributes_EthnicityArray read FEthnicity write FEthnicity stored HasEthnicity;
    property ESRBAgeRating : string read FESRBAgeRating write FESRBAgeRating stored HasESRBAgeRating;
    property ExternalDisplaySupportDescription : string read FExternalDisplaySupportDescription write FExternalDisplaySupportDescription stored HasExternalDisplaySupportDescription;
    property FabricType : string read FFabricType write FFabricType stored HasFabricType;
    property FaxNumber : string read FFaxNumber write FFaxNumber stored HasFaxNumber;
    property Feature : ItemAttributes_FeatureArray read FFeature write FFeature stored HasFeature;
    property FilmColorType : string read FFilmColorType write FFilmColorType stored HasFilmColorType;
    property FirstIssueLeadTime : StringWithUnits_Type read FFirstIssueLeadTime write FFirstIssueLeadTime stored HasFirstIssueLeadTime;
    property FlavorName : string read FFlavorName write FFlavorName stored HasFlavorName;
    property FloppyDiskDriveDescription : string read FFloppyDiskDriveDescription write FFloppyDiskDriveDescription stored HasFloppyDiskDriveDescription;
    property Format : ItemAttributes_FormatArray read FFormat write FFormat stored HasFormat;
    property FormFactor : ItemAttributes_FormFactorArray read FFormFactor write FFormFactor stored HasFormFactor;
    property GemType : string read FGemType write FGemType stored HasGemType;
    property GemTypeSetElement : ItemAttributes_GemTypeSetElementArray read FGemTypeSetElement write FGemTypeSetElement stored HasGemTypeSetElement;
    property Gender : ItemAttributes_GenderArray read FGender write FGender stored HasGender;
    property Genre : string read FGenre write FGenre stored HasGenre;
    property GLProductGroup : string read FGLProductGroup write FGLProductGroup stored HasGLProductGroup;
    property GolfClubFlex : string read FGolfClubFlex write FGolfClubFlex stored HasGolfClubFlex;
    property GolfClubLoft : string read FGolfClubLoft write FGolfClubLoft stored HasGolfClubLoft;
    property GraphicsCardInterface : string read FGraphicsCardInterface write FGraphicsCardInterface stored HasGraphicsCardInterface;
    property GraphicsDescription : string read FGraphicsDescription write FGraphicsDescription stored HasGraphicsDescription;
    property GraphicsMemorySize : DecimalWithUnits_Type read FGraphicsMemorySize write FGraphicsMemorySize stored HasGraphicsMemorySize;
    property GuitarAttribute : string read FGuitarAttribute write FGuitarAttribute stored HasGuitarAttribute;
    property GuitarBridgeSystem : string read FGuitarBridgeSystem write FGuitarBridgeSystem stored HasGuitarBridgeSystem;
    property GuitarPickThickness : string read FGuitarPickThickness write FGuitarPickThickness stored HasGuitarPickThickness;
    property GuitarPickupConfiguration : string read FGuitarPickupConfiguration write FGuitarPickupConfiguration stored HasGuitarPickupConfiguration;
    property HandOrientation : string read FHandOrientation write FHandOrientation stored HasHandOrientation;
    property HardDiskCount : nonNegativeInteger read FHardDiskCount write FHardDiskCount stored HasHardDiskCount;
    property HardDiskSize : DecimalWithUnits_Type read FHardDiskSize write FHardDiskSize stored HasHardDiskSize;
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
    property IngredientsSetElement : ItemAttributes_IngredientsSetElementArray read FIngredientsSetElement write FIngredientsSetElement stored HasIngredientsSetElement;
    property InstrumentKey : string read FInstrumentKey write FInstrumentKey stored HasInstrumentKey;
    property Interest : ItemAttributes_InterestArray read FInterest write FInterest stored HasInterest;
    property IsAdultProduct : boolean read FIsAdultProduct write FIsAdultProduct stored HasIsAdultProduct;
    property IsAutographed : boolean read FIsAutographed write FIsAutographed stored HasIsAutographed;
    property ISBN : string read FISBN write FISBN stored HasISBN;
    property IsFragile : boolean read FIsFragile write FIsFragile stored HasIsFragile;
    property IsLabCreated : boolean read FIsLabCreated write FIsLabCreated stored HasIsLabCreated;
    property IsMemorabilia : boolean read FIsMemorabilia write FIsMemorabilia stored HasIsMemorabilia;
    property ISOEquivalent : NonNegativeIntegerWithUnits_Type read FISOEquivalent write FISOEquivalent stored HasISOEquivalent;
    property IsPreannounce : boolean read FIsPreannounce write FIsPreannounce stored HasIsPreannounce;
    property IssuesPerYear : string read FIssuesPerYear write FIssuesPerYear stored HasIssuesPerYear;
    property ItemDimensions : ItemAttributes_ItemDimensions_Type read FItemDimensions write FItemDimensions stored HasItemDimensions;
    property KeyboardDescription : string read FKeyboardDescription write FKeyboardDescription stored HasKeyboardDescription;
    property _Label : string read F_Label write F_Label stored Has_Label;
    property LanguageName : ItemAttributes_LanguageNameArray read FLanguageName write FLanguageName stored HasLanguageName;
    property Languages : ItemAttributes_Languages_Type read FLanguages write FLanguages stored HasLanguages;
    property LegalDisclaimer : string read FLegalDisclaimer write FLegalDisclaimer stored HasLegalDisclaimer;
    property LensType : string read FLensType write FLensType stored HasLensType;
    property LineVoltage : string read FLineVoltage write FLineVoltage stored HasLineVoltage;
    property ListPrice : Price_Type read FListPrice write FListPrice stored HasListPrice;
    property MacroFocusRange : string read FMacroFocusRange write FMacroFocusRange stored HasMacroFocusRange;
    property MagazineType : string read FMagazineType write FMagazineType stored HasMagazineType;
    property MalletHardness : string read FMalletHardness write FMalletHardness stored HasMalletHardness;
    property Manufacturer : string read FManufacturer write FManufacturer stored HasManufacturer;
    property ManufacturerLaborWarrantyDescription : string read FManufacturerLaborWarrantyDescription write FManufacturerLaborWarrantyDescription stored HasManufacturerLaborWarrantyDescription;
    property ManufacturerMaximumAge : DecimalWithUnits_Type read FManufacturerMaximumAge write FManufacturerMaximumAge stored HasManufacturerMaximumAge;
    property ManufacturerMinimumAge : DecimalWithUnits_Type read FManufacturerMinimumAge write FManufacturerMinimumAge stored HasManufacturerMinimumAge;
    property ManufacturerPartsWarrantyDescription : string read FManufacturerPartsWarrantyDescription write FManufacturerPartsWarrantyDescription stored HasManufacturerPartsWarrantyDescription;
    property MaterialType : string read FMaterialType write FMaterialType stored HasMaterialType;
    property MaterialTypeSetElement : ItemAttributes_MaterialTypeSetElementArray read FMaterialTypeSetElement write FMaterialTypeSetElement stored HasMaterialTypeSetElement;
    property MaximumAperture : DecimalWithUnits_Type read FMaximumAperture write FMaximumAperture stored HasMaximumAperture;
    property MaximumColorDepth : string read FMaximumColorDepth write FMaximumColorDepth stored HasMaximumColorDepth;
    property MaximumFocalLength : DecimalWithUnits_Type read FMaximumFocalLength write FMaximumFocalLength stored HasMaximumFocalLength;
    property MaximumHighResolutionImages : NonNegativeIntegerWithUnits_Type read FMaximumHighResolutionImages write FMaximumHighResolutionImages stored HasMaximumHighResolutionImages;
    property MaximumHorizontalResolution : NonNegativeIntegerWithUnits_Type read FMaximumHorizontalResolution write FMaximumHorizontalResolution stored HasMaximumHorizontalResolution;
    property MaximumLowResolutionImages : string read FMaximumLowResolutionImages write FMaximumLowResolutionImages stored HasMaximumLowResolutionImages;
    property MaximumResolution : DecimalWithUnits_Type read FMaximumResolution write FMaximumResolution stored HasMaximumResolution;
    property MaximumShutterSpeed : DecimalWithUnits_Type read FMaximumShutterSpeed write FMaximumShutterSpeed stored HasMaximumShutterSpeed;
    property MaximumVerticalResolution : NonNegativeIntegerWithUnits_Type read FMaximumVerticalResolution write FMaximumVerticalResolution stored HasMaximumVerticalResolution;
    property MaximumWeightRecommendation : DecimalWithUnits_Type read FMaximumWeightRecommendation write FMaximumWeightRecommendation stored HasMaximumWeightRecommendation;
    property MediaType : string read FMediaType write FMediaType stored HasMediaType;
    property MemorySlotsAvailable : string read FMemorySlotsAvailable write FMemorySlotsAvailable stored HasMemorySlotsAvailable;
    property MetalStamp : string read FMetalStamp write FMetalStamp stored HasMetalStamp;
    property MetalType : string read FMetalType write FMetalType stored HasMetalType;
    property MiniMovieDescription : string read FMiniMovieDescription write FMiniMovieDescription stored HasMiniMovieDescription;
    property MinimumFocalLength : DecimalWithUnits_Type read FMinimumFocalLength write FMinimumFocalLength stored HasMinimumFocalLength;
    property MinimumShutterSpeed : DecimalWithUnits_Type read FMinimumShutterSpeed write FMinimumShutterSpeed stored HasMinimumShutterSpeed;
    property Model : string read FModel write FModel stored HasModel;
    property ModelYear : nonNegativeInteger read FModelYear write FModelYear stored HasModelYear;
    property ModemDescription : string read FModemDescription write FModemDescription stored HasModemDescription;
    property MonitorSize : DecimalWithUnits_Type read FMonitorSize write FMonitorSize stored HasMonitorSize;
    property MonitorViewableDiagonalSize : DecimalWithUnits_Type read FMonitorViewableDiagonalSize write FMonitorViewableDiagonalSize stored HasMonitorViewableDiagonalSize;
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
    property OpticalSensorResolution : DecimalWithUnits_Type read FOpticalSensorResolution write FOpticalSensorResolution stored HasOpticalSensorResolution;
    property OpticalZoom : DecimalWithUnits_Type read FOpticalZoom write FOpticalZoom stored HasOpticalZoom;
    property OriginalReleaseDate : string read FOriginalReleaseDate write FOriginalReleaseDate stored HasOriginalReleaseDate;
    property OutputWattage : nonNegativeInteger read FOutputWattage write FOutputWattage stored HasOutputWattage;
    property PackageDimensions : ItemAttributes_PackageDimensions_Type read FPackageDimensions write FPackageDimensions stored HasPackageDimensions;
    property PackageQuantity : nonNegativeInteger read FPackageQuantity write FPackageQuantity stored HasPackageQuantity;
    property PantLength : ItemAttributes_PantLengthArray read FPantLength write FPantLength stored HasPantLength;
    property PantSize : ItemAttributes_PantSizeArray read FPantSize write FPantSize stored HasPantSize;
    property PearlLustre : string read FPearlLustre write FPearlLustre stored HasPearlLustre;
    property PearlMinimumColor : string read FPearlMinimumColor write FPearlMinimumColor stored HasPearlMinimumColor;
    property PearlShape : string read FPearlShape write FPearlShape stored HasPearlShape;
    property PearlStringingMethod : string read FPearlStringingMethod write FPearlStringingMethod stored HasPearlStringingMethod;
    property PearlSurfaceBlemishes : string read FPearlSurfaceBlemishes write FPearlSurfaceBlemishes stored HasPearlSurfaceBlemishes;
    property PearlType : string read FPearlType write FPearlType stored HasPearlType;
    property PearlUniformity : string read FPearlUniformity write FPearlUniformity stored HasPearlUniformity;
    property PhoneNumber : string read FPhoneNumber write FPhoneNumber stored HasPhoneNumber;
    property PhotoFlashType : ItemAttributes_PhotoFlashTypeArray read FPhotoFlashType write FPhotoFlashType stored HasPhotoFlashType;
    property PictureFormat : ItemAttributes_PictureFormatArray read FPictureFormat write FPictureFormat stored HasPictureFormat;
    property Platform : ItemAttributes_PlatformArray read FPlatform write FPlatform stored HasPlatform;
    property PriceRating : nonNegativeInteger read FPriceRating write FPriceRating stored HasPriceRating;
    property PrimaryColor : ItemAttributes_PrimaryColorArray read FPrimaryColor write FPrimaryColor stored HasPrimaryColor;
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
    property ReturnMethod : ItemAttributes_ReturnMethodArray read FReturnMethod write FReturnMethod stored HasReturnMethod;
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
    property RunningTime : DecimalWithUnits_Type read FRunningTime write FRunningTime stored HasRunningTime;
    property ScentName : string read FScentName write FScentName stored HasScentName;
    property SecondaryCacheSize : NonNegativeIntegerWithUnits_Type read FSecondaryCacheSize write FSecondaryCacheSize stored HasSecondaryCacheSize;
    property SettingType : string read FSettingType write FSettingType stored HasSettingType;
    property ShaftMaterialType : string read FShaftMaterialType write FShaftMaterialType stored HasShaftMaterialType;
    property ShoeSize : ItemAttributes_ShoeSizeArray read FShoeSize write FShoeSize stored HasShoeSize;
    property Size : string read FSize write FSize stored HasSize;
    property SizePerPearl : string read FSizePerPearl write FSizePerPearl stored HasSizePerPearl;
    property SkillLevel : string read FSkillLevel write FSkillLevel stored HasSkillLevel;
    property SKU : string read FSKU write FSKU stored HasSKU;
    property SoldInStores : string read FSoldInStores write FSoldInStores stored HasSoldInStores;
    property SoundCardDescription : string read FSoundCardDescription write FSoundCardDescription stored HasSoundCardDescription;
    property SpeakerCount : nonNegativeInteger read FSpeakerCount write FSpeakerCount stored HasSpeakerCount;
    property SpeakerDescription : string read FSpeakerDescription write FSpeakerDescription stored HasSpeakerDescription;
    property SpecialFeatures : ItemAttributes_SpecialFeaturesArray read FSpecialFeatures write FSpecialFeatures stored HasSpecialFeatures;
    property StoneClarity : string read FStoneClarity write FStoneClarity stored HasStoneClarity;
    property StoneColor : string read FStoneColor write FStoneColor stored HasStoneColor;
    property StoneCut : string read FStoneCut write FStoneCut stored HasStoneCut;
    property StoneShape : string read FStoneShape write FStoneShape stored HasStoneShape;
    property StoneWeight : DecimalWithUnits_Type read FStoneWeight write FStoneWeight stored HasStoneWeight;
    property Studio : string read FStudio write FStudio stored HasStudio;
    property Style : string read FStyle write FStyle stored HasStyle;
    property SubscriptionLength : NonNegativeIntegerWithUnits_Type read FSubscriptionLength write FSubscriptionLength stored HasSubscriptionLength;
    property SupportedImageType : ItemAttributes_SupportedImageTypeArray read FSupportedImageType write FSupportedImageType stored HasSupportedImageType;
    property SupportedMediaSize : string read FSupportedMediaSize write FSupportedMediaSize stored HasSupportedMediaSize;
    property SystemBusSpeed : DecimalWithUnits_Type read FSystemBusSpeed write FSystemBusSpeed stored HasSystemBusSpeed;
    property SystemMemorySizeMax : DecimalWithUnits_Type read FSystemMemorySizeMax write FSystemMemorySizeMax stored HasSystemMemorySizeMax;
    property SystemMemorySize : DecimalWithUnits_Type read FSystemMemorySize write FSystemMemorySize stored HasSystemMemorySize;
    property SystemMemoryType : string read FSystemMemoryType write FSystemMemoryType stored HasSystemMemoryType;
    property TargetBrand : ItemAttributes_TargetBrandArray read FTargetBrand write FTargetBrand stored HasTargetBrand;
    property TellingPageIndicator : string read FTellingPageIndicator write FTellingPageIndicator stored HasTellingPageIndicator;
    property TheatricalReleaseDate : string read FTheatricalReleaseDate write FTheatricalReleaseDate stored HasTheatricalReleaseDate;
    property Title : string read FTitle write FTitle stored HasTitle;
    property TotalDiamondWeight : DecimalWithUnits_Type read FTotalDiamondWeight write FTotalDiamondWeight stored HasTotalDiamondWeight;
    property TotalExternalBaysFree : nonNegativeInteger read FTotalExternalBaysFree write FTotalExternalBaysFree stored HasTotalExternalBaysFree;
    property TotalFirewirePorts : nonNegativeInteger read FTotalFirewirePorts write FTotalFirewirePorts stored HasTotalFirewirePorts;
    property TotalGemWeight : DecimalWithUnits_Type read FTotalGemWeight write FTotalGemWeight stored HasTotalGemWeight;
    property TotalInternalBaysFree : nonNegativeInteger read FTotalInternalBaysFree write FTotalInternalBaysFree stored HasTotalInternalBaysFree;
    property TotalMetalWeight : DecimalWithUnits_Type read FTotalMetalWeight write FTotalMetalWeight stored HasTotalMetalWeight;
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
    property WaterResistanceDepth : DecimalWithUnits_Type read FWaterResistanceDepth write FWaterResistanceDepth stored HasWaterResistanceDepth;
    property WEEETaxValue : Price_Type read FWEEETaxValue write FWEEETaxValue stored HasWEEETaxValue;
    property WirelessMicrophoneFrequency : nonNegativeInteger read FWirelessMicrophoneFrequency write FWirelessMicrophoneFrequency stored HasWirelessMicrophoneFrequency;
  end;

  MerchantItemAttributes_Creator_Type = class(TComplexStringContentRemotable)
  private
    FRole : string;
  published
    property Role : string read FRole write FRole;
  end;

  MerchantItemAttributes_ItemDimensions_Type = class(TBaseComplexRemotable)
  private
    FHeight : DecimalWithUnits_Type;
    FLength : DecimalWithUnits_Type;
    FWeight : DecimalWithUnits_Type;
    FWidth : DecimalWithUnits_Type;
  private
    function HasHeight() : Boolean;
    function HasLength() : Boolean;
    function HasWeight() : Boolean;
    function HasWidth() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Height : DecimalWithUnits_Type read FHeight write FHeight stored HasHeight;
    property Length : DecimalWithUnits_Type read FLength write FLength stored HasLength;
    property Weight : DecimalWithUnits_Type read FWeight write FWeight stored HasWeight;
    property Width : DecimalWithUnits_Type read FWidth write FWidth stored HasWidth;
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
    FHeight : DecimalWithUnits_Type;
    FLength : DecimalWithUnits_Type;
    FWeight : DecimalWithUnits_Type;
    FWidth : DecimalWithUnits_Type;
  private
    function HasHeight() : Boolean;
    function HasLength() : Boolean;
    function HasWeight() : Boolean;
    function HasWidth() : Boolean;
  public
    constructor Create();override;
    destructor Destroy();override;
  published
    property Height : DecimalWithUnits_Type read FHeight write FHeight stored HasHeight;
    property Length : DecimalWithUnits_Type read FLength write FLength stored HasLength;
    property Weight : DecimalWithUnits_Type read FWeight write FWeight stored HasWeight;
    property Width : DecimalWithUnits_Type read FWidth write FWidth stored HasWidth;
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

  MerchantItemAttributes_Type = class(TBaseComplexRemotable)
  private
    FActor : MerchantItemAttributes_ActorArray;
    FAddress : Address_Type;
    FAmazonMaximumAge : DecimalWithUnits_Type;
    FAmazonMinimumAge : DecimalWithUnits_Type;
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
    FBatteries : NonNegativeIntegerWithUnits_Type;
    FBatteryDescription : string;
    FBatteryType : string;
    FBezelMaterialType : string;
    FBinding : string;
    FBrand : string;
    FCalendarType : string;
    FCameraManualFeatures : MerchantItemAttributes_CameraManualFeaturesArray;
    FCaseDiameter : DecimalWithUnits_Type;
    FCaseMaterialType : string;
    FCaseThickness : DecimalWithUnits_Type;
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
    FContinuousShootingSpeed : DecimalWithUnits_Type;
    FCountry : string;
    FCountryOfOrigin : string;
    FCPUManufacturer : string;
    FCPUSpeed : DecimalWithUnits_Type;
    FCPUType : string;
    FCreator : MerchantItemAttributes_CreatorArray;
    FCuisine : string;
    FCustomizable : string;
    FDelayBetweenShots : DecimalWithUnits_Type;
    FDeliveryOption : string;
    FDepartment : string;
    FDescription : string;
    FDeweyDecimalNumber : string;
    FDialColor : string;
    FDialWindowMaterialType : string;
    FDigitalZoom : DecimalWithUnits_Type;
    FDirector : MerchantItemAttributes_DirectorArray;
    FDisplaySize : DecimalWithUnits_Type;
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
    FFirstIssueLeadTime : StringWithUnits_Type;
    FFloppyDiskDriveDescription : string;
    FFormat : MerchantItemAttributes_FormatArray;
    FFixedShippingCharge : Price_Type;
    FGemType : string;
    FGraphicsCardInterface : string;
    FGraphicsDescription : string;
    FGraphicsMemorySize : DecimalWithUnits_Type;
    FGuitarAttribute : string;
    FGuitarBridgeSystem : string;
    FGuitarPickThickness : string;
    FGuitarPickupConfiguration : string;
    FHardDiskCount : nonNegativeInteger;
    FHardDiskSize : NonNegativeIntegerWithUnits_Type;
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
    FISOEquivalent : NonNegativeIntegerWithUnits_Type;
    FIssuesPerYear : string;
    FItemDimensions : MerchantItemAttributes_ItemDimensions_Type;
    FKeyboardDescription : string;
    F_Label : string;
    FLanguages : MerchantItemAttributes_Languages_Type;
    FLegalDisclaimer : string;
    FLineVoltage : string;
    FListPrice : Price_Type;
    FMacroFocusRange : string;
    FMagazineType : string;
    FMalletHardness : string;
    FManufacturer : string;
    FManufacturerLaborWarrantyDescription : string;
    FManufacturerMaximumAge : DecimalWithUnits_Type;
    FManufacturerMinimumAge : DecimalWithUnits_Type;
    FManufacturerPartsWarrantyDescription : string;
    FMaterialType : string;
    FMaximumAperture : DecimalWithUnits_Type;
    FMaximumColorDepth : string;
    FMaximumFocalLength : DecimalWithUnits_Type;
    FMaximumHighResolutionImages : NonNegativeIntegerWithUnits_Type;
    FMaximumHorizontalResolution : NonNegativeIntegerWithUnits_Type;
    FMaximumLowResolutionImages : string;
    FMaximumResolution : DecimalWithUnits_Type;
    FMaximumShutterSpeed : DecimalWithUnits_Type;
    FMaximumVerticalResolution : NonNegativeIntegerWithUnits_Type;
    FMaximumWeightRecommendation : DecimalWithUnits_Type;
    FMemorySlotsAvailable : nonNegativeInteger;
    FMetalStamp : string;
    FMetalType : string;
    FMiniMovieDescription : string;
    FMinimumFocalLength : DecimalWithUnits_Type;
    FMinimumShutterSpeed : DecimalWithUnits_Type;
    FModel : string;
    FModelYear : nonNegativeInteger;
    FModemDescription : string;
    FMonitorSize : DecimalWithUnits_Type;
    FMonitorViewableDiagonalSize : DecimalWithUnits_Type;
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
    FOpticalZoom : DecimalWithUnits_Type;
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
    FSecondaryCacheSize : NonNegativeIntegerWithUnits_Type;
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
    FStoneWeight : DecimalWithUnits_Type;
    FStudio : string;
    FSubscriptionLength : NonNegativeIntegerWithUnits_Type;
    FSupportedImageType : MerchantItemAttributes_SupportedImageTypeArray;
    FSystemBusSpeed : DecimalWithUnits_Type;
    FSystemMemorySizeMax : DecimalWithUnits_Type;
    FSystemMemorySize : DecimalWithUnits_Type;
    FSystemMemoryType : string;
    FTellingPageIndicator : string;
    FTheatricalReleaseDate : string;
    FTitle : string;
    FTotalDiamondWeight : DecimalWithUnits_Type;
    FTotalExternalBaysFree : nonNegativeInteger;
    FTotalFirewirePorts : nonNegativeInteger;
    FTotalGemWeight : DecimalWithUnits_Type;
    FTotalInternalBaysFree : nonNegativeInteger;
    FTotalMetalWeight : DecimalWithUnits_Type;
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
    FWaterResistanceDepth : DecimalWithUnits_Type;
    FWirelessMicrophoneFrequency : nonNegativeInteger;
  private
    function HasActor() : Boolean;
    function HasAddress() : Boolean;
    function HasAmazonMaximumAge() : Boolean;
    function HasAmazonMinimumAge() : Boolean;
    function HasApertureModes() : Boolean;
    function HasArtist() : Boolean;
    function HasAspectRatio() : Boolean;
    function HasAssemblyInstructions() : Boolean;
    function HasAssemblyRequired() : Boolean;
    function HasAudienceRating() : Boolean;
    function HasAudioFormat() : Boolean;
    function HasAuthor() : Boolean;
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
    function HasCameraManualFeatures() : Boolean;
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
    function HasCreator() : Boolean;
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
    function HasDirector() : Boolean;
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
    function HasFeature() : Boolean;
    function HasFirstIssueLeadTime() : Boolean;
    function HasFloppyDiskDriveDescription() : Boolean;
    function HasFormat() : Boolean;
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
    function HasPhotoFlashType() : Boolean;
    function HasPictureFormat() : Boolean;
    function HasPlatform() : Boolean;
    function HasPriceRating() : Boolean;
    function HasProcessorCount() : Boolean;
    function HasProductGroup() : Boolean;
    function HasPromotionalTag() : Boolean;
    function HasPOBoxShippingExcluded() : Boolean;
    function HasPublicationDate() : Boolean;
    function HasPublisher() : Boolean;
    function HasPurchasingChannel() : Boolean;
    function HasReadingLevel() : Boolean;
    function HasRecorderTrackCount() : Boolean;
    function HasRegionCode() : Boolean;
    function HasRegionOfOrigin() : Boolean;
    function HasReleaseDate() : Boolean;
    function HasReturnMethod() : Boolean;
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
    function HasSpecialFeatures() : Boolean;
    function HasStoneClarity() : Boolean;
    function HasStoneColor() : Boolean;
    function HasStoneCut() : Boolean;
    function HasStoneShape() : Boolean;
    function HasStoneWeight() : Boolean;
    function HasStudio() : Boolean;
    function HasSubscriptionLength() : Boolean;
    function HasSupportedImageType() : Boolean;
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
    property Actor : MerchantItemAttributes_ActorArray read FActor write FActor stored HasActor;
    property Address : Address_Type read FAddress write FAddress stored HasAddress;
    property AmazonMaximumAge : DecimalWithUnits_Type read FAmazonMaximumAge write FAmazonMaximumAge stored HasAmazonMaximumAge;
    property AmazonMinimumAge : DecimalWithUnits_Type read FAmazonMinimumAge write FAmazonMinimumAge stored HasAmazonMinimumAge;
    property ApertureModes : string read FApertureModes write FApertureModes stored HasApertureModes;
    property Artist : MerchantItemAttributes_ArtistArray read FArtist write FArtist stored HasArtist;
    property AspectRatio : string read FAspectRatio write FAspectRatio stored HasAspectRatio;
    property AssemblyInstructions : string read FAssemblyInstructions write FAssemblyInstructions stored HasAssemblyInstructions;
    property AssemblyRequired : string read FAssemblyRequired write FAssemblyRequired stored HasAssemblyRequired;
    property AudienceRating : string read FAudienceRating write FAudienceRating stored HasAudienceRating;
    property AudioFormat : MerchantItemAttributes_AudioFormatArray read FAudioFormat write FAudioFormat stored HasAudioFormat;
    property Author : MerchantItemAttributes_AuthorArray read FAuthor write FAuthor stored HasAuthor;
    property BackFinding : string read FBackFinding write FBackFinding stored HasBackFinding;
    property BandMaterialType : string read FBandMaterialType write FBandMaterialType stored HasBandMaterialType;
    property BatteriesIncluded : string read FBatteriesIncluded write FBatteriesIncluded stored HasBatteriesIncluded;
    property BatteriesRequired : string read FBatteriesRequired write FBatteriesRequired stored HasBatteriesRequired;
    property Batteries : NonNegativeIntegerWithUnits_Type read FBatteries write FBatteries stored HasBatteries;
    property BatteryDescription : string read FBatteryDescription write FBatteryDescription stored HasBatteryDescription;
    property BatteryType : string read FBatteryType write FBatteryType stored HasBatteryType;
    property BezelMaterialType : string read FBezelMaterialType write FBezelMaterialType stored HasBezelMaterialType;
    property Binding : string read FBinding write FBinding stored HasBinding;
    property Brand : string read FBrand write FBrand stored HasBrand;
    property CalendarType : string read FCalendarType write FCalendarType stored HasCalendarType;
    property CameraManualFeatures : MerchantItemAttributes_CameraManualFeaturesArray read FCameraManualFeatures write FCameraManualFeatures stored HasCameraManualFeatures;
    property CaseDiameter : DecimalWithUnits_Type read FCaseDiameter write FCaseDiameter stored HasCaseDiameter;
    property CaseMaterialType : string read FCaseMaterialType write FCaseMaterialType stored HasCaseMaterialType;
    property CaseThickness : DecimalWithUnits_Type read FCaseThickness write FCaseThickness stored HasCaseThickness;
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
    property ContinuousShootingSpeed : DecimalWithUnits_Type read FContinuousShootingSpeed write FContinuousShootingSpeed stored HasContinuousShootingSpeed;
    property Country : string read FCountry write FCountry stored HasCountry;
    property CountryOfOrigin : string read FCountryOfOrigin write FCountryOfOrigin stored HasCountryOfOrigin;
    property CPUManufacturer : string read FCPUManufacturer write FCPUManufacturer stored HasCPUManufacturer;
    property CPUSpeed : DecimalWithUnits_Type read FCPUSpeed write FCPUSpeed stored HasCPUSpeed;
    property CPUType : string read FCPUType write FCPUType stored HasCPUType;
    property Creator : MerchantItemAttributes_CreatorArray read FCreator write FCreator stored HasCreator;
    property Cuisine : string read FCuisine write FCuisine stored HasCuisine;
    property Customizable : string read FCustomizable write FCustomizable stored HasCustomizable;
    property DelayBetweenShots : DecimalWithUnits_Type read FDelayBetweenShots write FDelayBetweenShots stored HasDelayBetweenShots;
    property DeliveryOption : string read FDeliveryOption write FDeliveryOption stored HasDeliveryOption;
    property Department : string read FDepartment write FDepartment stored HasDepartment;
    property Description : string read FDescription write FDescription stored HasDescription;
    property DeweyDecimalNumber : string read FDeweyDecimalNumber write FDeweyDecimalNumber stored HasDeweyDecimalNumber;
    property DialColor : string read FDialColor write FDialColor stored HasDialColor;
    property DialWindowMaterialType : string read FDialWindowMaterialType write FDialWindowMaterialType stored HasDialWindowMaterialType;
    property DigitalZoom : DecimalWithUnits_Type read FDigitalZoom write FDigitalZoom stored HasDigitalZoom;
    property Director : MerchantItemAttributes_DirectorArray read FDirector write FDirector stored HasDirector;
    property DisplaySize : DecimalWithUnits_Type read FDisplaySize write FDisplaySize stored HasDisplaySize;
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
    property Feature : MerchantItemAttributes_FeatureArray read FFeature write FFeature stored HasFeature;
    property FirstIssueLeadTime : StringWithUnits_Type read FFirstIssueLeadTime write FFirstIssueLeadTime stored HasFirstIssueLeadTime;
    property FloppyDiskDriveDescription : string read FFloppyDiskDriveDescription write FFloppyDiskDriveDescription stored HasFloppyDiskDriveDescription;
    property Format : MerchantItemAttributes_FormatArray read FFormat write FFormat stored HasFormat;
    property FixedShippingCharge : Price_Type read FFixedShippingCharge write FFixedShippingCharge stored HasFixedShippingCharge;
    property GemType : string read FGemType write FGemType stored HasGemType;
    property GraphicsCardInterface : string read FGraphicsCardInterface write FGraphicsCardInterface stored HasGraphicsCardInterface;
    property GraphicsDescription : string read FGraphicsDescription write FGraphicsDescription stored HasGraphicsDescription;
    property GraphicsMemorySize : DecimalWithUnits_Type read FGraphicsMemorySize write FGraphicsMemorySize stored HasGraphicsMemorySize;
    property GuitarAttribute : string read FGuitarAttribute write FGuitarAttribute stored HasGuitarAttribute;
    property GuitarBridgeSystem : string read FGuitarBridgeSystem write FGuitarBridgeSystem stored HasGuitarBridgeSystem;
    property GuitarPickThickness : string read FGuitarPickThickness write FGuitarPickThickness stored HasGuitarPickThickness;
    property GuitarPickupConfiguration : string read FGuitarPickupConfiguration write FGuitarPickupConfiguration stored HasGuitarPickupConfiguration;
    property HardDiskCount : nonNegativeInteger read FHardDiskCount write FHardDiskCount stored HasHardDiskCount;
    property HardDiskSize : NonNegativeIntegerWithUnits_Type read FHardDiskSize write FHardDiskSize stored HasHardDiskSize;
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
    property ISOEquivalent : NonNegativeIntegerWithUnits_Type read FISOEquivalent write FISOEquivalent stored HasISOEquivalent;
    property IssuesPerYear : string read FIssuesPerYear write FIssuesPerYear stored HasIssuesPerYear;
    property ItemDimensions : MerchantItemAttributes_ItemDimensions_Type read FItemDimensions write FItemDimensions stored HasItemDimensions;
    property KeyboardDescription : string read FKeyboardDescription write FKeyboardDescription stored HasKeyboardDescription;
    property _Label : string read F_Label write F_Label stored Has_Label;
    property Languages : MerchantItemAttributes_Languages_Type read FLanguages write FLanguages stored HasLanguages;
    property LegalDisclaimer : string read FLegalDisclaimer write FLegalDisclaimer stored HasLegalDisclaimer;
    property LineVoltage : string read FLineVoltage write FLineVoltage stored HasLineVoltage;
    property ListPrice : Price_Type read FListPrice write FListPrice stored HasListPrice;
    property MacroFocusRange : string read FMacroFocusRange write FMacroFocusRange stored HasMacroFocusRange;
    property MagazineType : string read FMagazineType write FMagazineType stored HasMagazineType;
    property MalletHardness : string read FMalletHardness write FMalletHardness stored HasMalletHardness;
    property Manufacturer : string read FManufacturer write FManufacturer stored HasManufacturer;
    property ManufacturerLaborWarrantyDescription : string read FManufacturerLaborWarrantyDescription write FManufacturerLaborWarrantyDescription stored HasManufacturerLaborWarrantyDescription;
    property ManufacturerMaximumAge : DecimalWithUnits_Type read FManufacturerMaximumAge write FManufacturerMaximumAge stored HasManufacturerMaximumAge;
    property ManufacturerMinimumAge : DecimalWithUnits_Type read FManufacturerMinimumAge write FManufacturerMinimumAge stored HasManufacturerMinimumAge;
    property ManufacturerPartsWarrantyDescription : string read FManufacturerPartsWarrantyDescription write FManufacturerPartsWarrantyDescription stored HasManufacturerPartsWarrantyDescription;
    property MaterialType : string read FMaterialType write FMaterialType stored HasMaterialType;
    property MaximumAperture : DecimalWithUnits_Type read FMaximumAperture write FMaximumAperture stored HasMaximumAperture;
    property MaximumColorDepth : string read FMaximumColorDepth write FMaximumColorDepth stored HasMaximumColorDepth;
    property MaximumFocalLength : DecimalWithUnits_Type read FMaximumFocalLength write FMaximumFocalLength stored HasMaximumFocalLength;
    property MaximumHighResolutionImages : NonNegativeIntegerWithUnits_Type read FMaximumHighResolutionImages write FMaximumHighResolutionImages stored HasMaximumHighResolutionImages;
    property MaximumHorizontalResolution : NonNegativeIntegerWithUnits_Type read FMaximumHorizontalResolution write FMaximumHorizontalResolution stored HasMaximumHorizontalResolution;
    property MaximumLowResolutionImages : string read FMaximumLowResolutionImages write FMaximumLowResolutionImages stored HasMaximumLowResolutionImages;
    property MaximumResolution : DecimalWithUnits_Type read FMaximumResolution write FMaximumResolution stored HasMaximumResolution;
    property MaximumShutterSpeed : DecimalWithUnits_Type read FMaximumShutterSpeed write FMaximumShutterSpeed stored HasMaximumShutterSpeed;
    property MaximumVerticalResolution : NonNegativeIntegerWithUnits_Type read FMaximumVerticalResolution write FMaximumVerticalResolution stored HasMaximumVerticalResolution;
    property MaximumWeightRecommendation : DecimalWithUnits_Type read FMaximumWeightRecommendation write FMaximumWeightRecommendation stored HasMaximumWeightRecommendation;
    property MemorySlotsAvailable : nonNegativeInteger read FMemorySlotsAvailable write FMemorySlotsAvailable stored HasMemorySlotsAvailable;
    property MetalStamp : string read FMetalStamp write FMetalStamp stored HasMetalStamp;
    property MetalType : string read FMetalType write FMetalType stored HasMetalType;
    property MiniMovieDescription : string read FMiniMovieDescription write FMiniMovieDescription stored HasMiniMovieDescription;
    property MinimumFocalLength : DecimalWithUnits_Type read FMinimumFocalLength write FMinimumFocalLength stored HasMinimumFocalLength;
    property MinimumShutterSpeed : DecimalWithUnits_Type read FMinimumShutterSpeed write FMinimumShutterSpeed stored HasMinimumShutterSpeed;
    property Model : string read FModel write FModel stored HasModel;
    property ModelYear : nonNegativeInteger read FModelYear write FModelYear stored HasModelYear;
    property ModemDescription : string read FModemDescription write FModemDescription stored HasModemDescription;
    property MonitorSize : DecimalWithUnits_Type read FMonitorSize write FMonitorSize stored HasMonitorSize;
    property MonitorViewableDiagonalSize : DecimalWithUnits_Type read FMonitorViewableDiagonalSize write FMonitorViewableDiagonalSize stored HasMonitorViewableDiagonalSize;
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
    property OpticalZoom : DecimalWithUnits_Type read FOpticalZoom write FOpticalZoom stored HasOpticalZoom;
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
    property PhotoFlashType : MerchantItemAttributes_PhotoFlashTypeArray read FPhotoFlashType write FPhotoFlashType stored HasPhotoFlashType;
    property PictureFormat : MerchantItemAttributes_PictureFormatArray read FPictureFormat write FPictureFormat stored HasPictureFormat;
    property Platform : MerchantItemAttributes_PlatformArray read FPlatform write FPlatform stored HasPlatform;
    property PriceRating : nonNegativeInteger read FPriceRating write FPriceRating stored HasPriceRating;
    property ProcessorCount : nonNegativeInteger read FProcessorCount write FProcessorCount stored HasProcessorCount;
    property ProductGroup : string read FProductGroup write FProductGroup stored HasProductGroup;
    property PromotionalTag : string read FPromotionalTag write FPromotionalTag stored HasPromotionalTag;
    property POBoxShippingExcluded : string read FPOBoxShippingExcluded write FPOBoxShippingExcluded stored HasPOBoxShippingExcluded;
    property PublicationDate : string read FPublicationDate write FPublicationDate stored HasPublicationDate;
    property Publisher : string read FPublisher write FPublisher stored HasPublisher;
    property PurchasingChannel : MerchantItemAttributes_PurchasingChannelArray read FPurchasingChannel write FPurchasingChannel stored HasPurchasingChannel;
    property ReadingLevel : string read FReadingLevel write FReadingLevel stored HasReadingLevel;
    property RecorderTrackCount : nonNegativeInteger read FRecorderTrackCount write FRecorderTrackCount stored HasRecorderTrackCount;
    property RegionCode : string read FRegionCode write FRegionCode stored HasRegionCode;
    property RegionOfOrigin : string read FRegionOfOrigin write FRegionOfOrigin stored HasRegionOfOrigin;
    property ReleaseDate : string read FReleaseDate write FReleaseDate stored HasReleaseDate;
    property ReturnMethod : MerchantItemAttributes_ReturnMethodArray read FReturnMethod write FReturnMethod stored HasReturnMethod;
    property RemovableMemory : string read FRemovableMemory write FRemovableMemory stored HasRemovableMemory;
    property ResolutionModes : string read FResolutionModes write FResolutionModes stored HasResolutionModes;
    property ReturnPolicy : string read FReturnPolicy write FReturnPolicy stored HasReturnPolicy;
    property RingSize : string read FRingSize write FRingSize stored HasRingSize;
    property SafetyWarning : string read FSafetyWarning write FSafetyWarning stored HasSafetyWarning;
    property SalesRestriction : string read FSalesRestriction write FSalesRestriction stored HasSalesRestriction;
    property SecondaryCacheSize : NonNegativeIntegerWithUnits_Type read FSecondaryCacheSize write FSecondaryCacheSize stored HasSecondaryCacheSize;
    property SettingType : string read FSettingType write FSettingType stored HasSettingType;
    property Size : string read FSize write FSize stored HasSize;
    property SKU : string read FSKU write FSKU stored HasSKU;
    property SoldInStores : string read FSoldInStores write FSoldInStores stored HasSoldInStores;
    property SizePerPearl : string read FSizePerPearl write FSizePerPearl stored HasSizePerPearl;
    property SkillLevel : string read FSkillLevel write FSkillLevel stored HasSkillLevel;
    property SoundCardDescription : string read FSoundCardDescription write FSoundCardDescription stored HasSoundCardDescription;
    property SpeakerCount : nonNegativeInteger read FSpeakerCount write FSpeakerCount stored HasSpeakerCount;
    property SpeakerDescription : string read FSpeakerDescription write FSpeakerDescription stored HasSpeakerDescription;
    property SpecialFeatures : MerchantItemAttributes_SpecialFeaturesArray read FSpecialFeatures write FSpecialFeatures stored HasSpecialFeatures;
    property StoneClarity : string read FStoneClarity write FStoneClarity stored HasStoneClarity;
    property StoneColor : string read FStoneColor write FStoneColor stored HasStoneColor;
    property StoneCut : string read FStoneCut write FStoneCut stored HasStoneCut;
    property StoneShape : string read FStoneShape write FStoneShape stored HasStoneShape;
    property StoneWeight : DecimalWithUnits_Type read FStoneWeight write FStoneWeight stored HasStoneWeight;
    property Studio : string read FStudio write FStudio stored HasStudio;
    property SubscriptionLength : NonNegativeIntegerWithUnits_Type read FSubscriptionLength write FSubscriptionLength stored HasSubscriptionLength;
    property SupportedImageType : MerchantItemAttributes_SupportedImageTypeArray read FSupportedImageType write FSupportedImageType stored HasSupportedImageType;
    property SystemBusSpeed : DecimalWithUnits_Type read FSystemBusSpeed write FSystemBusSpeed stored HasSystemBusSpeed;
    property SystemMemorySizeMax : DecimalWithUnits_Type read FSystemMemorySizeMax write FSystemMemorySizeMax stored HasSystemMemorySizeMax;
    property SystemMemorySize : DecimalWithUnits_Type read FSystemMemorySize write FSystemMemorySize stored HasSystemMemorySize;
    property SystemMemoryType : string read FSystemMemoryType write FSystemMemoryType stored HasSystemMemoryType;
    property TellingPageIndicator : string read FTellingPageIndicator write FTellingPageIndicator stored HasTellingPageIndicator;
    property TheatricalReleaseDate : string read FTheatricalReleaseDate write FTheatricalReleaseDate stored HasTheatricalReleaseDate;
    property Title : string read FTitle write FTitle stored HasTitle;
    property TotalDiamondWeight : DecimalWithUnits_Type read FTotalDiamondWeight write FTotalDiamondWeight stored HasTotalDiamondWeight;
    property TotalExternalBaysFree : nonNegativeInteger read FTotalExternalBaysFree write FTotalExternalBaysFree stored HasTotalExternalBaysFree;
    property TotalFirewirePorts : nonNegativeInteger read FTotalFirewirePorts write FTotalFirewirePorts stored HasTotalFirewirePorts;
    property TotalGemWeight : DecimalWithUnits_Type read FTotalGemWeight write FTotalGemWeight stored HasTotalGemWeight;
    property TotalInternalBaysFree : nonNegativeInteger read FTotalInternalBaysFree write FTotalInternalBaysFree stored HasTotalInternalBaysFree;
    property TotalMetalWeight : DecimalWithUnits_Type read FTotalMetalWeight write FTotalMetalWeight stored HasTotalMetalWeight;
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
    property WaterResistanceDepth : DecimalWithUnits_Type read FWaterResistanceDepth write FWaterResistanceDepth stored HasWaterResistanceDepth;
    property WirelessMicrophoneFrequency : nonNegativeInteger read FWirelessMicrophoneFrequency write FWirelessMicrophoneFrequency stored HasWirelessMicrophoneFrequency;
  end;

  NonNegativeIntegerWithUnits_Type = class(TComplexInt32UContentRemotable)
  private
    FUnits : string;
  published
    property Units : string read FUnits write FUnits;
  end;

  DecimalWithUnits_Type = class(TComplexFloatExtendedContentRemotable)
  private
    FUnits : string;
  published
    property Units : string read FUnits write FUnits;
  end;

  StringWithUnits_Type = class(TComplexStringContentRemotable)
  private
    FUnits : string;
  published
    property Units : string read FUnits write FUnits;
  end;

  Help_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): HelpRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : HelpRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): ItemSearchRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ItemSearchRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): ItemLookupRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ItemLookupRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): BrowseNodeLookupRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : BrowseNodeLookupRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): ListSearchRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ListSearchRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): ListLookupRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ListLookupRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): CustomerContentSearchRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CustomerContentSearchRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): CustomerContentLookupRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CustomerContentLookupRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): SimilarityLookupRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SimilarityLookupRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): SellerLookupRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SellerLookupRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): CartGetRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartGetRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): CartAddRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartAddRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): CartCreateRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartCreateRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): CartModifyRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartModifyRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): CartClearRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartClearRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): TransactionLookupRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TransactionLookupRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): SellerListingSearchRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SellerListingSearchRequest_Type Read GetItem;Default;
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
    function GetItem(AIndex: Integer): SellerListingLookupRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SellerListingLookupRequest_Type Read GetItem;Default;
  end;

  SellerListingLookupResponse_SellerListingsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SellerListings_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SellerListings_Type Read GetItem;Default;
  end;

  TagLookup_RequestArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TagLookupRequest_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TagLookupRequest_Type Read GetItem;Default;
  end;

  TagLookupResponse_TagsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Tags_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Tags_Type Read GetItem;Default;
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

  CartAddRequest_Items_Type_Item_Type_MetaDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartAddRequest_Items_Type_Item_Type_MetaData_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartAddRequest_Items_Type_Item_Type_MetaData_Type Read GetItem;Default;
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

  CartCreateRequest_Items_Type_Item_Type_MetaDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartCreateRequest_Items_Type_Item_Type_MetaData_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartCreateRequest_Items_Type_Item_Type_MetaData_Type Read GetItem;Default;
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

  TagLookupRequest_TagNameArray = class(TBaseSimpleTypeArrayRemotable)
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

  TagLookupRequest_ResponseGroupArray = class(TBaseSimpleTypeArrayRemotable)
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

  Arguments_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Arguments_Argument_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Arguments_Argument_Type Read GetItem;Default;
  end;

  HTTPHeaders_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): HTTPHeaders_Header_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : HTTPHeaders_Header_Type Read GetItem;Default;
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

  SearchResultsMap_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): SearchResultsMap_SearchIndex_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : SearchResultsMap_SearchIndex_Type Read GetItem;Default;
  end;

  Item_ImageSets_Type_ImageSetArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ImageSet_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ImageSet_Type Read GetItem;Default;
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

  Tags_TagArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Tag_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Tag_Type Read GetItem;Default;
  end;

  Tag_TaggedItemsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TaggedItems_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TaggedItems_Type Read GetItem;Default;
  end;

  Tag_TaggedListmaniaListsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TaggedListmaniaLists_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TaggedListmaniaLists_Type Read GetItem;Default;
  end;

  Tag_TaggedGuidesArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TaggedGuides_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TaggedGuides_Type Read GetItem;Default;
  end;

  Offers_OfferArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Offer_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Offer_Type Read GetItem;Default;
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

  Variations__ItemArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Item_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Item_Type Read GetItem;Default;
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

  EditorialReviews_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): EditorialReview_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : EditorialReview_Type Read GetItem;Default;
  end;

  Collections_Collection_Type_CollectionItemArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Collections_Collection_Type_CollectionItem_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Collections_Collection_Type_CollectionItem_Type Read GetItem;Default;
  end;

  Collections_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Collections_Collection_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Collections_Collection_Type Read GetItem;Default;
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

  Tracks_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Tracks_Disc_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Tracks_Disc_Type Read GetItem;Default;
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

  Accessories_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Accessories_Accessory_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Accessories_Accessory_Type Read GetItem;Default;
  end;

  Promotions_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Promotion_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Promotion_Type Read GetItem;Default;
  end;

  PromotionEligibilityRequirements_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): PromotionEligibilityRequirement_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : PromotionEligibilityRequirement_Type Read GetItem;Default;
  end;

  PromotionBenefits_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): PromotionBenefit_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : PromotionBenefit_Type Read GetItem;Default;
  end;

  BrowseNodes_BrowseNodeArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): BrowseNode_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : BrowseNode_Type Read GetItem;Default;
  end;

  BrowseNode_Properties_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): Property_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : Property_Type Read GetItem;Default;
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

  ListmaniaLists_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ListmaniaLists_ListmaniaList_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ListmaniaLists_ListmaniaList_Type Read GetItem;Default;
  end;

  CartItems_CartItemArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartItem_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartItem_Type Read GetItem;Default;
  end;

  SavedForLaterItems_SavedForLaterItemArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartItem_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartItem_Type Read GetItem;Default;
  end;

  CartItem_MetaData_Type = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CartItem_MetaData_Type_KeyValuePair_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CartItem_MetaData_Type_KeyValuePair_Type Read GetItem;Default;
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

  ItemAttributes_AgeArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_CategoryArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_CategoryBinArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_CharacterArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_EducationalFocusArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_EthnicityArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_GemTypeSetElementArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_GenderArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_IngredientsSetElementArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_InterestArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_LanguageNameArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_MaterialTypeSetElementArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_PantLengthArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_PantSizeArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_PrimaryColorArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_ShoeSizeArray = class(TBaseSimpleTypeArrayRemotable)
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

  ItemAttributes_TargetBrandArray = class(TBaseSimpleTypeArrayRemotable)
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
    ['{CBBBC8FF-F0BE-40D0-89A9-471498383C10}']
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
    function MultiOperation(
      const  MultiOperationParam : MultiOperation_Type
    ):MultiOperationResponse;
  end;

  procedure Register_AWSECommerceService_ServiceMetadata();

Implementation
uses metadata_repository;

{ Help_Type }

constructor Help_Type.Create();
begin
  inherited Create();
  FShared := HelpRequest_Type.Create();
  FRequest := Help_RequestArray.Create();
end;

destructor Help_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function Help_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function Help_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function Help_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function Help_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function Help_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function Help_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> HelpRequest_Type(0) );
end;

function Help_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> Help_RequestArray(0) );
end;

{ HelpResponse_Type }

constructor HelpResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FInformation := HelpResponse_InformationArray.Create();
end;

destructor HelpResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FInformation) then
    FreeAndNil(FInformation);
  inherited Destroy();
end;

function HelpResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function HelpResponse_Type.HasInformation() : Boolean;
begin
  Result := ( FInformation <> HelpResponse_InformationArray(0) );
end;

{ ItemSearch_Type }

constructor ItemSearch_Type.Create();
begin
  inherited Create();
  FShared := ItemSearchRequest_Type.Create();
  FRequest := ItemSearch_RequestArray.Create();
end;

destructor ItemSearch_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function ItemSearch_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function ItemSearch_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function ItemSearch_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function ItemSearch_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function ItemSearch_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function ItemSearch_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function ItemSearch_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> ItemSearchRequest_Type(0) );
end;

function ItemSearch_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> ItemSearch_RequestArray(0) );
end;

{ ItemSearchResponse_Type }

constructor ItemSearchResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FItems := ItemSearchResponse_ItemsArray.Create();
end;

destructor ItemSearchResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FItems) then
    FreeAndNil(FItems);
  inherited Destroy();
end;

function ItemSearchResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function ItemSearchResponse_Type.HasItems() : Boolean;
begin
  Result := ( FItems <> ItemSearchResponse_ItemsArray(0) );
end;

{ ItemLookup_Type }

constructor ItemLookup_Type.Create();
begin
  inherited Create();
  FShared := ItemLookupRequest_Type.Create();
  FRequest := ItemLookup_RequestArray.Create();
end;

destructor ItemLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function ItemLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function ItemLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function ItemLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function ItemLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function ItemLookup_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function ItemLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function ItemLookup_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> ItemLookupRequest_Type(0) );
end;

function ItemLookup_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> ItemLookup_RequestArray(0) );
end;

{ ItemLookupResponse_Type }

constructor ItemLookupResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FItems := ItemLookupResponse_ItemsArray.Create();
end;

destructor ItemLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FItems) then
    FreeAndNil(FItems);
  inherited Destroy();
end;

function ItemLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function ItemLookupResponse_Type.HasItems() : Boolean;
begin
  Result := ( FItems <> ItemLookupResponse_ItemsArray(0) );
end;

{ BrowseNodeLookup_Type }

constructor BrowseNodeLookup_Type.Create();
begin
  inherited Create();
  FShared := BrowseNodeLookupRequest_Type.Create();
  FRequest := BrowseNodeLookup_RequestArray.Create();
end;

destructor BrowseNodeLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function BrowseNodeLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function BrowseNodeLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function BrowseNodeLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function BrowseNodeLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function BrowseNodeLookup_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function BrowseNodeLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function BrowseNodeLookup_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> BrowseNodeLookupRequest_Type(0) );
end;

function BrowseNodeLookup_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> BrowseNodeLookup_RequestArray(0) );
end;

{ BrowseNodeLookupResponse_Type }

constructor BrowseNodeLookupResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FBrowseNodes := BrowseNodeLookupResponse_BrowseNodesArray.Create();
end;

destructor BrowseNodeLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FBrowseNodes) then
    FreeAndNil(FBrowseNodes);
  inherited Destroy();
end;

function BrowseNodeLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function BrowseNodeLookupResponse_Type.HasBrowseNodes() : Boolean;
begin
  Result := ( FBrowseNodes <> BrowseNodeLookupResponse_BrowseNodesArray(0) );
end;

{ ListSearch_Type }

constructor ListSearch_Type.Create();
begin
  inherited Create();
  FShared := ListSearchRequest_Type.Create();
  FRequest := ListSearch_RequestArray.Create();
end;

destructor ListSearch_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function ListSearch_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function ListSearch_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function ListSearch_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function ListSearch_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function ListSearch_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function ListSearch_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function ListSearch_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> ListSearchRequest_Type(0) );
end;

function ListSearch_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> ListSearch_RequestArray(0) );
end;

{ ListSearchResponse_Type }

constructor ListSearchResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FLists := ListSearchResponse_ListsArray.Create();
end;

destructor ListSearchResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FLists) then
    FreeAndNil(FLists);
  inherited Destroy();
end;

function ListSearchResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function ListSearchResponse_Type.HasLists() : Boolean;
begin
  Result := ( FLists <> ListSearchResponse_ListsArray(0) );
end;

{ ListLookup_Type }

constructor ListLookup_Type.Create();
begin
  inherited Create();
  FShared := ListLookupRequest_Type.Create();
  FRequest := ListLookup_RequestArray.Create();
end;

destructor ListLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function ListLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function ListLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function ListLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function ListLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function ListLookup_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function ListLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function ListLookup_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> ListLookupRequest_Type(0) );
end;

function ListLookup_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> ListLookup_RequestArray(0) );
end;

{ ListLookupResponse_Type }

constructor ListLookupResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FLists := ListLookupResponse_ListsArray.Create();
end;

destructor ListLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FLists) then
    FreeAndNil(FLists);
  inherited Destroy();
end;

function ListLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function ListLookupResponse_Type.HasLists() : Boolean;
begin
  Result := ( FLists <> ListLookupResponse_ListsArray(0) );
end;

{ CustomerContentSearch_Type }

constructor CustomerContentSearch_Type.Create();
begin
  inherited Create();
  FShared := CustomerContentSearchRequest_Type.Create();
  FRequest := CustomerContentSearch_RequestArray.Create();
end;

destructor CustomerContentSearch_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function CustomerContentSearch_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function CustomerContentSearch_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function CustomerContentSearch_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function CustomerContentSearch_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function CustomerContentSearch_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function CustomerContentSearch_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function CustomerContentSearch_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> CustomerContentSearchRequest_Type(0) );
end;

function CustomerContentSearch_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> CustomerContentSearch_RequestArray(0) );
end;

{ CustomerContentSearchResponse_Type }

constructor CustomerContentSearchResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FCustomers := CustomerContentSearchResponse_CustomersArray.Create();
end;

destructor CustomerContentSearchResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FCustomers) then
    FreeAndNil(FCustomers);
  inherited Destroy();
end;

function CustomerContentSearchResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function CustomerContentSearchResponse_Type.HasCustomers() : Boolean;
begin
  Result := ( FCustomers <> CustomerContentSearchResponse_CustomersArray(0) );
end;

{ CustomerContentLookup_Type }

constructor CustomerContentLookup_Type.Create();
begin
  inherited Create();
  FShared := CustomerContentLookupRequest_Type.Create();
  FRequest := CustomerContentLookup_RequestArray.Create();
end;

destructor CustomerContentLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function CustomerContentLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function CustomerContentLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function CustomerContentLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function CustomerContentLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function CustomerContentLookup_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function CustomerContentLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function CustomerContentLookup_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> CustomerContentLookupRequest_Type(0) );
end;

function CustomerContentLookup_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> CustomerContentLookup_RequestArray(0) );
end;

{ CustomerContentLookupResponse_Type }

constructor CustomerContentLookupResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FCustomers := CustomerContentLookupResponse_CustomersArray.Create();
end;

destructor CustomerContentLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FCustomers) then
    FreeAndNil(FCustomers);
  inherited Destroy();
end;

function CustomerContentLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function CustomerContentLookupResponse_Type.HasCustomers() : Boolean;
begin
  Result := ( FCustomers <> CustomerContentLookupResponse_CustomersArray(0) );
end;

{ SimilarityLookup_Type }

constructor SimilarityLookup_Type.Create();
begin
  inherited Create();
  FShared := SimilarityLookupRequest_Type.Create();
  FRequest := SimilarityLookup_RequestArray.Create();
end;

destructor SimilarityLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function SimilarityLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function SimilarityLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function SimilarityLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function SimilarityLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function SimilarityLookup_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function SimilarityLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function SimilarityLookup_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> SimilarityLookupRequest_Type(0) );
end;

function SimilarityLookup_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> SimilarityLookup_RequestArray(0) );
end;

{ SimilarityLookupResponse_Type }

constructor SimilarityLookupResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FItems := SimilarityLookupResponse_ItemsArray.Create();
end;

destructor SimilarityLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FItems) then
    FreeAndNil(FItems);
  inherited Destroy();
end;

function SimilarityLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function SimilarityLookupResponse_Type.HasItems() : Boolean;
begin
  Result := ( FItems <> SimilarityLookupResponse_ItemsArray(0) );
end;

{ SellerLookup_Type }

constructor SellerLookup_Type.Create();
begin
  inherited Create();
  FShared := SellerLookupRequest_Type.Create();
  FRequest := SellerLookup_RequestArray.Create();
end;

destructor SellerLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function SellerLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function SellerLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function SellerLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function SellerLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function SellerLookup_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function SellerLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function SellerLookup_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> SellerLookupRequest_Type(0) );
end;

function SellerLookup_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> SellerLookup_RequestArray(0) );
end;

{ SellerLookupResponse_Type }

constructor SellerLookupResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FSellers := SellerLookupResponse_SellersArray.Create();
end;

destructor SellerLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FSellers) then
    FreeAndNil(FSellers);
  inherited Destroy();
end;

function SellerLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function SellerLookupResponse_Type.HasSellers() : Boolean;
begin
  Result := ( FSellers <> SellerLookupResponse_SellersArray(0) );
end;

{ CartGet_Type }

constructor CartGet_Type.Create();
begin
  inherited Create();
  FShared := CartGetRequest_Type.Create();
  FRequest := CartGet_RequestArray.Create();
end;

destructor CartGet_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function CartGet_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function CartGet_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function CartGet_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function CartGet_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function CartGet_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function CartGet_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function CartGet_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> CartGetRequest_Type(0) );
end;

function CartGet_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> CartGet_RequestArray(0) );
end;

{ CartGetResponse_Type }

constructor CartGetResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FCart := CartGetResponse_CartArray.Create();
end;

destructor CartGetResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FCart) then
    FreeAndNil(FCart);
  inherited Destroy();
end;

function CartGetResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function CartGetResponse_Type.HasCart() : Boolean;
begin
  Result := ( FCart <> CartGetResponse_CartArray(0) );
end;

{ CartAdd_Type }

constructor CartAdd_Type.Create();
begin
  inherited Create();
  FShared := CartAddRequest_Type.Create();
  FRequest := CartAdd_RequestArray.Create();
end;

destructor CartAdd_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function CartAdd_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function CartAdd_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function CartAdd_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function CartAdd_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function CartAdd_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function CartAdd_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function CartAdd_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> CartAddRequest_Type(0) );
end;

function CartAdd_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> CartAdd_RequestArray(0) );
end;

{ CartAddResponse_Type }

constructor CartAddResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FCart := CartAddResponse_CartArray.Create();
end;

destructor CartAddResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FCart) then
    FreeAndNil(FCart);
  inherited Destroy();
end;

function CartAddResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function CartAddResponse_Type.HasCart() : Boolean;
begin
  Result := ( FCart <> CartAddResponse_CartArray(0) );
end;

{ CartCreate_Type }

constructor CartCreate_Type.Create();
begin
  inherited Create();
  FShared := CartCreateRequest_Type.Create();
  FRequest := CartCreate_RequestArray.Create();
end;

destructor CartCreate_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function CartCreate_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function CartCreate_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function CartCreate_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function CartCreate_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function CartCreate_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function CartCreate_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function CartCreate_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> CartCreateRequest_Type(0) );
end;

function CartCreate_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> CartCreate_RequestArray(0) );
end;

{ CartCreateResponse_Type }

constructor CartCreateResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FCart := CartCreateResponse_CartArray.Create();
end;

destructor CartCreateResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FCart) then
    FreeAndNil(FCart);
  inherited Destroy();
end;

function CartCreateResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function CartCreateResponse_Type.HasCart() : Boolean;
begin
  Result := ( FCart <> CartCreateResponse_CartArray(0) );
end;

{ CartModify_Type }

constructor CartModify_Type.Create();
begin
  inherited Create();
  FShared := CartModifyRequest_Type.Create();
  FRequest := CartModify_RequestArray.Create();
end;

destructor CartModify_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function CartModify_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function CartModify_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function CartModify_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function CartModify_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function CartModify_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function CartModify_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function CartModify_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> CartModifyRequest_Type(0) );
end;

function CartModify_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> CartModify_RequestArray(0) );
end;

{ CartModifyResponse_Type }

constructor CartModifyResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FCart := CartModifyResponse_CartArray.Create();
end;

destructor CartModifyResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FCart) then
    FreeAndNil(FCart);
  inherited Destroy();
end;

function CartModifyResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function CartModifyResponse_Type.HasCart() : Boolean;
begin
  Result := ( FCart <> CartModifyResponse_CartArray(0) );
end;

{ CartClear_Type }

constructor CartClear_Type.Create();
begin
  inherited Create();
  FShared := CartClearRequest_Type.Create();
  FRequest := CartClear_RequestArray.Create();
end;

destructor CartClear_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function CartClear_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function CartClear_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function CartClear_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function CartClear_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function CartClear_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function CartClear_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function CartClear_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> CartClearRequest_Type(0) );
end;

function CartClear_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> CartClear_RequestArray(0) );
end;

{ CartClearResponse_Type }

constructor CartClearResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FCart := CartClearResponse_CartArray.Create();
end;

destructor CartClearResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FCart) then
    FreeAndNil(FCart);
  inherited Destroy();
end;

function CartClearResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function CartClearResponse_Type.HasCart() : Boolean;
begin
  Result := ( FCart <> CartClearResponse_CartArray(0) );
end;

{ TransactionLookup_Type }

constructor TransactionLookup_Type.Create();
begin
  inherited Create();
  FShared := TransactionLookupRequest_Type.Create();
  FRequest := TransactionLookup_RequestArray.Create();
end;

destructor TransactionLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function TransactionLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function TransactionLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function TransactionLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function TransactionLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function TransactionLookup_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function TransactionLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function TransactionLookup_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> TransactionLookupRequest_Type(0) );
end;

function TransactionLookup_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> TransactionLookup_RequestArray(0) );
end;

{ TransactionLookupResponse_Type }

constructor TransactionLookupResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FTransactions := TransactionLookupResponse_TransactionsArray.Create();
end;

destructor TransactionLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FTransactions) then
    FreeAndNil(FTransactions);
  inherited Destroy();
end;

function TransactionLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function TransactionLookupResponse_Type.HasTransactions() : Boolean;
begin
  Result := ( FTransactions <> TransactionLookupResponse_TransactionsArray(0) );
end;

{ SellerListingSearch_Type }

constructor SellerListingSearch_Type.Create();
begin
  inherited Create();
  FShared := SellerListingSearchRequest_Type.Create();
  FRequest := SellerListingSearch_RequestArray.Create();
end;

destructor SellerListingSearch_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function SellerListingSearch_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function SellerListingSearch_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function SellerListingSearch_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function SellerListingSearch_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function SellerListingSearch_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function SellerListingSearch_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function SellerListingSearch_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> SellerListingSearchRequest_Type(0) );
end;

function SellerListingSearch_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> SellerListingSearch_RequestArray(0) );
end;

{ SellerListingSearchResponse_Type }

constructor SellerListingSearchResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FSellerListings := SellerListingSearchResponse_SellerListingsArray.Create();
end;

destructor SellerListingSearchResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FSellerListings) then
    FreeAndNil(FSellerListings);
  inherited Destroy();
end;

function SellerListingSearchResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function SellerListingSearchResponse_Type.HasSellerListings() : Boolean;
begin
  Result := ( FSellerListings <> SellerListingSearchResponse_SellerListingsArray(0) );
end;

{ SellerListingLookup_Type }

constructor SellerListingLookup_Type.Create();
begin
  inherited Create();
  FShared := SellerListingLookupRequest_Type.Create();
  FRequest := SellerListingLookup_RequestArray.Create();
end;

destructor SellerListingLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function SellerListingLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function SellerListingLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function SellerListingLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function SellerListingLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function SellerListingLookup_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function SellerListingLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function SellerListingLookup_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> SellerListingLookupRequest_Type(0) );
end;

function SellerListingLookup_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> SellerListingLookup_RequestArray(0) );
end;

{ SellerListingLookupResponse_Type }

constructor SellerListingLookupResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FSellerListings := SellerListingLookupResponse_SellerListingsArray.Create();
end;

destructor SellerListingLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FSellerListings) then
    FreeAndNil(FSellerListings);
  inherited Destroy();
end;

function SellerListingLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function SellerListingLookupResponse_Type.HasSellerListings() : Boolean;
begin
  Result := ( FSellerListings <> SellerListingLookupResponse_SellerListingsArray(0) );
end;

{ TagLookup_Type }

constructor TagLookup_Type.Create();
begin
  inherited Create();
  FShared := TagLookupRequest_Type.Create();
  FRequest := TagLookup_RequestArray.Create();
end;

destructor TagLookup_Type.Destroy();
begin
  if Assigned(FShared) then
    FreeAndNil(FShared);
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  inherited Destroy();
end;

function TagLookup_Type.HasMarketplaceDomain() : Boolean;
begin
  Result := ( FMarketplaceDomain <> '' );
end;

function TagLookup_Type.HasAWSAccessKeyId() : Boolean;
begin
  Result := ( FAWSAccessKeyId <> '' );
end;

function TagLookup_Type.HasSubscriptionId() : Boolean;
begin
  Result := ( FSubscriptionId <> '' );
end;

function TagLookup_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function TagLookup_Type.HasValidate() : Boolean;
begin
  Result := ( FValidate <> '' );
end;

function TagLookup_Type.HasXMLEscaping() : Boolean;
begin
  Result := ( FXMLEscaping <> '' );
end;

function TagLookup_Type.HasShared() : Boolean;
begin
  Result := ( FShared <> TagLookupRequest_Type(0) );
end;

function TagLookup_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> TagLookup_RequestArray(0) );
end;

{ TagLookupResponse_Type }

constructor TagLookupResponse_Type.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FTags := TagLookupResponse_TagsArray.Create();
end;

destructor TagLookupResponse_Type.Destroy();
begin
  if Assigned(FOperationRequest) then
    FreeAndNil(FOperationRequest);
  if Assigned(FTags) then
    FreeAndNil(FTags);
  inherited Destroy();
end;

function TagLookupResponse_Type.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function TagLookupResponse_Type.HasTags() : Boolean;
begin
  Result := ( FTags <> TagLookupResponse_TagsArray(0) );
end;

{ MultiOperation_Type }

constructor MultiOperation_Type.Create();
begin
  inherited Create();
  FHelp := Help_Type.Create();
  FItemSearch := ItemSearch_Type.Create();
  FItemLookup := ItemLookup_Type.Create();
  FListSearch := ListSearch_Type.Create();
  FListLookup := ListLookup_Type.Create();
  FCustomerContentSearch := CustomerContentSearch_Type.Create();
  FCustomerContentLookup := CustomerContentLookup_Type.Create();
  FSimilarityLookup := SimilarityLookup_Type.Create();
  FSellerLookup := SellerLookup_Type.Create();
  FCartGet := CartGet_Type.Create();
  FCartAdd := CartAdd_Type.Create();
  FCartCreate := CartCreate_Type.Create();
  FCartModify := CartModify_Type.Create();
  FCartClear := CartClear_Type.Create();
  FTransactionLookup := TransactionLookup_Type.Create();
  FSellerListingSearch := SellerListingSearch_Type.Create();
  FSellerListingLookup := SellerListingLookup_Type.Create();
  FTagLookup := TagLookup_Type.Create();
  FBrowseNodeLookup := BrowseNodeLookup_Type.Create();
end;

destructor MultiOperation_Type.Destroy();
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
  if Assigned(FTagLookup) then
    FreeAndNil(FTagLookup);
  if Assigned(FBrowseNodeLookup) then
    FreeAndNil(FBrowseNodeLookup);
  inherited Destroy();
end;

function MultiOperation_Type.HasHelp() : Boolean;
begin
  Result := ( FHelp <> Help_Type(0) );
end;

function MultiOperation_Type.HasItemSearch() : Boolean;
begin
  Result := ( FItemSearch <> ItemSearch_Type(0) );
end;

function MultiOperation_Type.HasItemLookup() : Boolean;
begin
  Result := ( FItemLookup <> ItemLookup_Type(0) );
end;

function MultiOperation_Type.HasListSearch() : Boolean;
begin
  Result := ( FListSearch <> ListSearch_Type(0) );
end;

function MultiOperation_Type.HasListLookup() : Boolean;
begin
  Result := ( FListLookup <> ListLookup_Type(0) );
end;

function MultiOperation_Type.HasCustomerContentSearch() : Boolean;
begin
  Result := ( FCustomerContentSearch <> CustomerContentSearch_Type(0) );
end;

function MultiOperation_Type.HasCustomerContentLookup() : Boolean;
begin
  Result := ( FCustomerContentLookup <> CustomerContentLookup_Type(0) );
end;

function MultiOperation_Type.HasSimilarityLookup() : Boolean;
begin
  Result := ( FSimilarityLookup <> SimilarityLookup_Type(0) );
end;

function MultiOperation_Type.HasSellerLookup() : Boolean;
begin
  Result := ( FSellerLookup <> SellerLookup_Type(0) );
end;

function MultiOperation_Type.HasCartGet() : Boolean;
begin
  Result := ( FCartGet <> CartGet_Type(0) );
end;

function MultiOperation_Type.HasCartAdd() : Boolean;
begin
  Result := ( FCartAdd <> CartAdd_Type(0) );
end;

function MultiOperation_Type.HasCartCreate() : Boolean;
begin
  Result := ( FCartCreate <> CartCreate_Type(0) );
end;

function MultiOperation_Type.HasCartModify() : Boolean;
begin
  Result := ( FCartModify <> CartModify_Type(0) );
end;

function MultiOperation_Type.HasCartClear() : Boolean;
begin
  Result := ( FCartClear <> CartClear_Type(0) );
end;

function MultiOperation_Type.HasTransactionLookup() : Boolean;
begin
  Result := ( FTransactionLookup <> TransactionLookup_Type(0) );
end;

function MultiOperation_Type.HasSellerListingSearch() : Boolean;
begin
  Result := ( FSellerListingSearch <> SellerListingSearch_Type(0) );
end;

function MultiOperation_Type.HasSellerListingLookup() : Boolean;
begin
  Result := ( FSellerListingLookup <> SellerListingLookup_Type(0) );
end;

function MultiOperation_Type.HasTagLookup() : Boolean;
begin
  Result := ( FTagLookup <> TagLookup_Type(0) );
end;

function MultiOperation_Type.HasBrowseNodeLookup() : Boolean;
begin
  Result := ( FBrowseNodeLookup <> BrowseNodeLookup_Type(0) );
end;

{ MultiOperationResponse }

constructor MultiOperationResponse.Create();
begin
  inherited Create();
  FOperationRequest := OperationRequest_Type.Create();
  FHelpResponse := HelpResponse_Type.Create();
  FItemSearchResponse := ItemSearchResponse_Type.Create();
  FItemLookupResponse := ItemLookupResponse_Type.Create();
  FListSearchResponse := ListSearchResponse_Type.Create();
  FListLookupResponse := ListLookupResponse_Type.Create();
  FCustomerContentSearchResponse := CustomerContentSearchResponse_Type.Create();
  FCustomerContentLookupResponse := CustomerContentLookupResponse_Type.Create();
  FSimilarityLookupResponse := SimilarityLookupResponse_Type.Create();
  FSellerLookupResponse := SellerLookupResponse_Type.Create();
  FCartGetResponse := CartGetResponse_Type.Create();
  FCartAddResponse := CartAddResponse_Type.Create();
  FCartCreateResponse := CartCreateResponse_Type.Create();
  FCartModifyResponse := CartModifyResponse_Type.Create();
  FCartClearResponse := CartClearResponse_Type.Create();
  FTransactionLookupResponse := TransactionLookupResponse_Type.Create();
  FSellerListingSearchResponse := SellerListingSearchResponse_Type.Create();
  FSellerListingLookupResponse := SellerListingLookupResponse_Type.Create();
  FTagLookupResponse := TagLookupResponse_Type.Create();
  FBrowseNodeLookupResponse := BrowseNodeLookupResponse_Type.Create();
end;

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
  if Assigned(FTagLookupResponse) then
    FreeAndNil(FTagLookupResponse);
  if Assigned(FBrowseNodeLookupResponse) then
    FreeAndNil(FBrowseNodeLookupResponse);
  inherited Destroy();
end;

function MultiOperationResponse.HasOperationRequest() : Boolean;
begin
  Result := ( FOperationRequest <> OperationRequest_Type(0) );
end;

function MultiOperationResponse.HasHelpResponse() : Boolean;
begin
  Result := ( FHelpResponse <> HelpResponse_Type(0) );
end;

function MultiOperationResponse.HasItemSearchResponse() : Boolean;
begin
  Result := ( FItemSearchResponse <> ItemSearchResponse_Type(0) );
end;

function MultiOperationResponse.HasItemLookupResponse() : Boolean;
begin
  Result := ( FItemLookupResponse <> ItemLookupResponse_Type(0) );
end;

function MultiOperationResponse.HasListSearchResponse() : Boolean;
begin
  Result := ( FListSearchResponse <> ListSearchResponse_Type(0) );
end;

function MultiOperationResponse.HasListLookupResponse() : Boolean;
begin
  Result := ( FListLookupResponse <> ListLookupResponse_Type(0) );
end;

function MultiOperationResponse.HasCustomerContentSearchResponse() : Boolean;
begin
  Result := ( FCustomerContentSearchResponse <> CustomerContentSearchResponse_Type(0) );
end;

function MultiOperationResponse.HasCustomerContentLookupResponse() : Boolean;
begin
  Result := ( FCustomerContentLookupResponse <> CustomerContentLookupResponse_Type(0) );
end;

function MultiOperationResponse.HasSimilarityLookupResponse() : Boolean;
begin
  Result := ( FSimilarityLookupResponse <> SimilarityLookupResponse_Type(0) );
end;

function MultiOperationResponse.HasSellerLookupResponse() : Boolean;
begin
  Result := ( FSellerLookupResponse <> SellerLookupResponse_Type(0) );
end;

function MultiOperationResponse.HasCartGetResponse() : Boolean;
begin
  Result := ( FCartGetResponse <> CartGetResponse_Type(0) );
end;

function MultiOperationResponse.HasCartAddResponse() : Boolean;
begin
  Result := ( FCartAddResponse <> CartAddResponse_Type(0) );
end;

function MultiOperationResponse.HasCartCreateResponse() : Boolean;
begin
  Result := ( FCartCreateResponse <> CartCreateResponse_Type(0) );
end;

function MultiOperationResponse.HasCartModifyResponse() : Boolean;
begin
  Result := ( FCartModifyResponse <> CartModifyResponse_Type(0) );
end;

function MultiOperationResponse.HasCartClearResponse() : Boolean;
begin
  Result := ( FCartClearResponse <> CartClearResponse_Type(0) );
end;

function MultiOperationResponse.HasTransactionLookupResponse() : Boolean;
begin
  Result := ( FTransactionLookupResponse <> TransactionLookupResponse_Type(0) );
end;

function MultiOperationResponse.HasSellerListingSearchResponse() : Boolean;
begin
  Result := ( FSellerListingSearchResponse <> SellerListingSearchResponse_Type(0) );
end;

function MultiOperationResponse.HasSellerListingLookupResponse() : Boolean;
begin
  Result := ( FSellerListingLookupResponse <> SellerListingLookupResponse_Type(0) );
end;

function MultiOperationResponse.HasTagLookupResponse() : Boolean;
begin
  Result := ( FTagLookupResponse <> TagLookupResponse_Type(0) );
end;

function MultiOperationResponse.HasBrowseNodeLookupResponse() : Boolean;
begin
  Result := ( FBrowseNodeLookupResponse <> BrowseNodeLookupResponse_Type(0) );
end;

{ Bin_Type }

constructor Bin_Type.Create();
begin
  inherited Create();
  FBinParameter := Bin_BinParameterArray.Create();
end;

destructor Bin_Type.Destroy();
begin
  if Assigned(FBinParameter) then
    FreeAndNil(FBinParameter);
  inherited Destroy();
end;

function Bin_Type.HasBinParameter() : Boolean;
begin
  Result := ( FBinParameter <> Bin_BinParameterArray(0) );
end;

{ SearchBinSet_Type }

constructor SearchBinSet_Type.Create();
begin
  inherited Create();
  FBin := SearchBinSet_BinArray.Create();
end;

destructor SearchBinSet_Type.Destroy();
begin
  if Assigned(FBin) then
    FreeAndNil(FBin);
  inherited Destroy();
end;

function SearchBinSet_Type.HasBin() : Boolean;
begin
  Result := ( FBin <> SearchBinSet_BinArray(0) );
end;

{ HelpRequest_Type }

constructor HelpRequest_Type.Create();
begin
  inherited Create();
  FResponseGroup := HelpRequest_ResponseGroupArray.Create();
end;

destructor HelpRequest_Type.Destroy();
begin
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function HelpRequest_Type.HasAbout() : Boolean;
begin
  Result := ( FAbout <> '' );
end;

function HelpRequest_Type.HasHelpType() : Boolean;
begin
  Result := True;
end;

function HelpRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> HelpRequest_ResponseGroupArray(0) );
end;

{ ItemSearchRequest_Type }

constructor ItemSearchRequest_Type.Create();
begin
  inherited Create();
  FAudienceRating := ItemSearchRequest_AudienceRatingArray.Create();
  FResponseGroup := ItemSearchRequest_ResponseGroupArray.Create();
end;

destructor ItemSearchRequest_Type.Destroy();
begin
  if Assigned(FAudienceRating) then
    FreeAndNil(FAudienceRating);
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function ItemSearchRequest_Type.HasActor() : Boolean;
begin
  Result := ( FActor <> '' );
end;

function ItemSearchRequest_Type.HasArtist() : Boolean;
begin
  Result := ( FArtist <> '' );
end;

function ItemSearchRequest_Type.HasAvailability() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest_Type.HasAudienceRating() : Boolean;
begin
  Result := ( FAudienceRating <> ItemSearchRequest_AudienceRatingArray(0) );
end;

function ItemSearchRequest_Type.HasAuthor() : Boolean;
begin
  Result := ( FAuthor <> '' );
end;

function ItemSearchRequest_Type.HasBrand() : Boolean;
begin
  Result := ( FBrand <> '' );
end;

function ItemSearchRequest_Type.HasBrowseNode() : Boolean;
begin
  Result := ( FBrowseNode <> '' );
end;

function ItemSearchRequest_Type.HasCity() : Boolean;
begin
  Result := ( FCity <> '' );
end;

function ItemSearchRequest_Type.HasComposer() : Boolean;
begin
  Result := ( FComposer <> '' );
end;

function ItemSearchRequest_Type.HasCondition() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest_Type.HasConductor() : Boolean;
begin
  Result := ( FConductor <> '' );
end;

function ItemSearchRequest_Type.HasCount() : Boolean;
begin
  Result := ( FCount <> positiveInteger(0) );
end;

function ItemSearchRequest_Type.HasCuisine() : Boolean;
begin
  Result := ( FCuisine <> '' );
end;

function ItemSearchRequest_Type.HasDeliveryMethod() : Boolean;
begin
  Result := True;
end;

function ItemSearchRequest_Type.HasDirector() : Boolean;
begin
  Result := ( FDirector <> '' );
end;

function ItemSearchRequest_Type.HasFutureLaunchDate() : Boolean;
begin
  Result := ( FFutureLaunchDate <> '' );
end;

function ItemSearchRequest_Type.HasISPUPostalCode() : Boolean;
begin
  Result := ( FISPUPostalCode <> '' );
end;

function ItemSearchRequest_Type.HasItemPage() : Boolean;
begin
  Result := ( FItemPage <> positiveInteger(0) );
end;

function ItemSearchRequest_Type.HasKeywords() : Boolean;
begin
  Result := ( FKeywords <> '' );
end;

function ItemSearchRequest_Type.HasManufacturer() : Boolean;
begin
  Result := ( FManufacturer <> '' );
end;

function ItemSearchRequest_Type.HasMaximumPrice() : Boolean;
begin
  Result := ( FMaximumPrice <> nonNegativeInteger(0) );
end;

function ItemSearchRequest_Type.HasMerchantId() : Boolean;
begin
  Result := ( FMerchantId <> '' );
end;

function ItemSearchRequest_Type.HasMinimumPrice() : Boolean;
begin
  Result := ( FMinimumPrice <> nonNegativeInteger(0) );
end;

function ItemSearchRequest_Type.HasMusicLabel() : Boolean;
begin
  Result := ( FMusicLabel <> '' );
end;

function ItemSearchRequest_Type.HasNeighborhood() : Boolean;
begin
  Result := ( FNeighborhood <> '' );
end;

function ItemSearchRequest_Type.HasOrchestra() : Boolean;
begin
  Result := ( FOrchestra <> '' );
end;

function ItemSearchRequest_Type.HasPostalCode() : Boolean;
begin
  Result := ( FPostalCode <> '' );
end;

function ItemSearchRequest_Type.HasPower() : Boolean;
begin
  Result := ( FPower <> '' );
end;

function ItemSearchRequest_Type.HasPublisher() : Boolean;
begin
  Result := ( FPublisher <> '' );
end;

function ItemSearchRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> ItemSearchRequest_ResponseGroupArray(0) );
end;

function ItemSearchRequest_Type.HasReviewSort() : Boolean;
begin
  Result := ( FReviewSort <> '' );
end;

function ItemSearchRequest_Type.HasSearchIndex() : Boolean;
begin
  Result := ( FSearchIndex <> '' );
end;

function ItemSearchRequest_Type.HasSort() : Boolean;
begin
  Result := ( FSort <> '' );
end;

function ItemSearchRequest_Type.HasState() : Boolean;
begin
  Result := ( FState <> '' );
end;

function ItemSearchRequest_Type.HasTagPage() : Boolean;
begin
  Result := ( FTagPage <> positiveInteger(0) );
end;

function ItemSearchRequest_Type.HasTagsPerPage() : Boolean;
begin
  Result := ( FTagsPerPage <> positiveInteger(0) );
end;

function ItemSearchRequest_Type.HasTagSort() : Boolean;
begin
  Result := ( FTagSort <> '' );
end;

function ItemSearchRequest_Type.HasTextStream() : Boolean;
begin
  Result := ( FTextStream <> '' );
end;

function ItemSearchRequest_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function ItemSearchRequest_Type.HasReleaseDate() : Boolean;
begin
  Result := ( FReleaseDate <> '' );
end;

{ ItemLookupRequest_Type }

constructor ItemLookupRequest_Type.Create();
begin
  inherited Create();
  FItemId := ItemLookupRequest_ItemIdArray.Create();
  FResponseGroup := ItemLookupRequest_ResponseGroupArray.Create();
end;

destructor ItemLookupRequest_Type.Destroy();
begin
  if Assigned(FItemId) then
    FreeAndNil(FItemId);
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function ItemLookupRequest_Type.HasCondition() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest_Type.HasDeliveryMethod() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest_Type.HasFutureLaunchDate() : Boolean;
begin
  Result := ( FFutureLaunchDate <> '' );
end;

function ItemLookupRequest_Type.HasIdType() : Boolean;
begin
  Result := True;
end;

function ItemLookupRequest_Type.HasISPUPostalCode() : Boolean;
begin
  Result := ( FISPUPostalCode <> '' );
end;

function ItemLookupRequest_Type.HasMerchantId() : Boolean;
begin
  Result := ( FMerchantId <> '' );
end;

function ItemLookupRequest_Type.HasOfferPage() : Boolean;
begin
  Result := ( FOfferPage <> positiveInteger(0) );
end;

function ItemLookupRequest_Type.HasItemId() : Boolean;
begin
  Result := ( FItemId <> ItemLookupRequest_ItemIdArray(0) );
end;

function ItemLookupRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> ItemLookupRequest_ResponseGroupArray(0) );
end;

function ItemLookupRequest_Type.HasReviewPage() : Boolean;
begin
  Result := ( FReviewPage <> positiveInteger(0) );
end;

function ItemLookupRequest_Type.HasReviewSort() : Boolean;
begin
  Result := ( FReviewSort <> '' );
end;

function ItemLookupRequest_Type.HasSearchIndex() : Boolean;
begin
  Result := ( FSearchIndex <> '' );
end;

function ItemLookupRequest_Type.HasSearchInsideKeywords() : Boolean;
begin
  Result := ( FSearchInsideKeywords <> '' );
end;

function ItemLookupRequest_Type.HasTagPage() : Boolean;
begin
  Result := ( FTagPage <> positiveInteger(0) );
end;

function ItemLookupRequest_Type.HasTagsPerPage() : Boolean;
begin
  Result := ( FTagsPerPage <> positiveInteger(0) );
end;

function ItemLookupRequest_Type.HasTagSort() : Boolean;
begin
  Result := ( FTagSort <> '' );
end;

function ItemLookupRequest_Type.HasVariationPage() : Boolean;
begin
  Result := ( FVariationPage <> '' );
end;

{ ListSearchRequest_Type }

constructor ListSearchRequest_Type.Create();
begin
  inherited Create();
  FResponseGroup := ListSearchRequest_ResponseGroupArray.Create();
end;

destructor ListSearchRequest_Type.Destroy();
begin
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function ListSearchRequest_Type.HasCity() : Boolean;
begin
  Result := ( FCity <> '' );
end;

function ListSearchRequest_Type.HasEmail() : Boolean;
begin
  Result := ( FEmail <> '' );
end;

function ListSearchRequest_Type.HasFirstName() : Boolean;
begin
  Result := ( FFirstName <> '' );
end;

function ListSearchRequest_Type.HasLastName() : Boolean;
begin
  Result := ( FLastName <> '' );
end;

function ListSearchRequest_Type.HasListPage() : Boolean;
begin
  Result := ( FListPage <> positiveInteger(0) );
end;

function ListSearchRequest_Type.HasName() : Boolean;
begin
  Result := ( FName <> '' );
end;

function ListSearchRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> ListSearchRequest_ResponseGroupArray(0) );
end;

function ListSearchRequest_Type.HasState() : Boolean;
begin
  Result := ( FState <> '' );
end;

{ ListLookupRequest_Type }

constructor ListLookupRequest_Type.Create();
begin
  inherited Create();
  FResponseGroup := ListLookupRequest_ResponseGroupArray.Create();
end;

destructor ListLookupRequest_Type.Destroy();
begin
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function ListLookupRequest_Type.HasCondition() : Boolean;
begin
  Result := True;
end;

function ListLookupRequest_Type.HasDeliveryMethod() : Boolean;
begin
  Result := True;
end;

function ListLookupRequest_Type.HasISPUPostalCode() : Boolean;
begin
  Result := ( FISPUPostalCode <> '' );
end;

function ListLookupRequest_Type.HasListId() : Boolean;
begin
  Result := ( FListId <> '' );
end;

function ListLookupRequest_Type.HasListType() : Boolean;
begin
  Result := True;
end;

function ListLookupRequest_Type.HasMerchantId() : Boolean;
begin
  Result := ( FMerchantId <> '' );
end;

function ListLookupRequest_Type.HasProductGroup() : Boolean;
begin
  Result := ( FProductGroup <> '' );
end;

function ListLookupRequest_Type.HasProductPage() : Boolean;
begin
  Result := ( FProductPage <> positiveInteger(0) );
end;

function ListLookupRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> ListLookupRequest_ResponseGroupArray(0) );
end;

function ListLookupRequest_Type.HasReviewSort() : Boolean;
begin
  Result := ( FReviewSort <> '' );
end;

function ListLookupRequest_Type.HasSort() : Boolean;
begin
  Result := ( FSort <> '' );
end;

{ CustomerContentSearchRequest_Type }

constructor CustomerContentSearchRequest_Type.Create();
begin
  inherited Create();
  FResponseGroup := CustomerContentSearchRequest_ResponseGroupArray.Create();
end;

destructor CustomerContentSearchRequest_Type.Destroy();
begin
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function CustomerContentSearchRequest_Type.HasCustomerPage() : Boolean;
begin
  Result := ( FCustomerPage <> positiveInteger(0) );
end;

function CustomerContentSearchRequest_Type.HasEmail() : Boolean;
begin
  Result := ( FEmail <> '' );
end;

function CustomerContentSearchRequest_Type.HasName() : Boolean;
begin
  Result := ( FName <> '' );
end;

function CustomerContentSearchRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> CustomerContentSearchRequest_ResponseGroupArray(0) );
end;

{ CustomerContentLookupRequest_Type }

constructor CustomerContentLookupRequest_Type.Create();
begin
  inherited Create();
  FResponseGroup := CustomerContentLookupRequest_ResponseGroupArray.Create();
end;

destructor CustomerContentLookupRequest_Type.Destroy();
begin
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function CustomerContentLookupRequest_Type.HasCustomerId() : Boolean;
begin
  Result := ( FCustomerId <> '' );
end;

function CustomerContentLookupRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> CustomerContentLookupRequest_ResponseGroupArray(0) );
end;

function CustomerContentLookupRequest_Type.HasReviewPage() : Boolean;
begin
  Result := ( FReviewPage <> positiveInteger(0) );
end;

function CustomerContentLookupRequest_Type.HasTagPage() : Boolean;
begin
  Result := ( FTagPage <> positiveInteger(0) );
end;

function CustomerContentLookupRequest_Type.HasTagsPerPage() : Boolean;
begin
  Result := ( FTagsPerPage <> positiveInteger(0) );
end;

function CustomerContentLookupRequest_Type.HasTagSort() : Boolean;
begin
  Result := ( FTagSort <> '' );
end;

{ SimilarityLookupRequest_Type }

constructor SimilarityLookupRequest_Type.Create();
begin
  inherited Create();
  FItemId := SimilarityLookupRequest_ItemIdArray.Create();
  FResponseGroup := SimilarityLookupRequest_ResponseGroupArray.Create();
end;

destructor SimilarityLookupRequest_Type.Destroy();
begin
  if Assigned(FItemId) then
    FreeAndNil(FItemId);
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function SimilarityLookupRequest_Type.HasCondition() : Boolean;
begin
  Result := True;
end;

function SimilarityLookupRequest_Type.HasDeliveryMethod() : Boolean;
begin
  Result := True;
end;

function SimilarityLookupRequest_Type.HasItemId() : Boolean;
begin
  Result := ( FItemId <> SimilarityLookupRequest_ItemIdArray(0) );
end;

function SimilarityLookupRequest_Type.HasISPUPostalCode() : Boolean;
begin
  Result := ( FISPUPostalCode <> '' );
end;

function SimilarityLookupRequest_Type.HasMerchantId() : Boolean;
begin
  Result := ( FMerchantId <> '' );
end;

function SimilarityLookupRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> SimilarityLookupRequest_ResponseGroupArray(0) );
end;

function SimilarityLookupRequest_Type.HasReviewSort() : Boolean;
begin
  Result := ( FReviewSort <> '' );
end;

function SimilarityLookupRequest_Type.HasSimilarityType() : Boolean;
begin
  Result := True;
end;

{ SellerLookupRequest_Type }

constructor SellerLookupRequest_Type.Create();
begin
  inherited Create();
  FResponseGroup := SellerLookupRequest_ResponseGroupArray.Create();
  FSellerId := SellerLookupRequest_SellerIdArray.Create();
end;

destructor SellerLookupRequest_Type.Destroy();
begin
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  if Assigned(FSellerId) then
    FreeAndNil(FSellerId);
  inherited Destroy();
end;

function SellerLookupRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> SellerLookupRequest_ResponseGroupArray(0) );
end;

function SellerLookupRequest_Type.HasSellerId() : Boolean;
begin
  Result := ( FSellerId <> SellerLookupRequest_SellerIdArray(0) );
end;

function SellerLookupRequest_Type.HasFeedbackPage() : Boolean;
begin
  Result := ( FFeedbackPage <> positiveInteger(0) );
end;

{ CartGetRequest_Type }

constructor CartGetRequest_Type.Create();
begin
  inherited Create();
  FResponseGroup := CartGetRequest_ResponseGroupArray.Create();
end;

destructor CartGetRequest_Type.Destroy();
begin
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function CartGetRequest_Type.HasCartId() : Boolean;
begin
  Result := ( FCartId <> '' );
end;

function CartGetRequest_Type.HasHMAC() : Boolean;
begin
  Result := ( FHMAC <> '' );
end;

function CartGetRequest_Type.HasMergeCart() : Boolean;
begin
  Result := ( FMergeCart <> '' );
end;

function CartGetRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> CartGetRequest_ResponseGroupArray(0) );
end;

function CartAddRequest_Items_Type_Item_Type_MetaData_Type.HasKey() : Boolean;
begin
  Result := ( FKey <> '' );
end;

function CartAddRequest_Items_Type_Item_Type_MetaData_Type.HasValue() : Boolean;
begin
  Result := ( FValue <> '' );
end;

{ CartAddRequest_Items_Type_Item_Type }

constructor CartAddRequest_Items_Type_Item_Type.Create();
begin
  inherited Create();
  FMetaData := CartAddRequest_Items_Type_Item_Type_MetaDataArray.Create();
end;

destructor CartAddRequest_Items_Type_Item_Type.Destroy();
begin
  if Assigned(FMetaData) then
    FreeAndNil(FMetaData);
  inherited Destroy();
end;

function CartAddRequest_Items_Type_Item_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function CartAddRequest_Items_Type_Item_Type.HasOfferListingId() : Boolean;
begin
  Result := ( FOfferListingId <> '' );
end;

function CartAddRequest_Items_Type_Item_Type.HasQuantity() : Boolean;
begin
  Result := ( FQuantity <> positiveInteger(0) );
end;

function CartAddRequest_Items_Type_Item_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function CartAddRequest_Items_Type_Item_Type.HasListItemId() : Boolean;
begin
  Result := ( FListItemId <> '' );
end;

function CartAddRequest_Items_Type_Item_Type.HasMetaData() : Boolean;
begin
  Result := ( FMetaData <> CartAddRequest_Items_Type_Item_Type_MetaDataArray(0) );
end;

{ CartAddRequest_Type }

constructor CartAddRequest_Type.Create();
begin
  inherited Create();
  FItems := CartAddRequest_Items_Type.Create();
  FResponseGroup := CartAddRequest_ResponseGroupArray.Create();
end;

destructor CartAddRequest_Type.Destroy();
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function CartAddRequest_Type.HasCartId() : Boolean;
begin
  Result := ( FCartId <> '' );
end;

function CartAddRequest_Type.HasHMAC() : Boolean;
begin
  Result := ( FHMAC <> '' );
end;

function CartAddRequest_Type.HasMergeCart() : Boolean;
begin
  Result := ( FMergeCart <> '' );
end;

function CartAddRequest_Type.HasItems() : Boolean;
begin
  Result := ( FItems <> CartAddRequest_Items_Type(0) );
end;

function CartAddRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> CartAddRequest_ResponseGroupArray(0) );
end;

function CartCreateRequest_Items_Type_Item_Type_MetaData_Type.HasKey() : Boolean;
begin
  Result := ( FKey <> '' );
end;

function CartCreateRequest_Items_Type_Item_Type_MetaData_Type.HasValue() : Boolean;
begin
  Result := ( FValue <> '' );
end;

{ CartCreateRequest_Items_Type_Item_Type }

constructor CartCreateRequest_Items_Type_Item_Type.Create();
begin
  inherited Create();
  FMetaData := CartCreateRequest_Items_Type_Item_Type_MetaDataArray.Create();
end;

destructor CartCreateRequest_Items_Type_Item_Type.Destroy();
begin
  if Assigned(FMetaData) then
    FreeAndNil(FMetaData);
  inherited Destroy();
end;

function CartCreateRequest_Items_Type_Item_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function CartCreateRequest_Items_Type_Item_Type.HasOfferListingId() : Boolean;
begin
  Result := ( FOfferListingId <> '' );
end;

function CartCreateRequest_Items_Type_Item_Type.HasQuantity() : Boolean;
begin
  Result := ( FQuantity <> positiveInteger(0) );
end;

function CartCreateRequest_Items_Type_Item_Type.HasAssociateTag() : Boolean;
begin
  Result := ( FAssociateTag <> '' );
end;

function CartCreateRequest_Items_Type_Item_Type.HasListItemId() : Boolean;
begin
  Result := ( FListItemId <> '' );
end;

function CartCreateRequest_Items_Type_Item_Type.HasMetaData() : Boolean;
begin
  Result := ( FMetaData <> CartCreateRequest_Items_Type_Item_Type_MetaDataArray(0) );
end;

{ CartCreateRequest_Type }

constructor CartCreateRequest_Type.Create();
begin
  inherited Create();
  FItems := CartCreateRequest_Items_Type.Create();
  FResponseGroup := CartCreateRequest_ResponseGroupArray.Create();
end;

destructor CartCreateRequest_Type.Destroy();
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function CartCreateRequest_Type.HasMergeCart() : Boolean;
begin
  Result := ( FMergeCart <> '' );
end;

function CartCreateRequest_Type.HasItems() : Boolean;
begin
  Result := ( FItems <> CartCreateRequest_Items_Type(0) );
end;

function CartCreateRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> CartCreateRequest_ResponseGroupArray(0) );
end;

function CartModifyRequest_Items_Type_Item_Type.HasAction() : Boolean;
begin
  Result := True;
end;

function CartModifyRequest_Items_Type_Item_Type.HasCartItemId() : Boolean;
begin
  Result := ( FCartItemId <> '' );
end;

function CartModifyRequest_Items_Type_Item_Type.HasQuantity() : Boolean;
begin
  Result := ( FQuantity <> nonNegativeInteger(0) );
end;

{ CartModifyRequest_Type }

constructor CartModifyRequest_Type.Create();
begin
  inherited Create();
  FItems := CartModifyRequest_Items_Type.Create();
  FResponseGroup := CartModifyRequest_ResponseGroupArray.Create();
end;

destructor CartModifyRequest_Type.Destroy();
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function CartModifyRequest_Type.HasCartId() : Boolean;
begin
  Result := ( FCartId <> '' );
end;

function CartModifyRequest_Type.HasHMAC() : Boolean;
begin
  Result := ( FHMAC <> '' );
end;

function CartModifyRequest_Type.HasMergeCart() : Boolean;
begin
  Result := ( FMergeCart <> '' );
end;

function CartModifyRequest_Type.HasItems() : Boolean;
begin
  Result := ( FItems <> CartModifyRequest_Items_Type(0) );
end;

function CartModifyRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> CartModifyRequest_ResponseGroupArray(0) );
end;

{ CartClearRequest_Type }

constructor CartClearRequest_Type.Create();
begin
  inherited Create();
  FResponseGroup := CartClearRequest_ResponseGroupArray.Create();
end;

destructor CartClearRequest_Type.Destroy();
begin
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function CartClearRequest_Type.HasCartId() : Boolean;
begin
  Result := ( FCartId <> '' );
end;

function CartClearRequest_Type.HasHMAC() : Boolean;
begin
  Result := ( FHMAC <> '' );
end;

function CartClearRequest_Type.HasMergeCart() : Boolean;
begin
  Result := ( FMergeCart <> '' );
end;

function CartClearRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> CartClearRequest_ResponseGroupArray(0) );
end;

{ TransactionLookupRequest_Type }

constructor TransactionLookupRequest_Type.Create();
begin
  inherited Create();
  FResponseGroup := TransactionLookupRequest_ResponseGroupArray.Create();
  FTransactionId := TransactionLookupRequest_TransactionIdArray.Create();
end;

destructor TransactionLookupRequest_Type.Destroy();
begin
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  if Assigned(FTransactionId) then
    FreeAndNil(FTransactionId);
  inherited Destroy();
end;

function TransactionLookupRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> TransactionLookupRequest_ResponseGroupArray(0) );
end;

function TransactionLookupRequest_Type.HasTransactionId() : Boolean;
begin
  Result := ( FTransactionId <> TransactionLookupRequest_TransactionIdArray(0) );
end;

{ SellerListingSearchRequest_Type }

constructor SellerListingSearchRequest_Type.Create();
begin
  inherited Create();
  FResponseGroup := SellerListingSearchRequest_ResponseGroupArray.Create();
end;

destructor SellerListingSearchRequest_Type.Destroy();
begin
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function SellerListingSearchRequest_Type.HasKeywords() : Boolean;
begin
  Result := ( FKeywords <> '' );
end;

function SellerListingSearchRequest_Type.HasListingPage() : Boolean;
begin
  Result := ( FListingPage <> positiveInteger(0) );
end;

function SellerListingSearchRequest_Type.HasOfferStatus() : Boolean;
begin
  Result := True;
end;

function SellerListingSearchRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> SellerListingSearchRequest_ResponseGroupArray(0) );
end;

function SellerListingSearchRequest_Type.HasSort() : Boolean;
begin
  Result := ( FSort <> '' );
end;

function SellerListingSearchRequest_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

{ SellerListingLookupRequest_Type }

constructor SellerListingLookupRequest_Type.Create();
begin
  inherited Create();
  FResponseGroup := SellerListingLookupRequest_ResponseGroupArray.Create();
end;

destructor SellerListingLookupRequest_Type.Destroy();
begin
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function SellerListingLookupRequest_Type.HasSellerId() : Boolean;
begin
  Result := ( FSellerId <> '' );
end;

function SellerListingLookupRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> SellerListingLookupRequest_ResponseGroupArray(0) );
end;

{ TagLookupRequest_Type }

constructor TagLookupRequest_Type.Create();
begin
  inherited Create();
  FTagName := TagLookupRequest_TagNameArray.Create();
  FResponseGroup := TagLookupRequest_ResponseGroupArray.Create();
end;

destructor TagLookupRequest_Type.Destroy();
begin
  if Assigned(FTagName) then
    FreeAndNil(FTagName);
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function TagLookupRequest_Type.HasTagName() : Boolean;
begin
  Result := ( FTagName <> TagLookupRequest_TagNameArray(0) );
end;

function TagLookupRequest_Type.HasCustomerId() : Boolean;
begin
  Result := ( FCustomerId <> '' );
end;

function TagLookupRequest_Type.HasTagPage() : Boolean;
begin
  Result := ( FTagPage <> positiveInteger(0) );
end;

function TagLookupRequest_Type.HasCount() : Boolean;
begin
  Result := ( FCount <> positiveInteger(0) );
end;

function TagLookupRequest_Type.HasTagSort() : Boolean;
begin
  Result := ( FTagSort <> '' );
end;

function TagLookupRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> TagLookupRequest_ResponseGroupArray(0) );
end;

{ BrowseNodeLookupRequest_Type }

constructor BrowseNodeLookupRequest_Type.Create();
begin
  inherited Create();
  FBrowseNodeId := BrowseNodeLookupRequest_BrowseNodeIdArray.Create();
  FResponseGroup := BrowseNodeLookupRequest_ResponseGroupArray.Create();
end;

destructor BrowseNodeLookupRequest_Type.Destroy();
begin
  if Assigned(FBrowseNodeId) then
    FreeAndNil(FBrowseNodeId);
  if Assigned(FResponseGroup) then
    FreeAndNil(FResponseGroup);
  inherited Destroy();
end;

function BrowseNodeLookupRequest_Type.HasBrowseNodeId() : Boolean;
begin
  Result := ( FBrowseNodeId <> BrowseNodeLookupRequest_BrowseNodeIdArray(0) );
end;

function BrowseNodeLookupRequest_Type.HasResponseGroup() : Boolean;
begin
  Result := ( FResponseGroup <> BrowseNodeLookupRequest_ResponseGroupArray(0) );
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
  if Assigned(FHTTPHeaders) then
    FreeAndNil(FHTTPHeaders);
  if Assigned(FArguments) then
    FreeAndNil(FArguments);
  if Assigned(FErrors) then
    FreeAndNil(FErrors);
  inherited Destroy();
end;

function OperationRequest_Type.HasHTTPHeaders() : Boolean;
begin
  Result := ( FHTTPHeaders <> HTTPHeaders_Type(0) );
end;

function OperationRequest_Type.HasRequestId() : Boolean;
begin
  Result := ( FRequestId <> '' );
end;

function OperationRequest_Type.HasArguments() : Boolean;
begin
  Result := ( FArguments <> Arguments_Type(0) );
end;

function OperationRequest_Type.HasErrors() : Boolean;
begin
  Result := ( FErrors <> Errors_Type(0) );
end;

function OperationRequest_Type.HasRequestProcessingTime() : Boolean;
begin
  Result := ( FRequestProcessingTime <> 0 );
end;

{ Request_Type }

constructor Request_Type.Create();
begin
  inherited Create();
  FHelpRequest := HelpRequest_Type.Create();
  FBrowseNodeLookupRequest := BrowseNodeLookupRequest_Type.Create();
  FItemSearchRequest := ItemSearchRequest_Type.Create();
  FItemLookupRequest := ItemLookupRequest_Type.Create();
  FListSearchRequest := ListSearchRequest_Type.Create();
  FListLookupRequest := ListLookupRequest_Type.Create();
  FCustomerContentSearchRequest := CustomerContentSearchRequest_Type.Create();
  FCustomerContentLookupRequest := CustomerContentLookupRequest_Type.Create();
  FSimilarityLookupRequest := SimilarityLookupRequest_Type.Create();
  FCartGetRequest := CartGetRequest_Type.Create();
  FCartAddRequest := CartAddRequest_Type.Create();
  FCartCreateRequest := CartCreateRequest_Type.Create();
  FCartModifyRequest := CartModifyRequest_Type.Create();
  FCartClearRequest := CartClearRequest_Type.Create();
  FTransactionLookupRequest := TransactionLookupRequest_Type.Create();
  FSellerListingSearchRequest := SellerListingSearchRequest_Type.Create();
  FSellerListingLookupRequest := SellerListingLookupRequest_Type.Create();
  FSellerLookupRequest := SellerLookupRequest_Type.Create();
  FTagLookupRequest := TagLookupRequest_Type.Create();
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
  if Assigned(FTagLookupRequest) then
    FreeAndNil(FTagLookupRequest);
  if Assigned(FErrors) then
    FreeAndNil(FErrors);
  inherited Destroy();
end;

function Request_Type.HasIsValid() : Boolean;
begin
  Result := ( FIsValid <> '' );
end;

function Request_Type.HasHelpRequest() : Boolean;
begin
  Result := ( FHelpRequest <> HelpRequest_Type(0) );
end;

function Request_Type.HasBrowseNodeLookupRequest() : Boolean;
begin
  Result := ( FBrowseNodeLookupRequest <> BrowseNodeLookupRequest_Type(0) );
end;

function Request_Type.HasItemSearchRequest() : Boolean;
begin
  Result := ( FItemSearchRequest <> ItemSearchRequest_Type(0) );
end;

function Request_Type.HasItemLookupRequest() : Boolean;
begin
  Result := ( FItemLookupRequest <> ItemLookupRequest_Type(0) );
end;

function Request_Type.HasListSearchRequest() : Boolean;
begin
  Result := ( FListSearchRequest <> ListSearchRequest_Type(0) );
end;

function Request_Type.HasListLookupRequest() : Boolean;
begin
  Result := ( FListLookupRequest <> ListLookupRequest_Type(0) );
end;

function Request_Type.HasCustomerContentSearchRequest() : Boolean;
begin
  Result := ( FCustomerContentSearchRequest <> CustomerContentSearchRequest_Type(0) );
end;

function Request_Type.HasCustomerContentLookupRequest() : Boolean;
begin
  Result := ( FCustomerContentLookupRequest <> CustomerContentLookupRequest_Type(0) );
end;

function Request_Type.HasSimilarityLookupRequest() : Boolean;
begin
  Result := ( FSimilarityLookupRequest <> SimilarityLookupRequest_Type(0) );
end;

function Request_Type.HasCartGetRequest() : Boolean;
begin
  Result := ( FCartGetRequest <> CartGetRequest_Type(0) );
end;

function Request_Type.HasCartAddRequest() : Boolean;
begin
  Result := ( FCartAddRequest <> CartAddRequest_Type(0) );
end;

function Request_Type.HasCartCreateRequest() : Boolean;
begin
  Result := ( FCartCreateRequest <> CartCreateRequest_Type(0) );
end;

function Request_Type.HasCartModifyRequest() : Boolean;
begin
  Result := ( FCartModifyRequest <> CartModifyRequest_Type(0) );
end;

function Request_Type.HasCartClearRequest() : Boolean;
begin
  Result := ( FCartClearRequest <> CartClearRequest_Type(0) );
end;

function Request_Type.HasTransactionLookupRequest() : Boolean;
begin
  Result := ( FTransactionLookupRequest <> TransactionLookupRequest_Type(0) );
end;

function Request_Type.HasSellerListingSearchRequest() : Boolean;
begin
  Result := ( FSellerListingSearchRequest <> SellerListingSearchRequest_Type(0) );
end;

function Request_Type.HasSellerListingLookupRequest() : Boolean;
begin
  Result := ( FSellerListingLookupRequest <> SellerListingLookupRequest_Type(0) );
end;

function Request_Type.HasSellerLookupRequest() : Boolean;
begin
  Result := ( FSellerLookupRequest <> SellerLookupRequest_Type(0) );
end;

function Request_Type.HasTagLookupRequest() : Boolean;
begin
  Result := ( FTagLookupRequest <> TagLookupRequest_Type(0) );
end;

function Request_Type.HasErrors() : Boolean;
begin
  Result := ( FErrors <> Errors_Type(0) );
end;

{ Information_Type }

constructor Information_Type.Create();
begin
  inherited Create();
  FRequest := Request_Type.Create();
  FOperationInformation := Information_OperationInformationArray.Create();
  FResponseGroupInformation := Information_ResponseGroupInformationArray.Create();
end;

destructor Information_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  if Assigned(FOperationInformation) then
    FreeAndNil(FOperationInformation);
  if Assigned(FResponseGroupInformation) then
    FreeAndNil(FResponseGroupInformation);
  inherited Destroy();
end;

function Information_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> Request_Type(0) );
end;

function Information_Type.HasOperationInformation() : Boolean;
begin
  Result := ( FOperationInformation <> Information_OperationInformationArray(0) );
end;

function Information_Type.HasResponseGroupInformation() : Boolean;
begin
  Result := ( FResponseGroupInformation <> Information_ResponseGroupInformationArray(0) );
end;

{ Items_Type }

constructor Items_Type.Create();
begin
  inherited Create();
  FRequest := Request_Type.Create();
  FCorrectedQuery := CorrectedQuery_Type.Create();
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
  if Assigned(FSearchResultsMap) then
    FreeAndNil(FSearchResultsMap);
  if Assigned(F_Item) then
    FreeAndNil(F_Item);
  if Assigned(FSearchBinSets) then
    FreeAndNil(FSearchBinSets);
  inherited Destroy();
end;

function Items_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> Request_Type(0) );
end;

function Items_Type.HasCorrectedQuery() : Boolean;
begin
  Result := ( FCorrectedQuery <> CorrectedQuery_Type(0) );
end;

function Items_Type.HasQid() : Boolean;
begin
  Result := ( FQid <> '' );
end;

function Items_Type.HasEngineQuery() : Boolean;
begin
  Result := ( FEngineQuery <> '' );
end;

function Items_Type.HasTotalResults() : Boolean;
begin
  Result := ( FTotalResults <> nonNegativeInteger(0) );
end;

function Items_Type.HasTotalPages() : Boolean;
begin
  Result := ( FTotalPages <> nonNegativeInteger(0) );
end;

function Items_Type.HasSearchResultsMap() : Boolean;
begin
  Result := ( FSearchResultsMap <> SearchResultsMap_Type(0) );
end;

function Items_Type.Has_Item() : Boolean;
begin
  Result := ( F_Item <> Items__ItemArray(0) );
end;

function Items_Type.HasSearchBinSets() : Boolean;
begin
  Result := ( FSearchBinSets <> SearchBinSets_Type(0) );
end;

function CorrectedQuery_Type.HasKeywords() : Boolean;
begin
  Result := ( FKeywords <> '' );
end;

function CorrectedQuery_Type.HasMessage() : Boolean;
begin
  Result := ( FMessage <> '' );
end;

{ Lists_Type }

constructor Lists_Type.Create();
begin
  inherited Create();
  FRequest := Request_Type.Create();
  FList := Lists_ListArray.Create();
end;

destructor Lists_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  if Assigned(FList) then
    FreeAndNil(FList);
  inherited Destroy();
end;

function Lists_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> Request_Type(0) );
end;

function Lists_Type.HasTotalResults() : Boolean;
begin
  Result := ( FTotalResults <> nonNegativeInteger(0) );
end;

function Lists_Type.HasTotalPages() : Boolean;
begin
  Result := ( FTotalPages <> nonNegativeInteger(0) );
end;

function Lists_Type.HasList() : Boolean;
begin
  Result := ( FList <> Lists_ListArray(0) );
end;

{ Customers_Type }

constructor Customers_Type.Create();
begin
  inherited Create();
  FRequest := Request_Type.Create();
  FCustomer := Customers_CustomerArray.Create();
end;

destructor Customers_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  if Assigned(FCustomer) then
    FreeAndNil(FCustomer);
  inherited Destroy();
end;

function Customers_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> Request_Type(0) );
end;

function Customers_Type.HasTotalResults() : Boolean;
begin
  Result := ( FTotalResults <> nonNegativeInteger(0) );
end;

function Customers_Type.HasTotalPages() : Boolean;
begin
  Result := ( FTotalPages <> nonNegativeInteger(0) );
end;

function Customers_Type.HasCustomer() : Boolean;
begin
  Result := ( FCustomer <> Customers_CustomerArray(0) );
end;

{ Cart_Type }

constructor Cart_Type.Create();
begin
  inherited Create();
  FRequest := Request_Type.Create();
  FSubTotal := Price_Type.Create();
  FCartItems := CartItems_Type.Create();
  FSavedForLaterItems := SavedForLaterItems_Type.Create();
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
  if Assigned(FSimilarProducts) then
    FreeAndNil(FSimilarProducts);
  if Assigned(FTopSellers) then
    FreeAndNil(FTopSellers);
  if Assigned(FNewReleases) then
    FreeAndNil(FNewReleases);
  if Assigned(FSimilarViewedProducts) then
    FreeAndNil(FSimilarViewedProducts);
  if Assigned(FOtherCategoriesSimilarProducts) then
    FreeAndNil(FOtherCategoriesSimilarProducts);
  inherited Destroy();
end;

function Cart_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> Request_Type(0) );
end;

function Cart_Type.HasPurchaseURL() : Boolean;
begin
  Result := ( FPurchaseURL <> '' );
end;

function Cart_Type.HasSubTotal() : Boolean;
begin
  Result := ( FSubTotal <> Price_Type(0) );
end;

function Cart_Type.HasCartItems() : Boolean;
begin
  Result := ( FCartItems <> CartItems_Type(0) );
end;

function Cart_Type.HasSavedForLaterItems() : Boolean;
begin
  Result := ( FSavedForLaterItems <> SavedForLaterItems_Type(0) );
end;

function Cart_Type.HasSimilarProducts() : Boolean;
begin
  Result := ( FSimilarProducts <> SimilarProducts_Type(0) );
end;

function Cart_Type.HasTopSellers() : Boolean;
begin
  Result := ( FTopSellers <> TopSellers_Type(0) );
end;

function Cart_Type.HasNewReleases() : Boolean;
begin
  Result := ( FNewReleases <> NewReleases_Type(0) );
end;

function Cart_Type.HasSimilarViewedProducts() : Boolean;
begin
  Result := ( FSimilarViewedProducts <> SimilarViewedProducts_Type(0) );
end;

function Cart_Type.HasOtherCategoriesSimilarProducts() : Boolean;
begin
  Result := ( FOtherCategoriesSimilarProducts <> OtherCategoriesSimilarProducts_Type(0) );
end;

{ Transactions_Type }

constructor Transactions_Type.Create();
begin
  inherited Create();
  FRequest := Request_Type.Create();
  FTransaction := Transactions_TransactionArray.Create();
end;

destructor Transactions_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  if Assigned(FTransaction) then
    FreeAndNil(FTransaction);
  inherited Destroy();
end;

function Transactions_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> Request_Type(0) );
end;

function Transactions_Type.HasTotalResults() : Boolean;
begin
  Result := ( FTotalResults <> nonNegativeInteger(0) );
end;

function Transactions_Type.HasTotalPages() : Boolean;
begin
  Result := ( FTotalPages <> nonNegativeInteger(0) );
end;

{ Sellers_Type }

constructor Sellers_Type.Create();
begin
  inherited Create();
  FRequest := Request_Type.Create();
  FSeller := Sellers_SellerArray.Create();
end;

destructor Sellers_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  if Assigned(FSeller) then
    FreeAndNil(FSeller);
  inherited Destroy();
end;

function Sellers_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> Request_Type(0) );
end;

function Sellers_Type.HasTotalResults() : Boolean;
begin
  Result := ( FTotalResults <> nonNegativeInteger(0) );
end;

function Sellers_Type.HasTotalPages() : Boolean;
begin
  Result := ( FTotalPages <> nonNegativeInteger(0) );
end;

function Sellers_Type.HasSeller() : Boolean;
begin
  Result := ( FSeller <> Sellers_SellerArray(0) );
end;

{ SellerListings_Type }

constructor SellerListings_Type.Create();
begin
  inherited Create();
  FRequest := Request_Type.Create();
  FSellerListing := SellerListings_SellerListingArray.Create();
end;

destructor SellerListings_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  if Assigned(FSellerListing) then
    FreeAndNil(FSellerListing);
  inherited Destroy();
end;

function SellerListings_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> Request_Type(0) );
end;

function SellerListings_Type.HasTotalResults() : Boolean;
begin
  Result := ( FTotalResults <> nonNegativeInteger(0) );
end;

function SellerListings_Type.HasTotalPages() : Boolean;
begin
  Result := ( FTotalPages <> nonNegativeInteger(0) );
end;

function SellerListings_Type.HasSellerListing() : Boolean;
begin
  Result := ( FSellerListing <> SellerListings_SellerListingArray(0) );
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
  if Assigned(FRequiredParameters) then
    FreeAndNil(FRequiredParameters);
  if Assigned(FAvailableParameters) then
    FreeAndNil(FAvailableParameters);
  if Assigned(FDefaultResponseGroups) then
    FreeAndNil(FDefaultResponseGroups);
  if Assigned(FAvailableResponseGroups) then
    FreeAndNil(FAvailableResponseGroups);
  inherited Destroy();
end;

function OperationInformation_Type.HasName() : Boolean;
begin
  Result := ( FName <> '' );
end;

function OperationInformation_Type.HasDescription() : Boolean;
begin
  Result := ( FDescription <> '' );
end;

function OperationInformation_Type.HasRequiredParameters() : Boolean;
begin
  Result := ( FRequiredParameters <> OperationInformation_RequiredParameters_Type(0) );
end;

function OperationInformation_Type.HasAvailableParameters() : Boolean;
begin
  Result := ( FAvailableParameters <> OperationInformation_AvailableParameters_Type(0) );
end;

function OperationInformation_Type.HasDefaultResponseGroups() : Boolean;
begin
  Result := ( FDefaultResponseGroups <> OperationInformation_DefaultResponseGroups_Type(0) );
end;

function OperationInformation_Type.HasAvailableResponseGroups() : Boolean;
begin
  Result := ( FAvailableResponseGroups <> OperationInformation_AvailableResponseGroups_Type(0) );
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
  if Assigned(FValidOperations) then
    FreeAndNil(FValidOperations);
  if Assigned(FElements) then
    FreeAndNil(FElements);
  inherited Destroy();
end;

function ResponseGroupInformation_Type.HasName() : Boolean;
begin
  Result := ( FName <> '' );
end;

function ResponseGroupInformation_Type.HasCreationDate() : Boolean;
begin
  Result := ( FCreationDate <> '' );
end;

function ResponseGroupInformation_Type.HasValidOperations() : Boolean;
begin
  Result := ( FValidOperations <> ResponseGroupInformation_ValidOperations_Type(0) );
end;

function ResponseGroupInformation_Type.HasElements() : Boolean;
begin
  Result := ( FElements <> ResponseGroupInformation_Elements_Type(0) );
end;

{ List_Type }

constructor List_Type.Create();
begin
  inherited Create();
  FImage := Image_Type.Create();
  FTags := Tags_Type.Create();
  FListItem := List_ListItemArray.Create();
end;

destructor List_Type.Destroy();
begin
  if Assigned(FImage) then
    FreeAndNil(FImage);
  if Assigned(FTags) then
    FreeAndNil(FTags);
  if Assigned(FListItem) then
    FreeAndNil(FListItem);
  inherited Destroy();
end;

function List_Type.HasListURL() : Boolean;
begin
  Result := ( FListURL <> '' );
end;

function List_Type.HasRegistryNumber() : Boolean;
begin
  Result := ( FRegistryNumber <> '' );
end;

function List_Type.HasListName() : Boolean;
begin
  Result := ( FListName <> '' );
end;

function List_Type.HasListType() : Boolean;
begin
  Result := True;
end;

function List_Type.HasTotalItems() : Boolean;
begin
  Result := ( FTotalItems <> nonNegativeInteger(0) );
end;

function List_Type.HasTotalPages() : Boolean;
begin
  Result := ( FTotalPages <> nonNegativeInteger(0) );
end;

function List_Type.HasDateCreated() : Boolean;
begin
  Result := ( FDateCreated <> '' );
end;

function List_Type.HasOccasionDate() : Boolean;
begin
  Result := ( FOccasionDate <> '' );
end;

function List_Type.HasCustomerName() : Boolean;
begin
  Result := ( FCustomerName <> '' );
end;

function List_Type.HasPartnerName() : Boolean;
begin
  Result := ( FPartnerName <> '' );
end;

function List_Type.HasAdditionalName() : Boolean;
begin
  Result := ( FAdditionalName <> '' );
end;

function List_Type.HasComment() : Boolean;
begin
  Result := ( FComment <> '' );
end;

function List_Type.HasImage() : Boolean;
begin
  Result := ( FImage <> Image_Type(0) );
end;

function List_Type.HasAverageRating() : Boolean;
begin
  Result := ( FAverageRating <> 0 );
end;

function List_Type.HasTotalVotes() : Boolean;
begin
  Result := ( FTotalVotes <> nonNegativeInteger(0) );
end;

function List_Type.HasTotalTimesRead() : Boolean;
begin
  Result := ( FTotalTimesRead <> nonNegativeInteger(0) );
end;

function List_Type.HasTags() : Boolean;
begin
  Result := ( FTags <> Tags_Type(0) );
end;

function List_Type.HasListItem() : Boolean;
begin
  Result := ( FListItem <> List_ListItemArray(0) );
end;

{ ListItem_Type }

constructor ListItem_Type.Create();
begin
  inherited Create();
  F_Item := Item_Type.Create();
end;

destructor ListItem_Type.Destroy();
begin
  if Assigned(F_Item) then
    FreeAndNil(F_Item);
  inherited Destroy();
end;

function ListItem_Type.HasListItemId() : Boolean;
begin
  Result := ( FListItemId <> '' );
end;

function ListItem_Type.HasDateAdded() : Boolean;
begin
  Result := ( FDateAdded <> '' );
end;

function ListItem_Type.HasComment() : Boolean;
begin
  Result := ( FComment <> '' );
end;

function ListItem_Type.HasQuantityDesired() : Boolean;
begin
  Result := ( FQuantityDesired <> '' );
end;

function ListItem_Type.HasQuantityReceived() : Boolean;
begin
  Result := ( FQuantityReceived <> '' );
end;

function ListItem_Type.Has_Item() : Boolean;
begin
  Result := ( F_Item <> Item_Type(0) );
end;

function Customer_Location_Type.HasUserDefinedLocation() : Boolean;
begin
  Result := ( FUserDefinedLocation <> '' );
end;

function Customer_Location_Type.HasCity() : Boolean;
begin
  Result := ( FCity <> '' );
end;

function Customer_Location_Type.HasState() : Boolean;
begin
  Result := ( FState <> '' );
end;

function Customer_Location_Type.HasCountry() : Boolean;
begin
  Result := ( FCountry <> '' );
end;

{ Customer_Type }

constructor Customer_Type.Create();
begin
  inherited Create();
  FLocation := Customer_Location_Type.Create();
  FCustomerReviews := Customer_CustomerReviewsArray.Create();
  FTags := Tags_Type.Create();
end;

destructor Customer_Type.Destroy();
begin
  if Assigned(FLocation) then
    FreeAndNil(FLocation);
  if Assigned(FCustomerReviews) then
    FreeAndNil(FCustomerReviews);
  if Assigned(FTags) then
    FreeAndNil(FTags);
  inherited Destroy();
end;

function Customer_Type.HasNickname() : Boolean;
begin
  Result := ( FNickname <> '' );
end;

function Customer_Type.HasBirthday() : Boolean;
begin
  Result := ( FBirthday <> '' );
end;

function Customer_Type.HasWishListId() : Boolean;
begin
  Result := ( FWishListId <> '' );
end;

function Customer_Type.HasLocation() : Boolean;
begin
  Result := ( FLocation <> Customer_Location_Type(0) );
end;

function Customer_Type.HasCustomerReviews() : Boolean;
begin
  Result := ( FCustomerReviews <> Customer_CustomerReviewsArray(0) );
end;

function Customer_Type.HasTags() : Boolean;
begin
  Result := ( FTags <> Tags_Type(0) );
end;

{ SearchResultsMap_SearchIndex_Type }

constructor SearchResultsMap_SearchIndex_Type.Create();
begin
  inherited Create();
  FCorrectedQuery := CorrectedQuery_Type.Create();
  FASIN := SearchResultsMap_SearchIndex_Type_ASINArray.Create();
end;

destructor SearchResultsMap_SearchIndex_Type.Destroy();
begin
  if Assigned(FCorrectedQuery) then
    FreeAndNil(FCorrectedQuery);
  if Assigned(FASIN) then
    FreeAndNil(FASIN);
  inherited Destroy();
end;

function SearchResultsMap_SearchIndex_Type.HasResults() : Boolean;
begin
  Result := ( FResults <> nonNegativeInteger(0) );
end;

function SearchResultsMap_SearchIndex_Type.HasPages() : Boolean;
begin
  Result := ( FPages <> nonNegativeInteger(0) );
end;

function SearchResultsMap_SearchIndex_Type.HasCorrectedQuery() : Boolean;
begin
  Result := ( FCorrectedQuery <> CorrectedQuery_Type(0) );
end;

{ Item_ImageSets_Type }

constructor Item_ImageSets_Type.Create();
begin
  inherited Create();
  FImageSet := Item_ImageSets_Type_ImageSetArray.Create();
end;

destructor Item_ImageSets_Type.Destroy();
begin
  if Assigned(FImageSet) then
    FreeAndNil(FImageSet);
  inherited Destroy();
end;

function Item_ImageSets_Type.HasMerchantId() : Boolean;
begin
  Result := ( FMerchantId <> '' );
end;

function Item_ImageSets_Type.HasImageSet() : Boolean;
begin
  Result := ( FImageSet <> Item_ImageSets_Type_ImageSetArray(0) );
end;

function Item_AlternateVersions_Type_AlternateVersion_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function Item_AlternateVersions_Type_AlternateVersion_Type.HasBinding() : Boolean;
begin
  Result := ( FBinding <> '' );
end;

{ Item_Type }

constructor Item_Type.Create();
begin
  inherited Create();
  FErrors := Errors_Type.Create();
  FSmallImage := Image_Type.Create();
  FMediumImage := Image_Type.Create();
  FLargeImage := Image_Type.Create();
  FImageSets := _Item_ImageSetsArray.Create();
  FItemAttributes := ItemAttributes_Type.Create();
  FMerchantItemAttributes := MerchantItemAttributes_Type.Create();
  FCollections := Collections_Type.Create();
  FSubjects := Item_Subjects_Type.Create();
  FOfferSummary := OfferSummary_Type.Create();
  FOffers := Offers_Type.Create();
  FVariationSummary := VariationSummary_Type.Create();
  FVariations := Variations_Type.Create();
  FCustomerReviews := CustomerReviews_Type.Create();
  FEditorialReviews := EditorialReviews_Type.Create();
  FSimilarProducts := SimilarProducts_Type.Create();
  FAccessories := Accessories_Type.Create();
  FTracks := Tracks_Type.Create();
  FBrowseNodes := BrowseNodes_Type.Create();
  FTags := Tags_Type.Create();
  FListmaniaLists := ListmaniaLists_Type.Create();
  FSearchInside := SearchInside_Type.Create();
  FAlternateVersions := Item_AlternateVersions_Type.Create();
end;

destructor Item_Type.Destroy();
begin
  if Assigned(FErrors) then
    FreeAndNil(FErrors);
  if Assigned(FSmallImage) then
    FreeAndNil(FSmallImage);
  if Assigned(FMediumImage) then
    FreeAndNil(FMediumImage);
  if Assigned(FLargeImage) then
    FreeAndNil(FLargeImage);
  if Assigned(FImageSets) then
    FreeAndNil(FImageSets);
  if Assigned(FItemAttributes) then
    FreeAndNil(FItemAttributes);
  if Assigned(FMerchantItemAttributes) then
    FreeAndNil(FMerchantItemAttributes);
  if Assigned(FCollections) then
    FreeAndNil(FCollections);
  if Assigned(FSubjects) then
    FreeAndNil(FSubjects);
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
  if Assigned(FEditorialReviews) then
    FreeAndNil(FEditorialReviews);
  if Assigned(FSimilarProducts) then
    FreeAndNil(FSimilarProducts);
  if Assigned(FAccessories) then
    FreeAndNil(FAccessories);
  if Assigned(FTracks) then
    FreeAndNil(FTracks);
  if Assigned(FBrowseNodes) then
    FreeAndNil(FBrowseNodes);
  if Assigned(FTags) then
    FreeAndNil(FTags);
  if Assigned(FListmaniaLists) then
    FreeAndNil(FListmaniaLists);
  if Assigned(FSearchInside) then
    FreeAndNil(FSearchInside);
  if Assigned(FAlternateVersions) then
    FreeAndNil(FAlternateVersions);
  inherited Destroy();
end;

function Item_Type.HasParentASIN() : Boolean;
begin
  Result := ( FParentASIN <> '' );
end;

function Item_Type.HasErrors() : Boolean;
begin
  Result := ( FErrors <> Errors_Type(0) );
end;

function Item_Type.HasDetailPageURL() : Boolean;
begin
  Result := ( FDetailPageURL <> '' );
end;

function Item_Type.HasSalesRank() : Boolean;
begin
  Result := ( FSalesRank <> '' );
end;

function Item_Type.HasSmallImage() : Boolean;
begin
  Result := ( FSmallImage <> Image_Type(0) );
end;

function Item_Type.HasMediumImage() : Boolean;
begin
  Result := ( FMediumImage <> Image_Type(0) );
end;

function Item_Type.HasLargeImage() : Boolean;
begin
  Result := ( FLargeImage <> Image_Type(0) );
end;

function Item_Type.HasImageSets() : Boolean;
begin
  Result := ( FImageSets <> _Item_ImageSetsArray(0) );
end;

function Item_Type.HasItemAttributes() : Boolean;
begin
  Result := ( FItemAttributes <> ItemAttributes_Type(0) );
end;

function Item_Type.HasMerchantItemAttributes() : Boolean;
begin
  Result := ( FMerchantItemAttributes <> MerchantItemAttributes_Type(0) );
end;

function Item_Type.HasCollections() : Boolean;
begin
  Result := ( FCollections <> Collections_Type(0) );
end;

function Item_Type.HasSubjects() : Boolean;
begin
  Result := ( FSubjects <> Item_Subjects_Type(0) );
end;

function Item_Type.HasOfferSummary() : Boolean;
begin
  Result := ( FOfferSummary <> OfferSummary_Type(0) );
end;

function Item_Type.HasOffers() : Boolean;
begin
  Result := ( FOffers <> Offers_Type(0) );
end;

function Item_Type.HasVariationSummary() : Boolean;
begin
  Result := ( FVariationSummary <> VariationSummary_Type(0) );
end;

function Item_Type.HasVariations() : Boolean;
begin
  Result := ( FVariations <> Variations_Type(0) );
end;

function Item_Type.HasCustomerReviews() : Boolean;
begin
  Result := ( FCustomerReviews <> CustomerReviews_Type(0) );
end;

function Item_Type.HasEditorialReviews() : Boolean;
begin
  Result := ( FEditorialReviews <> EditorialReviews_Type(0) );
end;

function Item_Type.HasSimilarProducts() : Boolean;
begin
  Result := ( FSimilarProducts <> SimilarProducts_Type(0) );
end;

function Item_Type.HasAccessories() : Boolean;
begin
  Result := ( FAccessories <> Accessories_Type(0) );
end;

function Item_Type.HasTracks() : Boolean;
begin
  Result := ( FTracks <> Tracks_Type(0) );
end;

function Item_Type.HasBrowseNodes() : Boolean;
begin
  Result := ( FBrowseNodes <> BrowseNodes_Type(0) );
end;

function Item_Type.HasTags() : Boolean;
begin
  Result := ( FTags <> Tags_Type(0) );
end;

function Item_Type.HasListmaniaLists() : Boolean;
begin
  Result := ( FListmaniaLists <> ListmaniaLists_Type(0) );
end;

function Item_Type.HasSearchInside() : Boolean;
begin
  Result := ( FSearchInside <> SearchInside_Type(0) );
end;

function Item_Type.HasAlternateVersions() : Boolean;
begin
  Result := ( FAlternateVersions <> Item_AlternateVersions_Type(0) );
end;

{ Tags_Type }

constructor Tags_Type.Create();
begin
  inherited Create();
  FRequest := Request_Type.Create();
  FFirstTagging := Tagging_Type.Create();
  FLastTagging := Tagging_Type.Create();
  FTag := Tags_TagArray.Create();
end;

destructor Tags_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  if Assigned(FFirstTagging) then
    FreeAndNil(FFirstTagging);
  if Assigned(FLastTagging) then
    FreeAndNil(FLastTagging);
  if Assigned(FTag) then
    FreeAndNil(FTag);
  inherited Destroy();
end;

function Tags_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> Request_Type(0) );
end;

function Tags_Type.HasDistinctTags() : Boolean;
begin
  Result := ( FDistinctTags <> '' );
end;

function Tags_Type.HasDistinctItems() : Boolean;
begin
  Result := ( FDistinctItems <> '' );
end;

function Tags_Type.HasDistinctUsers() : Boolean;
begin
  Result := ( FDistinctUsers <> '' );
end;

function Tags_Type.HasTotalUsages() : Boolean;
begin
  Result := ( FTotalUsages <> '' );
end;

function Tags_Type.HasFirstTagging() : Boolean;
begin
  Result := ( FFirstTagging <> Tagging_Type(0) );
end;

function Tags_Type.HasLastTagging() : Boolean;
begin
  Result := ( FLastTagging <> Tagging_Type(0) );
end;

function Tags_Type.HasTag() : Boolean;
begin
  Result := ( FTag <> Tags_TagArray(0) );
end;

{ Tag_Type }

constructor Tag_Type.Create();
begin
  inherited Create();
  FFirstTagging := Tagging_Type.Create();
  FLastTagging := Tagging_Type.Create();
  FTaggedItems := Tag_TaggedItemsArray.Create();
  FTaggedListmaniaLists := Tag_TaggedListmaniaListsArray.Create();
  FTaggedGuides := Tag_TaggedGuidesArray.Create();
end;

destructor Tag_Type.Destroy();
begin
  if Assigned(FFirstTagging) then
    FreeAndNil(FFirstTagging);
  if Assigned(FLastTagging) then
    FreeAndNil(FLastTagging);
  if Assigned(FTaggedItems) then
    FreeAndNil(FTaggedItems);
  if Assigned(FTaggedListmaniaLists) then
    FreeAndNil(FTaggedListmaniaLists);
  if Assigned(FTaggedGuides) then
    FreeAndNil(FTaggedGuides);
  inherited Destroy();
end;

function Tag_Type.HasName() : Boolean;
begin
  Result := ( FName <> '' );
end;

function Tag_Type.HasTagType() : Boolean;
begin
  Result := True;
end;

function Tag_Type.HasDistinctItems() : Boolean;
begin
  Result := ( FDistinctItems <> '' );
end;

function Tag_Type.HasDistinctUsers() : Boolean;
begin
  Result := ( FDistinctUsers <> '' );
end;

function Tag_Type.HasTotalUsages() : Boolean;
begin
  Result := ( FTotalUsages <> '' );
end;

function Tag_Type.HasFirstTagging() : Boolean;
begin
  Result := ( FFirstTagging <> Tagging_Type(0) );
end;

function Tag_Type.HasLastTagging() : Boolean;
begin
  Result := ( FLastTagging <> Tagging_Type(0) );
end;

function Tag_Type.HasTaggedItems() : Boolean;
begin
  Result := ( FTaggedItems <> Tag_TaggedItemsArray(0) );
end;

function Tag_Type.HasTaggedListmaniaLists() : Boolean;
begin
  Result := ( FTaggedListmaniaLists <> Tag_TaggedListmaniaListsArray(0) );
end;

function Tag_Type.HasTaggedGuides() : Boolean;
begin
  Result := ( FTaggedGuides <> Tag_TaggedGuidesArray(0) );
end;

{ TaggedItems_Type }

constructor TaggedItems_Type.Create();
begin
  inherited Create();
  F_Item := Item_Type.Create();
  FFirstTagging := Tagging_Type.Create();
  FLastTagging := Tagging_Type.Create();
end;

destructor TaggedItems_Type.Destroy();
begin
  if Assigned(F_Item) then
    FreeAndNil(F_Item);
  if Assigned(FFirstTagging) then
    FreeAndNil(FFirstTagging);
  if Assigned(FLastTagging) then
    FreeAndNil(FLastTagging);
  inherited Destroy();
end;

function TaggedItems_Type.Has_Item() : Boolean;
begin
  Result := ( F_Item <> Item_Type(0) );
end;

function TaggedItems_Type.HasDistinctUsers() : Boolean;
begin
  Result := ( FDistinctUsers <> '' );
end;

function TaggedItems_Type.HasTotalUsages() : Boolean;
begin
  Result := ( FTotalUsages <> '' );
end;

function TaggedItems_Type.HasFirstTagging() : Boolean;
begin
  Result := ( FFirstTagging <> Tagging_Type(0) );
end;

function TaggedItems_Type.HasLastTagging() : Boolean;
begin
  Result := ( FLastTagging <> Tagging_Type(0) );
end;

{ TaggedListmaniaLists_Type }

constructor TaggedListmaniaLists_Type.Create();
begin
  inherited Create();
  FList := List_Type.Create();
  FFirstTagging := Tagging_Type.Create();
  FLastTagging := Tagging_Type.Create();
end;

destructor TaggedListmaniaLists_Type.Destroy();
begin
  if Assigned(FList) then
    FreeAndNil(FList);
  if Assigned(FFirstTagging) then
    FreeAndNil(FFirstTagging);
  if Assigned(FLastTagging) then
    FreeAndNil(FLastTagging);
  inherited Destroy();
end;

function TaggedListmaniaLists_Type.HasList() : Boolean;
begin
  Result := ( FList <> List_Type(0) );
end;

function TaggedListmaniaLists_Type.HasDistinctUsers() : Boolean;
begin
  Result := ( FDistinctUsers <> '' );
end;

function TaggedListmaniaLists_Type.HasTotalUsages() : Boolean;
begin
  Result := ( FTotalUsages <> '' );
end;

function TaggedListmaniaLists_Type.HasFirstTagging() : Boolean;
begin
  Result := ( FFirstTagging <> Tagging_Type(0) );
end;

function TaggedListmaniaLists_Type.HasLastTagging() : Boolean;
begin
  Result := ( FLastTagging <> Tagging_Type(0) );
end;

{ TaggedGuides_Type }

constructor TaggedGuides_Type.Create();
begin
  inherited Create();
  FGuide := Guide_Type.Create();
  FFirstTagging := Tagging_Type.Create();
  FLastTagging := Tagging_Type.Create();
end;

destructor TaggedGuides_Type.Destroy();
begin
  if Assigned(FGuide) then
    FreeAndNil(FGuide);
  if Assigned(FFirstTagging) then
    FreeAndNil(FFirstTagging);
  if Assigned(FLastTagging) then
    FreeAndNil(FLastTagging);
  inherited Destroy();
end;

function TaggedGuides_Type.HasGuide() : Boolean;
begin
  Result := ( FGuide <> Guide_Type(0) );
end;

function TaggedGuides_Type.HasDistinctUsers() : Boolean;
begin
  Result := ( FDistinctUsers <> '' );
end;

function TaggedGuides_Type.HasTotalUsages() : Boolean;
begin
  Result := ( FTotalUsages <> '' );
end;

function TaggedGuides_Type.HasFirstTagging() : Boolean;
begin
  Result := ( FFirstTagging <> Tagging_Type(0) );
end;

function TaggedGuides_Type.HasLastTagging() : Boolean;
begin
  Result := ( FLastTagging <> Tagging_Type(0) );
end;

function Guide_Type.HasGuideId() : Boolean;
begin
  Result := ( FGuideId <> '' );
end;

function Tagging_Type.HasName() : Boolean;
begin
  Result := ( FName <> '' );
end;

function Tagging_Type.HasEntityId() : Boolean;
begin
  Result := ( FEntityId <> '' );
end;

function Tagging_Type.HasUserId() : Boolean;
begin
  Result := ( FUserId <> '' );
end;

function Tagging_Type.HasTime() : Boolean;
begin
  Result := ( FTime <> '' );
end;

{ OfferSummary_Type }

constructor OfferSummary_Type.Create();
begin
  inherited Create();
  FLowestNewPrice := Price_Type.Create();
  FLowestUsedPrice := Price_Type.Create();
  FLowestCollectiblePrice := Price_Type.Create();
  FLowestRefurbishedPrice := Price_Type.Create();
end;

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
  Result := ( FLowestNewPrice <> Price_Type(0) );
end;

function OfferSummary_Type.HasLowestUsedPrice() : Boolean;
begin
  Result := ( FLowestUsedPrice <> Price_Type(0) );
end;

function OfferSummary_Type.HasLowestCollectiblePrice() : Boolean;
begin
  Result := ( FLowestCollectiblePrice <> Price_Type(0) );
end;

function OfferSummary_Type.HasLowestRefurbishedPrice() : Boolean;
begin
  Result := ( FLowestRefurbishedPrice <> Price_Type(0) );
end;

function OfferSummary_Type.HasTotalNew() : Boolean;
begin
  Result := ( FTotalNew <> '' );
end;

function OfferSummary_Type.HasTotalUsed() : Boolean;
begin
  Result := ( FTotalUsed <> '' );
end;

function OfferSummary_Type.HasTotalCollectible() : Boolean;
begin
  Result := ( FTotalCollectible <> '' );
end;

function OfferSummary_Type.HasTotalRefurbished() : Boolean;
begin
  Result := ( FTotalRefurbished <> '' );
end;

{ Offers_Type }

constructor Offers_Type.Create();
begin
  inherited Create();
  FOffer := Offers_OfferArray.Create();
end;

destructor Offers_Type.Destroy();
begin
  if Assigned(FOffer) then
    FreeAndNil(FOffer);
  inherited Destroy();
end;

function Offers_Type.HasTotalOffers() : Boolean;
begin
  Result := ( FTotalOffers <> nonNegativeInteger(0) );
end;

function Offers_Type.HasTotalOfferPages() : Boolean;
begin
  Result := ( FTotalOfferPages <> nonNegativeInteger(0) );
end;

function Offers_Type.HasOffer() : Boolean;
begin
  Result := ( FOffer <> Offers_OfferArray(0) );
end;

{ Offer_Type }

constructor Offer_Type.Create();
begin
  inherited Create();
  FMerchant := Merchant_Type.Create();
  FSeller := Seller_Type.Create();
  FOfferAttributes := OfferAttributes_Type.Create();
  FOfferListing := Offer_OfferListingArray.Create();
  FLoyaltyPoints := LoyaltyPoints_Type.Create();
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
  if Assigned(FOfferListing) then
    FreeAndNil(FOfferListing);
  if Assigned(FLoyaltyPoints) then
    FreeAndNil(FLoyaltyPoints);
  if Assigned(FPromotions) then
    FreeAndNil(FPromotions);
  inherited Destroy();
end;

function Offer_Type.HasMerchant() : Boolean;
begin
  Result := ( FMerchant <> Merchant_Type(0) );
end;

function Offer_Type.HasSeller() : Boolean;
begin
  Result := ( FSeller <> Seller_Type(0) );
end;

function Offer_Type.HasOfferAttributes() : Boolean;
begin
  Result := ( FOfferAttributes <> OfferAttributes_Type(0) );
end;

function Offer_Type.HasOfferListing() : Boolean;
begin
  Result := ( FOfferListing <> Offer_OfferListingArray(0) );
end;

function Offer_Type.HasLoyaltyPoints() : Boolean;
begin
  Result := ( FLoyaltyPoints <> LoyaltyPoints_Type(0) );
end;

function Offer_Type.HasPromotions() : Boolean;
begin
  Result := ( FPromotions <> Promotions_Type(0) );
end;

function OfferAttributes_Type.HasCondition() : Boolean;
begin
  Result := ( FCondition <> '' );
end;

function OfferAttributes_Type.HasSubCondition() : Boolean;
begin
  Result := ( FSubCondition <> '' );
end;

function OfferAttributes_Type.HasConditionNote() : Boolean;
begin
  Result := ( FConditionNote <> '' );
end;

function OfferAttributes_Type.HasWillShipExpedited() : Boolean;
begin
  Result := ( FWillShipExpedited <> boolean(0) );
end;

function OfferAttributes_Type.HasWillShipInternational() : Boolean;
begin
  Result := ( FWillShipInternational <> boolean(0) );
end;

function Merchant_Type.HasName() : Boolean;
begin
  Result := ( FName <> '' );
end;

function Merchant_Type.HasGlancePage() : Boolean;
begin
  Result := ( FGlancePage <> '' );
end;

function Merchant_Type.HasAverageFeedbackRating() : Boolean;
begin
  Result := ( FAverageFeedbackRating <> 0 );
end;

function Merchant_Type.HasTotalFeedback() : Boolean;
begin
  Result := ( FTotalFeedback <> nonNegativeInteger(0) );
end;

function Merchant_Type.HasTotalFeedbackPages() : Boolean;
begin
  Result := ( FTotalFeedbackPages <> nonNegativeInteger(0) );
end;

function OfferListing_AvailabilityAttributes_Type.HasAvailabilityType() : Boolean;
begin
  Result := ( FAvailabilityType <> '' );
end;

function OfferListing_AvailabilityAttributes_Type.HasIsPreorder() : Boolean;
begin
  Result := ( FIsPreorder <> boolean(0) );
end;

function OfferListing_AvailabilityAttributes_Type.HasMinimumHours() : Boolean;
begin
  Result := ( FMinimumHours <> integer(0) );
end;

function OfferListing_AvailabilityAttributes_Type.HasMaximumHours() : Boolean;
begin
  Result := ( FMaximumHours <> integer(0) );
end;

{ OfferListing_ShippingCharge_Type }

constructor OfferListing_ShippingCharge_Type.Create();
begin
  inherited Create();
  FShippingPrice := Price_Type.Create();
end;

destructor OfferListing_ShippingCharge_Type.Destroy();
begin
  if Assigned(FShippingPrice) then
    FreeAndNil(FShippingPrice);
  inherited Destroy();
end;

{ OfferListing_Type }

constructor OfferListing_Type.Create();
begin
  inherited Create();
  FPrice := Price_Type.Create();
  FSalePrice := Price_Type.Create();
  FAmountSaved := Price_Type.Create();
  FAvailabilityAttributes := OfferListing_AvailabilityAttributes_Type.Create();
  FISPUStoreAddress := Address_Type.Create();
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
  if Assigned(FShippingCharge) then
    FreeAndNil(FShippingCharge);
  inherited Destroy();
end;

function OfferListing_Type.HasOfferListingId() : Boolean;
begin
  Result := ( FOfferListingId <> '' );
end;

function OfferListing_Type.HasExchangeId() : Boolean;
begin
  Result := ( FExchangeId <> '' );
end;

function OfferListing_Type.HasPrice() : Boolean;
begin
  Result := ( FPrice <> Price_Type(0) );
end;

function OfferListing_Type.HasSalePrice() : Boolean;
begin
  Result := ( FSalePrice <> Price_Type(0) );
end;

function OfferListing_Type.HasAmountSaved() : Boolean;
begin
  Result := ( FAmountSaved <> Price_Type(0) );
end;

function OfferListing_Type.HasPercentageSaved() : Boolean;
begin
  Result := ( FPercentageSaved <> nonNegativeInteger(0) );
end;

function OfferListing_Type.HasAvailability() : Boolean;
begin
  Result := ( FAvailability <> '' );
end;

function OfferListing_Type.HasAvailabilityAttributes() : Boolean;
begin
  Result := ( FAvailabilityAttributes <> OfferListing_AvailabilityAttributes_Type(0) );
end;

function OfferListing_Type.HasQuantity() : Boolean;
begin
  Result := ( FQuantity <> integer(0) );
end;

function OfferListing_Type.HasISPUStoreAddress() : Boolean;
begin
  Result := ( FISPUStoreAddress <> Address_Type(0) );
end;

function OfferListing_Type.HasISPUStoreHours() : Boolean;
begin
  Result := ( FISPUStoreHours <> '' );
end;

function OfferListing_Type.HasIsEligibleForSuperSaverShipping() : Boolean;
begin
  Result := ( FIsEligibleForSuperSaverShipping <> boolean(0) );
end;

function OfferListing_Type.HasSalesRestriction() : Boolean;
begin
  Result := ( FSalesRestriction <> '' );
end;

function OfferListing_Type.HasShippingCharge() : Boolean;
begin
  Result := ( FShippingCharge <> OfferListing_ShippingChargeArray(0) );
end;

{ LoyaltyPoints_Type }

constructor LoyaltyPoints_Type.Create();
begin
  inherited Create();
  FTypicalRedemptionValue := Price_Type.Create();
end;

destructor LoyaltyPoints_Type.Destroy();
begin
  if Assigned(FTypicalRedemptionValue) then
    FreeAndNil(FTypicalRedemptionValue);
  inherited Destroy();
end;

function LoyaltyPoints_Type.HasPoints() : Boolean;
begin
  Result := ( FPoints <> nonNegativeInteger(0) );
end;

function LoyaltyPoints_Type.HasTypicalRedemptionValue() : Boolean;
begin
  Result := ( FTypicalRedemptionValue <> Price_Type(0) );
end;

{ VariationSummary_Type }

constructor VariationSummary_Type.Create();
begin
  inherited Create();
  FLowestPrice := Price_Type.Create();
  FHighestPrice := Price_Type.Create();
  FLowestSalePrice := Price_Type.Create();
  FHighestSalePrice := Price_Type.Create();
end;

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
  Result := ( FLowestPrice <> Price_Type(0) );
end;

function VariationSummary_Type.HasHighestPrice() : Boolean;
begin
  Result := ( FHighestPrice <> Price_Type(0) );
end;

function VariationSummary_Type.HasLowestSalePrice() : Boolean;
begin
  Result := ( FLowestSalePrice <> Price_Type(0) );
end;

function VariationSummary_Type.HasHighestSalePrice() : Boolean;
begin
  Result := ( FHighestSalePrice <> Price_Type(0) );
end;

function VariationSummary_Type.HasSingleMerchantId() : Boolean;
begin
  Result := ( FSingleMerchantId <> '' );
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
  if Assigned(FVariationDimensions) then
    FreeAndNil(FVariationDimensions);
  if Assigned(F_Item) then
    FreeAndNil(F_Item);
  inherited Destroy();
end;

function Variations_Type.HasTotalVariations() : Boolean;
begin
  Result := ( FTotalVariations <> nonNegativeInteger(0) );
end;

function Variations_Type.HasTotalVariationPages() : Boolean;
begin
  Result := ( FTotalVariationPages <> nonNegativeInteger(0) );
end;

function Variations_Type.HasVariationDimensions() : Boolean;
begin
  Result := ( FVariationDimensions <> VariationDimensions_Type(0) );
end;

function Variations_Type.Has_Item() : Boolean;
begin
  Result := ( F_Item <> Variations__ItemArray(0) );
end;

{ Collections_Collection_Type_CollectionSummary_Type }

constructor Collections_Collection_Type_CollectionSummary_Type.Create();
begin
  inherited Create();
  FLowestListPrice := Price_Type.Create();
  FHighestListPrice := Price_Type.Create();
  FLowestSalePrice := Price_Type.Create();
  FHighestSalePrice := Price_Type.Create();
end;

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
  Result := ( FLowestListPrice <> Price_Type(0) );
end;

function Collections_Collection_Type_CollectionSummary_Type.HasHighestListPrice() : Boolean;
begin
  Result := ( FHighestListPrice <> Price_Type(0) );
end;

function Collections_Collection_Type_CollectionSummary_Type.HasLowestSalePrice() : Boolean;
begin
  Result := ( FLowestSalePrice <> Price_Type(0) );
end;

function Collections_Collection_Type_CollectionSummary_Type.HasHighestSalePrice() : Boolean;
begin
  Result := ( FHighestSalePrice <> Price_Type(0) );
end;

function Collections_Collection_Type_CollectionParent_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function Collections_Collection_Type_CollectionParent_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function Collections_Collection_Type_CollectionItem_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function Collections_Collection_Type_CollectionItem_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

{ Collections_Collection_Type }

constructor Collections_Collection_Type.Create();
begin
  inherited Create();
  FCollectionSummary := Collections_Collection_Type_CollectionSummary_Type.Create();
  FCollectionParent := Collections_Collection_Type_CollectionParent_Type.Create();
  FCollectionItem := Collections_Collection_Type_CollectionItemArray.Create();
end;

destructor Collections_Collection_Type.Destroy();
begin
  if Assigned(FCollectionSummary) then
    FreeAndNil(FCollectionSummary);
  if Assigned(FCollectionParent) then
    FreeAndNil(FCollectionParent);
  if Assigned(FCollectionItem) then
    FreeAndNil(FCollectionItem);
  inherited Destroy();
end;

function Collections_Collection_Type.HasCollectionSummary() : Boolean;
begin
  Result := ( FCollectionSummary <> Collections_Collection_Type_CollectionSummary_Type(0) );
end;

function Collections_Collection_Type.HasCollectionParent() : Boolean;
begin
  Result := ( FCollectionParent <> Collections_Collection_Type_CollectionParent_Type(0) );
end;

function Collections_Collection_Type.HasCollectionItem() : Boolean;
begin
  Result := ( FCollectionItem <> Collections_Collection_Type_CollectionItemArray(0) );
end;

function EditorialReview_Type.HasSource() : Boolean;
begin
  Result := ( FSource <> '' );
end;

function EditorialReview_Type.HasContent() : Boolean;
begin
  Result := ( FContent <> '' );
end;

function EditorialReview_Type.HasIsLinkSuppressed() : Boolean;
begin
  Result := ( FIsLinkSuppressed <> boolean(0) );
end;

{ CustomerReviews_Type }

constructor CustomerReviews_Type.Create();
begin
  inherited Create();
  FReview := CustomerReviews_ReviewArray.Create();
end;

destructor CustomerReviews_Type.Destroy();
begin
  if Assigned(FReview) then
    FreeAndNil(FReview);
  inherited Destroy();
end;

function CustomerReviews_Type.HasAverageRating() : Boolean;
begin
  Result := ( FAverageRating <> 0 );
end;

function CustomerReviews_Type.HasTotalReviews() : Boolean;
begin
  Result := ( FTotalReviews <> nonNegativeInteger(0) );
end;

function CustomerReviews_Type.HasTotalReviewPages() : Boolean;
begin
  Result := ( FTotalReviewPages <> nonNegativeInteger(0) );
end;

function CustomerReviews_Type.HasReview() : Boolean;
begin
  Result := ( FReview <> CustomerReviews_ReviewArray(0) );
end;

{ Review_Type }

constructor Review_Type.Create();
begin
  inherited Create();
  FReviewer := Reviewer_Type.Create();
end;

destructor Review_Type.Destroy();
begin
  if Assigned(FReviewer) then
    FreeAndNil(FReviewer);
  inherited Destroy();
end;

function Review_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function Review_Type.HasRating() : Boolean;
begin
  Result := ( FRating <> 0 );
end;

function Review_Type.HasHelpfulVotes() : Boolean;
begin
  Result := ( FHelpfulVotes <> nonNegativeInteger(0) );
end;

function Review_Type.HasCustomerId() : Boolean;
begin
  Result := ( FCustomerId <> '' );
end;

function Review_Type.HasReviewer() : Boolean;
begin
  Result := ( FReviewer <> Reviewer_Type(0) );
end;

function Review_Type.HasTotalVotes() : Boolean;
begin
  Result := ( FTotalVotes <> nonNegativeInteger(0) );
end;

function Review_Type.HasDate() : Boolean;
begin
  Result := ( FDate <> '' );
end;

function Review_Type.HasSummary() : Boolean;
begin
  Result := ( FSummary <> '' );
end;

function Review_Type.HasContent() : Boolean;
begin
  Result := ( FContent <> '' );
end;

function Reviewer_Type.HasCustomerId() : Boolean;
begin
  Result := ( FCustomerId <> '' );
end;

function Reviewer_Type.HasName() : Boolean;
begin
  Result := ( FName <> '' );
end;

function Reviewer_Type.HasNickname() : Boolean;
begin
  Result := ( FNickname <> '' );
end;

function Reviewer_Type.HasLocation() : Boolean;
begin
  Result := ( FLocation <> '' );
end;

{ Tracks_Disc_Type }

constructor Tracks_Disc_Type.Create();
begin
  inherited Create();
  FTrack := Tracks_Disc_Type_TrackArray.Create();
end;

destructor Tracks_Disc_Type.Destroy();
begin
  if Assigned(FTrack) then
    FreeAndNil(FTrack);
  inherited Destroy();
end;

function SimilarProducts_SimilarProduct_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function SimilarProducts_SimilarProduct_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function TopSellers_TopSeller_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function TopSellers_TopSeller_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function NewReleases_NewRelease_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function NewReleases_NewRelease_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function SimilarViewedProducts_SimilarViewedProduct_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function SimilarViewedProducts_SimilarViewedProduct_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function OtherCategoriesSimilarProducts_OtherCategoriesSimilarProduct_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function Accessories_Accessory_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function Accessories_Accessory_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function Promotion_Summary_Type.HasCategory() : Boolean;
begin
  Result := ( FCategory <> '' );
end;

function Promotion_Summary_Type.HasStartDate() : Boolean;
begin
  Result := ( FStartDate <> '' );
end;

function Promotion_Summary_Type.HasEndDate() : Boolean;
begin
  Result := ( FEndDate <> '' );
end;

function Promotion_Summary_Type.HasEligibilityRequirementDescription() : Boolean;
begin
  Result := ( FEligibilityRequirementDescription <> '' );
end;

function Promotion_Summary_Type.HasBenefitDescription() : Boolean;
begin
  Result := ( FBenefitDescription <> '' );
end;

function Promotion_Summary_Type.HasTermsAndConditions() : Boolean;
begin
  Result := ( FTermsAndConditions <> '' );
end;

{ Promotion_Details_Type }

constructor Promotion_Details_Type.Create();
begin
  inherited Create();
  FEligibilityRequirements := PromotionEligibilityRequirements_Type.Create();
  FBenefits := PromotionBenefits_Type.Create();
  FItemApplicability := PromotionItemApplicability_Type.Create();
end;

destructor Promotion_Details_Type.Destroy();
begin
  if Assigned(FEligibilityRequirements) then
    FreeAndNil(FEligibilityRequirements);
  if Assigned(FBenefits) then
    FreeAndNil(FBenefits);
  if Assigned(FItemApplicability) then
    FreeAndNil(FItemApplicability);
  inherited Destroy();
end;

function Promotion_Details_Type.HasMerchantPromotionId() : Boolean;
begin
  Result := ( FMerchantPromotionId <> '' );
end;

function Promotion_Details_Type.HasGroupClaimCode() : Boolean;
begin
  Result := ( FGroupClaimCode <> '' );
end;

function Promotion_Details_Type.HasCouponCombinationType() : Boolean;
begin
  Result := ( FCouponCombinationType <> '' );
end;

function Promotion_Details_Type.HasStartDate() : Boolean;
begin
  Result := ( FStartDate <> '' );
end;

function Promotion_Details_Type.HasEndDate() : Boolean;
begin
  Result := ( FEndDate <> '' );
end;

function Promotion_Details_Type.HasTermsAndConditions() : Boolean;
begin
  Result := ( FTermsAndConditions <> '' );
end;

function Promotion_Details_Type.HasEligibilityRequirements() : Boolean;
begin
  Result := ( FEligibilityRequirements <> PromotionEligibilityRequirements_Type(0) );
end;

function Promotion_Details_Type.HasBenefits() : Boolean;
begin
  Result := ( FBenefits <> PromotionBenefits_Type(0) );
end;

function Promotion_Details_Type.HasItemApplicability() : Boolean;
begin
  Result := ( FItemApplicability <> PromotionItemApplicability_Type(0) );
end;

function Promotion_Details_Type.HasMerchandisingMessage() : Boolean;
begin
  Result := ( FMerchandisingMessage <> '' );
end;

{ Promotion_Type }

constructor Promotion_Type.Create();
begin
  inherited Create();
  FSummary := Promotion_Summary_Type.Create();
  FDetails := Promotion_Details_Type.Create();
end;

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
  Result := ( FSummary <> Promotion_Summary_Type(0) );
end;

function Promotion_Type.HasDetails() : Boolean;
begin
  Result := ( FDetails <> Promotion_Details_Type(0) );
end;

{ PromotionBenefit_Type }

constructor PromotionBenefit_Type.Create();
begin
  inherited Create();
  FFixedAmount := Price_Type.Create();
  FCeiling := Price_Type.Create();
end;

destructor PromotionBenefit_Type.Destroy();
begin
  if Assigned(FFixedAmount) then
    FreeAndNil(FFixedAmount);
  if Assigned(FCeiling) then
    FreeAndNil(FCeiling);
  inherited Destroy();
end;

function PromotionBenefit_Type.HasQuantity() : Boolean;
begin
  Result := ( FQuantity <> integer(0) );
end;

function PromotionBenefit_Type.HasPercentOff() : Boolean;
begin
  Result := ( FPercentOff <> 0 );
end;

function PromotionBenefit_Type.HasFixedAmount() : Boolean;
begin
  Result := ( FFixedAmount <> Price_Type(0) );
end;

function PromotionBenefit_Type.HasCeiling() : Boolean;
begin
  Result := ( FCeiling <> Price_Type(0) );
end;

{ PromotionEligibilityRequirement_Type }

constructor PromotionEligibilityRequirement_Type.Create();
begin
  inherited Create();
  FCurrencyAmount := Price_Type.Create();
end;

destructor PromotionEligibilityRequirement_Type.Destroy();
begin
  if Assigned(FCurrencyAmount) then
    FreeAndNil(FCurrencyAmount);
  inherited Destroy();
end;

function PromotionEligibilityRequirement_Type.HasQuantity() : Boolean;
begin
  Result := ( FQuantity <> integer(0) );
end;

function PromotionEligibilityRequirement_Type.HasCurrencyAmount() : Boolean;
begin
  Result := ( FCurrencyAmount <> Price_Type(0) );
end;

{ BrowseNodes_Type }

constructor BrowseNodes_Type.Create();
begin
  inherited Create();
  FRequest := Request_Type.Create();
  FBrowseNode := BrowseNodes_BrowseNodeArray.Create();
end;

destructor BrowseNodes_Type.Destroy();
begin
  if Assigned(FRequest) then
    FreeAndNil(FRequest);
  if Assigned(FBrowseNode) then
    FreeAndNil(FBrowseNode);
  inherited Destroy();
end;

function BrowseNodes_Type.HasRequest() : Boolean;
begin
  Result := ( FRequest <> Request_Type(0) );
end;

function BrowseNodes_Type.HasBrowseNode() : Boolean;
begin
  Result := ( FBrowseNode <> BrowseNodes_BrowseNodeArray(0) );
end;

function Property_Type.HasName() : Boolean;
begin
  Result := ( FName <> '' );
end;

function Property_Type.HasValue() : Boolean;
begin
  Result := ( FValue <> '' );
end;

{ BrowseNode_Type }

constructor BrowseNode_Type.Create();
begin
  inherited Create();
  FProperties := BrowseNode_Properties_Type.Create();
  FChildren := BrowseNode_Children_Type.Create();
  FAncestors := BrowseNode_Ancestors_Type.Create();
  FTopSellers := TopSellers_Type.Create();
  FNewReleases := NewReleases_Type.Create();
end;

destructor BrowseNode_Type.Destroy();
begin
  if Assigned(FProperties) then
    FreeAndNil(FProperties);
  if Assigned(FChildren) then
    FreeAndNil(FChildren);
  if Assigned(FAncestors) then
    FreeAndNil(FAncestors);
  if Assigned(FTopSellers) then
    FreeAndNil(FTopSellers);
  if Assigned(FNewReleases) then
    FreeAndNil(FNewReleases);
  inherited Destroy();
end;

function BrowseNode_Type.HasBrowseNodeId() : Boolean;
begin
  Result := ( FBrowseNodeId <> '' );
end;

function BrowseNode_Type.HasName() : Boolean;
begin
  Result := ( FName <> '' );
end;

function BrowseNode_Type.HasIsCategoryRoot() : Boolean;
begin
  Result := ( FIsCategoryRoot <> boolean(0) );
end;

function BrowseNode_Type.HasProperties() : Boolean;
begin
  Result := ( FProperties <> BrowseNode_Properties_Type(0) );
end;

function BrowseNode_Type.HasChildren() : Boolean;
begin
  Result := ( FChildren <> BrowseNode_Children_Type(0) );
end;

function BrowseNode_Type.HasAncestors() : Boolean;
begin
  Result := ( FAncestors <> BrowseNode_Ancestors_Type(0) );
end;

function BrowseNode_Type.HasTopSellers() : Boolean;
begin
  Result := ( FTopSellers <> TopSellers_Type(0) );
end;

function BrowseNode_Type.HasNewReleases() : Boolean;
begin
  Result := ( FNewReleases <> NewReleases_Type(0) );
end;

function ListmaniaLists_ListmaniaList_Type.HasListName() : Boolean;
begin
  Result := ( FListName <> '' );
end;

function SearchInside_Excerpt_Type.HasChecksum() : Boolean;
begin
  Result := ( FChecksum <> '' );
end;

function SearchInside_Excerpt_Type.HasPageType() : Boolean;
begin
  Result := ( FPageType <> '' );
end;

function SearchInside_Excerpt_Type.HasPageNumber() : Boolean;
begin
  Result := ( FPageNumber <> '' );
end;

function SearchInside_Excerpt_Type.HasSequenceNumber() : Boolean;
begin
  Result := ( FSequenceNumber <> '' );
end;

function SearchInside_Excerpt_Type.HasText() : Boolean;
begin
  Result := ( FText <> '' );
end;

{ SearchInside_Type }

constructor SearchInside_Type.Create();
begin
  inherited Create();
  FExcerpt := SearchInside_Excerpt_Type.Create();
end;

destructor SearchInside_Type.Destroy();
begin
  if Assigned(FExcerpt) then
    FreeAndNil(FExcerpt);
  inherited Destroy();
end;

function SearchInside_Type.HasTotalExcerpts() : Boolean;
begin
  Result := ( FTotalExcerpts <> nonNegativeInteger(0) );
end;

function SearchInside_Type.HasExcerpt() : Boolean;
begin
  Result := ( FExcerpt <> SearchInside_Excerpt_Type(0) );
end;

{ CartItems_Type }

constructor CartItems_Type.Create();
begin
  inherited Create();
  FSubTotal := Price_Type.Create();
  FCartItem := CartItems_CartItemArray.Create();
end;

destructor CartItems_Type.Destroy();
begin
  if Assigned(FSubTotal) then
    FreeAndNil(FSubTotal);
  if Assigned(FCartItem) then
    FreeAndNil(FCartItem);
  inherited Destroy();
end;

function CartItems_Type.HasSubTotal() : Boolean;
begin
  Result := ( FSubTotal <> Price_Type(0) );
end;

{ SavedForLaterItems_Type }

constructor SavedForLaterItems_Type.Create();
begin
  inherited Create();
  FSubTotal := Price_Type.Create();
  FSavedForLaterItem := SavedForLaterItems_SavedForLaterItemArray.Create();
end;

destructor SavedForLaterItems_Type.Destroy();
begin
  if Assigned(FSubTotal) then
    FreeAndNil(FSubTotal);
  if Assigned(FSavedForLaterItem) then
    FreeAndNil(FSavedForLaterItem);
  inherited Destroy();
end;

function SavedForLaterItems_Type.HasSubTotal() : Boolean;
begin
  Result := ( FSubTotal <> Price_Type(0) );
end;

{ CartItem_Type }

constructor CartItem_Type.Create();
begin
  inherited Create();
  FMetaData := CartItem_MetaData_Type.Create();
  FPrice := Price_Type.Create();
  FItemTotal := Price_Type.Create();
end;

destructor CartItem_Type.Destroy();
begin
  if Assigned(FMetaData) then
    FreeAndNil(FMetaData);
  if Assigned(FPrice) then
    FreeAndNil(FPrice);
  if Assigned(FItemTotal) then
    FreeAndNil(FItemTotal);
  inherited Destroy();
end;

function CartItem_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function CartItem_Type.HasExchangeId() : Boolean;
begin
  Result := ( FExchangeId <> '' );
end;

function CartItem_Type.HasMerchantId() : Boolean;
begin
  Result := ( FMerchantId <> '' );
end;

function CartItem_Type.HasSellerId() : Boolean;
begin
  Result := ( FSellerId <> '' );
end;

function CartItem_Type.HasSellerNickname() : Boolean;
begin
  Result := ( FSellerNickname <> '' );
end;

function CartItem_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function CartItem_Type.HasProductGroup() : Boolean;
begin
  Result := ( FProductGroup <> '' );
end;

function CartItem_Type.HasListOwner() : Boolean;
begin
  Result := ( FListOwner <> '' );
end;

function CartItem_Type.HasListType() : Boolean;
begin
  Result := ( FListType <> '' );
end;

function CartItem_Type.HasMetaData() : Boolean;
begin
  Result := ( FMetaData <> CartItem_MetaData_Type(0) );
end;

function CartItem_Type.HasPrice() : Boolean;
begin
  Result := ( FPrice <> Price_Type(0) );
end;

function CartItem_Type.HasItemTotal() : Boolean;
begin
  Result := ( FItemTotal <> Price_Type(0) );
end;

{ Transaction_Totals_Type }

constructor Transaction_Totals_Type.Create();
begin
  inherited Create();
  FTotal := Price_Type.Create();
  FSubtotal := Price_Type.Create();
  FTax := Price_Type.Create();
  FShippingCharge := Price_Type.Create();
  FPromotion := Price_Type.Create();
end;

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

{ Transaction_Shipments_Type_Shipment_Type }

constructor Transaction_Shipments_Type_Shipment_Type.Create();
begin
  inherited Create();
  FShipmentItems := Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type.Create();
  FPackages := Transaction_Shipments_Type_Shipment_Type_Packages_Type.Create();
end;

destructor Transaction_Shipments_Type_Shipment_Type.Destroy();
begin
  if Assigned(FShipmentItems) then
    FreeAndNil(FShipmentItems);
  if Assigned(FPackages) then
    FreeAndNil(FPackages);
  inherited Destroy();
end;

function Transaction_Shipments_Type_Shipment_Type.HasShipmentItems() : Boolean;
begin
  Result := ( FShipmentItems <> Transaction_Shipments_Type_Shipment_Type_ShipmentItems_Type(0) );
end;

function Transaction_Shipments_Type_Shipment_Type.HasPackages() : Boolean;
begin
  Result := ( FPackages <> Transaction_Shipments_Type_Shipment_Type_Packages_Type(0) );
end;

{ Transaction_Type }

constructor Transaction_Type.Create();
begin
  inherited Create();
  FTotals := Transaction_Totals_Type.Create();
  FTransactionItems := Transaction_TransactionItems_Type.Create();
  FShipments := Transaction_Shipments_Type.Create();
end;

destructor Transaction_Type.Destroy();
begin
  if Assigned(FTotals) then
    FreeAndNil(FTotals);
  if Assigned(FTransactionItems) then
    FreeAndNil(FTransactionItems);
  if Assigned(FShipments) then
    FreeAndNil(FShipments);
  inherited Destroy();
end;

function Transaction_Type.HasSellerName() : Boolean;
begin
  Result := ( FSellerName <> '' );
end;

function Transaction_Type.HasPayingCustomerId() : Boolean;
begin
  Result := ( FPayingCustomerId <> '' );
end;

function Transaction_Type.HasOrderingCustomerId() : Boolean;
begin
  Result := ( FOrderingCustomerId <> '' );
end;

function Transaction_Type.HasTotals() : Boolean;
begin
  Result := ( FTotals <> Transaction_Totals_Type(0) );
end;

function Transaction_Type.HasTransactionItems() : Boolean;
begin
  Result := ( FTransactionItems <> Transaction_TransactionItems_Type(0) );
end;

function Transaction_Type.HasShipments() : Boolean;
begin
  Result := ( FShipments <> Transaction_Shipments_Type(0) );
end;

{ TransactionItem_Type }

constructor TransactionItem_Type.Create();
begin
  inherited Create();
  FUnitPrice := Price_Type.Create();
  FTotalPrice := Price_Type.Create();
  FChildTransactionItems := TransactionItem_ChildTransactionItems_Type.Create();
end;

destructor TransactionItem_Type.Destroy();
begin
  if Assigned(FUnitPrice) then
    FreeAndNil(FUnitPrice);
  if Assigned(FTotalPrice) then
    FreeAndNil(FTotalPrice);
  if Assigned(FChildTransactionItems) then
    FreeAndNil(FChildTransactionItems);
  inherited Destroy();
end;

function TransactionItem_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function TransactionItem_Type.HasChildTransactionItems() : Boolean;
begin
  Result := ( FChildTransactionItems <> TransactionItem_ChildTransactionItems_Type(0) );
end;

function Seller_Location_Type.HasUserDefinedLocation() : Boolean;
begin
  Result := ( FUserDefinedLocation <> '' );
end;

function Seller_Location_Type.HasCity() : Boolean;
begin
  Result := ( FCity <> '' );
end;

function Seller_Location_Type.HasState() : Boolean;
begin
  Result := ( FState <> '' );
end;

function Seller_Location_Type.HasCountry() : Boolean;
begin
  Result := ( FCountry <> '' );
end;

function Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type.HasCount() : Boolean;
begin
  Result := ( FCount <> nonNegativeInteger(0) );
end;

function Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type.HasPercentage() : Boolean;
begin
  Result := ( FPercentage <> nonNegativeInteger(0) );
end;

{ Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type }

constructor Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type.Create();
begin
  inherited Create();
  FSellerFeedbackRating := Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray.Create();
end;

destructor Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type.Destroy();
begin
  if Assigned(FSellerFeedbackRating) then
    FreeAndNil(FSellerFeedbackRating);
  inherited Destroy();
end;

function Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type.HasSellerFeedbackRating() : Boolean;
begin
  Result := ( FSellerFeedbackRating <> Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRatingArray(0) );
end;

{ Seller_Type }

constructor Seller_Type.Create();
begin
  inherited Create();
  FLocation := Seller_Location_Type.Create();
  FSellerFeedbackSummary := Seller_SellerFeedbackSummary_Type.Create();
  FSellerFeedback := SellerFeedback_Type.Create();
end;

destructor Seller_Type.Destroy();
begin
  if Assigned(FLocation) then
    FreeAndNil(FLocation);
  if Assigned(FSellerFeedbackSummary) then
    FreeAndNil(FSellerFeedbackSummary);
  if Assigned(FSellerFeedback) then
    FreeAndNil(FSellerFeedback);
  inherited Destroy();
end;

function Seller_Type.HasSellerName() : Boolean;
begin
  Result := ( FSellerName <> '' );
end;

function Seller_Type.HasSellerLegalName() : Boolean;
begin
  Result := ( FSellerLegalName <> '' );
end;

function Seller_Type.HasNickname() : Boolean;
begin
  Result := ( FNickname <> '' );
end;

function Seller_Type.HasGlancePage() : Boolean;
begin
  Result := ( FGlancePage <> '' );
end;

function Seller_Type.HasAbout() : Boolean;
begin
  Result := ( FAbout <> '' );
end;

function Seller_Type.HasMoreAbout() : Boolean;
begin
  Result := ( FMoreAbout <> '' );
end;

function Seller_Type.HasLocation() : Boolean;
begin
  Result := ( FLocation <> Seller_Location_Type(0) );
end;

function Seller_Type.HasAverageFeedbackRating() : Boolean;
begin
  Result := ( FAverageFeedbackRating <> 0 );
end;

function Seller_Type.HasTotalFeedback() : Boolean;
begin
  Result := ( FTotalFeedback <> nonNegativeInteger(0) );
end;

function Seller_Type.HasTotalFeedbackPages() : Boolean;
begin
  Result := ( FTotalFeedbackPages <> nonNegativeInteger(0) );
end;

function Seller_Type.HasSellerFeedbackSummary() : Boolean;
begin
  Result := ( FSellerFeedbackSummary <> Seller_SellerFeedbackSummary_Type(0) );
end;

function Seller_Type.HasSellerFeedback() : Boolean;
begin
  Result := ( FSellerFeedback <> SellerFeedback_Type(0) );
end;

function SellerFeedback_Feedback_Type.HasRating() : Boolean;
begin
  Result := ( FRating <> nonNegativeInteger(0) );
end;

function SellerFeedback_Feedback_Type.HasComment() : Boolean;
begin
  Result := ( FComment <> '' );
end;

function SellerFeedback_Feedback_Type.HasDate() : Boolean;
begin
  Result := ( FDate <> '' );
end;

function SellerFeedback_Feedback_Type.HasRatedBy() : Boolean;
begin
  Result := ( FRatedBy <> '' );
end;

function Address_Type.HasName() : Boolean;
begin
  Result := ( FName <> '' );
end;

function Address_Type.HasAddress1() : Boolean;
begin
  Result := ( FAddress1 <> '' );
end;

function Address_Type.HasAddress2() : Boolean;
begin
  Result := ( FAddress2 <> '' );
end;

function Address_Type.HasAddress3() : Boolean;
begin
  Result := ( FAddress3 <> '' );
end;

function Address_Type.HasCity() : Boolean;
begin
  Result := ( FCity <> '' );
end;

function Address_Type.HasState() : Boolean;
begin
  Result := ( FState <> '' );
end;

function Address_Type.HasPostalCode() : Boolean;
begin
  Result := ( FPostalCode <> '' );
end;

function Address_Type.HasCountry() : Boolean;
begin
  Result := ( FCountry <> '' );
end;

{ SellerListing_Type }

constructor SellerListing_Type.Create();
begin
  inherited Create();
  FPrice := Price_Type.Create();
  FSeller := Seller_Type.Create();
end;

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
  Result := ( FExchangeId <> '' );
end;

function SellerListing_Type.HasListingId() : Boolean;
begin
  Result := ( FListingId <> '' );
end;

function SellerListing_Type.HasASIN() : Boolean;
begin
  Result := ( FASIN <> '' );
end;

function SellerListing_Type.HasSKU() : Boolean;
begin
  Result := ( FSKU <> '' );
end;

function SellerListing_Type.HasUPC() : Boolean;
begin
  Result := ( FUPC <> '' );
end;

function SellerListing_Type.HasEAN() : Boolean;
begin
  Result := ( FEAN <> '' );
end;

function SellerListing_Type.HasWillShipExpedited() : Boolean;
begin
  Result := ( FWillShipExpedited <> boolean(0) );
end;

function SellerListing_Type.HasWillShipInternational() : Boolean;
begin
  Result := ( FWillShipInternational <> boolean(0) );
end;

function SellerListing_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function SellerListing_Type.HasPrice() : Boolean;
begin
  Result := ( FPrice <> Price_Type(0) );
end;

function SellerListing_Type.HasStartDate() : Boolean;
begin
  Result := ( FStartDate <> '' );
end;

function SellerListing_Type.HasEndDate() : Boolean;
begin
  Result := ( FEndDate <> '' );
end;

function SellerListing_Type.HasStatus() : Boolean;
begin
  Result := ( FStatus <> '' );
end;

function SellerListing_Type.HasQuantity() : Boolean;
begin
  Result := ( FQuantity <> '' );
end;

function SellerListing_Type.HasCondition() : Boolean;
begin
  Result := ( FCondition <> '' );
end;

function SellerListing_Type.HasSubCondition() : Boolean;
begin
  Result := ( FSubCondition <> '' );
end;

function SellerListing_Type.HasSeller() : Boolean;
begin
  Result := ( FSeller <> Seller_Type(0) );
end;

function Price_Type.HasAmount() : Boolean;
begin
  Result := ( FAmount <> integer(0) );
end;

function Price_Type.HasCurrencyCode() : Boolean;
begin
  Result := ( FCurrencyCode <> '' );
end;

{ ImageSet_Type }

constructor ImageSet_Type.Create();
begin
  inherited Create();
  FSwatchImage := Image_Type.Create();
  FSmallImage := Image_Type.Create();
  FThumbnailImage := Image_Type.Create();
  FTinyImage := Image_Type.Create();
  FMediumImage := Image_Type.Create();
  FLargeImage := Image_Type.Create();
end;

destructor ImageSet_Type.Destroy();
begin
  if Assigned(FSwatchImage) then
    FreeAndNil(FSwatchImage);
  if Assigned(FSmallImage) then
    FreeAndNil(FSmallImage);
  if Assigned(FThumbnailImage) then
    FreeAndNil(FThumbnailImage);
  if Assigned(FTinyImage) then
    FreeAndNil(FTinyImage);
  if Assigned(FMediumImage) then
    FreeAndNil(FMediumImage);
  if Assigned(FLargeImage) then
    FreeAndNil(FLargeImage);
  inherited Destroy();
end;

function ImageSet_Type.HasSwatchImage() : Boolean;
begin
  Result := ( FSwatchImage <> Image_Type(0) );
end;

function ImageSet_Type.HasSmallImage() : Boolean;
begin
  Result := ( FSmallImage <> Image_Type(0) );
end;

function ImageSet_Type.HasThumbnailImage() : Boolean;
begin
  Result := ( FThumbnailImage <> Image_Type(0) );
end;

function ImageSet_Type.HasTinyImage() : Boolean;
begin
  Result := ( FTinyImage <> Image_Type(0) );
end;

function ImageSet_Type.HasMediumImage() : Boolean;
begin
  Result := ( FMediumImage <> Image_Type(0) );
end;

function ImageSet_Type.HasLargeImage() : Boolean;
begin
  Result := ( FLargeImage <> Image_Type(0) );
end;

{ Image_Type }

constructor Image_Type.Create();
begin
  inherited Create();
  FHeight := DecimalWithUnits_Type.Create();
  FWidth := DecimalWithUnits_Type.Create();
end;

destructor Image_Type.Destroy();
begin
  if Assigned(FHeight) then
    FreeAndNil(FHeight);
  if Assigned(FWidth) then
    FreeAndNil(FWidth);
  inherited Destroy();
end;

function Image_Type.HasIsVerified() : Boolean;
begin
  Result := ( FIsVerified <> '' );
end;

{ ItemAttributes_ItemDimensions_Type }

constructor ItemAttributes_ItemDimensions_Type.Create();
begin
  inherited Create();
  FHeight := DecimalWithUnits_Type.Create();
  FLength := DecimalWithUnits_Type.Create();
  FWeight := DecimalWithUnits_Type.Create();
  FWidth := DecimalWithUnits_Type.Create();
end;

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
  Result := ( FHeight <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_ItemDimensions_Type.HasLength() : Boolean;
begin
  Result := ( FLength <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_ItemDimensions_Type.HasWeight() : Boolean;
begin
  Result := ( FWeight <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_ItemDimensions_Type.HasWidth() : Boolean;
begin
  Result := ( FWidth <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Languages_Type_Language_Type.Has_Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

function ItemAttributes_Languages_Type_Language_Type.HasAudioFormat() : Boolean;
begin
  Result := ( FAudioFormat <> '' );
end;

{ ItemAttributes_PackageDimensions_Type }

constructor ItemAttributes_PackageDimensions_Type.Create();
begin
  inherited Create();
  FHeight := DecimalWithUnits_Type.Create();
  FLength := DecimalWithUnits_Type.Create();
  FWeight := DecimalWithUnits_Type.Create();
  FWidth := DecimalWithUnits_Type.Create();
end;

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
  Result := ( FHeight <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_PackageDimensions_Type.HasLength() : Boolean;
begin
  Result := ( FLength <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_PackageDimensions_Type.HasWeight() : Boolean;
begin
  Result := ( FWeight <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_PackageDimensions_Type.HasWidth() : Boolean;
begin
  Result := ( FWidth <> DecimalWithUnits_Type(0) );
end;

{ ItemAttributes_Type }

constructor ItemAttributes_Type.Create();
begin
  inherited Create();
  FActor := ItemAttributes_ActorArray.Create();
  FAddress := Address_Type.Create();
  FAge := ItemAttributes_AgeArray.Create();
  FAmazonMaximumAge := DecimalWithUnits_Type.Create();
  FAmazonMinimumAge := DecimalWithUnits_Type.Create();
  FArtist := ItemAttributes_ArtistArray.Create();
  FAudioFormat := ItemAttributes_AudioFormatArray.Create();
  FAuthor := ItemAttributes_AuthorArray.Create();
  FBatteries := NonNegativeIntegerWithUnits_Type.Create();
  FCameraManualFeatures := ItemAttributes_CameraManualFeaturesArray.Create();
  FCaseDiameter := DecimalWithUnits_Type.Create();
  FCaseThickness := DecimalWithUnits_Type.Create();
  FCategory := ItemAttributes_CategoryArray.Create();
  FCategoryBin := ItemAttributes_CategoryBinArray.Create();
  FCharacter := ItemAttributes_CharacterArray.Create();
  FCompatibleDevices := ItemAttributes_CompatibleDevicesArray.Create();
  FContinuousShootingSpeed := DecimalWithUnits_Type.Create();
  FCPUSpeed := DecimalWithUnits_Type.Create();
  FCreator := ItemAttributes_CreatorArray.Create();
  FDataLinkProtocol := ItemAttributes_DataLinkProtocolArray.Create();
  FDelayBetweenShots := DecimalWithUnits_Type.Create();
  FDigitalZoom := DecimalWithUnits_Type.Create();
  FDirector := ItemAttributes_DirectorArray.Create();
  FDisplaySize := DecimalWithUnits_Type.Create();
  FEducationalFocus := ItemAttributes_EducationalFocusArray.Create();
  FEthnicity := ItemAttributes_EthnicityArray.Create();
  FFeature := ItemAttributes_FeatureArray.Create();
  FFirstIssueLeadTime := StringWithUnits_Type.Create();
  FFormat := ItemAttributes_FormatArray.Create();
  FFormFactor := ItemAttributes_FormFactorArray.Create();
  FGemTypeSetElement := ItemAttributes_GemTypeSetElementArray.Create();
  FGender := ItemAttributes_GenderArray.Create();
  FGraphicsMemorySize := DecimalWithUnits_Type.Create();
  FHardDiskSize := DecimalWithUnits_Type.Create();
  FIngredientsSetElement := ItemAttributes_IngredientsSetElementArray.Create();
  FInterest := ItemAttributes_InterestArray.Create();
  FISOEquivalent := NonNegativeIntegerWithUnits_Type.Create();
  FItemDimensions := ItemAttributes_ItemDimensions_Type.Create();
  FLanguageName := ItemAttributes_LanguageNameArray.Create();
  FLanguages := ItemAttributes_Languages_Type.Create();
  FListPrice := Price_Type.Create();
  FManufacturerMaximumAge := DecimalWithUnits_Type.Create();
  FManufacturerMinimumAge := DecimalWithUnits_Type.Create();
  FMaterialTypeSetElement := ItemAttributes_MaterialTypeSetElementArray.Create();
  FMaximumAperture := DecimalWithUnits_Type.Create();
  FMaximumFocalLength := DecimalWithUnits_Type.Create();
  FMaximumHighResolutionImages := NonNegativeIntegerWithUnits_Type.Create();
  FMaximumHorizontalResolution := NonNegativeIntegerWithUnits_Type.Create();
  FMaximumResolution := DecimalWithUnits_Type.Create();
  FMaximumShutterSpeed := DecimalWithUnits_Type.Create();
  FMaximumVerticalResolution := NonNegativeIntegerWithUnits_Type.Create();
  FMaximumWeightRecommendation := DecimalWithUnits_Type.Create();
  FMinimumFocalLength := DecimalWithUnits_Type.Create();
  FMinimumShutterSpeed := DecimalWithUnits_Type.Create();
  FMonitorSize := DecimalWithUnits_Type.Create();
  FMonitorViewableDiagonalSize := DecimalWithUnits_Type.Create();
  FOpticalSensorResolution := DecimalWithUnits_Type.Create();
  FOpticalZoom := DecimalWithUnits_Type.Create();
  FPackageDimensions := ItemAttributes_PackageDimensions_Type.Create();
  FPantLength := ItemAttributes_PantLengthArray.Create();
  FPantSize := ItemAttributes_PantSizeArray.Create();
  FPhotoFlashType := ItemAttributes_PhotoFlashTypeArray.Create();
  FPictureFormat := ItemAttributes_PictureFormatArray.Create();
  FPlatform := ItemAttributes_PlatformArray.Create();
  FPrimaryColor := ItemAttributes_PrimaryColorArray.Create();
  FReturnMethod := ItemAttributes_ReturnMethodArray.Create();
  FRunningTime := DecimalWithUnits_Type.Create();
  FSecondaryCacheSize := NonNegativeIntegerWithUnits_Type.Create();
  FShoeSize := ItemAttributes_ShoeSizeArray.Create();
  FSpecialFeatures := ItemAttributes_SpecialFeaturesArray.Create();
  FStoneWeight := DecimalWithUnits_Type.Create();
  FSubscriptionLength := NonNegativeIntegerWithUnits_Type.Create();
  FSupportedImageType := ItemAttributes_SupportedImageTypeArray.Create();
  FSystemBusSpeed := DecimalWithUnits_Type.Create();
  FSystemMemorySizeMax := DecimalWithUnits_Type.Create();
  FSystemMemorySize := DecimalWithUnits_Type.Create();
  FTargetBrand := ItemAttributes_TargetBrandArray.Create();
  FTotalDiamondWeight := DecimalWithUnits_Type.Create();
  FTotalGemWeight := DecimalWithUnits_Type.Create();
  FTotalMetalWeight := DecimalWithUnits_Type.Create();
  FWaterResistanceDepth := DecimalWithUnits_Type.Create();
  FWEEETaxValue := Price_Type.Create();
end;

destructor ItemAttributes_Type.Destroy();
begin
  if Assigned(FActor) then
    FreeAndNil(FActor);
  if Assigned(FAddress) then
    FreeAndNil(FAddress);
  if Assigned(FAge) then
    FreeAndNil(FAge);
  if Assigned(FAmazonMaximumAge) then
    FreeAndNil(FAmazonMaximumAge);
  if Assigned(FAmazonMinimumAge) then
    FreeAndNil(FAmazonMinimumAge);
  if Assigned(FArtist) then
    FreeAndNil(FArtist);
  if Assigned(FAudioFormat) then
    FreeAndNil(FAudioFormat);
  if Assigned(FAuthor) then
    FreeAndNil(FAuthor);
  if Assigned(FBatteries) then
    FreeAndNil(FBatteries);
  if Assigned(FCameraManualFeatures) then
    FreeAndNil(FCameraManualFeatures);
  if Assigned(FCaseDiameter) then
    FreeAndNil(FCaseDiameter);
  if Assigned(FCaseThickness) then
    FreeAndNil(FCaseThickness);
  if Assigned(FCategory) then
    FreeAndNil(FCategory);
  if Assigned(FCategoryBin) then
    FreeAndNil(FCategoryBin);
  if Assigned(FCharacter) then
    FreeAndNil(FCharacter);
  if Assigned(FCompatibleDevices) then
    FreeAndNil(FCompatibleDevices);
  if Assigned(FContinuousShootingSpeed) then
    FreeAndNil(FContinuousShootingSpeed);
  if Assigned(FCPUSpeed) then
    FreeAndNil(FCPUSpeed);
  if Assigned(FCreator) then
    FreeAndNil(FCreator);
  if Assigned(FDataLinkProtocol) then
    FreeAndNil(FDataLinkProtocol);
  if Assigned(FDelayBetweenShots) then
    FreeAndNil(FDelayBetweenShots);
  if Assigned(FDigitalZoom) then
    FreeAndNil(FDigitalZoom);
  if Assigned(FDirector) then
    FreeAndNil(FDirector);
  if Assigned(FDisplaySize) then
    FreeAndNil(FDisplaySize);
  if Assigned(FEducationalFocus) then
    FreeAndNil(FEducationalFocus);
  if Assigned(FEthnicity) then
    FreeAndNil(FEthnicity);
  if Assigned(FFeature) then
    FreeAndNil(FFeature);
  if Assigned(FFirstIssueLeadTime) then
    FreeAndNil(FFirstIssueLeadTime);
  if Assigned(FFormat) then
    FreeAndNil(FFormat);
  if Assigned(FFormFactor) then
    FreeAndNil(FFormFactor);
  if Assigned(FGemTypeSetElement) then
    FreeAndNil(FGemTypeSetElement);
  if Assigned(FGender) then
    FreeAndNil(FGender);
  if Assigned(FGraphicsMemorySize) then
    FreeAndNil(FGraphicsMemorySize);
  if Assigned(FHardDiskSize) then
    FreeAndNil(FHardDiskSize);
  if Assigned(FIngredientsSetElement) then
    FreeAndNil(FIngredientsSetElement);
  if Assigned(FInterest) then
    FreeAndNil(FInterest);
  if Assigned(FISOEquivalent) then
    FreeAndNil(FISOEquivalent);
  if Assigned(FItemDimensions) then
    FreeAndNil(FItemDimensions);
  if Assigned(FLanguageName) then
    FreeAndNil(FLanguageName);
  if Assigned(FLanguages) then
    FreeAndNil(FLanguages);
  if Assigned(FListPrice) then
    FreeAndNil(FListPrice);
  if Assigned(FManufacturerMaximumAge) then
    FreeAndNil(FManufacturerMaximumAge);
  if Assigned(FManufacturerMinimumAge) then
    FreeAndNil(FManufacturerMinimumAge);
  if Assigned(FMaterialTypeSetElement) then
    FreeAndNil(FMaterialTypeSetElement);
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
  if Assigned(FPantLength) then
    FreeAndNil(FPantLength);
  if Assigned(FPantSize) then
    FreeAndNil(FPantSize);
  if Assigned(FPhotoFlashType) then
    FreeAndNil(FPhotoFlashType);
  if Assigned(FPictureFormat) then
    FreeAndNil(FPictureFormat);
  if Assigned(FPlatform) then
    FreeAndNil(FPlatform);
  if Assigned(FPrimaryColor) then
    FreeAndNil(FPrimaryColor);
  if Assigned(FReturnMethod) then
    FreeAndNil(FReturnMethod);
  if Assigned(FRunningTime) then
    FreeAndNil(FRunningTime);
  if Assigned(FSecondaryCacheSize) then
    FreeAndNil(FSecondaryCacheSize);
  if Assigned(FShoeSize) then
    FreeAndNil(FShoeSize);
  if Assigned(FSpecialFeatures) then
    FreeAndNil(FSpecialFeatures);
  if Assigned(FStoneWeight) then
    FreeAndNil(FStoneWeight);
  if Assigned(FSubscriptionLength) then
    FreeAndNil(FSubscriptionLength);
  if Assigned(FSupportedImageType) then
    FreeAndNil(FSupportedImageType);
  if Assigned(FSystemBusSpeed) then
    FreeAndNil(FSystemBusSpeed);
  if Assigned(FSystemMemorySizeMax) then
    FreeAndNil(FSystemMemorySizeMax);
  if Assigned(FSystemMemorySize) then
    FreeAndNil(FSystemMemorySize);
  if Assigned(FTargetBrand) then
    FreeAndNil(FTargetBrand);
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

function ItemAttributes_Type.HasActor() : Boolean;
begin
  Result := ( FActor <> ItemAttributes_ActorArray(0) );
end;

function ItemAttributes_Type.HasAddress() : Boolean;
begin
  Result := ( FAddress <> Address_Type(0) );
end;

function ItemAttributes_Type.HasAge() : Boolean;
begin
  Result := ( FAge <> ItemAttributes_AgeArray(0) );
end;

function ItemAttributes_Type.HasAmazonMaximumAge() : Boolean;
begin
  Result := ( FAmazonMaximumAge <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasAmazonMinimumAge() : Boolean;
begin
  Result := ( FAmazonMinimumAge <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasAnalogVideoFormat() : Boolean;
begin
  Result := ( FAnalogVideoFormat <> '' );
end;

function ItemAttributes_Type.HasApertureModes() : Boolean;
begin
  Result := ( FApertureModes <> '' );
end;

function ItemAttributes_Type.HasArtist() : Boolean;
begin
  Result := ( FArtist <> ItemAttributes_ArtistArray(0) );
end;

function ItemAttributes_Type.HasAspectRatio() : Boolean;
begin
  Result := ( FAspectRatio <> '' );
end;

function ItemAttributes_Type.HasAssemblyInstructions() : Boolean;
begin
  Result := ( FAssemblyInstructions <> '' );
end;

function ItemAttributes_Type.HasAssemblyRequired() : Boolean;
begin
  Result := ( FAssemblyRequired <> '' );
end;

function ItemAttributes_Type.HasAudienceRating() : Boolean;
begin
  Result := ( FAudienceRating <> '' );
end;

function ItemAttributes_Type.HasAudioFormat() : Boolean;
begin
  Result := ( FAudioFormat <> ItemAttributes_AudioFormatArray(0) );
end;

function ItemAttributes_Type.HasAuthor() : Boolean;
begin
  Result := ( FAuthor <> ItemAttributes_AuthorArray(0) );
end;

function ItemAttributes_Type.HasBackFinding() : Boolean;
begin
  Result := ( FBackFinding <> '' );
end;

function ItemAttributes_Type.HasBandMaterialType() : Boolean;
begin
  Result := ( FBandMaterialType <> '' );
end;

function ItemAttributes_Type.HasBatteriesIncluded() : Boolean;
begin
  Result := ( FBatteriesIncluded <> '' );
end;

function ItemAttributes_Type.HasBatteriesRequired() : Boolean;
begin
  Result := ( FBatteriesRequired <> '' );
end;

function ItemAttributes_Type.HasBatteries() : Boolean;
begin
  Result := ( FBatteries <> NonNegativeIntegerWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasBatteryDescription() : Boolean;
begin
  Result := ( FBatteryDescription <> '' );
end;

function ItemAttributes_Type.HasBatteryType() : Boolean;
begin
  Result := ( FBatteryType <> '' );
end;

function ItemAttributes_Type.HasBezelMaterialType() : Boolean;
begin
  Result := ( FBezelMaterialType <> '' );
end;

function ItemAttributes_Type.HasBinding() : Boolean;
begin
  Result := ( FBinding <> '' );
end;

function ItemAttributes_Type.HasBrand() : Boolean;
begin
  Result := ( FBrand <> '' );
end;

function ItemAttributes_Type.HasCalendarType() : Boolean;
begin
  Result := ( FCalendarType <> '' );
end;

function ItemAttributes_Type.HasCameraManualFeatures() : Boolean;
begin
  Result := ( FCameraManualFeatures <> ItemAttributes_CameraManualFeaturesArray(0) );
end;

function ItemAttributes_Type.HasCaseDiameter() : Boolean;
begin
  Result := ( FCaseDiameter <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasCaseMaterialType() : Boolean;
begin
  Result := ( FCaseMaterialType <> '' );
end;

function ItemAttributes_Type.HasCaseThickness() : Boolean;
begin
  Result := ( FCaseThickness <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasCaseType() : Boolean;
begin
  Result := ( FCaseType <> '' );
end;

function ItemAttributes_Type.HasCatalogNumber() : Boolean;
begin
  Result := ( FCatalogNumber <> '' );
end;

function ItemAttributes_Type.HasCategory() : Boolean;
begin
  Result := ( FCategory <> ItemAttributes_CategoryArray(0) );
end;

function ItemAttributes_Type.HasCategoryBin() : Boolean;
begin
  Result := ( FCategoryBin <> ItemAttributes_CategoryBinArray(0) );
end;

function ItemAttributes_Type.HasCDRWDescription() : Boolean;
begin
  Result := ( FCDRWDescription <> '' );
end;

function ItemAttributes_Type.HasChainType() : Boolean;
begin
  Result := ( FChainType <> '' );
end;

function ItemAttributes_Type.HasCharacter() : Boolean;
begin
  Result := ( FCharacter <> ItemAttributes_CharacterArray(0) );
end;

function ItemAttributes_Type.HasCEROAgeRating() : Boolean;
begin
  Result := ( FCEROAgeRating <> '' );
end;

function ItemAttributes_Type.HasClaspType() : Boolean;
begin
  Result := ( FClaspType <> '' );
end;

function ItemAttributes_Type.HasClothingSize() : Boolean;
begin
  Result := ( FClothingSize <> '' );
end;

function ItemAttributes_Type.HasClubType() : Boolean;
begin
  Result := ( FClubType <> '' );
end;

function ItemAttributes_Type.HasColor() : Boolean;
begin
  Result := ( FColor <> '' );
end;

function ItemAttributes_Type.HasCompatibility() : Boolean;
begin
  Result := ( FCompatibility <> '' );
end;

function ItemAttributes_Type.HasCompatibleDevices() : Boolean;
begin
  Result := ( FCompatibleDevices <> ItemAttributes_CompatibleDevicesArray(0) );
end;

function ItemAttributes_Type.HasComputerHardwareType() : Boolean;
begin
  Result := ( FComputerHardwareType <> '' );
end;

function ItemAttributes_Type.HasComputerPlatform() : Boolean;
begin
  Result := ( FComputerPlatform <> '' );
end;

function ItemAttributes_Type.HasConnectivity() : Boolean;
begin
  Result := ( FConnectivity <> '' );
end;

function ItemAttributes_Type.HasContinuousShootingSpeed() : Boolean;
begin
  Result := ( FContinuousShootingSpeed <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasCountry() : Boolean;
begin
  Result := ( FCountry <> '' );
end;

function ItemAttributes_Type.HasCPUManufacturer() : Boolean;
begin
  Result := ( FCPUManufacturer <> '' );
end;

function ItemAttributes_Type.HasCPUSpeed() : Boolean;
begin
  Result := ( FCPUSpeed <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasCPUType() : Boolean;
begin
  Result := ( FCPUType <> '' );
end;

function ItemAttributes_Type.HasCreator() : Boolean;
begin
  Result := ( FCreator <> ItemAttributes_CreatorArray(0) );
end;

function ItemAttributes_Type.HasCuisine() : Boolean;
begin
  Result := ( FCuisine <> '' );
end;

function ItemAttributes_Type.HasDataLinkProtocol() : Boolean;
begin
  Result := ( FDataLinkProtocol <> ItemAttributes_DataLinkProtocolArray(0) );
end;

function ItemAttributes_Type.HasDeliveryOption() : Boolean;
begin
  Result := ( FDeliveryOption <> '' );
end;

function ItemAttributes_Type.HasDelayBetweenShots() : Boolean;
begin
  Result := ( FDelayBetweenShots <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasDepartment() : Boolean;
begin
  Result := ( FDepartment <> '' );
end;

function ItemAttributes_Type.HasDeweyDecimalNumber() : Boolean;
begin
  Result := ( FDeweyDecimalNumber <> '' );
end;

function ItemAttributes_Type.HasDialColor() : Boolean;
begin
  Result := ( FDialColor <> '' );
end;

function ItemAttributes_Type.HasDialWindowMaterialType() : Boolean;
begin
  Result := ( FDialWindowMaterialType <> '' );
end;

function ItemAttributes_Type.HasDigitalZoom() : Boolean;
begin
  Result := ( FDigitalZoom <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasDirector() : Boolean;
begin
  Result := ( FDirector <> ItemAttributes_DirectorArray(0) );
end;

function ItemAttributes_Type.HasDisplayColorSupport() : Boolean;
begin
  Result := ( FDisplayColorSupport <> '' );
end;

function ItemAttributes_Type.HasDisplaySize() : Boolean;
begin
  Result := ( FDisplaySize <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasDrumSetPieceQuantity() : Boolean;
begin
  Result := ( FDrumSetPieceQuantity <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasDVDLayers() : Boolean;
begin
  Result := ( FDVDLayers <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasDVDRWDescription() : Boolean;
begin
  Result := ( FDVDRWDescription <> '' );
end;

function ItemAttributes_Type.HasDVDSides() : Boolean;
begin
  Result := ( FDVDSides <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasDPCI() : Boolean;
begin
  Result := ( FDPCI <> '' );
end;

function ItemAttributes_Type.HasEAN() : Boolean;
begin
  Result := ( FEAN <> '' );
end;

function ItemAttributes_Type.HasEdition() : Boolean;
begin
  Result := ( FEdition <> '' );
end;

function ItemAttributes_Type.HasEducationalFocus() : Boolean;
begin
  Result := ( FEducationalFocus <> ItemAttributes_EducationalFocusArray(0) );
end;

function ItemAttributes_Type.HasEthnicity() : Boolean;
begin
  Result := ( FEthnicity <> ItemAttributes_EthnicityArray(0) );
end;

function ItemAttributes_Type.HasESRBAgeRating() : Boolean;
begin
  Result := ( FESRBAgeRating <> '' );
end;

function ItemAttributes_Type.HasExternalDisplaySupportDescription() : Boolean;
begin
  Result := ( FExternalDisplaySupportDescription <> '' );
end;

function ItemAttributes_Type.HasFabricType() : Boolean;
begin
  Result := ( FFabricType <> '' );
end;

function ItemAttributes_Type.HasFaxNumber() : Boolean;
begin
  Result := ( FFaxNumber <> '' );
end;

function ItemAttributes_Type.HasFeature() : Boolean;
begin
  Result := ( FFeature <> ItemAttributes_FeatureArray(0) );
end;

function ItemAttributes_Type.HasFilmColorType() : Boolean;
begin
  Result := ( FFilmColorType <> '' );
end;

function ItemAttributes_Type.HasFirstIssueLeadTime() : Boolean;
begin
  Result := ( FFirstIssueLeadTime <> StringWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasFlavorName() : Boolean;
begin
  Result := ( FFlavorName <> '' );
end;

function ItemAttributes_Type.HasFloppyDiskDriveDescription() : Boolean;
begin
  Result := ( FFloppyDiskDriveDescription <> '' );
end;

function ItemAttributes_Type.HasFormat() : Boolean;
begin
  Result := ( FFormat <> ItemAttributes_FormatArray(0) );
end;

function ItemAttributes_Type.HasFormFactor() : Boolean;
begin
  Result := ( FFormFactor <> ItemAttributes_FormFactorArray(0) );
end;

function ItemAttributes_Type.HasGemType() : Boolean;
begin
  Result := ( FGemType <> '' );
end;

function ItemAttributes_Type.HasGemTypeSetElement() : Boolean;
begin
  Result := ( FGemTypeSetElement <> ItemAttributes_GemTypeSetElementArray(0) );
end;

function ItemAttributes_Type.HasGender() : Boolean;
begin
  Result := ( FGender <> ItemAttributes_GenderArray(0) );
end;

function ItemAttributes_Type.HasGenre() : Boolean;
begin
  Result := ( FGenre <> '' );
end;

function ItemAttributes_Type.HasGLProductGroup() : Boolean;
begin
  Result := ( FGLProductGroup <> '' );
end;

function ItemAttributes_Type.HasGolfClubFlex() : Boolean;
begin
  Result := ( FGolfClubFlex <> '' );
end;

function ItemAttributes_Type.HasGolfClubLoft() : Boolean;
begin
  Result := ( FGolfClubLoft <> '' );
end;

function ItemAttributes_Type.HasGraphicsCardInterface() : Boolean;
begin
  Result := ( FGraphicsCardInterface <> '' );
end;

function ItemAttributes_Type.HasGraphicsDescription() : Boolean;
begin
  Result := ( FGraphicsDescription <> '' );
end;

function ItemAttributes_Type.HasGraphicsMemorySize() : Boolean;
begin
  Result := ( FGraphicsMemorySize <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasGuitarAttribute() : Boolean;
begin
  Result := ( FGuitarAttribute <> '' );
end;

function ItemAttributes_Type.HasGuitarBridgeSystem() : Boolean;
begin
  Result := ( FGuitarBridgeSystem <> '' );
end;

function ItemAttributes_Type.HasGuitarPickThickness() : Boolean;
begin
  Result := ( FGuitarPickThickness <> '' );
end;

function ItemAttributes_Type.HasGuitarPickupConfiguration() : Boolean;
begin
  Result := ( FGuitarPickupConfiguration <> '' );
end;

function ItemAttributes_Type.HasHandOrientation() : Boolean;
begin
  Result := ( FHandOrientation <> '' );
end;

function ItemAttributes_Type.HasHardDiskCount() : Boolean;
begin
  Result := ( FHardDiskCount <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasHardDiskSize() : Boolean;
begin
  Result := ( FHardDiskSize <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasHardDiskInterface() : Boolean;
begin
  Result := ( FHardDiskInterface <> '' );
end;

function ItemAttributes_Type.HasHardwarePlatform() : Boolean;
begin
  Result := ( FHardwarePlatform <> '' );
end;

function ItemAttributes_Type.HasHasAutoFocus() : Boolean;
begin
  Result := ( FHasAutoFocus <> boolean(0) );
end;

function ItemAttributes_Type.HasHasBurstMode() : Boolean;
begin
  Result := ( FHasBurstMode <> boolean(0) );
end;

function ItemAttributes_Type.HasHasInCameraEditing() : Boolean;
begin
  Result := ( FHasInCameraEditing <> boolean(0) );
end;

function ItemAttributes_Type.HasHasRedEyeReduction() : Boolean;
begin
  Result := ( FHasRedEyeReduction <> boolean(0) );
end;

function ItemAttributes_Type.HasHasSelfTimer() : Boolean;
begin
  Result := ( FHasSelfTimer <> boolean(0) );
end;

function ItemAttributes_Type.HasHasTripodMount() : Boolean;
begin
  Result := ( FHasTripodMount <> boolean(0) );
end;

function ItemAttributes_Type.HasHasVideoOut() : Boolean;
begin
  Result := ( FHasVideoOut <> boolean(0) );
end;

function ItemAttributes_Type.HasHasViewfinder() : Boolean;
begin
  Result := ( FHasViewfinder <> boolean(0) );
end;

function ItemAttributes_Type.HasHazardousMaterialType() : Boolean;
begin
  Result := ( FHazardousMaterialType <> '' );
end;

function ItemAttributes_Type.HasHoursOfOperation() : Boolean;
begin
  Result := ( FHoursOfOperation <> '' );
end;

function ItemAttributes_Type.HasIncludedSoftware() : Boolean;
begin
  Result := ( FIncludedSoftware <> '' );
end;

function ItemAttributes_Type.HasIncludesMp3Player() : Boolean;
begin
  Result := ( FIncludesMp3Player <> boolean(0) );
end;

function ItemAttributes_Type.HasIngredients() : Boolean;
begin
  Result := ( FIngredients <> '' );
end;

function ItemAttributes_Type.HasIngredientsSetElement() : Boolean;
begin
  Result := ( FIngredientsSetElement <> ItemAttributes_IngredientsSetElementArray(0) );
end;

function ItemAttributes_Type.HasInstrumentKey() : Boolean;
begin
  Result := ( FInstrumentKey <> '' );
end;

function ItemAttributes_Type.HasInterest() : Boolean;
begin
  Result := ( FInterest <> ItemAttributes_InterestArray(0) );
end;

function ItemAttributes_Type.HasIsAdultProduct() : Boolean;
begin
  Result := ( FIsAdultProduct <> boolean(0) );
end;

function ItemAttributes_Type.HasIsAutographed() : Boolean;
begin
  Result := ( FIsAutographed <> boolean(0) );
end;

function ItemAttributes_Type.HasISBN() : Boolean;
begin
  Result := ( FISBN <> '' );
end;

function ItemAttributes_Type.HasIsFragile() : Boolean;
begin
  Result := ( FIsFragile <> boolean(0) );
end;

function ItemAttributes_Type.HasIsLabCreated() : Boolean;
begin
  Result := ( FIsLabCreated <> boolean(0) );
end;

function ItemAttributes_Type.HasIsMemorabilia() : Boolean;
begin
  Result := ( FIsMemorabilia <> boolean(0) );
end;

function ItemAttributes_Type.HasISOEquivalent() : Boolean;
begin
  Result := ( FISOEquivalent <> NonNegativeIntegerWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasIsPreannounce() : Boolean;
begin
  Result := ( FIsPreannounce <> boolean(0) );
end;

function ItemAttributes_Type.HasIssuesPerYear() : Boolean;
begin
  Result := ( FIssuesPerYear <> '' );
end;

function ItemAttributes_Type.HasItemDimensions() : Boolean;
begin
  Result := ( FItemDimensions <> ItemAttributes_ItemDimensions_Type(0) );
end;

function ItemAttributes_Type.HasKeyboardDescription() : Boolean;
begin
  Result := ( FKeyboardDescription <> '' );
end;

function ItemAttributes_Type.Has_Label() : Boolean;
begin
  Result := ( F_Label <> '' );
end;

function ItemAttributes_Type.HasLanguageName() : Boolean;
begin
  Result := ( FLanguageName <> ItemAttributes_LanguageNameArray(0) );
end;

function ItemAttributes_Type.HasLanguages() : Boolean;
begin
  Result := ( FLanguages <> ItemAttributes_Languages_Type(0) );
end;

function ItemAttributes_Type.HasLegalDisclaimer() : Boolean;
begin
  Result := ( FLegalDisclaimer <> '' );
end;

function ItemAttributes_Type.HasLensType() : Boolean;
begin
  Result := ( FLensType <> '' );
end;

function ItemAttributes_Type.HasLineVoltage() : Boolean;
begin
  Result := ( FLineVoltage <> '' );
end;

function ItemAttributes_Type.HasListPrice() : Boolean;
begin
  Result := ( FListPrice <> Price_Type(0) );
end;

function ItemAttributes_Type.HasMacroFocusRange() : Boolean;
begin
  Result := ( FMacroFocusRange <> '' );
end;

function ItemAttributes_Type.HasMagazineType() : Boolean;
begin
  Result := ( FMagazineType <> '' );
end;

function ItemAttributes_Type.HasMalletHardness() : Boolean;
begin
  Result := ( FMalletHardness <> '' );
end;

function ItemAttributes_Type.HasManufacturer() : Boolean;
begin
  Result := ( FManufacturer <> '' );
end;

function ItemAttributes_Type.HasManufacturerLaborWarrantyDescription() : Boolean;
begin
  Result := ( FManufacturerLaborWarrantyDescription <> '' );
end;

function ItemAttributes_Type.HasManufacturerMaximumAge() : Boolean;
begin
  Result := ( FManufacturerMaximumAge <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasManufacturerMinimumAge() : Boolean;
begin
  Result := ( FManufacturerMinimumAge <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasManufacturerPartsWarrantyDescription() : Boolean;
begin
  Result := ( FManufacturerPartsWarrantyDescription <> '' );
end;

function ItemAttributes_Type.HasMaterialType() : Boolean;
begin
  Result := ( FMaterialType <> '' );
end;

function ItemAttributes_Type.HasMaterialTypeSetElement() : Boolean;
begin
  Result := ( FMaterialTypeSetElement <> ItemAttributes_MaterialTypeSetElementArray(0) );
end;

function ItemAttributes_Type.HasMaximumAperture() : Boolean;
begin
  Result := ( FMaximumAperture <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasMaximumColorDepth() : Boolean;
begin
  Result := ( FMaximumColorDepth <> '' );
end;

function ItemAttributes_Type.HasMaximumFocalLength() : Boolean;
begin
  Result := ( FMaximumFocalLength <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasMaximumHighResolutionImages() : Boolean;
begin
  Result := ( FMaximumHighResolutionImages <> NonNegativeIntegerWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasMaximumHorizontalResolution() : Boolean;
begin
  Result := ( FMaximumHorizontalResolution <> NonNegativeIntegerWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasMaximumLowResolutionImages() : Boolean;
begin
  Result := ( FMaximumLowResolutionImages <> '' );
end;

function ItemAttributes_Type.HasMaximumResolution() : Boolean;
begin
  Result := ( FMaximumResolution <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasMaximumShutterSpeed() : Boolean;
begin
  Result := ( FMaximumShutterSpeed <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasMaximumVerticalResolution() : Boolean;
begin
  Result := ( FMaximumVerticalResolution <> NonNegativeIntegerWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasMaximumWeightRecommendation() : Boolean;
begin
  Result := ( FMaximumWeightRecommendation <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasMediaType() : Boolean;
begin
  Result := ( FMediaType <> '' );
end;

function ItemAttributes_Type.HasMemorySlotsAvailable() : Boolean;
begin
  Result := ( FMemorySlotsAvailable <> '' );
end;

function ItemAttributes_Type.HasMetalStamp() : Boolean;
begin
  Result := ( FMetalStamp <> '' );
end;

function ItemAttributes_Type.HasMetalType() : Boolean;
begin
  Result := ( FMetalType <> '' );
end;

function ItemAttributes_Type.HasMiniMovieDescription() : Boolean;
begin
  Result := ( FMiniMovieDescription <> '' );
end;

function ItemAttributes_Type.HasMinimumFocalLength() : Boolean;
begin
  Result := ( FMinimumFocalLength <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasMinimumShutterSpeed() : Boolean;
begin
  Result := ( FMinimumShutterSpeed <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasModel() : Boolean;
begin
  Result := ( FModel <> '' );
end;

function ItemAttributes_Type.HasModelYear() : Boolean;
begin
  Result := ( FModelYear <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasModemDescription() : Boolean;
begin
  Result := ( FModemDescription <> '' );
end;

function ItemAttributes_Type.HasMonitorSize() : Boolean;
begin
  Result := ( FMonitorSize <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasMonitorViewableDiagonalSize() : Boolean;
begin
  Result := ( FMonitorViewableDiagonalSize <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasMouseDescription() : Boolean;
begin
  Result := ( FMouseDescription <> '' );
end;

function ItemAttributes_Type.HasMPN() : Boolean;
begin
  Result := ( FMPN <> '' );
end;

function ItemAttributes_Type.HasMusicalStyle() : Boolean;
begin
  Result := ( FMusicalStyle <> '' );
end;

function ItemAttributes_Type.HasNativeResolution() : Boolean;
begin
  Result := ( FNativeResolution <> '' );
end;

function ItemAttributes_Type.HasNeighborhood() : Boolean;
begin
  Result := ( FNeighborhood <> '' );
end;

function ItemAttributes_Type.HasNetworkInterfaceDescription() : Boolean;
begin
  Result := ( FNetworkInterfaceDescription <> '' );
end;

function ItemAttributes_Type.HasNotebookDisplayTechnology() : Boolean;
begin
  Result := ( FNotebookDisplayTechnology <> '' );
end;

function ItemAttributes_Type.HasNotebookPointingDeviceDescription() : Boolean;
begin
  Result := ( FNotebookPointingDeviceDescription <> '' );
end;

function ItemAttributes_Type.HasNumberOfDiscs() : Boolean;
begin
  Result := ( FNumberOfDiscs <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasNumberOfIssues() : Boolean;
begin
  Result := ( FNumberOfIssues <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasNumberOfItems() : Boolean;
begin
  Result := ( FNumberOfItems <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasNumberOfKeys() : Boolean;
begin
  Result := ( FNumberOfKeys <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasNumberOfPages() : Boolean;
begin
  Result := ( FNumberOfPages <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasNumberOfPearls() : Boolean;
begin
  Result := ( FNumberOfPearls <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasNumberOfRapidFireShots() : Boolean;
begin
  Result := ( FNumberOfRapidFireShots <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasNumberOfStones() : Boolean;
begin
  Result := ( FNumberOfStones <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasNumberOfStrings() : Boolean;
begin
  Result := ( FNumberOfStrings <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasNumberOfTracks() : Boolean;
begin
  Result := ( FNumberOfTracks <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasOperatingSystem() : Boolean;
begin
  Result := ( FOperatingSystem <> '' );
end;

function ItemAttributes_Type.HasOpticalSensorResolution() : Boolean;
begin
  Result := ( FOpticalSensorResolution <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasOpticalZoom() : Boolean;
begin
  Result := ( FOpticalZoom <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasOriginalReleaseDate() : Boolean;
begin
  Result := ( FOriginalReleaseDate <> '' );
end;

function ItemAttributes_Type.HasOutputWattage() : Boolean;
begin
  Result := ( FOutputWattage <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasPackageDimensions() : Boolean;
begin
  Result := ( FPackageDimensions <> ItemAttributes_PackageDimensions_Type(0) );
end;

function ItemAttributes_Type.HasPackageQuantity() : Boolean;
begin
  Result := ( FPackageQuantity <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasPantLength() : Boolean;
begin
  Result := ( FPantLength <> ItemAttributes_PantLengthArray(0) );
end;

function ItemAttributes_Type.HasPantSize() : Boolean;
begin
  Result := ( FPantSize <> ItemAttributes_PantSizeArray(0) );
end;

function ItemAttributes_Type.HasPearlLustre() : Boolean;
begin
  Result := ( FPearlLustre <> '' );
end;

function ItemAttributes_Type.HasPearlMinimumColor() : Boolean;
begin
  Result := ( FPearlMinimumColor <> '' );
end;

function ItemAttributes_Type.HasPearlShape() : Boolean;
begin
  Result := ( FPearlShape <> '' );
end;

function ItemAttributes_Type.HasPearlStringingMethod() : Boolean;
begin
  Result := ( FPearlStringingMethod <> '' );
end;

function ItemAttributes_Type.HasPearlSurfaceBlemishes() : Boolean;
begin
  Result := ( FPearlSurfaceBlemishes <> '' );
end;

function ItemAttributes_Type.HasPearlType() : Boolean;
begin
  Result := ( FPearlType <> '' );
end;

function ItemAttributes_Type.HasPearlUniformity() : Boolean;
begin
  Result := ( FPearlUniformity <> '' );
end;

function ItemAttributes_Type.HasPhoneNumber() : Boolean;
begin
  Result := ( FPhoneNumber <> '' );
end;

function ItemAttributes_Type.HasPhotoFlashType() : Boolean;
begin
  Result := ( FPhotoFlashType <> ItemAttributes_PhotoFlashTypeArray(0) );
end;

function ItemAttributes_Type.HasPictureFormat() : Boolean;
begin
  Result := ( FPictureFormat <> ItemAttributes_PictureFormatArray(0) );
end;

function ItemAttributes_Type.HasPlatform() : Boolean;
begin
  Result := ( FPlatform <> ItemAttributes_PlatformArray(0) );
end;

function ItemAttributes_Type.HasPriceRating() : Boolean;
begin
  Result := ( FPriceRating <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasPrimaryColor() : Boolean;
begin
  Result := ( FPrimaryColor <> ItemAttributes_PrimaryColorArray(0) );
end;

function ItemAttributes_Type.HasProcessorCount() : Boolean;
begin
  Result := ( FProcessorCount <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasProductGroup() : Boolean;
begin
  Result := ( FProductGroup <> '' );
end;

function ItemAttributes_Type.HasProductSiteLaunchDate() : Boolean;
begin
  Result := ( FProductSiteLaunchDate <> '' );
end;

function ItemAttributes_Type.HasProductTypeName() : Boolean;
begin
  Result := ( FProductTypeName <> '' );
end;

function ItemAttributes_Type.HasProductTypeSubcategory() : Boolean;
begin
  Result := ( FProductTypeSubcategory <> '' );
end;

function ItemAttributes_Type.HasPromotionalTag() : Boolean;
begin
  Result := ( FPromotionalTag <> '' );
end;

function ItemAttributes_Type.HasPublicationDate() : Boolean;
begin
  Result := ( FPublicationDate <> '' );
end;

function ItemAttributes_Type.HasPublisher() : Boolean;
begin
  Result := ( FPublisher <> '' );
end;

function ItemAttributes_Type.HasPOBoxShippingExcluded() : Boolean;
begin
  Result := ( FPOBoxShippingExcluded <> '' );
end;

function ItemAttributes_Type.HasReadingLevel() : Boolean;
begin
  Result := ( FReadingLevel <> '' );
end;

function ItemAttributes_Type.HasReturnMethod() : Boolean;
begin
  Result := ( FReturnMethod <> ItemAttributes_ReturnMethodArray(0) );
end;

function ItemAttributes_Type.HasRecorderTrackCount() : Boolean;
begin
  Result := ( FRecorderTrackCount <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasRegionCode() : Boolean;
begin
  Result := ( FRegionCode <> '' );
end;

function ItemAttributes_Type.HasRegionOfOrigin() : Boolean;
begin
  Result := ( FRegionOfOrigin <> '' );
end;

function ItemAttributes_Type.HasReturnPolicy() : Boolean;
begin
  Result := ( FReturnPolicy <> '' );
end;

function ItemAttributes_Type.HasReleaseDate() : Boolean;
begin
  Result := ( FReleaseDate <> '' );
end;

function ItemAttributes_Type.HasRemovableMemory() : Boolean;
begin
  Result := ( FRemovableMemory <> '' );
end;

function ItemAttributes_Type.HasRemovableStorage() : Boolean;
begin
  Result := ( FRemovableStorage <> '' );
end;

function ItemAttributes_Type.HasRequiredVoltageRange() : Boolean;
begin
  Result := ( FRequiredVoltageRange <> '' );
end;

function ItemAttributes_Type.HasResolutionModes() : Boolean;
begin
  Result := ( FResolutionModes <> '' );
end;

function ItemAttributes_Type.HasRingSize() : Boolean;
begin
  Result := ( FRingSize <> '' );
end;

function ItemAttributes_Type.HasRunningTime() : Boolean;
begin
  Result := ( FRunningTime <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasScentName() : Boolean;
begin
  Result := ( FScentName <> '' );
end;

function ItemAttributes_Type.HasSecondaryCacheSize() : Boolean;
begin
  Result := ( FSecondaryCacheSize <> NonNegativeIntegerWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasSettingType() : Boolean;
begin
  Result := ( FSettingType <> '' );
end;

function ItemAttributes_Type.HasShaftMaterialType() : Boolean;
begin
  Result := ( FShaftMaterialType <> '' );
end;

function ItemAttributes_Type.HasShoeSize() : Boolean;
begin
  Result := ( FShoeSize <> ItemAttributes_ShoeSizeArray(0) );
end;

function ItemAttributes_Type.HasSize() : Boolean;
begin
  Result := ( FSize <> '' );
end;

function ItemAttributes_Type.HasSizePerPearl() : Boolean;
begin
  Result := ( FSizePerPearl <> '' );
end;

function ItemAttributes_Type.HasSkillLevel() : Boolean;
begin
  Result := ( FSkillLevel <> '' );
end;

function ItemAttributes_Type.HasSKU() : Boolean;
begin
  Result := ( FSKU <> '' );
end;

function ItemAttributes_Type.HasSoldInStores() : Boolean;
begin
  Result := ( FSoldInStores <> '' );
end;

function ItemAttributes_Type.HasSoundCardDescription() : Boolean;
begin
  Result := ( FSoundCardDescription <> '' );
end;

function ItemAttributes_Type.HasSpeakerCount() : Boolean;
begin
  Result := ( FSpeakerCount <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasSpeakerDescription() : Boolean;
begin
  Result := ( FSpeakerDescription <> '' );
end;

function ItemAttributes_Type.HasSpecialFeatures() : Boolean;
begin
  Result := ( FSpecialFeatures <> ItemAttributes_SpecialFeaturesArray(0) );
end;

function ItemAttributes_Type.HasStoneClarity() : Boolean;
begin
  Result := ( FStoneClarity <> '' );
end;

function ItemAttributes_Type.HasStoneColor() : Boolean;
begin
  Result := ( FStoneColor <> '' );
end;

function ItemAttributes_Type.HasStoneCut() : Boolean;
begin
  Result := ( FStoneCut <> '' );
end;

function ItemAttributes_Type.HasStoneShape() : Boolean;
begin
  Result := ( FStoneShape <> '' );
end;

function ItemAttributes_Type.HasStoneWeight() : Boolean;
begin
  Result := ( FStoneWeight <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasStudio() : Boolean;
begin
  Result := ( FStudio <> '' );
end;

function ItemAttributes_Type.HasStyle() : Boolean;
begin
  Result := ( FStyle <> '' );
end;

function ItemAttributes_Type.HasSubscriptionLength() : Boolean;
begin
  Result := ( FSubscriptionLength <> NonNegativeIntegerWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasSupportedImageType() : Boolean;
begin
  Result := ( FSupportedImageType <> ItemAttributes_SupportedImageTypeArray(0) );
end;

function ItemAttributes_Type.HasSupportedMediaSize() : Boolean;
begin
  Result := ( FSupportedMediaSize <> '' );
end;

function ItemAttributes_Type.HasSystemBusSpeed() : Boolean;
begin
  Result := ( FSystemBusSpeed <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasSystemMemorySizeMax() : Boolean;
begin
  Result := ( FSystemMemorySizeMax <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasSystemMemorySize() : Boolean;
begin
  Result := ( FSystemMemorySize <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasSystemMemoryType() : Boolean;
begin
  Result := ( FSystemMemoryType <> '' );
end;

function ItemAttributes_Type.HasTargetBrand() : Boolean;
begin
  Result := ( FTargetBrand <> ItemAttributes_TargetBrandArray(0) );
end;

function ItemAttributes_Type.HasTellingPageIndicator() : Boolean;
begin
  Result := ( FTellingPageIndicator <> '' );
end;

function ItemAttributes_Type.HasTheatricalReleaseDate() : Boolean;
begin
  Result := ( FTheatricalReleaseDate <> '' );
end;

function ItemAttributes_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function ItemAttributes_Type.HasTotalDiamondWeight() : Boolean;
begin
  Result := ( FTotalDiamondWeight <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasTotalExternalBaysFree() : Boolean;
begin
  Result := ( FTotalExternalBaysFree <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasTotalFirewirePorts() : Boolean;
begin
  Result := ( FTotalFirewirePorts <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasTotalGemWeight() : Boolean;
begin
  Result := ( FTotalGemWeight <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasTotalInternalBaysFree() : Boolean;
begin
  Result := ( FTotalInternalBaysFree <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasTotalMetalWeight() : Boolean;
begin
  Result := ( FTotalMetalWeight <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasTotalNTSCPALPorts() : Boolean;
begin
  Result := ( FTotalNTSCPALPorts <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasTotalParallelPorts() : Boolean;
begin
  Result := ( FTotalParallelPorts <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasTotalPCCardSlots() : Boolean;
begin
  Result := ( FTotalPCCardSlots <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasTotalPCISlotsFree() : Boolean;
begin
  Result := ( FTotalPCISlotsFree <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasTotalSerialPorts() : Boolean;
begin
  Result := ( FTotalSerialPorts <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasTotalSVideoOutPorts() : Boolean;
begin
  Result := ( FTotalSVideoOutPorts <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasTotalUSB2Ports() : Boolean;
begin
  Result := ( FTotalUSB2Ports <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasTotalUSBPorts() : Boolean;
begin
  Result := ( FTotalUSBPorts <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasTotalVGAOutPorts() : Boolean;
begin
  Result := ( FTotalVGAOutPorts <> nonNegativeInteger(0) );
end;

function ItemAttributes_Type.HasUPC() : Boolean;
begin
  Result := ( FUPC <> '' );
end;

function ItemAttributes_Type.HasVariationDenomination() : Boolean;
begin
  Result := ( FVariationDenomination <> '' );
end;

function ItemAttributes_Type.HasVariationDescription() : Boolean;
begin
  Result := ( FVariationDescription <> '' );
end;

function ItemAttributes_Type.HasWarranty() : Boolean;
begin
  Result := ( FWarranty <> '' );
end;

function ItemAttributes_Type.HasWatchMovementType() : Boolean;
begin
  Result := ( FWatchMovementType <> '' );
end;

function ItemAttributes_Type.HasWaterResistanceDepth() : Boolean;
begin
  Result := ( FWaterResistanceDepth <> DecimalWithUnits_Type(0) );
end;

function ItemAttributes_Type.HasWEEETaxValue() : Boolean;
begin
  Result := ( FWEEETaxValue <> Price_Type(0) );
end;

function ItemAttributes_Type.HasWirelessMicrophoneFrequency() : Boolean;
begin
  Result := ( FWirelessMicrophoneFrequency <> nonNegativeInteger(0) );
end;

{ MerchantItemAttributes_ItemDimensions_Type }

constructor MerchantItemAttributes_ItemDimensions_Type.Create();
begin
  inherited Create();
  FHeight := DecimalWithUnits_Type.Create();
  FLength := DecimalWithUnits_Type.Create();
  FWeight := DecimalWithUnits_Type.Create();
  FWidth := DecimalWithUnits_Type.Create();
end;

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
  Result := ( FHeight <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_ItemDimensions_Type.HasLength() : Boolean;
begin
  Result := ( FLength <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_ItemDimensions_Type.HasWeight() : Boolean;
begin
  Result := ( FWeight <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_ItemDimensions_Type.HasWidth() : Boolean;
begin
  Result := ( FWidth <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Languages_Type_Language_Type.HasAudioFormat() : Boolean;
begin
  Result := ( FAudioFormat <> '' );
end;

{ MerchantItemAttributes_PackageDimensions_Type }

constructor MerchantItemAttributes_PackageDimensions_Type.Create();
begin
  inherited Create();
  FHeight := DecimalWithUnits_Type.Create();
  FLength := DecimalWithUnits_Type.Create();
  FWeight := DecimalWithUnits_Type.Create();
  FWidth := DecimalWithUnits_Type.Create();
end;

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
  Result := ( FHeight <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_PackageDimensions_Type.HasLength() : Boolean;
begin
  Result := ( FLength <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_PackageDimensions_Type.HasWeight() : Boolean;
begin
  Result := ( FWeight <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_PackageDimensions_Type.HasWidth() : Boolean;
begin
  Result := ( FWidth <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_VendorRebate_Type.Has_Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

function MerchantItemAttributes_VendorRebate_Type.HasStartDate() : Boolean;
begin
  Result := ( FStartDate <> '' );
end;

function MerchantItemAttributes_VendorRebate_Type.HasEndDate() : Boolean;
begin
  Result := ( FEndDate <> '' );
end;

{ MerchantItemAttributes_Type }

constructor MerchantItemAttributes_Type.Create();
begin
  inherited Create();
  FActor := MerchantItemAttributes_ActorArray.Create();
  FAddress := Address_Type.Create();
  FAmazonMaximumAge := DecimalWithUnits_Type.Create();
  FAmazonMinimumAge := DecimalWithUnits_Type.Create();
  FArtist := MerchantItemAttributes_ArtistArray.Create();
  FAudioFormat := MerchantItemAttributes_AudioFormatArray.Create();
  FAuthor := MerchantItemAttributes_AuthorArray.Create();
  FBatteries := NonNegativeIntegerWithUnits_Type.Create();
  FCameraManualFeatures := MerchantItemAttributes_CameraManualFeaturesArray.Create();
  FCaseDiameter := DecimalWithUnits_Type.Create();
  FCaseThickness := DecimalWithUnits_Type.Create();
  FContinuousShootingSpeed := DecimalWithUnits_Type.Create();
  FCPUSpeed := DecimalWithUnits_Type.Create();
  FCreator := MerchantItemAttributes_CreatorArray.Create();
  FDelayBetweenShots := DecimalWithUnits_Type.Create();
  FDigitalZoom := DecimalWithUnits_Type.Create();
  FDirector := MerchantItemAttributes_DirectorArray.Create();
  FDisplaySize := DecimalWithUnits_Type.Create();
  FFeature := MerchantItemAttributes_FeatureArray.Create();
  FFirstIssueLeadTime := StringWithUnits_Type.Create();
  FFormat := MerchantItemAttributes_FormatArray.Create();
  FFixedShippingCharge := Price_Type.Create();
  FGraphicsMemorySize := DecimalWithUnits_Type.Create();
  FHardDiskSize := NonNegativeIntegerWithUnits_Type.Create();
  FISOEquivalent := NonNegativeIntegerWithUnits_Type.Create();
  FItemDimensions := MerchantItemAttributes_ItemDimensions_Type.Create();
  FLanguages := MerchantItemAttributes_Languages_Type.Create();
  FListPrice := Price_Type.Create();
  FManufacturerMaximumAge := DecimalWithUnits_Type.Create();
  FManufacturerMinimumAge := DecimalWithUnits_Type.Create();
  FMaximumAperture := DecimalWithUnits_Type.Create();
  FMaximumFocalLength := DecimalWithUnits_Type.Create();
  FMaximumHighResolutionImages := NonNegativeIntegerWithUnits_Type.Create();
  FMaximumHorizontalResolution := NonNegativeIntegerWithUnits_Type.Create();
  FMaximumResolution := DecimalWithUnits_Type.Create();
  FMaximumShutterSpeed := DecimalWithUnits_Type.Create();
  FMaximumVerticalResolution := NonNegativeIntegerWithUnits_Type.Create();
  FMaximumWeightRecommendation := DecimalWithUnits_Type.Create();
  FMinimumFocalLength := DecimalWithUnits_Type.Create();
  FMinimumShutterSpeed := DecimalWithUnits_Type.Create();
  FMonitorSize := DecimalWithUnits_Type.Create();
  FMonitorViewableDiagonalSize := DecimalWithUnits_Type.Create();
  FOpticalZoom := DecimalWithUnits_Type.Create();
  FPackageDimensions := MerchantItemAttributes_PackageDimensions_Type.Create();
  FPhotoFlashType := MerchantItemAttributes_PhotoFlashTypeArray.Create();
  FPictureFormat := MerchantItemAttributes_PictureFormatArray.Create();
  FPlatform := MerchantItemAttributes_PlatformArray.Create();
  FPurchasingChannel := MerchantItemAttributes_PurchasingChannelArray.Create();
  FReturnMethod := MerchantItemAttributes_ReturnMethodArray.Create();
  FSecondaryCacheSize := NonNegativeIntegerWithUnits_Type.Create();
  FSpecialFeatures := MerchantItemAttributes_SpecialFeaturesArray.Create();
  FStoneWeight := DecimalWithUnits_Type.Create();
  FSubscriptionLength := NonNegativeIntegerWithUnits_Type.Create();
  FSupportedImageType := MerchantItemAttributes_SupportedImageTypeArray.Create();
  FSystemBusSpeed := DecimalWithUnits_Type.Create();
  FSystemMemorySizeMax := DecimalWithUnits_Type.Create();
  FSystemMemorySize := DecimalWithUnits_Type.Create();
  FTotalDiamondWeight := DecimalWithUnits_Type.Create();
  FTotalGemWeight := DecimalWithUnits_Type.Create();
  FTotalMetalWeight := DecimalWithUnits_Type.Create();
  FVendorRebate := MerchantItemAttributes_VendorRebate_Type.Create();
  FWaterResistanceDepth := DecimalWithUnits_Type.Create();
end;

destructor MerchantItemAttributes_Type.Destroy();
begin
  if Assigned(FActor) then
    FreeAndNil(FActor);
  if Assigned(FAddress) then
    FreeAndNil(FAddress);
  if Assigned(FAmazonMaximumAge) then
    FreeAndNil(FAmazonMaximumAge);
  if Assigned(FAmazonMinimumAge) then
    FreeAndNil(FAmazonMinimumAge);
  if Assigned(FArtist) then
    FreeAndNil(FArtist);
  if Assigned(FAudioFormat) then
    FreeAndNil(FAudioFormat);
  if Assigned(FAuthor) then
    FreeAndNil(FAuthor);
  if Assigned(FBatteries) then
    FreeAndNil(FBatteries);
  if Assigned(FCameraManualFeatures) then
    FreeAndNil(FCameraManualFeatures);
  if Assigned(FCaseDiameter) then
    FreeAndNil(FCaseDiameter);
  if Assigned(FCaseThickness) then
    FreeAndNil(FCaseThickness);
  if Assigned(FContinuousShootingSpeed) then
    FreeAndNil(FContinuousShootingSpeed);
  if Assigned(FCPUSpeed) then
    FreeAndNil(FCPUSpeed);
  if Assigned(FCreator) then
    FreeAndNil(FCreator);
  if Assigned(FDelayBetweenShots) then
    FreeAndNil(FDelayBetweenShots);
  if Assigned(FDigitalZoom) then
    FreeAndNil(FDigitalZoom);
  if Assigned(FDirector) then
    FreeAndNil(FDirector);
  if Assigned(FDisplaySize) then
    FreeAndNil(FDisplaySize);
  if Assigned(FFeature) then
    FreeAndNil(FFeature);
  if Assigned(FFirstIssueLeadTime) then
    FreeAndNil(FFirstIssueLeadTime);
  if Assigned(FFormat) then
    FreeAndNil(FFormat);
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
  if Assigned(FLanguages) then
    FreeAndNil(FLanguages);
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
  if Assigned(FPhotoFlashType) then
    FreeAndNil(FPhotoFlashType);
  if Assigned(FPictureFormat) then
    FreeAndNil(FPictureFormat);
  if Assigned(FPlatform) then
    FreeAndNil(FPlatform);
  if Assigned(FPurchasingChannel) then
    FreeAndNil(FPurchasingChannel);
  if Assigned(FReturnMethod) then
    FreeAndNil(FReturnMethod);
  if Assigned(FSecondaryCacheSize) then
    FreeAndNil(FSecondaryCacheSize);
  if Assigned(FSpecialFeatures) then
    FreeAndNil(FSpecialFeatures);
  if Assigned(FStoneWeight) then
    FreeAndNil(FStoneWeight);
  if Assigned(FSubscriptionLength) then
    FreeAndNil(FSubscriptionLength);
  if Assigned(FSupportedImageType) then
    FreeAndNil(FSupportedImageType);
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

function MerchantItemAttributes_Type.HasActor() : Boolean;
begin
  Result := ( FActor <> MerchantItemAttributes_ActorArray(0) );
end;

function MerchantItemAttributes_Type.HasAddress() : Boolean;
begin
  Result := ( FAddress <> Address_Type(0) );
end;

function MerchantItemAttributes_Type.HasAmazonMaximumAge() : Boolean;
begin
  Result := ( FAmazonMaximumAge <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasAmazonMinimumAge() : Boolean;
begin
  Result := ( FAmazonMinimumAge <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasApertureModes() : Boolean;
begin
  Result := ( FApertureModes <> '' );
end;

function MerchantItemAttributes_Type.HasArtist() : Boolean;
begin
  Result := ( FArtist <> MerchantItemAttributes_ArtistArray(0) );
end;

function MerchantItemAttributes_Type.HasAspectRatio() : Boolean;
begin
  Result := ( FAspectRatio <> '' );
end;

function MerchantItemAttributes_Type.HasAssemblyInstructions() : Boolean;
begin
  Result := ( FAssemblyInstructions <> '' );
end;

function MerchantItemAttributes_Type.HasAssemblyRequired() : Boolean;
begin
  Result := ( FAssemblyRequired <> '' );
end;

function MerchantItemAttributes_Type.HasAudienceRating() : Boolean;
begin
  Result := ( FAudienceRating <> '' );
end;

function MerchantItemAttributes_Type.HasAudioFormat() : Boolean;
begin
  Result := ( FAudioFormat <> MerchantItemAttributes_AudioFormatArray(0) );
end;

function MerchantItemAttributes_Type.HasAuthor() : Boolean;
begin
  Result := ( FAuthor <> MerchantItemAttributes_AuthorArray(0) );
end;

function MerchantItemAttributes_Type.HasBackFinding() : Boolean;
begin
  Result := ( FBackFinding <> '' );
end;

function MerchantItemAttributes_Type.HasBandMaterialType() : Boolean;
begin
  Result := ( FBandMaterialType <> '' );
end;

function MerchantItemAttributes_Type.HasBatteriesIncluded() : Boolean;
begin
  Result := ( FBatteriesIncluded <> '' );
end;

function MerchantItemAttributes_Type.HasBatteriesRequired() : Boolean;
begin
  Result := ( FBatteriesRequired <> '' );
end;

function MerchantItemAttributes_Type.HasBatteries() : Boolean;
begin
  Result := ( FBatteries <> NonNegativeIntegerWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasBatteryDescription() : Boolean;
begin
  Result := ( FBatteryDescription <> '' );
end;

function MerchantItemAttributes_Type.HasBatteryType() : Boolean;
begin
  Result := ( FBatteryType <> '' );
end;

function MerchantItemAttributes_Type.HasBezelMaterialType() : Boolean;
begin
  Result := ( FBezelMaterialType <> '' );
end;

function MerchantItemAttributes_Type.HasBinding() : Boolean;
begin
  Result := ( FBinding <> '' );
end;

function MerchantItemAttributes_Type.HasBrand() : Boolean;
begin
  Result := ( FBrand <> '' );
end;

function MerchantItemAttributes_Type.HasCalendarType() : Boolean;
begin
  Result := ( FCalendarType <> '' );
end;

function MerchantItemAttributes_Type.HasCameraManualFeatures() : Boolean;
begin
  Result := ( FCameraManualFeatures <> MerchantItemAttributes_CameraManualFeaturesArray(0) );
end;

function MerchantItemAttributes_Type.HasCaseDiameter() : Boolean;
begin
  Result := ( FCaseDiameter <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasCaseMaterialType() : Boolean;
begin
  Result := ( FCaseMaterialType <> '' );
end;

function MerchantItemAttributes_Type.HasCaseThickness() : Boolean;
begin
  Result := ( FCaseThickness <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasCaseType() : Boolean;
begin
  Result := ( FCaseType <> '' );
end;

function MerchantItemAttributes_Type.HasCatalogNumber() : Boolean;
begin
  Result := ( FCatalogNumber <> '' );
end;

function MerchantItemAttributes_Type.HasCDRWDescription() : Boolean;
begin
  Result := ( FCDRWDescription <> '' );
end;

function MerchantItemAttributes_Type.HasChainType() : Boolean;
begin
  Result := ( FChainType <> '' );
end;

function MerchantItemAttributes_Type.HasClaspType() : Boolean;
begin
  Result := ( FClaspType <> '' );
end;

function MerchantItemAttributes_Type.HasClothingSize() : Boolean;
begin
  Result := ( FClothingSize <> '' );
end;

function MerchantItemAttributes_Type.HasColor() : Boolean;
begin
  Result := ( FColor <> '' );
end;

function MerchantItemAttributes_Type.HasCompatibility() : Boolean;
begin
  Result := ( FCompatibility <> '' );
end;

function MerchantItemAttributes_Type.HasComputerHardwareType() : Boolean;
begin
  Result := ( FComputerHardwareType <> '' );
end;

function MerchantItemAttributes_Type.HasComputerPlatform() : Boolean;
begin
  Result := ( FComputerPlatform <> '' );
end;

function MerchantItemAttributes_Type.HasConnectivity() : Boolean;
begin
  Result := ( FConnectivity <> '' );
end;

function MerchantItemAttributes_Type.HasContinuousShootingSpeed() : Boolean;
begin
  Result := ( FContinuousShootingSpeed <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasCountry() : Boolean;
begin
  Result := ( FCountry <> '' );
end;

function MerchantItemAttributes_Type.HasCountryOfOrigin() : Boolean;
begin
  Result := ( FCountryOfOrigin <> '' );
end;

function MerchantItemAttributes_Type.HasCPUManufacturer() : Boolean;
begin
  Result := ( FCPUManufacturer <> '' );
end;

function MerchantItemAttributes_Type.HasCPUSpeed() : Boolean;
begin
  Result := ( FCPUSpeed <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasCPUType() : Boolean;
begin
  Result := ( FCPUType <> '' );
end;

function MerchantItemAttributes_Type.HasCreator() : Boolean;
begin
  Result := ( FCreator <> MerchantItemAttributes_CreatorArray(0) );
end;

function MerchantItemAttributes_Type.HasCuisine() : Boolean;
begin
  Result := ( FCuisine <> '' );
end;

function MerchantItemAttributes_Type.HasCustomizable() : Boolean;
begin
  Result := ( FCustomizable <> '' );
end;

function MerchantItemAttributes_Type.HasDelayBetweenShots() : Boolean;
begin
  Result := ( FDelayBetweenShots <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasDeliveryOption() : Boolean;
begin
  Result := ( FDeliveryOption <> '' );
end;

function MerchantItemAttributes_Type.HasDepartment() : Boolean;
begin
  Result := ( FDepartment <> '' );
end;

function MerchantItemAttributes_Type.HasDescription() : Boolean;
begin
  Result := ( FDescription <> '' );
end;

function MerchantItemAttributes_Type.HasDeweyDecimalNumber() : Boolean;
begin
  Result := ( FDeweyDecimalNumber <> '' );
end;

function MerchantItemAttributes_Type.HasDialColor() : Boolean;
begin
  Result := ( FDialColor <> '' );
end;

function MerchantItemAttributes_Type.HasDialWindowMaterialType() : Boolean;
begin
  Result := ( FDialWindowMaterialType <> '' );
end;

function MerchantItemAttributes_Type.HasDigitalZoom() : Boolean;
begin
  Result := ( FDigitalZoom <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasDirector() : Boolean;
begin
  Result := ( FDirector <> MerchantItemAttributes_DirectorArray(0) );
end;

function MerchantItemAttributes_Type.HasDisplaySize() : Boolean;
begin
  Result := ( FDisplaySize <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasDrumSetPieceQuantity() : Boolean;
begin
  Result := ( FDrumSetPieceQuantity <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasDVDLayers() : Boolean;
begin
  Result := ( FDVDLayers <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasDVDRWDescription() : Boolean;
begin
  Result := ( FDVDRWDescription <> '' );
end;

function MerchantItemAttributes_Type.HasDVDSides() : Boolean;
begin
  Result := ( FDVDSides <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasDPCI() : Boolean;
begin
  Result := ( FDPCI <> '' );
end;

function MerchantItemAttributes_Type.HasEAN() : Boolean;
begin
  Result := ( FEAN <> '' );
end;

function MerchantItemAttributes_Type.HasEdition() : Boolean;
begin
  Result := ( FEdition <> '' );
end;

function MerchantItemAttributes_Type.HasESRBAgeRating() : Boolean;
begin
  Result := ( FESRBAgeRating <> '' );
end;

function MerchantItemAttributes_Type.HasExternalDisplaySupportDescription() : Boolean;
begin
  Result := ( FExternalDisplaySupportDescription <> '' );
end;

function MerchantItemAttributes_Type.HasFabricType() : Boolean;
begin
  Result := ( FFabricType <> '' );
end;

function MerchantItemAttributes_Type.HasFaxNumber() : Boolean;
begin
  Result := ( FFaxNumber <> '' );
end;

function MerchantItemAttributes_Type.HasFeature() : Boolean;
begin
  Result := ( FFeature <> MerchantItemAttributes_FeatureArray(0) );
end;

function MerchantItemAttributes_Type.HasFirstIssueLeadTime() : Boolean;
begin
  Result := ( FFirstIssueLeadTime <> StringWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasFloppyDiskDriveDescription() : Boolean;
begin
  Result := ( FFloppyDiskDriveDescription <> '' );
end;

function MerchantItemAttributes_Type.HasFormat() : Boolean;
begin
  Result := ( FFormat <> MerchantItemAttributes_FormatArray(0) );
end;

function MerchantItemAttributes_Type.HasFixedShippingCharge() : Boolean;
begin
  Result := ( FFixedShippingCharge <> Price_Type(0) );
end;

function MerchantItemAttributes_Type.HasGemType() : Boolean;
begin
  Result := ( FGemType <> '' );
end;

function MerchantItemAttributes_Type.HasGraphicsCardInterface() : Boolean;
begin
  Result := ( FGraphicsCardInterface <> '' );
end;

function MerchantItemAttributes_Type.HasGraphicsDescription() : Boolean;
begin
  Result := ( FGraphicsDescription <> '' );
end;

function MerchantItemAttributes_Type.HasGraphicsMemorySize() : Boolean;
begin
  Result := ( FGraphicsMemorySize <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasGuitarAttribute() : Boolean;
begin
  Result := ( FGuitarAttribute <> '' );
end;

function MerchantItemAttributes_Type.HasGuitarBridgeSystem() : Boolean;
begin
  Result := ( FGuitarBridgeSystem <> '' );
end;

function MerchantItemAttributes_Type.HasGuitarPickThickness() : Boolean;
begin
  Result := ( FGuitarPickThickness <> '' );
end;

function MerchantItemAttributes_Type.HasGuitarPickupConfiguration() : Boolean;
begin
  Result := ( FGuitarPickupConfiguration <> '' );
end;

function MerchantItemAttributes_Type.HasHardDiskCount() : Boolean;
begin
  Result := ( FHardDiskCount <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasHardDiskSize() : Boolean;
begin
  Result := ( FHardDiskSize <> NonNegativeIntegerWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasHasAutoFocus() : Boolean;
begin
  Result := ( FHasAutoFocus <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasHasBurstMode() : Boolean;
begin
  Result := ( FHasBurstMode <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasHasInCameraEditing() : Boolean;
begin
  Result := ( FHasInCameraEditing <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasHasRedEyeReduction() : Boolean;
begin
  Result := ( FHasRedEyeReduction <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasHasSelfTimer() : Boolean;
begin
  Result := ( FHasSelfTimer <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasHasTripodMount() : Boolean;
begin
  Result := ( FHasTripodMount <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasHasVideoOut() : Boolean;
begin
  Result := ( FHasVideoOut <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasHasViewfinder() : Boolean;
begin
  Result := ( FHasViewfinder <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasHazardousMaterialType() : Boolean;
begin
  Result := ( FHazardousMaterialType <> '' );
end;

function MerchantItemAttributes_Type.HasHoursOfOperation() : Boolean;
begin
  Result := ( FHoursOfOperation <> '' );
end;

function MerchantItemAttributes_Type.HasIncludedSoftware() : Boolean;
begin
  Result := ( FIncludedSoftware <> '' );
end;

function MerchantItemAttributes_Type.HasIncludesMp3Player() : Boolean;
begin
  Result := ( FIncludesMp3Player <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasIndications() : Boolean;
begin
  Result := ( FIndications <> '' );
end;

function MerchantItemAttributes_Type.HasIngredients() : Boolean;
begin
  Result := ( FIngredients <> '' );
end;

function MerchantItemAttributes_Type.HasInstrumentKey() : Boolean;
begin
  Result := ( FInstrumentKey <> '' );
end;

function MerchantItemAttributes_Type.HasIsAutographed() : Boolean;
begin
  Result := ( FIsAutographed <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasISBN() : Boolean;
begin
  Result := ( FISBN <> '' );
end;

function MerchantItemAttributes_Type.HasIsFragile() : Boolean;
begin
  Result := ( FIsFragile <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasIsLabCreated() : Boolean;
begin
  Result := ( FIsLabCreated <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasIsMemorabilia() : Boolean;
begin
  Result := ( FIsMemorabilia <> boolean(0) );
end;

function MerchantItemAttributes_Type.HasISOEquivalent() : Boolean;
begin
  Result := ( FISOEquivalent <> NonNegativeIntegerWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasIssuesPerYear() : Boolean;
begin
  Result := ( FIssuesPerYear <> '' );
end;

function MerchantItemAttributes_Type.HasItemDimensions() : Boolean;
begin
  Result := ( FItemDimensions <> MerchantItemAttributes_ItemDimensions_Type(0) );
end;

function MerchantItemAttributes_Type.HasKeyboardDescription() : Boolean;
begin
  Result := ( FKeyboardDescription <> '' );
end;

function MerchantItemAttributes_Type.Has_Label() : Boolean;
begin
  Result := ( F_Label <> '' );
end;

function MerchantItemAttributes_Type.HasLanguages() : Boolean;
begin
  Result := ( FLanguages <> MerchantItemAttributes_Languages_Type(0) );
end;

function MerchantItemAttributes_Type.HasLegalDisclaimer() : Boolean;
begin
  Result := ( FLegalDisclaimer <> '' );
end;

function MerchantItemAttributes_Type.HasLineVoltage() : Boolean;
begin
  Result := ( FLineVoltage <> '' );
end;

function MerchantItemAttributes_Type.HasListPrice() : Boolean;
begin
  Result := ( FListPrice <> Price_Type(0) );
end;

function MerchantItemAttributes_Type.HasMacroFocusRange() : Boolean;
begin
  Result := ( FMacroFocusRange <> '' );
end;

function MerchantItemAttributes_Type.HasMagazineType() : Boolean;
begin
  Result := ( FMagazineType <> '' );
end;

function MerchantItemAttributes_Type.HasMalletHardness() : Boolean;
begin
  Result := ( FMalletHardness <> '' );
end;

function MerchantItemAttributes_Type.HasManufacturer() : Boolean;
begin
  Result := ( FManufacturer <> '' );
end;

function MerchantItemAttributes_Type.HasManufacturerLaborWarrantyDescription() : Boolean;
begin
  Result := ( FManufacturerLaborWarrantyDescription <> '' );
end;

function MerchantItemAttributes_Type.HasManufacturerMaximumAge() : Boolean;
begin
  Result := ( FManufacturerMaximumAge <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasManufacturerMinimumAge() : Boolean;
begin
  Result := ( FManufacturerMinimumAge <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasManufacturerPartsWarrantyDescription() : Boolean;
begin
  Result := ( FManufacturerPartsWarrantyDescription <> '' );
end;

function MerchantItemAttributes_Type.HasMaterialType() : Boolean;
begin
  Result := ( FMaterialType <> '' );
end;

function MerchantItemAttributes_Type.HasMaximumAperture() : Boolean;
begin
  Result := ( FMaximumAperture <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasMaximumColorDepth() : Boolean;
begin
  Result := ( FMaximumColorDepth <> '' );
end;

function MerchantItemAttributes_Type.HasMaximumFocalLength() : Boolean;
begin
  Result := ( FMaximumFocalLength <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasMaximumHighResolutionImages() : Boolean;
begin
  Result := ( FMaximumHighResolutionImages <> NonNegativeIntegerWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasMaximumHorizontalResolution() : Boolean;
begin
  Result := ( FMaximumHorizontalResolution <> NonNegativeIntegerWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasMaximumLowResolutionImages() : Boolean;
begin
  Result := ( FMaximumLowResolutionImages <> '' );
end;

function MerchantItemAttributes_Type.HasMaximumResolution() : Boolean;
begin
  Result := ( FMaximumResolution <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasMaximumShutterSpeed() : Boolean;
begin
  Result := ( FMaximumShutterSpeed <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasMaximumVerticalResolution() : Boolean;
begin
  Result := ( FMaximumVerticalResolution <> NonNegativeIntegerWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasMaximumWeightRecommendation() : Boolean;
begin
  Result := ( FMaximumWeightRecommendation <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasMemorySlotsAvailable() : Boolean;
begin
  Result := ( FMemorySlotsAvailable <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasMetalStamp() : Boolean;
begin
  Result := ( FMetalStamp <> '' );
end;

function MerchantItemAttributes_Type.HasMetalType() : Boolean;
begin
  Result := ( FMetalType <> '' );
end;

function MerchantItemAttributes_Type.HasMiniMovieDescription() : Boolean;
begin
  Result := ( FMiniMovieDescription <> '' );
end;

function MerchantItemAttributes_Type.HasMinimumFocalLength() : Boolean;
begin
  Result := ( FMinimumFocalLength <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasMinimumShutterSpeed() : Boolean;
begin
  Result := ( FMinimumShutterSpeed <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasModel() : Boolean;
begin
  Result := ( FModel <> '' );
end;

function MerchantItemAttributes_Type.HasModelYear() : Boolean;
begin
  Result := ( FModelYear <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasModemDescription() : Boolean;
begin
  Result := ( FModemDescription <> '' );
end;

function MerchantItemAttributes_Type.HasMonitorSize() : Boolean;
begin
  Result := ( FMonitorSize <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasMonitorViewableDiagonalSize() : Boolean;
begin
  Result := ( FMonitorViewableDiagonalSize <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasMouseDescription() : Boolean;
begin
  Result := ( FMouseDescription <> '' );
end;

function MerchantItemAttributes_Type.HasMPN() : Boolean;
begin
  Result := ( FMPN <> '' );
end;

function MerchantItemAttributes_Type.HasMusicalStyle() : Boolean;
begin
  Result := ( FMusicalStyle <> '' );
end;

function MerchantItemAttributes_Type.HasNativeResolution() : Boolean;
begin
  Result := ( FNativeResolution <> '' );
end;

function MerchantItemAttributes_Type.HasNeighborhood() : Boolean;
begin
  Result := ( FNeighborhood <> '' );
end;

function MerchantItemAttributes_Type.HasNetworkInterfaceDescription() : Boolean;
begin
  Result := ( FNetworkInterfaceDescription <> '' );
end;

function MerchantItemAttributes_Type.HasNotebookDisplayTechnology() : Boolean;
begin
  Result := ( FNotebookDisplayTechnology <> '' );
end;

function MerchantItemAttributes_Type.HasNotebookPointingDeviceDescription() : Boolean;
begin
  Result := ( FNotebookPointingDeviceDescription <> '' );
end;

function MerchantItemAttributes_Type.HasNumberOfDiscs() : Boolean;
begin
  Result := ( FNumberOfDiscs <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasNumberOfIssues() : Boolean;
begin
  Result := ( FNumberOfIssues <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasNumberOfItems() : Boolean;
begin
  Result := ( FNumberOfItems <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasNumberOfKeys() : Boolean;
begin
  Result := ( FNumberOfKeys <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasNumberOfPages() : Boolean;
begin
  Result := ( FNumberOfPages <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasNumberOfPearls() : Boolean;
begin
  Result := ( FNumberOfPearls <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasNumberOfRapidFireShots() : Boolean;
begin
  Result := ( FNumberOfRapidFireShots <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasNumberOfStones() : Boolean;
begin
  Result := ( FNumberOfStones <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasNumberOfStrings() : Boolean;
begin
  Result := ( FNumberOfStrings <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasNumberOfTracks() : Boolean;
begin
  Result := ( FNumberOfTracks <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasOpticalZoom() : Boolean;
begin
  Result := ( FOpticalZoom <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasOriginalReleaseDate() : Boolean;
begin
  Result := ( FOriginalReleaseDate <> '' );
end;

function MerchantItemAttributes_Type.HasOutputWattage() : Boolean;
begin
  Result := ( FOutputWattage <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasPackageDimensions() : Boolean;
begin
  Result := ( FPackageDimensions <> MerchantItemAttributes_PackageDimensions_Type(0) );
end;

function MerchantItemAttributes_Type.HasPearlLustre() : Boolean;
begin
  Result := ( FPearlLustre <> '' );
end;

function MerchantItemAttributes_Type.HasPearlMinimumColor() : Boolean;
begin
  Result := ( FPearlMinimumColor <> '' );
end;

function MerchantItemAttributes_Type.HasPearlShape() : Boolean;
begin
  Result := ( FPearlShape <> '' );
end;

function MerchantItemAttributes_Type.HasPearlStringingMethod() : Boolean;
begin
  Result := ( FPearlStringingMethod <> '' );
end;

function MerchantItemAttributes_Type.HasPearlSurfaceBlemishes() : Boolean;
begin
  Result := ( FPearlSurfaceBlemishes <> '' );
end;

function MerchantItemAttributes_Type.HasPearlType() : Boolean;
begin
  Result := ( FPearlType <> '' );
end;

function MerchantItemAttributes_Type.HasPearlUniformity() : Boolean;
begin
  Result := ( FPearlUniformity <> '' );
end;

function MerchantItemAttributes_Type.HasPhoneNumber() : Boolean;
begin
  Result := ( FPhoneNumber <> '' );
end;

function MerchantItemAttributes_Type.HasPhotoFlashType() : Boolean;
begin
  Result := ( FPhotoFlashType <> MerchantItemAttributes_PhotoFlashTypeArray(0) );
end;

function MerchantItemAttributes_Type.HasPictureFormat() : Boolean;
begin
  Result := ( FPictureFormat <> MerchantItemAttributes_PictureFormatArray(0) );
end;

function MerchantItemAttributes_Type.HasPlatform() : Boolean;
begin
  Result := ( FPlatform <> MerchantItemAttributes_PlatformArray(0) );
end;

function MerchantItemAttributes_Type.HasPriceRating() : Boolean;
begin
  Result := ( FPriceRating <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasProcessorCount() : Boolean;
begin
  Result := ( FProcessorCount <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasProductGroup() : Boolean;
begin
  Result := ( FProductGroup <> '' );
end;

function MerchantItemAttributes_Type.HasPromotionalTag() : Boolean;
begin
  Result := ( FPromotionalTag <> '' );
end;

function MerchantItemAttributes_Type.HasPOBoxShippingExcluded() : Boolean;
begin
  Result := ( FPOBoxShippingExcluded <> '' );
end;

function MerchantItemAttributes_Type.HasPublicationDate() : Boolean;
begin
  Result := ( FPublicationDate <> '' );
end;

function MerchantItemAttributes_Type.HasPublisher() : Boolean;
begin
  Result := ( FPublisher <> '' );
end;

function MerchantItemAttributes_Type.HasPurchasingChannel() : Boolean;
begin
  Result := ( FPurchasingChannel <> MerchantItemAttributes_PurchasingChannelArray(0) );
end;

function MerchantItemAttributes_Type.HasReadingLevel() : Boolean;
begin
  Result := ( FReadingLevel <> '' );
end;

function MerchantItemAttributes_Type.HasRecorderTrackCount() : Boolean;
begin
  Result := ( FRecorderTrackCount <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasRegionCode() : Boolean;
begin
  Result := ( FRegionCode <> '' );
end;

function MerchantItemAttributes_Type.HasRegionOfOrigin() : Boolean;
begin
  Result := ( FRegionOfOrigin <> '' );
end;

function MerchantItemAttributes_Type.HasReleaseDate() : Boolean;
begin
  Result := ( FReleaseDate <> '' );
end;

function MerchantItemAttributes_Type.HasReturnMethod() : Boolean;
begin
  Result := ( FReturnMethod <> MerchantItemAttributes_ReturnMethodArray(0) );
end;

function MerchantItemAttributes_Type.HasRemovableMemory() : Boolean;
begin
  Result := ( FRemovableMemory <> '' );
end;

function MerchantItemAttributes_Type.HasResolutionModes() : Boolean;
begin
  Result := ( FResolutionModes <> '' );
end;

function MerchantItemAttributes_Type.HasReturnPolicy() : Boolean;
begin
  Result := ( FReturnPolicy <> '' );
end;

function MerchantItemAttributes_Type.HasRingSize() : Boolean;
begin
  Result := ( FRingSize <> '' );
end;

function MerchantItemAttributes_Type.HasSafetyWarning() : Boolean;
begin
  Result := ( FSafetyWarning <> '' );
end;

function MerchantItemAttributes_Type.HasSalesRestriction() : Boolean;
begin
  Result := ( FSalesRestriction <> '' );
end;

function MerchantItemAttributes_Type.HasSecondaryCacheSize() : Boolean;
begin
  Result := ( FSecondaryCacheSize <> NonNegativeIntegerWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasSettingType() : Boolean;
begin
  Result := ( FSettingType <> '' );
end;

function MerchantItemAttributes_Type.HasSize() : Boolean;
begin
  Result := ( FSize <> '' );
end;

function MerchantItemAttributes_Type.HasSKU() : Boolean;
begin
  Result := ( FSKU <> '' );
end;

function MerchantItemAttributes_Type.HasSoldInStores() : Boolean;
begin
  Result := ( FSoldInStores <> '' );
end;

function MerchantItemAttributes_Type.HasSizePerPearl() : Boolean;
begin
  Result := ( FSizePerPearl <> '' );
end;

function MerchantItemAttributes_Type.HasSkillLevel() : Boolean;
begin
  Result := ( FSkillLevel <> '' );
end;

function MerchantItemAttributes_Type.HasSoundCardDescription() : Boolean;
begin
  Result := ( FSoundCardDescription <> '' );
end;

function MerchantItemAttributes_Type.HasSpeakerCount() : Boolean;
begin
  Result := ( FSpeakerCount <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasSpeakerDescription() : Boolean;
begin
  Result := ( FSpeakerDescription <> '' );
end;

function MerchantItemAttributes_Type.HasSpecialFeatures() : Boolean;
begin
  Result := ( FSpecialFeatures <> MerchantItemAttributes_SpecialFeaturesArray(0) );
end;

function MerchantItemAttributes_Type.HasStoneClarity() : Boolean;
begin
  Result := ( FStoneClarity <> '' );
end;

function MerchantItemAttributes_Type.HasStoneColor() : Boolean;
begin
  Result := ( FStoneColor <> '' );
end;

function MerchantItemAttributes_Type.HasStoneCut() : Boolean;
begin
  Result := ( FStoneCut <> '' );
end;

function MerchantItemAttributes_Type.HasStoneShape() : Boolean;
begin
  Result := ( FStoneShape <> '' );
end;

function MerchantItemAttributes_Type.HasStoneWeight() : Boolean;
begin
  Result := ( FStoneWeight <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasStudio() : Boolean;
begin
  Result := ( FStudio <> '' );
end;

function MerchantItemAttributes_Type.HasSubscriptionLength() : Boolean;
begin
  Result := ( FSubscriptionLength <> NonNegativeIntegerWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasSupportedImageType() : Boolean;
begin
  Result := ( FSupportedImageType <> MerchantItemAttributes_SupportedImageTypeArray(0) );
end;

function MerchantItemAttributes_Type.HasSystemBusSpeed() : Boolean;
begin
  Result := ( FSystemBusSpeed <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasSystemMemorySizeMax() : Boolean;
begin
  Result := ( FSystemMemorySizeMax <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasSystemMemorySize() : Boolean;
begin
  Result := ( FSystemMemorySize <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasSystemMemoryType() : Boolean;
begin
  Result := ( FSystemMemoryType <> '' );
end;

function MerchantItemAttributes_Type.HasTellingPageIndicator() : Boolean;
begin
  Result := ( FTellingPageIndicator <> '' );
end;

function MerchantItemAttributes_Type.HasTheatricalReleaseDate() : Boolean;
begin
  Result := ( FTheatricalReleaseDate <> '' );
end;

function MerchantItemAttributes_Type.HasTitle() : Boolean;
begin
  Result := ( FTitle <> '' );
end;

function MerchantItemAttributes_Type.HasTotalDiamondWeight() : Boolean;
begin
  Result := ( FTotalDiamondWeight <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasTotalExternalBaysFree() : Boolean;
begin
  Result := ( FTotalExternalBaysFree <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasTotalFirewirePorts() : Boolean;
begin
  Result := ( FTotalFirewirePorts <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasTotalGemWeight() : Boolean;
begin
  Result := ( FTotalGemWeight <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasTotalInternalBaysFree() : Boolean;
begin
  Result := ( FTotalInternalBaysFree <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasTotalMetalWeight() : Boolean;
begin
  Result := ( FTotalMetalWeight <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasTotalNTSCPALPorts() : Boolean;
begin
  Result := ( FTotalNTSCPALPorts <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasTotalParallelPorts() : Boolean;
begin
  Result := ( FTotalParallelPorts <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasTotalPCCardSlots() : Boolean;
begin
  Result := ( FTotalPCCardSlots <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasTotalPCISlotsFree() : Boolean;
begin
  Result := ( FTotalPCISlotsFree <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasTotalSerialPorts() : Boolean;
begin
  Result := ( FTotalSerialPorts <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasTotalSVideoOutPorts() : Boolean;
begin
  Result := ( FTotalSVideoOutPorts <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasTotalUSB2Ports() : Boolean;
begin
  Result := ( FTotalUSB2Ports <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasTotalUSBPorts() : Boolean;
begin
  Result := ( FTotalUSBPorts <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasTotalVGAOutPorts() : Boolean;
begin
  Result := ( FTotalVGAOutPorts <> nonNegativeInteger(0) );
end;

function MerchantItemAttributes_Type.HasUPC() : Boolean;
begin
  Result := ( FUPC <> '' );
end;

function MerchantItemAttributes_Type.HasVariationDenomination() : Boolean;
begin
  Result := ( FVariationDenomination <> '' );
end;

function MerchantItemAttributes_Type.HasVariationDescription() : Boolean;
begin
  Result := ( FVariationDescription <> '' );
end;

function MerchantItemAttributes_Type.HasVendorRebate() : Boolean;
begin
  Result := ( FVendorRebate <> MerchantItemAttributes_VendorRebate_Type(0) );
end;

function MerchantItemAttributes_Type.HasWarranty() : Boolean;
begin
  Result := ( FWarranty <> '' );
end;

function MerchantItemAttributes_Type.HasWatchMovementType() : Boolean;
begin
  Result := ( FWatchMovementType <> '' );
end;

function MerchantItemAttributes_Type.HasWebsiteBuyability() : Boolean;
begin
  Result := ( FWebsiteBuyability <> '' );
end;

function MerchantItemAttributes_Type.HasWaterResistanceDepth() : Boolean;
begin
  Result := ( FWaterResistanceDepth <> DecimalWithUnits_Type(0) );
end;

function MerchantItemAttributes_Type.HasWirelessMicrophoneFrequency() : Boolean;
begin
  Result := ( FWirelessMicrophoneFrequency <> nonNegativeInteger(0) );
end;

{ Help_RequestArray }

function Help_RequestArray.GetItem(AIndex: Integer): HelpRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As HelpRequest_Type;
end;

class function Help_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= HelpRequest_Type;
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

function ItemSearch_RequestArray.GetItem(AIndex: Integer): ItemSearchRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As ItemSearchRequest_Type;
end;

class function ItemSearch_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ItemSearchRequest_Type;
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

function ItemLookup_RequestArray.GetItem(AIndex: Integer): ItemLookupRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As ItemLookupRequest_Type;
end;

class function ItemLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ItemLookupRequest_Type;
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

function BrowseNodeLookup_RequestArray.GetItem(AIndex: Integer): BrowseNodeLookupRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As BrowseNodeLookupRequest_Type;
end;

class function BrowseNodeLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= BrowseNodeLookupRequest_Type;
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

function ListSearch_RequestArray.GetItem(AIndex: Integer): ListSearchRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As ListSearchRequest_Type;
end;

class function ListSearch_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ListSearchRequest_Type;
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

function ListLookup_RequestArray.GetItem(AIndex: Integer): ListLookupRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As ListLookupRequest_Type;
end;

class function ListLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ListLookupRequest_Type;
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

function CustomerContentSearch_RequestArray.GetItem(AIndex: Integer): CustomerContentSearchRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As CustomerContentSearchRequest_Type;
end;

class function CustomerContentSearch_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CustomerContentSearchRequest_Type;
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

function CustomerContentLookup_RequestArray.GetItem(AIndex: Integer): CustomerContentLookupRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As CustomerContentLookupRequest_Type;
end;

class function CustomerContentLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CustomerContentLookupRequest_Type;
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

function SimilarityLookup_RequestArray.GetItem(AIndex: Integer): SimilarityLookupRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As SimilarityLookupRequest_Type;
end;

class function SimilarityLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SimilarityLookupRequest_Type;
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

function SellerLookup_RequestArray.GetItem(AIndex: Integer): SellerLookupRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As SellerLookupRequest_Type;
end;

class function SellerLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SellerLookupRequest_Type;
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

function CartGet_RequestArray.GetItem(AIndex: Integer): CartGetRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As CartGetRequest_Type;
end;

class function CartGet_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartGetRequest_Type;
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

function CartAdd_RequestArray.GetItem(AIndex: Integer): CartAddRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As CartAddRequest_Type;
end;

class function CartAdd_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartAddRequest_Type;
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

function CartCreate_RequestArray.GetItem(AIndex: Integer): CartCreateRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As CartCreateRequest_Type;
end;

class function CartCreate_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartCreateRequest_Type;
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

function CartModify_RequestArray.GetItem(AIndex: Integer): CartModifyRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As CartModifyRequest_Type;
end;

class function CartModify_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartModifyRequest_Type;
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

function CartClear_RequestArray.GetItem(AIndex: Integer): CartClearRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As CartClearRequest_Type;
end;

class function CartClear_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartClearRequest_Type;
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

function TransactionLookup_RequestArray.GetItem(AIndex: Integer): TransactionLookupRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As TransactionLookupRequest_Type;
end;

class function TransactionLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TransactionLookupRequest_Type;
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

function SellerListingSearch_RequestArray.GetItem(AIndex: Integer): SellerListingSearchRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As SellerListingSearchRequest_Type;
end;

class function SellerListingSearch_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SellerListingSearchRequest_Type;
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

function SellerListingLookup_RequestArray.GetItem(AIndex: Integer): SellerListingLookupRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As SellerListingLookupRequest_Type;
end;

class function SellerListingLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SellerListingLookupRequest_Type;
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

{ TagLookup_RequestArray }

function TagLookup_RequestArray.GetItem(AIndex: Integer): TagLookupRequest_Type;
begin
  Result := Inherited GetItem(AIndex) As TagLookupRequest_Type;
end;

class function TagLookup_RequestArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TagLookupRequest_Type;
end;

{ TagLookupResponse_TagsArray }

function TagLookupResponse_TagsArray.GetItem(AIndex: Integer): Tags_Type;
begin
  Result := Inherited GetItem(AIndex) As Tags_Type;
end;

class function TagLookupResponse_TagsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Tags_Type;
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

{ CartAddRequest_Items_Type_Item_Type_MetaDataArray }

function CartAddRequest_Items_Type_Item_Type_MetaDataArray.GetItem(AIndex: Integer): CartAddRequest_Items_Type_Item_Type_MetaData_Type;
begin
  Result := Inherited GetItem(AIndex) As CartAddRequest_Items_Type_Item_Type_MetaData_Type;
end;

class function CartAddRequest_Items_Type_Item_Type_MetaDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartAddRequest_Items_Type_Item_Type_MetaData_Type;
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

{ CartCreateRequest_Items_Type_Item_Type_MetaDataArray }

function CartCreateRequest_Items_Type_Item_Type_MetaDataArray.GetItem(AIndex: Integer): CartCreateRequest_Items_Type_Item_Type_MetaData_Type;
begin
  Result := Inherited GetItem(AIndex) As CartCreateRequest_Items_Type_Item_Type_MetaData_Type;
end;

class function CartCreateRequest_Items_Type_Item_Type_MetaDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartCreateRequest_Items_Type_Item_Type_MetaData_Type;
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

{ TagLookupRequest_TagNameArray }

function TagLookupRequest_TagNameArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TagLookupRequest_TagNameArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TagLookupRequest_TagNameArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TagLookupRequest_TagNameArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('TagName',TypeInfo(string),FData[AIndex]);
end;

procedure TagLookupRequest_TagNameArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'TagName';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function TagLookupRequest_TagNameArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure TagLookupRequest_TagNameArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ TagLookupRequest_ResponseGroupArray }

function TagLookupRequest_ResponseGroupArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TagLookupRequest_ResponseGroupArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TagLookupRequest_ResponseGroupArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TagLookupRequest_ResponseGroupArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ResponseGroup',TypeInfo(string),FData[AIndex]);
end;

procedure TagLookupRequest_ResponseGroupArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ResponseGroup';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function TagLookupRequest_ResponseGroupArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure TagLookupRequest_ResponseGroupArray.SetLength(const ANewSize: Integer);
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

{ Arguments_Type }

function Arguments_Type.GetItem(AIndex: Integer): Arguments_Argument_Type;
begin
  Result := Inherited GetItem(AIndex) As Arguments_Argument_Type;
end;

class function Arguments_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Arguments_Argument_Type;
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

{ SearchResultsMap_Type }

function SearchResultsMap_Type.GetItem(AIndex: Integer): SearchResultsMap_SearchIndex_Type;
begin
  Result := Inherited GetItem(AIndex) As SearchResultsMap_SearchIndex_Type;
end;

class function SearchResultsMap_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= SearchResultsMap_SearchIndex_Type;
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

{ Tags_TagArray }

function Tags_TagArray.GetItem(AIndex: Integer): Tag_Type;
begin
  Result := Inherited GetItem(AIndex) As Tag_Type;
end;

class function Tags_TagArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Tag_Type;
end;

{ Tag_TaggedItemsArray }

function Tag_TaggedItemsArray.GetItem(AIndex: Integer): TaggedItems_Type;
begin
  Result := Inherited GetItem(AIndex) As TaggedItems_Type;
end;

class function Tag_TaggedItemsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TaggedItems_Type;
end;

{ Tag_TaggedListmaniaListsArray }

function Tag_TaggedListmaniaListsArray.GetItem(AIndex: Integer): TaggedListmaniaLists_Type;
begin
  Result := Inherited GetItem(AIndex) As TaggedListmaniaLists_Type;
end;

class function Tag_TaggedListmaniaListsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TaggedListmaniaLists_Type;
end;

{ Tag_TaggedGuidesArray }

function Tag_TaggedGuidesArray.GetItem(AIndex: Integer): TaggedGuides_Type;
begin
  Result := Inherited GetItem(AIndex) As TaggedGuides_Type;
end;

class function Tag_TaggedGuidesArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TaggedGuides_Type;
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

{ Variations__ItemArray }

function Variations__ItemArray.GetItem(AIndex: Integer): Item_Type;
begin
  Result := Inherited GetItem(AIndex) As Item_Type;
end;

class function Variations__ItemArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Item_Type;
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

{ EditorialReviews_Type }

function EditorialReviews_Type.GetItem(AIndex: Integer): EditorialReview_Type;
begin
  Result := Inherited GetItem(AIndex) As EditorialReview_Type;
end;

class function EditorialReviews_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= EditorialReview_Type;
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

{ Collections_Type }

function Collections_Type.GetItem(AIndex: Integer): Collections_Collection_Type;
begin
  Result := Inherited GetItem(AIndex) As Collections_Collection_Type;
end;

class function Collections_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Collections_Collection_Type;
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

{ Tracks_Type }

function Tracks_Type.GetItem(AIndex: Integer): Tracks_Disc_Type;
begin
  Result := Inherited GetItem(AIndex) As Tracks_Disc_Type;
end;

class function Tracks_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Tracks_Disc_Type;
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

{ Accessories_Type }

function Accessories_Type.GetItem(AIndex: Integer): Accessories_Accessory_Type;
begin
  Result := Inherited GetItem(AIndex) As Accessories_Accessory_Type;
end;

class function Accessories_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Accessories_Accessory_Type;
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

{ PromotionEligibilityRequirements_Type }

function PromotionEligibilityRequirements_Type.GetItem(AIndex: Integer): PromotionEligibilityRequirement_Type;
begin
  Result := Inherited GetItem(AIndex) As PromotionEligibilityRequirement_Type;
end;

class function PromotionEligibilityRequirements_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= PromotionEligibilityRequirement_Type;
end;

{ PromotionBenefits_Type }

function PromotionBenefits_Type.GetItem(AIndex: Integer): PromotionBenefit_Type;
begin
  Result := Inherited GetItem(AIndex) As PromotionBenefit_Type;
end;

class function PromotionBenefits_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= PromotionBenefit_Type;
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

{ BrowseNode_Properties_Type }

function BrowseNode_Properties_Type.GetItem(AIndex: Integer): Property_Type;
begin
  Result := Inherited GetItem(AIndex) As Property_Type;
end;

class function BrowseNode_Properties_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Property_Type;
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

{ ListmaniaLists_Type }

function ListmaniaLists_Type.GetItem(AIndex: Integer): ListmaniaLists_ListmaniaList_Type;
begin
  Result := Inherited GetItem(AIndex) As ListmaniaLists_ListmaniaList_Type;
end;

class function ListmaniaLists_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ListmaniaLists_ListmaniaList_Type;
end;

{ CartItems_CartItemArray }

function CartItems_CartItemArray.GetItem(AIndex: Integer): CartItem_Type;
begin
  Result := Inherited GetItem(AIndex) As CartItem_Type;
end;

class function CartItems_CartItemArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartItem_Type;
end;

{ SavedForLaterItems_SavedForLaterItemArray }

function SavedForLaterItems_SavedForLaterItemArray.GetItem(AIndex: Integer): CartItem_Type;
begin
  Result := Inherited GetItem(AIndex) As CartItem_Type;
end;

class function SavedForLaterItems_SavedForLaterItemArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartItem_Type;
end;

{ CartItem_MetaData_Type }

function CartItem_MetaData_Type.GetItem(AIndex: Integer): CartItem_MetaData_Type_KeyValuePair_Type;
begin
  Result := Inherited GetItem(AIndex) As CartItem_MetaData_Type_KeyValuePair_Type;
end;

class function CartItem_MetaData_Type.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CartItem_MetaData_Type_KeyValuePair_Type;
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

{ ItemAttributes_AgeArray }

function ItemAttributes_AgeArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_AgeArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_AgeArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_AgeArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Age',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_AgeArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Age';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_AgeArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_AgeArray.SetLength(const ANewSize: Integer);
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

{ ItemAttributes_CategoryArray }

function ItemAttributes_CategoryArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_CategoryArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_CategoryArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_CategoryArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Category',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_CategoryArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Category';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_CategoryArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_CategoryArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_CategoryBinArray }

function ItemAttributes_CategoryBinArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_CategoryBinArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_CategoryBinArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_CategoryBinArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('CategoryBin',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_CategoryBinArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'CategoryBin';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_CategoryBinArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_CategoryBinArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_CharacterArray }

function ItemAttributes_CharacterArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_CharacterArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_CharacterArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_CharacterArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Character',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_CharacterArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Character';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_CharacterArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_CharacterArray.SetLength(const ANewSize: Integer);
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

{ ItemAttributes_EducationalFocusArray }

function ItemAttributes_EducationalFocusArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_EducationalFocusArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_EducationalFocusArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_EducationalFocusArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('EducationalFocus',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_EducationalFocusArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'EducationalFocus';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_EducationalFocusArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_EducationalFocusArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_EthnicityArray }

function ItemAttributes_EthnicityArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_EthnicityArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_EthnicityArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_EthnicityArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Ethnicity',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_EthnicityArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Ethnicity';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_EthnicityArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_EthnicityArray.SetLength(const ANewSize: Integer);
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

{ ItemAttributes_GemTypeSetElementArray }

function ItemAttributes_GemTypeSetElementArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_GemTypeSetElementArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_GemTypeSetElementArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_GemTypeSetElementArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('GemTypeSetElement',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_GemTypeSetElementArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'GemTypeSetElement';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_GemTypeSetElementArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_GemTypeSetElementArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_GenderArray }

function ItemAttributes_GenderArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_GenderArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_GenderArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_GenderArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Gender',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_GenderArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Gender';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_GenderArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_GenderArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_IngredientsSetElementArray }

function ItemAttributes_IngredientsSetElementArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_IngredientsSetElementArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_IngredientsSetElementArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_IngredientsSetElementArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('IngredientsSetElement',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_IngredientsSetElementArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'IngredientsSetElement';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_IngredientsSetElementArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_IngredientsSetElementArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_InterestArray }

function ItemAttributes_InterestArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_InterestArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_InterestArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_InterestArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Interest',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_InterestArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Interest';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_InterestArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_InterestArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_LanguageNameArray }

function ItemAttributes_LanguageNameArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_LanguageNameArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_LanguageNameArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_LanguageNameArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('LanguageName',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_LanguageNameArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'LanguageName';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_LanguageNameArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_LanguageNameArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_MaterialTypeSetElementArray }

function ItemAttributes_MaterialTypeSetElementArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_MaterialTypeSetElementArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_MaterialTypeSetElementArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_MaterialTypeSetElementArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('MaterialTypeSetElement',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_MaterialTypeSetElementArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'MaterialTypeSetElement';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_MaterialTypeSetElementArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_MaterialTypeSetElementArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_PantLengthArray }

function ItemAttributes_PantLengthArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_PantLengthArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_PantLengthArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_PantLengthArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('PantLength',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_PantLengthArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'PantLength';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_PantLengthArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_PantLengthArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

{ ItemAttributes_PantSizeArray }

function ItemAttributes_PantSizeArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_PantSizeArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_PantSizeArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_PantSizeArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('PantSize',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_PantSizeArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'PantSize';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_PantSizeArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_PantSizeArray.SetLength(const ANewSize: Integer);
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

{ ItemAttributes_PrimaryColorArray }

function ItemAttributes_PrimaryColorArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_PrimaryColorArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_PrimaryColorArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_PrimaryColorArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('PrimaryColor',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_PrimaryColorArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'PrimaryColor';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_PrimaryColorArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_PrimaryColorArray.SetLength(const ANewSize: Integer);
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

{ ItemAttributes_ShoeSizeArray }

function ItemAttributes_ShoeSizeArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_ShoeSizeArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_ShoeSizeArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_ShoeSizeArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('ShoeSize',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_ShoeSizeArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'ShoeSize';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_ShoeSizeArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_ShoeSizeArray.SetLength(const ANewSize: Integer);
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

{ ItemAttributes_TargetBrandArray }

function ItemAttributes_TargetBrandArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure ItemAttributes_TargetBrandArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function ItemAttributes_TargetBrandArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure ItemAttributes_TargetBrandArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('TargetBrand',TypeInfo(string),FData[AIndex]);
end;

procedure ItemAttributes_TargetBrandArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'TargetBrand';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function ItemAttributes_TargetBrandArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure ItemAttributes_TargetBrandArray.SetLength(const ANewSize: Integer);
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
    '_E_N_',
    'Help'
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
    '_E_N_',
    'ItemSearch'
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
    '_E_N_',
    'ItemLookup'
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
    '_E_N_',
    'BrowseNodeLookup'
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
    '_E_N_',
    'ListSearch'
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
    '_E_N_',
    'ListLookup'
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
    '_E_N_',
    'CustomerContentSearch'
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
    '_E_N_',
    'CustomerContentLookup'
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
    '_E_N_',
    'SimilarityLookup'
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
    '_E_N_',
    'SellerLookup'
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
    '_E_N_',
    'CartGet'
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
    '_E_N_',
    'CartAdd'
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
    '_E_N_',
    'CartCreate'
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
    '_E_N_',
    'CartModify'
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
    '_E_N_',
    'CartClear'
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
    '_E_N_',
    'TransactionLookup'
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
    '_E_N_',
    'SellerListingSearch'
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
    '_E_N_',
    'SellerListingLookup'
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
    'TagLookup',
    '_E_N_',
    'TagLookup'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'TagLookup',
    'TRANSPORT_soapAction',
    'http://soap.amazon.com'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'TagLookup',
    'FORMAT_Input_EncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'TagLookup',
    'FORMAT_OutputEncodingStyle',
    'literal'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'AWSECommerceServicePortType',
    'MultiOperation',
    '_E_N_',
    'MultiOperation'
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
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemLookupRequest_IdType_Type),'ItemLookupRequest_IdType_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListSearchRequest_ListType_Type),'ListSearchRequest_ListType_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListLookupRequest_ListType_Type),'ListLookupRequest_ListType_Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ListLookupRequest_ListType_Type)].RegisterExternalPropertyName('ListLookupRequest_ListType_Type_WishList','WishList');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ListLookupRequest_ListType_Type)].RegisterExternalPropertyName('ListLookupRequest_ListType_Type_WeddingRegistry','WeddingRegistry');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarityLookupRequest_SimilarityType_Type),'SimilarityLookupRequest_SimilarityType_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModifyRequest_Items_Type_Item_Type_Action_Type),'CartModifyRequest_Items_Type_Item_Type_Action_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingSearchRequest_OfferStatus_Type),'SellerListingSearchRequest_OfferStatus_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingLookupRequest_IdType_Type),'SellerListingLookupRequest_IdType_Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerListingLookupRequest_IdType_Type)].RegisterExternalPropertyName('SellerListingLookupRequest_IdType_Type_ASIN','ASIN');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SellerListingLookupRequest_IdType_Type)].RegisterExternalPropertyName('SellerListingLookupRequest_IdType_Type_SKU','SKU');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Condition_Type),'Condition');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(DeliveryMethod_Type),'DeliveryMethod');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(AudienceRating_Type),'AudienceRating');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(AudienceRating_Type)].RegisterExternalPropertyName('AudienceRating_PG_13','PG-13');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(AudienceRating_Type)].RegisterExternalPropertyName('AudienceRating_NC_17','NC-17');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(AudienceRating_Type)].RegisterExternalPropertyName('AudienceRating__6','6');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(AudienceRating_Type)].RegisterExternalPropertyName('AudienceRating__12','12');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(AudienceRating_Type)].RegisterExternalPropertyName('AudienceRating__16','16');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(AudienceRating_Type)].RegisterExternalPropertyName('AudienceRating__18','18');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(List_ListType_Type),'List_ListType_Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(List_ListType_Type)].RegisterExternalPropertyName('List_ListType_Type_WishList','WishList');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(List_ListType_Type)].RegisterExternalPropertyName('List_ListType_Type_WeddingRegistry','WeddingRegistry');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(List_ListType_Type)].RegisterExternalPropertyName('List_ListType_Type_BabyRegistry','BabyRegistry');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(List_ListType_Type)].RegisterExternalPropertyName('List_ListType_Type_Listmania','Listmania');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tag_TagType_Type),'Tag_TagType_Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Tag_TagType_Type)].RegisterExternalPropertyName('Tag_TagType_Type_Item','Item');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Help_Type),'Help');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(HelpResponse_Type),'HelpResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemSearch_Type),'ItemSearch');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemSearchResponse_Type),'ItemSearchResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemLookup_Type),'ItemLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemLookupResponse_Type),'ItemLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodeLookup_Type),'BrowseNodeLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodeLookupResponse_Type),'BrowseNodeLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListSearch_Type),'ListSearch');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListSearchResponse_Type),'ListSearchResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListLookup_Type),'ListLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListLookupResponse_Type),'ListLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentSearch_Type),'CustomerContentSearch');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentSearchResponse_Type),'CustomerContentSearchResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentLookup_Type),'CustomerContentLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentLookupResponse_Type),'CustomerContentLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarityLookup_Type),'SimilarityLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarityLookupResponse_Type),'SimilarityLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerLookup_Type),'SellerLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerLookupResponse_Type),'SellerLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartGet_Type),'CartGet');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartGetResponse_Type),'CartGetResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAdd_Type),'CartAdd');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddResponse_Type),'CartAddResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreate_Type),'CartCreate');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreateResponse_Type),'CartCreateResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModify_Type),'CartModify');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModifyResponse_Type),'CartModifyResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartClear_Type),'CartClear');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartClearResponse_Type),'CartClearResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionLookup_Type),'TransactionLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionLookupResponse_Type),'TransactionLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingSearch_Type),'SellerListingSearch');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingSearchResponse_Type),'SellerListingSearchResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingLookup_Type),'SellerListingLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingLookupResponse_Type),'SellerListingLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TagLookup_Type),'TagLookup');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TagLookupResponse_Type),'TagLookupResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MultiOperation_Type),'MultiOperation');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MultiOperationResponse),'MultiOperationResponse');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Bin_BinParameter_Type),'Bin_BinParameter_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Bin_Type),'Bin');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchBinSet_Type),'SearchBinSet');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(HelpRequest_Type),'HelpRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemSearchRequest_Type),'ItemSearchRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemLookupRequest_Type),'ItemLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListSearchRequest_Type),'ListSearchRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListLookupRequest_Type),'ListLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentSearchRequest_Type),'CustomerContentSearchRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerContentLookupRequest_Type),'CustomerContentLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarityLookupRequest_Type),'SimilarityLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerLookupRequest_Type),'SellerLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartGetRequest_Type),'CartGetRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddRequest_Items_Type_Item_Type_MetaData_Type),'CartAddRequest_Items_Type_Item_Type_MetaData_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddRequest_Items_Type_Item_Type),'CartAddRequest_Items_Type_Item_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddRequest_Type),'CartAddRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreateRequest_Items_Type_Item_Type_MetaData_Type),'CartCreateRequest_Items_Type_Item_Type_MetaData_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreateRequest_Items_Type_Item_Type),'CartCreateRequest_Items_Type_Item_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreateRequest_Type),'CartCreateRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModifyRequest_Items_Type_Item_Type),'CartModifyRequest_Items_Type_Item_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartModifyRequest_Type),'CartModifyRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartClearRequest_Type),'CartClearRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionLookupRequest_Type),'TransactionLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingSearchRequest_Type),'SellerListingSearchRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListingLookupRequest_Type),'SellerListingLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TagLookupRequest_Type),'TagLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodeLookupRequest_Type),'BrowseNodeLookupRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OperationRequest_Type),'OperationRequest');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Request_Type),'Request');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Arguments_Argument_Type),'Arguments_Argument_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(HTTPHeaders_Header_Type),'HTTPHeaders_Header_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Errors_Error_Type),'Errors_Error_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Information_Type),'Information');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Items_Type),'Items');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CorrectedQuery_Type),'CorrectedQuery');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Lists_Type),'Lists');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Customers_Type),'Customers');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Cart_Type),'Cart');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transactions_Type),'Transactions');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Sellers_Type),'Sellers');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListings_Type),'SellerListings');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OperationInformation_Type),'OperationInformation');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ResponseGroupInformation_Type),'ResponseGroupInformation');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(List_Type),'List');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListItem_Type),'ListItem');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Customer_Location_Type),'Customer_Location_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Customer_Type),'Customer');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchResultsMap_SearchIndex_Type),'SearchResultsMap_SearchIndex_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Item_ImageSets_Type),'Item_ImageSets_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Item_AlternateVersions_Type_AlternateVersion_Type),'Item_AlternateVersions_Type_AlternateVersion_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Item_Type),'Item');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tags_Type),'Tags');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tag_Type),'Tag');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TaggedItems_Type),'TaggedItems');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TaggedListmaniaLists_Type),'TaggedListmaniaLists');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TaggedGuides_Type),'TaggedGuides');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Guide_Type),'Guide');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tagging_Type),'Tagging');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OfferSummary_Type),'OfferSummary');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Offers_Type),'Offers');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Offer_Type),'Offer');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OfferAttributes_Type),'OfferAttributes');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Merchant_Type),'Merchant');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OfferListing_AvailabilityAttributes_Type),'OfferListing_AvailabilityAttributes_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OfferListing_ShippingCharge_Type),'OfferListing_ShippingCharge_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OfferListing_Type),'OfferListing');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(LoyaltyPoints_Type),'LoyaltyPoints');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(VariationSummary_Type),'VariationSummary');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Variations_Type),'Variations');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Collections_Collection_Type_CollectionSummary_Type),'Collections_Collection_Type_CollectionSummary_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Collections_Collection_Type_CollectionParent_Type),'Collections_Collection_Type_CollectionParent_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Collections_Collection_Type_CollectionItem_Type),'Collections_Collection_Type_CollectionItem_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Collections_Collection_Type),'Collections_Collection_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(EditorialReview_Type),'EditorialReview');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerReviews_Type),'CustomerReviews');
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
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Promotion_Summary_Type),'Promotion_Summary_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Promotion_Details_Type),'Promotion_Details_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Promotion_Type),'Promotion');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(PromotionBenefit_Type),'PromotionBenefit');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(PromotionEligibilityRequirement_Type),'PromotionEligibilityRequirement');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(PromotionItemApplicability_Type),'PromotionItemApplicability');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodes_Type),'BrowseNodes');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Property_Type),'Property');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNode_Type),'BrowseNode');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListmaniaLists_ListmaniaList_Type),'ListmaniaLists_ListmaniaList_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchInside_Excerpt_Type),'SearchInside_Excerpt_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchInside_Type),'SearchInside');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartItems_Type),'CartItems');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SavedForLaterItems_Type),'SavedForLaterItems');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartItem_MetaData_Type_KeyValuePair_Type),'CartItem_MetaData_Type_KeyValuePair_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartItem_Type),'CartItem');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transaction_Totals_Type),'Transaction_Totals_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transaction_Shipments_Type_Shipment_Type_Packages_Type_Package_Type),'Transaction_Shipments_Type_Shipment_Type_Packages_Type_Package_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transaction_Shipments_Type_Shipment_Type),'Transaction_Shipments_Type_Shipment_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Transaction_Type),'Transaction');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TransactionItem_Type),'TransactionItem');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Seller_Location_Type),'Seller_Location_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type),'Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type),'Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Seller_Type),'Seller');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerFeedback_Feedback_Type),'SellerFeedback_Feedback_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Address_Type),'Address');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SellerListing_Type),'SellerListing');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Price_Type),'Price');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ImageSet_Type),'ImageSet');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Image_Type),'Image');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_Creator_Type),'ItemAttributes_Creator_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_ItemDimensions_Type),'ItemAttributes_ItemDimensions_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_Languages_Type_Language_Type),'ItemAttributes_Languages_Type_Language_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_PackageDimensions_Type),'ItemAttributes_PackageDimensions_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_Type),'ItemAttributes');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_Creator_Type),'MerchantItemAttributes_Creator_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_ItemDimensions_Type),'MerchantItemAttributes_ItemDimensions_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_Languages_Type_Language_Type),'MerchantItemAttributes_Languages_Type_Language_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_PackageDimensions_Type),'MerchantItemAttributes_PackageDimensions_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_VendorRebate_Type),'MerchantItemAttributes_VendorRebate_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(MerchantItemAttributes_Type),'MerchantItemAttributes');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(NonNegativeIntegerWithUnits_Type),'NonNegativeIntegerWithUnits');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(DecimalWithUnits_Type),'DecimalWithUnits');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(StringWithUnits_Type),'StringWithUnits');
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
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TagLookup_RequestArray),'TagLookup_RequestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TagLookup_RequestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TagLookupResponse_TagsArray),'TagLookupResponse_TagsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TagLookupResponse_TagsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
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
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddRequest_Items_Type_Item_Type_MetaDataArray),'CartAddRequest_Items_Type_Item_Type_MetaDataArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartAddRequest_Items_Type_Item_Type_MetaDataArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddRequest_Items_Type),'CartAddRequest_Items_Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartAddRequest_Items_Type)].RegisterExternalPropertyName(sARRAY_ITEM,'Item');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartAddRequest_ResponseGroupArray),'CartAddRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartAddRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartCreateRequest_Items_Type_Item_Type_MetaDataArray),'CartCreateRequest_Items_Type_Item_Type_MetaDataArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartCreateRequest_Items_Type_Item_Type_MetaDataArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
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
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TagLookupRequest_TagNameArray),'TagLookupRequest_TagNameArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TagLookupRequest_TagNameArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TagLookupRequest_ResponseGroupArray),'TagLookupRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TagLookupRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodeLookupRequest_BrowseNodeIdArray),'BrowseNodeLookupRequest_BrowseNodeIdArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(BrowseNodeLookupRequest_BrowseNodeIdArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodeLookupRequest_ResponseGroupArray),'BrowseNodeLookupRequest_ResponseGroupArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(BrowseNodeLookupRequest_ResponseGroupArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Arguments_Type),'Arguments');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(HTTPHeaders_Type),'HTTPHeaders');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Errors_Type),'Errors');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Information_OperationInformationArray),'Information_OperationInformationArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Information_OperationInformationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Information_ResponseGroupInformationArray),'Information_ResponseGroupInformationArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Information_ResponseGroupInformationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Items__ItemArray),'Items__ItemArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Items__ItemArray)].RegisterExternalPropertyName(sARRAY_ITEM,'Item');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Items__ItemArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Lists_ListArray),'Lists_ListArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Lists_ListArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Customers_CustomerArray),'Customers_CustomerArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Customers_CustomerArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
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
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SearchResultsMap_Type),'SearchResultsMap');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Item_ImageSets_Type_ImageSetArray),'Item_ImageSets_Type_ImageSetArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Item_ImageSets_Type_ImageSetArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Item_Subjects_Type),'Item_Subjects_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Item_AlternateVersions_Type),'Item_AlternateVersions_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(_Item_ImageSetsArray),'_Item_ImageSetsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(_Item_ImageSetsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tags_TagArray),'Tags_TagArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Tags_TagArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tag_TaggedItemsArray),'Tag_TaggedItemsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Tag_TaggedItemsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tag_TaggedListmaniaListsArray),'Tag_TaggedListmaniaListsArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Tag_TaggedListmaniaListsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tag_TaggedGuidesArray),'Tag_TaggedGuidesArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Tag_TaggedGuidesArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Offers_OfferArray),'Offers_OfferArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Offers_OfferArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Offer_OfferListingArray),'Offer_OfferListingArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Offer_OfferListingArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OfferListing_ShippingChargeArray),'OfferListing_ShippingChargeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(OfferListing_ShippingChargeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Variations__ItemArray),'Variations__ItemArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Variations__ItemArray)].RegisterExternalPropertyName(sARRAY_ITEM,'Item');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Variations__ItemArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(VariationDimensions_Type),'VariationDimensions');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(EditorialReviews_Type),'EditorialReviews');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Collections_Collection_Type_CollectionItemArray),'Collections_Collection_Type_CollectionItemArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Collections_Collection_Type_CollectionItemArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Collections_Type),'Collections');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CustomerReviews_ReviewArray),'CustomerReviews_ReviewArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CustomerReviews_ReviewArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tracks_Disc_Type_TrackArray),'Tracks_Disc_Type_TrackArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Tracks_Disc_Type_TrackArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Tracks_Type),'Tracks');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarProducts_Type),'SimilarProducts');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(TopSellers_Type),'TopSellers');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(NewReleases_Type),'NewReleases');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SimilarViewedProducts_Type),'SimilarViewedProducts');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(OtherCategoriesSimilarProducts_Type),'OtherCategoriesSimilarProducts');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Accessories_Type),'Accessories');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(Promotions_Type),'Promotions');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(PromotionEligibilityRequirements_Type),'PromotionEligibilityRequirements');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(PromotionBenefits_Type),'PromotionBenefits');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNodes_BrowseNodeArray),'BrowseNodes_BrowseNodeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(BrowseNodes_BrowseNodeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNode_Properties_Type),'BrowseNode_Properties_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNode_Children_Type),'BrowseNode_Children_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(BrowseNode_Ancestors_Type),'BrowseNode_Ancestors_Type');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ListmaniaLists_Type),'ListmaniaLists');
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartItems_CartItemArray),'CartItems_CartItemArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(CartItems_CartItemArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(SavedForLaterItems_SavedForLaterItemArray),'SavedForLaterItems_SavedForLaterItemArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(SavedForLaterItems_SavedForLaterItemArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(CartItem_MetaData_Type),'CartItem_MetaData_Type');
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
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_AgeArray),'ItemAttributes_AgeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_AgeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_ArtistArray),'ItemAttributes_ArtistArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_ArtistArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_AudioFormatArray),'ItemAttributes_AudioFormatArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_AudioFormatArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_AuthorArray),'ItemAttributes_AuthorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_AuthorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_CameraManualFeaturesArray),'ItemAttributes_CameraManualFeaturesArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_CameraManualFeaturesArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_CategoryArray),'ItemAttributes_CategoryArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_CategoryArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_CategoryBinArray),'ItemAttributes_CategoryBinArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_CategoryBinArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_CharacterArray),'ItemAttributes_CharacterArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_CharacterArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_CompatibleDevicesArray),'ItemAttributes_CompatibleDevicesArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_CompatibleDevicesArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_CreatorArray),'ItemAttributes_CreatorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_CreatorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_DataLinkProtocolArray),'ItemAttributes_DataLinkProtocolArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_DataLinkProtocolArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_DirectorArray),'ItemAttributes_DirectorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_DirectorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_EducationalFocusArray),'ItemAttributes_EducationalFocusArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_EducationalFocusArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_EthnicityArray),'ItemAttributes_EthnicityArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_EthnicityArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_FeatureArray),'ItemAttributes_FeatureArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_FeatureArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_FormatArray),'ItemAttributes_FormatArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_FormatArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_FormFactorArray),'ItemAttributes_FormFactorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_FormFactorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_GemTypeSetElementArray),'ItemAttributes_GemTypeSetElementArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_GemTypeSetElementArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_GenderArray),'ItemAttributes_GenderArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_GenderArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_IngredientsSetElementArray),'ItemAttributes_IngredientsSetElementArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_IngredientsSetElementArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_InterestArray),'ItemAttributes_InterestArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_InterestArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_LanguageNameArray),'ItemAttributes_LanguageNameArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_LanguageNameArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_MaterialTypeSetElementArray),'ItemAttributes_MaterialTypeSetElementArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_MaterialTypeSetElementArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_PantLengthArray),'ItemAttributes_PantLengthArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_PantLengthArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_PantSizeArray),'ItemAttributes_PantSizeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_PantSizeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_PhotoFlashTypeArray),'ItemAttributes_PhotoFlashTypeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_PhotoFlashTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_PictureFormatArray),'ItemAttributes_PictureFormatArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_PictureFormatArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_PlatformArray),'ItemAttributes_PlatformArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_PlatformArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_PrimaryColorArray),'ItemAttributes_PrimaryColorArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_PrimaryColorArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_ReturnMethodArray),'ItemAttributes_ReturnMethodArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_ReturnMethodArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_ShoeSizeArray),'ItemAttributes_ShoeSizeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_ShoeSizeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_SpecialFeaturesArray),'ItemAttributes_SpecialFeaturesArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_SpecialFeaturesArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_SupportedImageTypeArray),'ItemAttributes_SupportedImageTypeArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_SupportedImageTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  GetTypeRegistry().Register(sNAME_SPACE,TypeInfo(ItemAttributes_TargetBrandArray),'ItemAttributes_TargetBrandArray');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_TargetBrandArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
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

  SearchBinSet_Type.RegisterAttributeProperty('NarrowBy');
  HelpRequest_Type.RegisterAttributeProperty('About');
  ItemLookupRequest_Type.RegisterAttributeProperty('VariationPage');
  Arguments_Argument_Type.RegisterAttributeProperty('Name');
  Arguments_Argument_Type.RegisterAttributeProperty('Value');
  HTTPHeaders_Header_Type.RegisterAttributeProperty('Name');
  HTTPHeaders_Header_Type.RegisterAttributeProperty('Value');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Items_Type)].RegisterExternalPropertyName('_Item','Item');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ListItem_Type)].RegisterExternalPropertyName('_Item','Item');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(TaggedItems_Type)].RegisterExternalPropertyName('_Item','Item');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Variations_Type)].RegisterExternalPropertyName('_Item','Item');
  Tracks_Disc_Type_Track_Type.RegisterAttributeProperty('Number');
  Tracks_Disc_Type.RegisterAttributeProperty('Number');
  NewReleases_NewRelease_Type.RegisterAttributeProperty('Title');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type)].RegisterExternalPropertyName('_Type','Type');
  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type_SellerFeedbackRating_Type.RegisterAttributeProperty('_Type');
  Seller_SellerFeedbackSummary_Type_FeedbackDateRange_Type.RegisterAttributeProperty('Period');
  Seller_Type.RegisterAttributeProperty('SellerFeedbackSummary');
  ImageSet_Type.RegisterAttributeProperty('Category');
  ItemAttributes_Creator_Type.RegisterAttributeProperty('Role');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_Languages_Type_Language_Type)].RegisterExternalPropertyName('_Type','Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(ItemAttributes_Type)].RegisterExternalPropertyName('_Label','Label');
  MerchantItemAttributes_Creator_Type.RegisterAttributeProperty('Role');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_Languages_Type_Language_Type)].RegisterExternalPropertyName('_Type','Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_VendorRebate_Type)].RegisterExternalPropertyName('_Type','Type');
  GetTypeRegistry().ItemByTypeInfo[TypeInfo(MerchantItemAttributes_Type)].RegisterExternalPropertyName('_Label','Label');
  NonNegativeIntegerWithUnits_Type.RegisterAttributeProperty('Units');
  DecimalWithUnits_Type.RegisterAttributeProperty('Units');
  StringWithUnits_Type.RegisterAttributeProperty('Units');

End.
