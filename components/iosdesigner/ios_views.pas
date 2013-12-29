{ Fake components to mimic iOS's views in the Lazarus designer

  Copyright (C) 2012 Joost van der Sluis/CNOC joost@cnoc.nl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
// The name of this unit has to be of the same length as the name of the
// iPhoneAll unit.
unit iOS_Views;

{$mode objfpc}{$H+}
{$modeswitch ObjectiveC1}

interface

uses
  Classes, SysUtils, Math, types, DOM, XMLWrite, XMLRead, Graphics, strutils,
  ComponentReg;

type
  TXIBProperties = (
    bvNextResponder,
    bvOpaque,
    bvHighlighted,
    bvAlpha,
    bvText,
    bvTextColor,
    bvTextAlignment,
    bvNormalTitle,
    bvNormalTitleColor,
    bvSuperview,
    bvLines,
    bvBackgroundColor,
    bvSeparatorColor,
    bvFullScreen,
    bvClearContext,
    bvFlags,
    bvButtonType,
    bvLineBreak,
    bvPlaceHolder,
    bvFont,
    bvPrompt,
    bvRowHeight,
    bvSectionHeaderHeigh,
    bvSectionFooterHeight,
    bvSeparatorStyle,
    bvClipSubviews,
    bvShowSelectionOnTouch,
    bvBounceVertically,
    bvProgress,
    bvProgressTintColor,
    bvTrackTintColor,
    bvProgressViewStyle,
    bvEnabled,
    bvSegments,
    bvSelectedSegmentIndex);

  TXIBProperty = record
    APropertyName: string;
    ADefaultValue: string;
  end;

  id = ^objc_object;
  TcocoaEvent = procedure(sender: id) of object;

const
  XIBPropertiesStrings : array[TXIBProperties] of TXIBProperty = (
    (APropertyName: 'NSNextResponder'; ADefaultValue: ''),
    (APropertyName: 'IBUIOpaque'     ; ADefaultValue: 'YES'),
    (APropertyName: 'IBUIHighlighted'; ADefaultValue: 'NO'),
    (APropertyName: 'IBUIAlpha'      ; ADefaultValue: '1'),
    (APropertyName: 'IBUIText'       ; ADefaultValue: ''),
    (APropertyName: 'IBUITextColor'  ; ADefaultValue: ''),
    (APropertyName: 'IBUITextAlignment' ; ADefaultValue: '0'),
    (APropertyName: 'IBUINormalTitle' ; ADefaultValue: ''),
    (APropertyName: 'IBUINormalTitleColor' ; ADefaultValue: ''),
    (APropertyName: 'NSSuperview'    ; ADefaultValue: ''),
    (APropertyName: 'IBUINumberOfLines' ; ADefaultValue: '1'),
    (APropertyName: 'IBUIBackgroundColor' ; ADefaultValue: ''),
    (APropertyName: 'IBUISeparatorColor' ; ADefaultValue: ''),
    (APropertyName: 'IBUIResizesToFullScreen' ; ADefaultValue: 'NO'),
    (APropertyName: 'IBUIClearsContextBeforeDrawing' ; ADefaultValue: 'YES'),
    (APropertyName: 'NSvFlags'       ; ADefaultValue: '0'),
    (APropertyName: 'IBUIButtonType' ; ADefaultValue: '0'),
    (APropertyName: 'IBUILineBreakMode' ; ADefaultValue: '4'),
    (APropertyName: 'IBUIPlaceholder' ; ADefaultValue: ''),
    (APropertyName: 'IBUIFontDescription' ; ADefaultValue: ''),
    (APropertyName: 'IBPrompt'       ; ADefaultValue: ''),
    (APropertyName: 'IBUIRowHeight'  ; ADefaultValue: ''),
    (APropertyName: 'IBUISectionHeaderHeight' ; ADefaultValue: ''),
    (APropertyName: 'IBUISectionFooterHeight' ; ADefaultValue: ''),
    (APropertyName: 'IBUISeparatorStyle' ; ADefaultValue: ''),
    (APropertyName: 'IBUIClipsSubviews' ; ADefaultValue: ''),
    (APropertyName: 'IBUIShowsSelectionImmediatelyOnTouchBegin' ; ADefaultValue: ''),
    (APropertyName: 'IBUIAlwaysBounceVertical' ; ADefaultValue: ''),
    (APropertyName: 'IBUIProgress' ; ADefaultValue: '0'),
    (APropertyName: 'IBUIProgressTintColor' ; ADefaultValue: ''),
    (APropertyName: 'IBUITrackTintColor' ; ADefaultValue: ''),
    (APropertyName: 'IBUIProgressViewStyle' ; ADefaultValue: '0'),
    (APropertyName: 'IBUIEnabled'    ; ADefaultValue: 'YES'),
    (APropertyName: 'IBNumberOfSegments' ; ADefaultValue: '0'),
    (APropertyName: 'IBSelectedSegmentIndex' ; ADefaultValue: ''));

  EventNames : array[1..1] of string = (
    'onTouchDown');

type
  IMyWidgetDesigner = interface(IUnknown)
    ['{AB6C118F-9520-626A-ED24-378E04D03474}']
    procedure InvalidateRect(Sender: TObject; ARect: TRect; Erase: boolean);
    procedure ClearMyForm;
    function CreateComponent(ParentComp: TComponent;
                             TypeClass: TComponentClass;
                             const AUnitName: shortstring;
                             X,Y,W,H: Integer;
                             DisableAutoSize: boolean): TComponent;
  end;

  TiOSFakeFontType = (ftNotSet, ftSystem=1, ftSystemBold=2, ftSystemItalic=3, ftCustom=4);

  { TiOSFakeFontDescription }

  TiOSFakeFontDescription = class(TPersistent)
  private
    FXIBObjectElement: TDOMElement;
    procedure SetFont(AFontType: TiOSFakeFontType; APointSize: double);
    function GetFontType: TiOSFakeFontType;
    function GetpointSize: double;
    procedure SetFontType(AValue: TiOSFakeFontType);
    procedure SetpointSize(AValue: double);
  public
    constructor Create;
    procedure ApplyToLCLFont(AFont: TFont);
  published
    property FontType: TiOSFakeFontType read GetFontType write SetFontType;
    property pointSize: double read GetpointSize write SetpointSize;
  end;

  { tiOSFakeComponent }

  tiOSWriteDomMethod = function (AnObjectDomElement: TDOMElement): TDOMElement of object;
  TiOSXIBPos = (sLeft, sTop, sWidth, sHeight);

  TiOSXIBConnectionType = (ctOutlet, ctEvent);

  NSObject = class;
  tiOSFakeComponent = class(TComponent)
  private
    FStoredEvents: TStringList;

    FAcceptChildsAtDesignTime: boolean;
    FChilds: TFPList; // list of tiOSFakeComponent
    FTop: integer;
    FLeft: integer;
    FXIBObjectElement: TDOMElement;
    FParent: tiOSFakeComponent;
    FFont: TiOSFakeFontDescription;
    // iOS
    procedure AddChildToDom(const AValue: tiOSFakeComponent);
    procedure RemoveChildFromDom(const AValue: tiOSFakeComponent);
    function GetObjectID: integer;
    function GetPosition(APosition: TiOSXIBPos): integer;
    function GetRef: integer;
    function GetXIBDocument: TXMLDocument; virtual;
    function GetXIBOrderedObjects: TDOMElement;
    function GetXIBObjectRecords: TDOMElement;
    function GetXIBObjects: TDOMElement;
    function GetXIBRootObjects: TDOMElement;
    function GetXIBFlattenedProperties: TDOMElement;
    function GetXIBConnectionRecords: TDOMElement;
    function GetNextObjectID: integer;
    function GetXIBConnection(ASourceRef, ADestinationRef: int64;
      ConnectionType: TiOSXIBConnectionType; CreateIfNotExists: boolean; IBEventType: integer): TDOMElement;
    function FindComponentByRef(ARef: int64): tiOSFakeComponent;
    function FindOrderdObjectByRef(ARef: int64): TDOMElement;
  protected
    procedure SetXIBObjectElement(const AValue: TDOMElement);
    procedure AddChild(const AValue: tiOSFakeComponent); virtual;
    procedure RemoveChild(const AValue: tiOSFakeComponent); virtual;
    procedure SetParentComponent(Value: TComponent); override;
    procedure SetParent(const AValue: tiOSFakeComponent);
    procedure SetHeight(const AValue: integer);
    procedure SetLeft(const AValue: integer);
    procedure SetTop(const AValue: integer);
    procedure SetWidth(const AValue: integer);
    function GetChilds(Index: integer): tiOSFakeComponent;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure InternalInvalidateRect(ARect: TRect; Erase: boolean); virtual;
    function  StoreSizeAsFrameSize: boolean; virtual;

    procedure SetName(const NewName: TComponentName); override;
    // iOS
    procedure AddConnectionRecord(AnObjectDomElement: TDOMElement; AConnectionType, ALabel, AEventType: string);
    function GetIBClassName: string; virtual;
    function GetDesigner: IMyWidgetDesigner; virtual;
    function GetHeight: integer; virtual;
    function GetLeft: integer; virtual;
    function GetTop: integer; virtual;
    function GetWidth: integer; virtual;

    function StringToBytes(const AString: string): string;
    function BytesToString(AString: string): string;

    function GetXIBBoolean(index: TXIBProperties): boolean;
    procedure SetXIBBoolean(index: TXIBProperties; AValue: boolean);
    function GetXIBFloat(index: TXIBProperties): double; virtual;
    procedure SetXIBFloat(index: TXIBProperties; AValue: double);
    function GetXIBString(index: TXIBProperties; ANodeName: string): string;
    procedure SetXIBString(index: TXIBProperties; ANodeName, AValue: string);
    function GetXIBString(index: TXIBProperties): string;
    procedure SetXIBString(index: TXIBProperties; AValue: string);
    function GetXIBInteger(index: TXIBProperties): integer; virtual;
    procedure SetXIBInteger(index: TXIBProperties; AValue: integer);
    function GetXIBInt64(index: TXIBProperties): int64; virtual;
    procedure SetXIBInt64(index: TXIBProperties; AValue: int64);
    function GetXIBColor(index: TXIBProperties): TColor; virtual;
    procedure SetXIBColor(index: TXIBProperties; AValue: TColor);
    function GetXIBEvent(index: integer): TCocoaEvent;
    procedure SetXIBEvent(Index: integer; AValue: TCocoaEvent);
    function GetXIBFont(index: TXIBProperties): TiOSFakeFontDescription;

    function GetNSObject: NSObject; virtual;
    // Streaming
    procedure DefineProperties(Filer: TFiler); override;
    property Font: TiOSFakeFontDescription index bvFont read GetXIBFont; // write SetXIBFont;
  public
    procedure InitializeDefaults; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitializeDefaultChildren; virtual;
    procedure paint(ACanvas: TCanvas); virtual;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); virtual;
    procedure InvalidateRect(ARect: TRect; Erase: boolean);
    procedure Invalidate;
    function ChildCount: integer;
    property Parent: tiOSFakeComponent read FParent write SetParent;
    property Children[Index: integer]: tiOSFakeComponent read GetChilds;
    property AcceptChildsAtDesignTime: boolean read FAcceptChildsAtDesignTime;
    // iOS
    property Ref: integer read GetRef;
    property ObjectID: integer read GetObjectID;
    // Dom utils
    function AddElement(ADomNode: TDOMElement; AName: string): TDOMElement;
    function AddIBInt(ADomNode: TDOMElement; APropName: string; APropValue: integer; ADefaultValue: integer = MaxInt): TDOMElement;
    function AddIBDouble(ADomNode: TDOMElement; APropName: string; APropValue: double): TDOMElement;
    function AddIBFloat(ADomNode: TDOMElement; APropName: string; APropValue: double; ADefaultValue: double = MaxFloat): TDOMElement;
    function AddIBString(ADomNode: TDOMElement; APropName: string; APropValue: string): TDOMElement;
    function AddIBReference(ADomNode: TDOMElement; APropName: string; APropValue: tiOSFakeComponent; ForceValue: boolean = false): TDOMElement;
    function AddIBReference(ADomNode: TDOMElement; APropName: string; APropValue: string): TDOMElement;
    function AddIBBoolean(ADomNode: TDOMElement; APropName: string; APropValue: Boolean): TDOMElement;
    function AddIBBoolean(ADomNode: TDOMElement; APropName: string; APropValue: Boolean; ADefaultValue: boolean): TDOMElement;
    function AddIBBytes(ADomNode: TDOMElement; APropName: string; APropValue: string): TDOMElement;
    function AddIBObject(ADomNode: TDOMElement; APropName: string; AClass: string): TDOMElement;
    function AddIBObject(ADomDocument: TXMLDocument; APropName: string; AClass: string): TDOMElement;
    function AddIBColor(ADomNode: TDOMElement; APropName: string; AColor: TColor): TDOMElement;
    function AddIBFontDescription(ADomNode: TDOMElement; APropName: string; AFontDescription: TiOSFakeFontDescription): TDOMElement;
    // XIB Streaming
    property XIBObjectElement: TDOMElement read FXIBObjectElement;
  published
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
  end;

  { UIxcodePlaceholder }

  UIxcodePlaceholder =  class(tiOSFakeComponent)
  private
    function GetXcodeClassName: string;
    procedure SetXcodeClassName(AValue: string);
  public
    procedure paint(ACanvas: TCanvas); override;
    function GetIBClassName: string; override;
  published
    property XcodeClassName: string read GetXcodeClassName write SetXcodeClassName;
  end;

  { UIView }

  UIView = class(tiOSFakeComponent)
  private
    function GetFlags(AIndex: Integer): boolean;
    function GetXIBObject(AIndex: TXIBProperties): UIView;
    function ObtainSuperview: UIView;
    procedure SetFlags(AIndex: Integer; AValue: boolean);
    procedure SetXIBObject(AIndex: TXIBProperties; AValue: UIView);
  protected
    function GetPaintText: string; virtual;
    procedure SetName(const NewName: TComponentName); override;
    property Caption: string index bvText read GetXIBString write SetXIBString;
    procedure AddChild(const AValue: tiOSFakeComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitializeDefaults; override;
    procedure paint(ACanvas: TCanvas); override;
    function GetIBClassName: string; override;
    property NSSuperview: UIView index bvSuperview read GetXIBObject write SetXIBObject;
    property NSNextResponder: UIView index bvNextResponder read GetXIBObject write SetXIBObject;
    property Flags: Int64 index bvFlags read GetXIBInt64 write SetXIBInt64;
  published
    property Opaque: boolean index bvOpaque read GetXIBBoolean write SetXIBBoolean;
    property BackgroundColor: TColor index bvBackgroundColor read GetXIBColor write SetXIBColor;
    property Alpha: double index bvAlpha read GetXIBFloat write SetXIBFloat;
    property Hidden: boolean index 31 read GetFlags write SetFlags;
  end;
  TUIViewClass = class of UIView;

  { NSObject }

  NSObject = class(tiOSFakeComponent)
  private
    FDesigner: IMyWidgetDesigner;
    FFilesOwnerClass: string;
    FIsHiddenObject: boolean;
    FNIBDocument: TXMLDocument;
    FWidth, FHeight: integer;
    FXIBUsesObjectsForArrays: boolean;
    function GetFilesOwnerOutletName: string;
    function IsNIBRoot: boolean;
    function GetFilesOwnerID: string;
    procedure SetFilesOwnerOutletName(AValue: string);
  protected
    function GetNSObject: NSObject; override;
    procedure InternalInvalidateRect(ARect: TRect; Erase: boolean); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetLeft: integer; override;
    function GetTop: integer; override;
    function GetHeight: integer; override;
    function GetWidth: integer; override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDesigner: IMyWidgetDesigner; override;
    procedure InitializeDefaults; override;
    function GetIBClassName: string; override;
    property Designer: IMyWidgetDesigner read FDesigner write FDesigner;
    property NIBDocument: TXMLDocument read FNIBDocument;
    property XIBUsesObjectsForArrays: boolean read FXIBUsesObjectsForArrays;
  published
    property FilesOwnerClass: string read FFilesOwnerClass write FFilesOwnerClass stored IsNIBRoot;
    property IsHiddenObject: boolean read FIsHiddenObject write FIsHiddenObject stored IsNIBRoot;
    property FilesOwnerOutletName: string read GetFilesOwnerOutletName write SetFilesOwnerOutletName;
  end;

  UIResponder = class(NSObject);

  { UIWindow }

  TiOSFakeStatusBarStyle = (sbsGrey=0, sbsBlackTranslucent=1, sbsBlack = 2, sbsNone=3);

  UIWindow = class(UIView)
  private
    function GetStatusBar: TiOSFakeStatusBarStyle;
    procedure SetStatusBar(AValue: TiOSFakeStatusBarStyle);
  protected
    function StoreSizeAsFrameSize: boolean; override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    procedure InitializeDefaults; override;
    constructor Create(AOwner: TComponent); override;
    procedure paint(ACanvas: TCanvas); override;
    function GetIBClassName: string; override;
  published
    property StatusBar: TiOSFakeStatusBarStyle read GetStatusBar write SetStatusBar;
    property ResizesToFullScreen: boolean index bvFullScreen read GetXIBBoolean write SetXIBBoolean;
    property ClearGraphincsContext: boolean index bvClearContext read GetXIBBoolean write SetXIBBoolean;
  end;

  { UIViewController }

  UIViewController = class(tiOSFakeComponent)
  protected
    function GetHeight: integer; override;
    function GetWidth: integer; override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetIBClassName: string; override;
  end;

  { UINavigationBar }

  UINavigationBar = class(UIView)
  protected
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
    procedure paint(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetIBClassName: string; override;
  end;

  { UINavigationItem }

  UINavigationItem = class(UIView)
  protected
    function GetWidth: integer; override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetIBClassName: string; override;
  end;

  { UISegmentedControl }

  UISegmentedControl = class(UIView, IFPObserver)
  private
    FCreatingSegments: boolean;
    FSegments: TCollection;
    function GetSegments: TCollection;
  protected
    Procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
  public
    procedure InitializeDefaults; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure paint(ACanvas: TCanvas); override;
    function GetIBClassName: string; override;
    property SegmentCount: integer index bvSegments read GetXIBInteger;
  published
    property Segments: TCollection read GetSegments;
  end;

  { TiOSSegmentedControlSegment }

  TiOSSegmentedControlSegment = class(TCollectionItem)
  private
    FSegmentedControl: UISegmentedControl;
    function GetEnabled: boolean;
    function GetTitle: string;
    procedure SetEnabled(AValue: boolean);
    procedure SetTitle(AValue: string);
  published
    property Title: string read GetTitle write SetTitle;
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

  { UINavigationController }

  UINavigationController = class(UIViewController)
  private
  protected
     procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitializeDefaultChildren; override;
    function GetIBClassName: string; override;
  published
  end;

  { UIButton }

  TiOSFakeButtonType = (
    Custom = 0,
    RoundedRect = 1,
    DetailDisclosure = 2,
    InfoLight = 3,
    InfoDark = 4,
    ContactAdd = 5);

  UIButton = class(UIView)
  private
    FNSNextKeyView: UIView;
    function GetButtonType: TiOSFakeButtonType;
    procedure SetButtonType(AValue: TiOSFakeButtonType);
  protected
    function GetPaintText: string; override;
  public
    procedure InitializeDefaults; override;
    constructor Create(AOwner: TComponent); override;
    function GetIBClassName: string; override;
    procedure paint(ACanvas: TCanvas); override;

    property NSNextKeyView: UIView read FNSNextKeyView write FNSNextKeyView;
  published
    property NormalTitle: string index bvNormalTitle read GetXIBString write SetXIBString;
    property NormalTitleColor: TColor index bvNormalTitleColor read GetXIBColor write SetXIBColor;
    property ButtonType: TiOSFakeButtonType read GetButtonType write SetButtonType;
    property onTouchDown: TCocoaEvent index 1 read GetXIBEvent write SetXIBEvent;
    property Font;
  end;

  { UILabel }
  TLineBreaks = (lbWordWrap,lbCharacterMode,lbClip,lbTruncateHead,lbTruncateTail,lbTruncateMiddle);
  TiOSFakeAlignment = (alLeft, alCenter, alRight);

  UILabel = class(UIView)
  private
    function GetLineBreaks: TLineBreaks;
    procedure SetLineBreaks(AValue: TLineBreaks);
    function GetTextAlignment: TiOSFakeAlignment;
    procedure SetTextAlignment(AValue: TiOSFakeAlignment);
  public
    constructor Create(AOwner: TComponent); override;
    function GetIBClassName: string; override;
    procedure paint(ACanvas: TCanvas); override;
  published
    property Lines: integer index bvLines read GetXIBInteger write SetXIBInteger;
    property TextAlignment: TiOSFakeAlignment read GetTextAlignment write SetTextAlignment;
    property TextColor: TColor index bvTextColor read GetXIBColor write SetXIBColor;
    property Font;
    property Enabled: boolean index bvEnabled read GetXIBBoolean write SetXIBBoolean;
    property Highlighted: boolean index bvHighlighted read GetXIBBoolean write SetXIBBoolean;
    property LineBreaks: TLineBreaks read GetLineBreaks write SetLineBreaks;
    property Caption;
  end;

  { UITextField }

  UITextField = class(UIView)
  private
    function GetTextAlignment: TiOSFakeAlignment;
    procedure SetTextAlignment(AValue: TiOSFakeAlignment);
  public
    constructor Create(AOwner: TComponent); override;
    function GetIBClassName: string; override;
    procedure paint(ACanvas: TCanvas); override;
  published
    property Placeholder: string index bvPlaceHolder read GetXIBString write SetXIBString;
    property TextAlignment: TiOSFakeAlignment read GetTextAlignment write SetTextAlignment;
    property TextColor: TColor index bvTextColor read GetXIBColor write SetXIBColor;
    property caption;
    property font;
  end;

  TiOSFakeSeparatorStyle = (ssNone,ssSingleLine,ssSingleLineEtched);

  { UITableView }

  UITableView = class(UIView)
  private
    function GetSeparatorStyle: TiOSFakeSeparatorStyle;
    procedure SetSeparatorStyle(AValue: TiOSFakeSeparatorStyle);
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitializeDefaults; override;
    procedure paint(ACanvas: TCanvas); override;
    function GetIBClassName: string; override;
  published
    property RowHeight: double index bvRowHeight read GetXIBFloat write SetXIBFloat;
    property SectionHeaderHeight: double index bvSectionHeaderHeigh read GetXIBFloat write SetXIBFloat;
    property SectionFooterHeight: double index bvSectionFooterHeight read GetXIBFloat write SetXIBFloat;
    property SeparatorStyle: TiOSFakeSeparatorStyle read GetSeparatorStyle write SetSeparatorStyle;
    property SeparatorColor: TColor index bvSeparatorColor read GetXIBColor write SetXIBColor;
    property BackgroundColor: TColor index bvBackgroundColor read GetXIBColor write SetXIBColor;
    property ClipSubviews: boolean index bvClipSubviews read GetXIBBoolean write SetXIBBoolean;
    property ShowSelectionOnTouch: boolean index bvShowSelectionOnTouch read GetXIBBoolean write SetXIBBoolean;
    property BounceVertically: boolean index bvBounceVertically read GetXIBBoolean write SetXIBBoolean;
  end;

  { UISearchBar }

  UISearchBar = class(UIView)
  private
    procedure SetPrompt(AIndex: TXIBProperties; AValue: string);
  protected
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
    procedure paint(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitializeDefaults; override;
    function GetIBClassName: string; override;
  published
    property Caption;
    property Placeholder: string index bvPlaceHolder read GetXIBString write SetXIBString;
    property Prompt: string index bvPrompt read GetXIBString write SetPrompt;
  end;

  { UIProgressView }

  TiOSFakeProgressViewStyle = (pvUIProgressViewStyleDefault,pvUIProgressViewStyleBar);

  UIProgressView = class(UIView)
  private
    function GetProgressViewStyle: TiOSFakeProgressViewStyle;
    procedure SetProgressViewStyle(AValue: TiOSFakeProgressViewStyle);
  public
    constructor Create(AOwner: TComponent); override;
    function GetIBClassName: string; override;
  published
    property progressViewStyle: TiOSFakeProgressViewStyle read GetProgressViewStyle write SetProgressViewStyle;
    property progress: double index bvProgress read GetXIBFloat write SetXIBFloat;
    property progressTintColor: TColor index bvProgressTintColor read GetXIBColor write SetXIBColor;
    property trackTintColor: TColor index bvTrackTintColor read GetXIBColor write SetXIBColor;
  end;

  { TNIBObjectWriter }

  TNIBObjectWriter = class(TAbstractObjectWriter)
  private
    FStream: TStream;
    FIsWritten: Boolean;
  public
    constructor create(AStream: TStream); virtual;
    procedure BeginCollection; override;
    procedure BeginComponent(Component: TComponent; Flags: TFilerFlags;
      ChildPos: Integer); override;
    procedure BeginList; override;
    procedure EndList; override;
    procedure BeginProperty(const PropName: String); override;
    procedure EndProperty; override;

    //Please don't use write, better use WriteBinary whenever possible
    procedure Write(const Buffer; Count: Longint); override;
    procedure WriteBinary(const Buffer; Count: LongInt); override;
    procedure WriteBoolean(Value: Boolean); override;

    procedure WriteCurrency(const Value: Currency); override;
    procedure WriteIdent(const Ident: string); override;
    procedure WriteInteger(Value: Int64); override;
    procedure WriteUInt64(Value: QWord); override;
    procedure WriteMethodName(const Name: String); override;
    procedure WriteSet(Value: LongInt; SetType: Pointer); override;
    procedure WriteString(const Value: String); override;
    procedure WriteWideString(const Value: WideString); override;
    procedure WriteUnicodeString(const Value: UnicodeString); override;
    procedure WriteVariant(const VarValue: Variant);override;

    procedure WriteFloat(const Value: Extended);  override;
    procedure WriteSingle(const Value: Single); override;
    procedure WriteDate(const Value: TDateTime); override;
  end;

  { TXIBReader }

  TXIBReader = class(TReader)
  protected
    procedure SetRoot(ARoot: TComponent); override;
    function CreateDriver(Stream: TStream; BufSize: Integer): TAbstractObjectReader; override;
  end;

  { TXIBObjectReader }
  TXIBObjectReaderState = (rsMainProp, rsMainPropIsSet, rsHasRootChilds, rsHasChilds, rsRootObject, rsRootObjectProp, rsObject, rsObjectProp);

  TXIBObjectReader = class(TAbstractObjectReader)
  private
    FStream: TStream;
    FXMLDocument: TXMLDocument;
    FXIBDocumentSet: boolean;
    FConnectionRecords: TDOMElement;
    FOrderedObjects: TDOMElement;
    FCurrentObject: TDOMElement;
    FCurrentOrderedObject: TDOMElement;
    FMainOrderedObject: TDOMElement;

    FRootObjects: TDOMElement;
    FReadChilds: boolean;
    ReadState: TXIBObjectReaderState;
    FFilesOwnerID: int64;
    FRefStack: array of int64;
  private
    FXIBUsesObjectsForArrays: boolean;
    //function GetConnectionRef(AnIBConnectionRecord: TDOMElement; key: string): int64;
    function GetObjectFromRef(ARef: int64): TDOMElement;
    function GetConnectionLabel(AnIBConnectionRecord: TDOMElement): string;
    function GetNextChild(ACurrentNode: TDOMNode; ParentRef: int64): TDOMElement;
    function FindOrderedObjectByRef(ARef: int64): TDOMElement;
    function FindFirstChild: TDOMElement;
    function FindNextChild(ARef: int64): TDOMElement;
    function FindNextOrderedObject: TDOMElement;
    function GetRefFromOrderedObject(AnOrderedObject: TDOMElement): int64;
  public
    constructor create(AStream: TStream); virtual;
    procedure BeginRootComponent; override;
    procedure BeginComponent(var Flags: TFilerFlags; var AChildPos: Integer;
      var CompClassName, CompName: String); override;

    function NextValue: TValueType; override;
    function ReadValue: TValueType; override;
    function BeginProperty: String; override;

    { All ReadXXX methods are called _after_ the value type has been read! }
    procedure ReadBinary(const DestData: TMemoryStream); override;
    function ReadCurrency: Currency; override;
    function ReadIdent(ValueType: TValueType): String; override;
    function ReadInt8: ShortInt; override;
    function ReadInt16: SmallInt; override;
    function ReadInt32: LongInt; override;
    function ReadInt64: Int64; override;
    function ReadSet(EnumType: Pointer): Integer; override;
    function ReadStr: String; override;
    function ReadString(StringType: TValueType): String; override;
    function ReadWideString: WideString; override;
    function ReadUnicodeString: UnicodeString; override;
    procedure SkipComponent(SkipComponentInfos: Boolean); override;
    procedure SkipValue; override;
    property XIBUsesObjectsForArrays: boolean read FXIBUsesObjectsForArrays;
  end;

procedure ObtainBaseObjectInfoFromXIB(AStream: TStream; out AXMLDocument: TXMLDocument;
                                      out ARootObjects, AConnectionRecords, AOrderedObjects, AMainOrderedObject: TDOMElement;
                                      out AFilesOwnerID: int64;
                                      out AXIBUsesObjectsForArrays: boolean);
function FindKeyNode(AParentNode: TDOMNode; NodeName, Key: string): TDOMElement;
function GetKeyNode(AParentNode: TDOMNode; NodeName, Key: string; AClass: string =''): TDOMElement;
function AddElement(ADomNode: TDOMElement; AName: string): TDOMElement;


implementation

uses
  fpTemplate,
  variants, varutils,
  typinfo,
{$ifndef OutsideIDE}
  MacroIntf,
{$endif OutsideIDE}
  PropEdits,
  base64;

function FindKeyNode(AParentNode: TDOMNode; NodeName, Key: string): TDOMElement;
var
  ANode: TDOMNode;
begin
  result := nil;
  ANode := AParentNode.FirstChild;
  while assigned(ANode) do
    begin
    if (ANode.NodeName=NodeName) and (ANode is TDOMElement) and (TDOMElement(ANode).AttribStrings['key']=key) then
      begin
      result := TDOMElement(ANode);
      break;
      end;
    ANode := ANode.NextSibling;
    end;
end;

{ UIxcodePlaceholder }

function UIxcodePlaceholder.GetXcodeClassName: string;
begin
  result := XIBObjectElement.AttribStrings['class'];
end;

procedure UIxcodePlaceholder.SetXcodeClassName(AValue: string);
begin
  if AValue<>GetXcodeClassName then
    begin
    if assigned(GetClass(AValue)) then
      raise exception.create('Invalid class name for placeholder: class does exist.');
    XIBObjectElement.AttribStrings['class'] := AValue;
    end;
end;

procedure UIxcodePlaceholder.paint(ACanvas: TCanvas);
begin
  with ACanvas do
    begin
    Brush.Color:=clInactiveBorder;
    Pen.Color:=clGradientInactiveCaption;
    Brush.Style:=bsBDiagonal;
    pen.Style:=psSolid;
    // Background
    Rectangle(0,0,self.Width,self.Height);
    end;
end;

function UIxcodePlaceholder.GetIBClassName: string;
begin
  Result:=GetXcodeClassName;
end;

{ TiOSSegmentedControlSegment }

function TiOSSegmentedControlSegment.GetTitle: string;
var
  ANode: TDOMNode;
begin
  result := '';
  ANode := FindKeyNode(FSegmentedControl.XIBObjectElement, 'array', 'IBSegmentTitles');
  if assigned(ANode) then
    begin
    ANode := ANode.ChildNodes.Item[ID];
    if assigned(ANode) then
      result := ANode.TextContent;
    end
end;

function TiOSSegmentedControlSegment.GetEnabled: boolean;
begin
  result := FSegmentedControl.GetXIBInteger(bvSelectedSegmentIndex) = ID;
end;

procedure TiOSSegmentedControlSegment.SetEnabled(AValue: boolean);
begin
  if AValue then
    FSegmentedControl.SetXIBInteger(bvSelectedSegmentIndex, ID);
end;

procedure TiOSSegmentedControlSegment.SetTitle(AValue: string);
var
  ANode: TDOMNode;
begin
  ANode := FindKeyNode(FSegmentedControl.XIBObjectElement, 'array', 'IBSegmentTitles');
  ANode := ANode.ChildNodes.Item[ID];
  ANode.TextContent := AValue;
  FSegmentedControl.Invalidate;
end;

{ UISegmentedControl }

function UISegmentedControl.GetSegments: TCollection;
var
  ANode: TDOMNode;
  ASegment: TiOSSegmentedControlSegment;
  i: integer;
  SegCnt: integer;
begin
  SegCnt:=SegmentCount;
  if FSegments.Count<>SegCnt then
    begin
    FCreatingSegments:=true;
    for i := 0 to SegCnt-1 do
      begin
      ASegment := (FSegments.Add as TiOSSegmentedControlSegment);
      ASegment.FSegmentedControl := self;
      end;
    FCreatingSegments:=false;;
    end;
  result := FSegments;
end;

procedure UISegmentedControl.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
var
  ANode: TDOMElement;
  ASegment: TiOSSegmentedControlSegment;
begin
  if FCreatingSegments then
    Exit;
  if ASender=FSegments then
    begin
    case Operation of
      ooAddItem :
        begin
        ASegment := TObject(Data) as TiOSSegmentedControlSegment;
        ASegment.FSegmentedControl := self;
        ANode := GetKeyNode(XIBObjectElement, 'array', 'IBSegmentTitles');
        AddIBString(ANode, '', 'Segment '+IntToStr((ASegment).ID));
        ANode := GetKeyNode(XIBObjectElement, 'array', 'IBSegmentWidths', 'NSMutableArray');
        ANode := AddElement(ANode,'real');
        ANode.AttribStrings['value']:='0.0';
        ANode := GetKeyNode(XIBObjectElement, 'array', 'IBSegmentEnabledStates', 'NSMutableArray');
        ANode := AddElement(ANode,'boolean');
        ANode.AttribStrings['value']:='YES';
        ANode := GetKeyNode(XIBObjectElement, 'array', 'IBSegmentContentOffsets', 'NSMutableArray');
        AddIBString(ANode, '', '{0, 0}');
        ANode := GetKeyNode(XIBObjectElement, 'array', 'IBSegmentImages', 'NSMutableArray');
        AddIBObject(ANode, '', 'NSNull');
        SetXIBInteger(bvSegments, FSegments.Count);
        end;
      ooDeleteItem:
        begin
        ASegment := TObject(Data) as TiOSSegmentedControlSegment;
        ANode := GetKeyNode(XIBObjectElement, 'array', 'IBSegmentTitles');
        ANode.RemoveChild(ANode.ChildNodes.Item[ASegment.ID]);
        ANode := GetKeyNode(XIBObjectElement, 'array', 'IBSegmentWidths', 'NSMutableArray');
        ANode.RemoveChild(ANode.ChildNodes.Item[ASegment.ID]);
        ANode := GetKeyNode(XIBObjectElement, 'array', 'IBSegmentEnabledStates', 'NSMutableArray');
        ANode.RemoveChild(ANode.ChildNodes.Item[ASegment.ID]);
        ANode := GetKeyNode(XIBObjectElement, 'array', 'IBSegmentContentOffsets', 'NSMutableArray');
        ANode.RemoveChild(ANode.ChildNodes.Item[ASegment.ID]);
        ANode := GetKeyNode(XIBObjectElement, 'array', 'IBSegmentImages', 'NSMutableArray');
        ANode.RemoveChild(ANode.ChildNodes.Item[ASegment.ID]);
        SetXIBInteger(bvSegments, FSegments.Count);
        end;
    end; {case}
    end;
end;

procedure UISegmentedControl.InitializeDefaults;
begin
  inherited;
  Height:=44;;
  FSegments.Add;
  FSegments.Add;
  (FSegments.Items[0] as TiOSSegmentedControlSegment).Enabled:=true;
end;

constructor UISegmentedControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSegments := TCollection.Create(TiOSSegmentedControlSegment);
  FSegments.FPOAttachObserver(self);
  FAcceptChildsAtDesignTime := false;
end;

destructor UISegmentedControl.Destroy;
begin
  FSegments.Free;
  inherited Destroy;
end;

procedure UISegmentedControl.paint(ACanvas: TCanvas);
var
  ARadius: integer;
  ts: TTextStyle;
  i: Integer;
begin
  inherited;
  with ACanvas do
    begin
    brush.Color:=clWhite;
    pen.Color:=clGray;
    pen.Style := psSolid;

    ARadius := min(self.Width,self.Height);
    ARadius := min(ARadius, 22);

    RoundRect(0,0,self.Width,self.Height,ARadius, ARadius);
    Font.Color:=clBlack;
    self.Font.ApplyToLCLFont(Font);
    ts := ACanvas.TextStyle;
    ts.Alignment:=taCenter;
    ts.Layout:=tlCenter;

    for i := 0 to FSegments.Count-1 do
      begin
      if i > 0 then
        Line((self.Width div FSegments.Count)*i,0,(self.Width div FSegments.Count)*i,self.Height);
      TextRect(Rect((self.Width div FSegments.Count)*i,0,(self.Width div FSegments.Count)*(i+1),Self.Height), 1, 1, (FSegments.Items[i] as TiOSSegmentedControlSegment).Title, ts);
      end;
    end;
end;

function UISegmentedControl.GetIBClassName: string;
begin
  Result:='IBUISegmentedControl';
end;


{ TXIBReader }

procedure TXIBReader.SetRoot(ARoot: TComponent);
begin
end;

function TXIBReader.CreateDriver(Stream: TStream; BufSize: Integer): TAbstractObjectReader;
begin
  Result := TXIBObjectReader.Create(Stream);
end;

{ TXIBObjectReader }

function GetConnectionRef(AnIBConnectionRecord: TDOMElement; key: string): int64;
var
  ATempNode: TDOMElement;
  AReferenceNode: TDOMElement;
begin
  ATempNode := FindKeyNode(AnIBConnectionRecord,'object','connection') as TDOMElement;
  AReferenceNode := FindKeyNode(ATempNode,'reference',key);
  result:=StrToInt(TDOMElement(AReferenceNode).AttribStrings['ref']);
end;

function TXIBObjectReader.GetObjectFromRef(ARef: int64): TDOMElement;

  function FindObj(AStartNode: TDOMNode): TDOMElement;
  var
    ATempNode: TDOMNode;
  begin
    result := nil;
    ATempNode := AStartNode.FirstChild;

    while assigned(ATempNode) do
      begin
      if ((ATempNode.NodeName='object') or (ATempNode.NodeName='array')) and (ATempNode is TDOMElement) then
        begin
        if (TDOMElement(ATempNode).AttribStrings['id']=inttostr(ARef)) then
          begin
          result := TDOMElement(ATempNode);
          break;
          end;
        result := FindObj(ATempNode);
        if assigned(result) then
          break;
        end;
      ATempNode := ATempNode.NextSibling;
      end;
  end;

begin
  result := FindObj(FRootObjects);
end;

function TXIBObjectReader.GetConnectionLabel(AnIBConnectionRecord: TDOMElement): string;
var
  ATempNode: TDOMElement;
  ALabelNode: TDOMElement;
begin
  ATempNode := FindKeyNode(AnIBConnectionRecord,'object','connection') as TDOMElement;
  ALabelNode := FindKeyNode(ATempNode,'string','label');
  result:=ALabelNode.TextContent;
end;

function TXIBObjectReader.GetNextChild(ACurrentNode: TDOMNode; ParentRef: int64): TDOMElement;
begin
  result := nil;
  if not assigned(ACurrentNode) then
    ACurrentNode := FConnectionRecords.FirstChild
  else
    ACurrentNode := ACurrentNode.NextSibling;
  while assigned(ACurrentNode) do
    begin
    if (ACurrentNode.NodeName='object') then
      begin
      if GetConnectionRef(ACurrentNode as TDOMElement,'source')=ParentRef then
        begin
        result := ACurrentNode as TDOMElement;
        break
        end;
      end;
    ACurrentNode := ACurrentNode.NextSibling;
    end;
end;

function FindOrderdObjectByRefA(AnOrderedObjects: TDOMElement; ARef: int64): TDOMElement;
var
  ANode: TDOMNode;
  ARefNode: TDOMElement;
begin
  result := nil;
  ANode := AnOrderedObjects.FirstChild;
  while assigned(ANode) do
    begin
    if (ANode.NodeName='object') and (ANode is TDOMElement) and (TDOMElement(ANode).AttribStrings['class']='IBObjectRecord') then
      begin
      ARefNode := FindKeyNode(ANode,'reference','object');
      if assigned(ARefNode) and (ARefNode.AttribStrings['ref']=inttostr(ARef)) then
        begin
        result := ANode as TDOMElement;
        break;
        end;
      end;
    ANode := ANode.NextSibling;
    end;
end;

function TXIBObjectReader.FindOrderedObjectByRef(ARef: int64): TDOMElement;
begin
  result := FindOrderdObjectByRefA(FOrderedObjects, ARef);
end;

function TXIBObjectReader.FindFirstChild: TDOMElement;
var
  ANode: TDOMNode;
begin
  result := nil;
  ANode := FindKeyNode(FCurrentOrderedObject,'array','children');
  if not assigned(ANode) then
    ANode := FindKeyNode(FCurrentOrderedObject,'object','children');
  if assigned(ANode) then
    begin
    ANode := ANode.FirstChild;
    while assigned(ANode) do
      begin
      if (ANode.NodeName='reference') then
        begin
        result := anode as TDOMElement;
        break;
        end;
      ANode := ANode.NextSibling;
      end;
    end;
end;

function TXIBObjectReader.FindNextChild(ARef: int64): TDOMElement;
var
  ANode: TDOMElement;
  Pass: boolean;
begin
  result := nil;
  pass := false;
  ANode := FindKeyNode(FCurrentOrderedObject,'object','children');
  if not assigned(ANode) then
    ANode := FindKeyNode(FCurrentOrderedObject,'array','children');
  if assigned(ANode) then
    begin
    ANode := ANode.FirstChild as TDOMElement;
    while assigned(ANode) do
      begin
      if (ANode.NodeName='reference') then
        begin
        if Pass then
          begin
          result := anode as TDOMElement;
          break;
          end
        else if ANode.AttribStrings['ref']=IntToStr(ARef) then
          pass := true;
        end;
      ANode := ANode.NextSibling as TDOMElement;
      end;
    end;
end;

function TXIBObjectReader.FindNextOrderedObject: TDOMElement;
var
  ANode: TDOMElement;
  ARefNode: TDOMElement;
begin
  result := nil;
  if not assigned(FCurrentOrderedObject) then
    begin
    ANode := FOrderedObjects.FirstChild as TDOMElement;
    assert(ANode.NodeName<>'object');
    end
  else
    ANode := FCurrentOrderedObject;

  ANode := ANode.NextSibling as TDOMElement;
  while assigned(ANode) do
    begin
    // This function is to search for root-objects which are encapsulated
    // within the main-object.
    // So ignore the main object itself, the file's owner and the main responder
    // (The IBProxyObjects with a negative ObjectID)
    // Also ignore the objects with a parent<>0, since those are not root-objects
    // and are handled elsewhere.
    if (ANode.NodeName='object') and (ANode<>FMainOrderedObject) then
      begin
      ARefNode := FindKeyNode(ANode,'reference','parent');
      if assigned(ARefNode) and (ARefNode.AttribStrings['ref']='0') then
        begin
        ARefNode := FindKeyNode(ANode,'reference','object');
        if ARefNode.AttribStrings['ref']<>inttostr(FFilesOwnerID) then
          begin
          ARefNode := FindKeyNode(ANode, 'int','objectID');
          if StrToInt(ARefNode.TextContent)>0 then
            begin
            result := ANode;
            break;
            end;
          end;
        end;
      end;
    ANode := ANode.NextSibling as TDOMElement;
    end;
end;

function TXIBObjectReader.GetRefFromOrderedObject(AnOrderedObject: TDOMElement): int64;
var
  ANode: TDOMElement;
begin
  ANode := FindKeyNode(AnOrderedObject, 'reference' ,'object');
  result := StrToInt(ANode.AttribStrings['ref']);
end;

constructor TXIBObjectReader.create(AStream: TStream);
begin
  FStream := AStream;
end;

procedure ObtainBaseObjectInfoFromXIB(AStream: TStream; out AXMLDocument: TXMLDocument;
                                      out ARootObjects, AConnectionRecords, AOrderedObjects, AMainOrderedObject: TDOMElement;
                                      out AFilesOwnerID: int64;
                                      out AXIBUsesObjectsForArrays: boolean);
var
  ArchiveNode, DataNode: TDOMNode;
  Objects: TDOMNode;
  ANode, ATempNode: TDOMNode;
  AReferenceNode: TDOMNode;
  MainObjectRef: int64;

begin
  ReadXMLFile(AXMLDocument, AStream);

  ArchiveNode := AXMLDocument.FirstChild;
  assert(ArchiveNode.NodeName='archive');
  DataNode := ArchiveNode.FindNode('data');
  ARootObjects := FindKeyNode(DataNode,'array','IBDocument.RootObjects');
  if not assigned(ARootObjects) then
    // Older XCode versions this:
    begin
    ARootObjects := FindKeyNode(DataNode,'object','IBDocument.RootObjects');
    AXIBUsesObjectsForArrays:=true;
    end
  else
    AXIBUsesObjectsForArrays:=False;

  ANode := ARootObjects.FirstChild;
  while assigned(ANode) do
    begin
    if (ANode.NodeName='object') and (ANode is TDOMElement) and (TDOMElement(ANode).AttribStrings['class']='IBProxyObject') then
      begin
      ATempNode := FindKeyNode(ANode, 'string', 'IBProxiedObjectIdentifier');
      if ATempNode.TextContent='IBFilesOwner' then
        begin
        AFilesOwnerID:=StrToInt(TDOMElement(ANode).AttribStrings['id']);
        break;
        end;
      end;
    ANode := ANode.NextSibling;
    end;

  Objects := FindKeyNode(DataNode,'object','IBDocument.Objects');
  AConnectionRecords := FindKeyNode(Objects,'array','connectionRecords') as TDOMElement;
  if not assigned(AConnectionRecords) then
    // Interface builder v3.1 and below uses 'object' instead of 'array'
    AConnectionRecords := FindKeyNode(Objects,'object','connectionRecords') as TDOMElement;
  ANode := FindKeyNode(Objects,'object','objectRecords');
  AOrderedObjects := FindKeyNode(ANode,'array','orderedObjects');
  if not assigned(AOrderedObjects) then
    // Interface builder v3.1 and below uses 'object' instead of 'array'
    AOrderedObjects := FindKeyNode(ANode,'object','orderedObjects');

  MainObjectRef:=0;
  ANode := AConnectionRecords.FirstChild;
  while assigned(ANode) do
    begin
    if (ANode.NodeName='object') and (ANode is TDOMElement) and (TDOMElement(ANode).AttribStrings['class']='IBConnectionRecord') then
      begin
      //FCurrentConnectionNode := TDOMElement(ANode);
      if GetConnectionRef(TDOMElement(ANode),'source')=AFilesOwnerID then
        begin
        MainObjectRef:=GetConnectionRef(TDOMElement(ANode),'destination');
        break;
        end;
      end;
    ANode := ANode.NextSibling;
    end;

  if MainObjectRef<>0 then
    AMainOrderedObject := FindOrderdObjectByRefA(AOrderedObjects, MainObjectRef)
  else
    begin
    ANode := AOrderedObjects.FirstChild;
    while assigned(ANode) do
      begin
      if (ANode.NodeName='object') and (ANode is TDOMElement) and (TDOMElement(ANode).AttribStrings['class']='IBObjectRecord') then
        begin
        ATempNode := FindKeyNode(ANode,'int','objectID');
        if StrToIntDef(ATempNode.TextContent,0)>0 then
          begin
          AMainOrderedObject := ANode as TDOMElement;
          break;
          end;
        end;
      ANode := ANode.NextSibling;
      end;
    end;

end;

procedure TXIBObjectReader.BeginRootComponent;
begin
  ObtainBaseObjectInfoFromXIB(FStream, FXMLDocument, FRootObjects, FConnectionRecords, FOrderedObjects, FMainOrderedObject, FFilesOwnerID, FXIBUsesObjectsForArrays);

  FCurrentOrderedObject := FMainOrderedObject;

  ReadState := rsMainProp;
end;

procedure TXIBObjectReader.BeginComponent(var Flags: TFilerFlags;
  var AChildPos: Integer; var CompClassName, CompName: String);
var
  ObjectNode: TDOMElement;
  Ref: Int64;
begin
  Ref := GetRefFromOrderedObject(FCurrentOrderedObject);

  ObjectNode := GetObjectFromRef(Ref);
  CompClassName:=ObjectNode.AttribStrings['class'];

  ObjectNode := FindKeyNode(FCurrentOrderedObject,'string','objectName');
  if assigned(ObjectNode) then
    CompName:=ObjectNode.TextContent;

  if copy(CompClassName,1,2)='IB' then
    begin
    CompClassName:=copy(CompClassName,3,250);
    if not assigned(IDEComponentPalette.FindComponent(CompClassName)) then
      CompClassName := 'UIxcodePlaceholder';
    end;
  FReadChilds := False;
  if ReadState=rsHasRootChilds then
    ReadState:=rsRootObjectProp
  else if ReadState=rsHasChilds then
    ReadState:=rsObjectProp;
end;

function TXIBObjectReader.NextValue: TValueType;
begin
  case ReadState of
    rsMainProp,
    rsRootObjectProp,
    rsObjectProp    : result := vaString;
    rsMainPropIsSet : result := vaNull;
    rsHasRootChilds,
    rsHasChilds     : if assigned(FCurrentOrderedObject) then result := vaIdent else Result := vaNull;
    rsObject,
    rsRootObject    : result := vaNull;
  end;
end;

function TXIBObjectReader.ReadValue: TValueType;
var
  ANode: TDOMElement;
  CurrentChildRef: TDOMElement;
  Ref: Int64;
begin
  case readstate of
    rsMainPropIsSet:
      begin
      FCurrentOrderedObject := nil;
      FCurrentOrderedObject := FindNextOrderedObject;
      if assigned(FCurrentOrderedObject) then
        begin
        SetLength(FRefStack,length(FRefStack)+1);
        FRefStack[high(FRefStack)]:=GetRefFromOrderedObject(FCurrentOrderedObject);
        end;
      result := vaNull;
      readstate := rsHasRootChilds;
      end;
    rsRootObject:
      begin
      // The properties of the root-object are read, now see if there are any
      // chilren.
      CurrentChildRef := FindFirstChild;
      if assigned(CurrentChildRef) then
        begin
        FCurrentOrderedObject := FindOrderedObjectByRef(StrToInt(CurrentChildRef.AttribStrings['ref']));
        SetLength(FRefStack,length(FRefStack)+1);
        FRefStack[high(FRefStack)]:=GetRefFromOrderedObject(FCurrentOrderedObject);
        end
      else
        FCurrentOrderedObject := nil;
      ReadState:=rsHasChilds;
      result := vaNull;
      end;
    rsObject:
      begin
      CurrentChildRef := FindFirstChild;
      if assigned(CurrentChildRef) then
        begin
        FCurrentOrderedObject := FindOrderedObjectByRef(StrToInt(CurrentChildRef.AttribStrings['ref']));
        SetLength(FRefStack,length(FRefStack)+1);
        FRefStack[high(FRefStack)]:=GetRefFromOrderedObject(FCurrentOrderedObject);
        end
      else
        FCurrentOrderedObject := nil;
      ReadState:=rsHasChilds;
      result := vaNull;
      end;
    rsHasChilds:
      begin
      // The current object ran out of childs, so go back to the prior object
      if length(FRefStack)>1 then
        begin
        ref := FRefStack[high(FRefStack)];
        setlength(FRefStack, high(FRefStack));
        FCurrentOrderedObject := FindOrderedObjectByRef(ref);
        ANode := FindKeyNode(FCurrentOrderedObject,'reference','parent');
        FCurrentOrderedObject := FindOrderedObjectByRef(StrToInt(ANode.AttribStrings['ref']));

        CurrentChildRef := FindNextChild(ref);
        if assigned(CurrentChildRef) then
          begin
          FCurrentOrderedObject := FindOrderedObjectByRef(StrToInt(CurrentChildRef.AttribStrings['ref']));
          SetLength(FRefStack,length(FRefStack)+1);
          FRefStack[high(FRefStack)]:=GetRefFromOrderedObject(FCurrentOrderedObject);
          end
        else
          FCurrentOrderedObject := nil;;
        end
      else
        begin
        ref := FRefStack[high(FRefStack)];
        setlength(FRefStack, high(FRefStack));
        FCurrentOrderedObject := FindOrderedObjectByRef(ref);
        FCurrentOrderedObject := FindNextOrderedObject;
        if assigned(FCurrentOrderedObject) then
          begin
          SetLength(FRefStack,length(FRefStack)+1);
          FRefStack[high(FRefStack)]:=GetRefFromOrderedObject(FCurrentOrderedObject);
          end;
        result := vaNull;
        readstate := rsHasRootChilds;
        end;
      result := vaNull;
      end
    else
      result := vaNull;
    end;
end;

function TXIBObjectReader.BeginProperty: String;
begin
  if assigned(FCurrentOrderedObject) then
    FCurrentObject := GetObjectFromRef(GetRefFromOrderedObject(FCurrentOrderedObject));
  case ReadState of
    rsMainProp       : readstate := rsMainPropIsSet;
    rsObjectProp     : ReadState := rsObject;
    rsRootObjectProp : ReadState := rsRootObject;
  end;
  result := '';
end;

procedure TXIBObjectReader.ReadBinary(const DestData: TMemoryStream);
begin

end;

function TXIBObjectReader.ReadCurrency: Currency;
begin

end;

function TXIBObjectReader.ReadIdent(ValueType: TValueType): String;
begin

end;

function TXIBObjectReader.ReadInt8: ShortInt;
begin

end;

function TXIBObjectReader.ReadInt16: SmallInt;
begin

end;

function TXIBObjectReader.ReadInt32: LongInt;
begin

end;

function TXIBObjectReader.ReadInt64: Int64;
begin

end;

function TXIBObjectReader.ReadSet(EnumType: Pointer): Integer;
begin

end;

function TXIBObjectReader.ReadStr: String;
begin

end;

function TXIBObjectReader.ReadString(StringType: TValueType): String;
begin

end;

function TXIBObjectReader.ReadWideString: WideString;
begin

end;

function TXIBObjectReader.ReadUnicodeString: UnicodeString;
begin

end;

procedure TXIBObjectReader.SkipComponent(SkipComponentInfos: Boolean);
begin

end;

procedure TXIBObjectReader.SkipValue;
begin

end;

{ TNIBObjectWriter }

constructor TNIBObjectWriter.create(AStream: TStream);
begin
  FStream := AStream;
end;

procedure TNIBObjectWriter.BeginCollection;
begin

end;

procedure TNIBObjectWriter.BeginComponent(Component: TComponent;
  Flags: TFilerFlags; ChildPos: Integer);
var
  FakeComp: tiOSFakeComponent;
  AnElement: TDOMElement;
begin
  if not FIsWritten then
    begin
    if (Component is NSObject) then
      begin
      FakeComp := tiOSFakeComponent(Component);
      AnElement := FakeComp.GetXIBFlattenedProperties;
      AnElement := GetKeyNode(AnElement,'string', inttostr(FakeComp.ObjectID) +'.CustomClassName');
      AnElement.TextContent:=FakeComp.ClassName;
      WriteXML(NSObject(Component).FNIBDocument, FStream);
      FIsWritten:=true;
      end;
    end;
end;

procedure TNIBObjectWriter.BeginList;
begin

end;

procedure TNIBObjectWriter.EndList;
begin

end;

procedure TNIBObjectWriter.BeginProperty(const PropName: String);
begin

end;

procedure TNIBObjectWriter.EndProperty;
begin

end;

procedure TNIBObjectWriter.Write(const Buffer; Count: Longint);
begin

end;

procedure TNIBObjectWriter.WriteBinary(const Buffer; Count: LongInt);
begin

end;

procedure TNIBObjectWriter.WriteBoolean(Value: Boolean);
begin

end;

procedure TNIBObjectWriter.WriteCurrency(const Value: Currency);
begin

end;

procedure TNIBObjectWriter.WriteIdent(const Ident: string);
begin

end;

procedure TNIBObjectWriter.WriteInteger(Value: Int64);
begin

end;

procedure TNIBObjectWriter.WriteUInt64(Value: QWord);
begin

end;

procedure TNIBObjectWriter.WriteMethodName(const Name: String);
begin

end;

procedure TNIBObjectWriter.WriteSet(Value: LongInt; SetType: Pointer);
begin

end;

procedure TNIBObjectWriter.WriteString(const Value: String);
begin

end;

procedure TNIBObjectWriter.WriteWideString(const Value: WideString);
begin

end;

procedure TNIBObjectWriter.WriteUnicodeString(const Value: UnicodeString);
begin

end;

procedure TNIBObjectWriter.WriteVariant(const VarValue: Variant);
begin

end;

procedure TNIBObjectWriter.WriteFloat(const Value: Extended);
begin

end;

procedure TNIBObjectWriter.WriteSingle(const Value: Single);
begin

end;

procedure TNIBObjectWriter.WriteDate(const Value: TDateTime);
begin

end;

function UIProgressView.GetProgressViewStyle: TiOSFakeProgressViewStyle;
begin
  result := TiOSFakeProgressViewStyle(GetXIBInteger(bvProgressViewStyle));
end;

procedure UIProgressView.SetProgressViewStyle(AValue: TiOSFakeProgressViewStyle);
begin
  SetXIBInteger(bvProgressViewStyle,ord(AValue));
end;

{ UIProgressView }
{
procedure UIProgressView.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBFloat(AnObjectDomElement,'IBUIProgress',FProgress);
  AddIBInt(AnObjectDomElement,'IBUIProgressViewStyle',ord(progressViewStyle),0);
  AddIBColor(AnObjectDomElement,'IBUIProgressTintColor',progressTintColor);
  AddIBColor(AnObjectDomElement,'IBUITrackTintColor',trackTintColor);
end;
}
constructor UIProgressView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime := false;
  progressTintColor := clDefault;
  trackTintColor := clDefault;
end;

function UIProgressView.GetIBClassName: string;
begin
  Result:='IBUIProgressView';
end;

{ UINavigationItem }

function UINavigationItem.GetWidth: integer;
begin
  if assigned(parent) then
    result := parent.Width
  else
    result := 320;
end;

procedure UINavigationItem.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  inherited SetBounds(Left, Top, Width, Height);
end;

constructor UINavigationItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height:=44;
end;

function UINavigationItem.GetIBClassName: string;
begin
  Result:='IBUINavigationItem';
end;

{ UIViewController }
{
procedure UIViewController.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  ASubElement: TDOMElement;
  i: Integer;
begin
  AddIBString(AnObjectDomElement,'targetRuntimeIdentifier','IBCocoaTouchFramework');

  for i := 0 to ChildCount-1 do
    begin
    ASubElement := Children[i].WriteToObject(AnObjectDomElement);
    if Children[i] is UIView then
      ASubElement.AttribStrings['key']:='IBUIView'
    else
      ASubElement.AttribStrings['key']:=Children[i].GetIBClassName;
    end;
end;
}
function UIViewController.GetHeight: integer;
begin
{  if FHeight<>0 then
    result := FHeight
  else if assigned(parent) then
    result := min(Parent.Height,480)
  else
    result := 480;}
  result := inherited;
end;

function UIViewController.GetWidth: integer;
begin
  result := inherited;
{  if FWidth <> 0 then
    result := FWidth
  else if assigned(parent) then
    result := min(Parent.Width,320)
  else
    result := 320;}
end;

procedure UIViewController.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  inherited;
{
  if (FWidth<>0) or (FHeight<>0) or ((width=320) and (height=480)) then
    inherited SetBounds(NewLeft, NewTop, Width, Height)
  else
    inherited SetBounds(Left, Top, Width, Height)}
end;

constructor UIViewController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=true;
end;

function UIViewController.GetIBClassName: string;
begin
  Result:='IBUIViewController';
end;

{ UINavigationBar }
{
procedure UINavigationBar.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  AnElement: TDOMElement;
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AnElement := AnObjectDomElement.FindNode('NSFrame') as TDOMElement;
  if assigned(AnElement) then
    AnElement.NodeValue:='{{0, -44}, {0, 44}}';
end;
}
procedure UINavigationBar.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  inherited SetBounds(0, 0, 0, 0);
end;

procedure UINavigationBar.paint(ACanvas: TCanvas);
begin
  // Do not paint;
end;

constructor UINavigationBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

function UINavigationBar.GetIBClassName: string;
begin
  Result:='IBUINavigationBar';
end;

{ UINavigationController }
{
procedure UINavigationController.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  NavigationBarElement: TDOMElement;
  ViewControllesArray: TDOMElement;
  ViewController: TDOMElement;
  NavigationItemElement: TDOMElement;
  i: Integer;
begin
  AddIBString(AnObjectDomElement,'targetRuntimeIdentifier','IBCocoaTouchFramework');

  ViewControllesArray := AddIBObject(AnObjectDomElement,'IBUIViewControllers','NSMutableArray');
  AddIBBoolean(ViewControllesArray,'EncodedWithXMLCoder',true);

  for i := 0 to ChildCount-1 do
    begin
    if Children[i] is UINavigationBar then
      begin
      NavigationBarElement := Children[i].WriteToObject(AnObjectDomElement);
      NavigationBarElement.AttribStrings['key']:=Children[i].GetIBClassName;
      end;
    if children[i] is UIViewController then
      begin
      ViewController := children[i].WriteToObject(ViewControllesArray);
      AddIBString(ViewController,'targetRuntimeIdentifier','IBCocoaTouchFramework');
      AddIBReference(ViewController,'IBUIParentViewController',self,true);
      end;
    end;

end;
}
procedure UINavigationController.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  inherited SetBounds(NewLeft, NewTop, Width, Height);
end;

constructor UINavigationController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
  //FWidth:=320;
  //FHeight:=480;
end;

procedure UINavigationController.InitializeDefaultChildren;
var
  AChild: tiOSFakeComponent;
  AViewController: TComponent;
  Designer: IMyWidgetDesigner;
begin
  designer := GetDesigner;
  if assigned(designer) then
    begin
    Designer.CreateComponent(self,UINavigationBar,'iOS_Views',0,0,0,0,true);
    AViewController := Designer.CreateComponent(self,UIViewController,'iOS_Views',0,0,0,0,true);
    Designer.CreateComponent(AViewController,UINavigationItem,'iOS_Views',0,0,0,0,true);
    end
  else
    begin
    AChild := UINavigationBar.Create(self);
    AChild.Parent := self;

    AViewController := UIViewController.Create(self);
    tiOSFakeComponent(AViewController).Parent := self;

    AChild := UINavigationItem.Create(self);
    AChild.Parent := tiOSFakeComponent(AViewController);
    end
end;

function UINavigationController.GetIBClassName: string;
begin
  Result:='IBUINavigationController';
end;

{ UIWindow }

function UIWindow.GetStatusBar: TiOSFakeStatusBarStyle;
var
  ANode: TDOMNode;
begin
  if Assigned(XIBObjectElement) then
    begin
    ANode := FindKeyNode(XIBObjectElement, 'object', 'IBUISimulatedStatusBarMetrics');
    if assigned(ANode) then
      begin
      ANode := FindKeyNode(ANode, 'int', 'IBUIStatusBarStyle');
      if assigned(ANode) then
        begin
        result := TiOSFakeStatusBarStyle(StrToInt(ANode.TextContent));
        end
      else
        result := sbsGrey;
      end
    else
      result := sbsNone;
    end
  else
    result := sbsNone;
end;

procedure UIWindow.SetStatusBar(AValue: TiOSFakeStatusBarStyle);
var
  ANode: TDOMNode;
begin
  if AValue = sbsNone then
    begin
    ANode := FindKeyNode(XIBObjectElement, 'object', 'IBUISimulatedStatusBarMetrics');
    if assigned(ANode) then
      ANode.ParentNode.RemoveChild(ANode);
    end
  else
    begin
    ANode := GetKeyNode(XIBObjectElement, 'object', 'IBUISimulatedStatusBarMetrics', 'IBUISimulatedStatusBarMetrics');
    if AValue<>sbsGrey then
      begin
      ANode := GetKeyNode(ANode, 'int', 'IBUIStatusBarStyle');
      anode.TextContent:=IntToStr(ord(AValue));
      end
    else
      begin
      ANode := FindKeyNode(ANode, 'int', 'IBUIStatusBarStyle');
      if assigned(ANode) then
        ANode.ParentNode.RemoveChild(ANode);
      end;
    end;
end;

function UIWindow.StoreSizeAsFrameSize: boolean;
begin
  Result:=true;
end;
{
procedure UIWindow.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBReference(AnObjectDomElement,'NSWindow',self);
  AddIBString(AnObjectDomElement,'targetRuntimeIdentifier','IBCocoaTouchFramework');
  AddIBBoolean(AnObjectDomElement,'IBUIResizesToFullScreen',True);
end;
}
procedure UIWindow.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  inherited SetBounds(NewLeft, NewTop, Width, Height);
end;

procedure UIWindow.InitializeDefaults;
begin
  inherited InitializeDefaults;
  Inherited SetBounds(left, top, 320, 480);
  BackgroundColor:=clWhite;
  StatusBar:=sbsGrey;
  ResizesToFullScreen:=true;
  ClearGraphincsContext := False;
end;

constructor UIWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLeft := 10;
  FTop := 10;
  BackgroundColor:=clWhite;
end;

procedure UIWindow.paint(ACanvas: TCanvas);
begin
  with ACanvas do
    begin
    Brush.Style:=bsSolid;
    Brush.Color:=BackgroundColor;
    // outer frame
    Pen.Style:=psSolid;
    Pen.Color:=clWhite;
    // This rectangle is drawn inside the window, it should be drawn around it.
    Rectangle(0,0,self.Width,self.Height);
    if StatusBar<>sbsNone then
      begin
      if StatusBar = sbsBlack then
        begin
        Brush.Color:=clBlack;
        Pen.Color:=clBlack;
        end
      else
        begin
        Brush.Color:=clGray;
        Pen.Color:=clGray;
        end;
      Rectangle(0,0,self.Width,20);
      Pen.Color:=clBlack;
      line(0,19,self.Width,19);
      end;
    end;
end;

function UIWindow.GetIBClassName: string;
begin
  Result:='IBUIWindow';
end;

{ NSObject }

function NSObject.GetFilesOwnerOutletName: string;
var
  AnElement: TDOMElement;
begin
  result := '';
  AnElement := GetXIBConnection(841351856, Ref, ctOutlet, false, -1);
  if assigned(AnElement) then
    begin
    AnElement := FindKeyNode(AnElement, 'string' ,'label');
    if assigned(AnElement) then
      result := AnElement.TextContent;
    end;
end;

function NSObject.IsNIBRoot: boolean;
begin
  result := Parent=Nil;
end;

function NSObject.GetFilesOwnerID: string;
begin
  if IsHiddenObject then
    Result:='841351856'
  else
    Result:='664661524';
end;

procedure NSObject.SetFilesOwnerOutletName(AValue: string);
var
  AnElement: TDOMElement;
begin
  if AValue=GetFilesOwnerOutletName then
    Exit;

  AnElement := GetXIBConnection(841351856, Ref, ctOutlet, True, -1);
  AnElement := GetKeyNode(AnElement, 'string' ,'label');
  AnElement.TextContent := AValue;
end;

function NSObject.GetNSObject: NSObject;
begin
  Result:=self;
end;

procedure NSObject.InternalInvalidateRect(ARect: TRect; Erase: boolean);
begin
  if (Parent=nil) and (Designer<>nil) then
    Designer.InvalidateRect(Self,ARect,Erase);
end;

procedure NSObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  if filer is TXIBReader then
    begin
    FNIBDocument.Free;
    FNIBDocument := TXIBObjectReader(TReader(Filer).Driver).FXMLDocument;
    FXIBUsesObjectsForArrays := TXIBObjectReader(TReader(Filer).Driver).XIBUsesObjectsForArrays;
    end;
end;

function NSObject.GetLeft: integer;
begin
  // There is no way to store this in the xib file
  result := FLeft;
end;

function NSObject.GetTop: integer;
begin
  // There is no way to store this in the xib file
  result := FTop;
end;

function NSObject.GetHeight: integer;
begin
  // There is no way to store this in the xib file
  result := FHeight;
end;

function NSObject.GetWidth: integer;
begin
  // There is no way to store this in the xib file
  result := FWidth;
end;

procedure NSObject.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  if (Left=NewLeft) and (Top=NewTop) and (Width=NewWidth) and (Height=NewHeight) then
    exit;
  Invalidate;
  FLeft:=NewLeft;
  FTop:=NewTop;
  FWidth:=NewWidth;
  FHeight:=NewHeight;
  Invalidate;
end;

constructor NSObject.Create(AOwner: TComponent);
var
  ss: TStringStream;
  s: string;
begin
  inherited Create(AOwner);
  FIsHiddenObject:=true;
  FWidth:=340;
  FHeight:=500;
  FTop := 100;
  FLeft := 100;

  s :='<archive type="com.apple.InterfaceBuilder3.CocoaTouch.XIB" version="8.00">' +
      ' <data>' +
      '		<int key="IBDocument.SystemTarget">1280</int>' +
      '		<string key="IBDocument.SystemVersion">11D50</string>' +
      '		<string key="IBDocument.InterfaceBuilderVersion">2182</string>' +
      '		<string key="IBDocument.AppKitVersion">1138.32</string>' +
      '		<string key="IBDocument.HIToolboxVersion">568.00</string>' +
      '		<object class="NSMutableDictionary" key="IBDocument.PluginVersions">' +
      '			<string key="NS.key.0">com.apple.InterfaceBuilder.IBCocoaTouchPlugin</string>' +
      '			<string key="NS.object.0">1181</string>' +
      '		</object>' +
      '		<array key="IBDocument.IntegratedClassDependencies">' +
      '			<string>IBUIWindow</string>' +
      '			<string>IBUICustomObject</string>' +
      '			<string>IBUIButton</string>' +
      '			<string>IBProxyObject</string>' +
      '		</array>' +
      '		<array key="IBDocument.PluginDependencies">' +
      '			<string>com.apple.InterfaceBuilder.IBCocoaTouchPlugin</string>' +
      '		</array>' +
      '		<object class="NSMutableDictionary" key="IBDocument.Metadata">' +
      '			<string key="NS.key.0">PluginDependencyRecalculationVersion</string>' +
      '			<integer value="1" key="NS.object.0"/>' +
      '		</object>' +
      '		<array class="NSMutableArray" key="IBDocument.RootObjects" id="1000">' +
      '                 <object class="IBProxyObject" id="841351856">' +
      '              	        <string key="IBProxiedObjectIdentifier">IBFilesOwner</string>' +
      '         	        <string key="targetRuntimeIdentifier">IBCocoaTouchFramework</string>' +
      '                 </object>' +
      '                 <object class="IBProxyObject" id="371349661">' +
      '         	        <string key="IBProxiedObjectIdentifier">IBFirstResponder</string>' +
      '      	                <string key="targetRuntimeIdentifier">IBCocoaTouchFramework</string>' +
      '                 </object>' +
      '		</array>' +
      '		<object class="IBObjectContainer" key="IBDocument.Objects">' +
      '			<array class="NSMutableArray" key="connectionRecords"/>' +
      '			<object class="IBMutableOrderedSet" key="objectRecords">' +
      '				<array key="orderedObjects">' +
      '					<object class="IBObjectRecord">' +
      '						<int key="objectID">0</int>' +
      '						<array key="object" id="0"/>' +
      '						<reference key="children" ref="1000"/>' +
      '						<nil key="parent"/>' +
      '					</object>' +
      '                                 <object class="IBObjectRecord">' +
      '      	                                <int key="objectID">-1</int>' +
      '      	                                <reference key="object" ref="841351856"/>' +
      '      	                                <reference key="parent" ref="0"/>' +
      '      	                                <string key="objectName">File''s Owner</string>' +
      '                                 </object>' +
      '                                 <object class="IBObjectRecord">' +
      '      	                                <int key="objectID">-2</int>' +
      '      	                                <reference key="object" ref="371349661"/>' +
      '      	                                <reference key="parent" ref="0"/>' +
      '                                 </object>' +
      '				</array>' +
      '			</object>' +
      '                 <dictionary class="NSMutableDictionary" key="flattenedProperties">' +
      '                         <string key="-1.IBPluginDependency">com.apple.InterfaceBuilder.IBCocoaTouchPlugin</string>' +
      '                         <string key="-2.IBPluginDependency">com.apple.InterfaceBuilder.IBCocoaTouchPlugin</string>' +
      '                 </dictionary>' +
      '			<dictionary class="NSMutableDictionary" key="unlocalizedProperties"/>' +
      '			<nil key="activeLocalization"/>' +
      '			<dictionary class="NSMutableDictionary" key="localizations"/>' +
      '			<nil key="sourceID"/>' +
      '			<int key="maxID">1</int>' +
      '		</object>' +
      '		<object class="IBClassDescriber" key="IBDocument.Classes">' +
      '			<object class="NSMutableArray" key="referencedPartialClassDescriptions">' +
      '				<bool key="EncodedWithXMLCoder">YES</bool>' +
      '				<object class="IBPartialClassDescription">' +
      '					<string key="className">AppDelegate</string>' +
      '					<string key="superclassName">NSObject</string>' +
      '					<object class="NSMutableDictionary" key="outlets">' +
      '						<string key="NS.key.0">window</string>' +
      '						<string key="NS.object.0">UIWindow</string>' +
      '					</object>' +
      '					<object class="NSMutableDictionary" key="toOneOutletInfosByName">' +
      '						<string key="NS.key.0">window</string>' +
      '						<object class="IBToOneOutletInfo" key="NS.object.0">' +
      '							<string key="name">window</string>' +
      '							<string key="candidateClassName">UIWindow</string>' +
      '						</object>' +
      '					</object>' +
      '					<object class="IBClassDescriptionSource" key="sourceIdentifier">' +
      '						<string key="majorKey">IBProjectSource</string>' +
      '						<string key="minorKey">./Classes/AppDelegate.h</string>' +
      '					</object>' +
      '				</object>' +
      '			</object>' +
      '		</object>' +
      '		<int key="IBDocument.localizationMode">0</int>' +
      '		<string key="IBDocument.TargetRuntimeIdentifier">IBCocoaTouchFramework</string>' +
      '		<object class="NSMutableDictionary" key="IBDocument.PluginDeclaredDevelopmentDependencies">' +
      '			<string key="NS.key.0">com.apple.InterfaceBuilder.CocoaTouchPlugin.InterfaceBuilder3</string>' +
      '			<real value="4300" key="NS.object.0"/>' +
      '		</object>' +
      '		<bool key="IBDocument.PluginDeclaredDependenciesTrackSystemTargetVersion">YES</bool>' +
      '		<int key="IBDocument.defaultPropertyAccessControl">3</int>' +
      '		<string key="IBCocoaTouchPluginVersion">1181</string>' +
      '	</data>' +
      '</archive>';
  ss := TStringStream.Create(s);
  try
    ReadXMLFile(FNIBDocument, ss);
  finally
    ss.Free;
  end;
  AddChildToDom(self);
end;

destructor NSObject.Destroy;
begin
  inherited Destroy;
  if assigned(Designer) then
    Designer.ClearMyForm;
  FNIBDocument.Free;
end;

function NSObject.GetDesigner: IMyWidgetDesigner;
begin
  Result:=Designer;
end;

procedure NSObject.InitializeDefaults;
begin
  inherited InitializeDefaults;
  FilesOwnerOutletName:='delegate';
end;

function NSObject.GetIBClassName: string;
begin
  Result:='IBUICustomObject';
end;

function tiOSFakeComponent.GetNextObjectID: integer;
var
  AMaxNode: TDOMNode;
  i: integer;
begin
  AMaxNode := GetKeyNode(GetXIBObjects,'int','maxID');
  i := StrToIntDef(AMaxNode.TextContent,1);
  inc(i);
  AMaxNode.TextContent:=IntToStr(i);
  result := i;
end;

function tiOSFakeComponent.GetXIBConnection(ASourceRef, ADestinationRef: int64;
  ConnectionType: TiOSXIBConnectionType; CreateIfNotExists: boolean; IBEventType: integer): TDOMElement;
var
  AnElement: TDOMElement;
  AnOutletElement: TDOMElement;
  SourceElement: TDOMElement;
  DestElement: TDOMElement;
  EventTypeElement: TDOMElement;
  AnIBEventType: integer;
begin
  result := nil;
  AnElement := GetXIBConnectionRecords.FirstChild as TDOMElement;
  while assigned(AnElement) do
    begin
    if (AnElement.NodeName='object') and (AnElement.AttribStrings['class']='IBConnectionRecord') then
      begin
      AnOutletElement := FindKeyNode(AnElement, 'object', 'connection');
      if assigned(AnOutletElement) then
        begin
        SourceElement := FindKeyNode(AnOutletElement, 'reference', 'source');
        DestElement := FindKeyNode(AnOutletElement, 'reference', 'destination');

        if assigned(SourceElement) and (SourceElement.AttribStrings['ref']=IntToStr(ASourceRef)) and
           ((assigned(DestElement) and (DestElement.AttribStrings['ref']=IntToStr(ADestinationRef))) or
            (ConnectionType=ctEvent)) then
          begin
          EventTypeElement := FindKeyNode(AnOutletElement,'int','IBEventType');
          if assigned(EventTypeElement) then
            AnIBEventType:=StrToIntDef(EventTypeElement.TextContent,-1)
          else
            AnIBEventType:=-1;

          if ((ConnectionType=ctOutlet) and (AnOutletElement.AttribStrings['class']='IBCocoaTouchOutletConnection')) or
             ((ConnectionType=ctEvent) and (AnOutletElement.AttribStrings['class']='IBCocoaTouchEventConnection') and (AnIBEventType=IBEventType)) then
            begin
            result := AnOutletElement;
            exit;
            end;
          end;
        end;
      end;
    AnElement := AnElement.NextSibling as TDOMElement;
    end;
  if CreateIfNotExists then
    begin
    AnElement := AddElement(GetXIBConnectionRecords, 'object');
    AnElement.AttribStrings['class']:='IBConnectionRecord';
    AnOutletElement := GetKeyNode(AnElement,'object','connection');
    if ConnectionType=ctOutlet then
      AnOutletElement.AttribStrings['class'] := 'IBCocoaTouchOutletConnection'
    else if ConnectionType=ctEvent then
      AnOutletElement.AttribStrings['class'] := 'IBCocoaTouchEventConnection';
    SourceElement := GetKeyNode(AnOutletElement,'reference','source');
    SourceElement.AttribStrings['ref'] := IntToStr(ASourceRef);
    DestElement := GetKeyNode(AnOutletElement,'reference','destination');
    DestElement.AttribStrings['ref'] := IntToStr(ADestinationRef);
    if ConnectionType=ctEvent then
      GetKeyNode(AnOutletElement,'int', 'IBEventType').TextContent := IntToStr(IBEventType);
    result := AnOutletElement;

    GetKeyNode(AnElement,'int','connectionID').TextContent:=IntToStr(GetNextObjectID);
    end;
end;

function tiOSFakeComponent.FindComponentByRef(ARef: int64): tiOSFakeComponent;
var
  i: integer;
begin
  if Ref=ARef then
    result := self
  else
    begin
    for i := 0 to ChildCount-1 do
      begin
      result := Children[i].FindComponentByRef(ARef);
      if assigned(result) then
        exit;
      end;
    result := nil;
    end;
end;

{ tiOSFakeComponent }

procedure tiOSFakeComponent.SetHeight(const AValue: integer);
begin
  SetBounds(Left,Top,Width,AValue);
end;

procedure tiOSFakeComponent.SetLeft(const AValue: integer);
begin
  SetBounds(AValue,Top,Width,Height);
end;

function tiOSFakeComponent.GetObjectID: integer;
var
  ANode: TDOMNode;
begin
  ANode := FindOrderdObjectByRef(Ref);
  ANode := FindKeyNode(ANode,'int','objectID');
  result := StrToInt(ANode.TextContent);
end;

function tiOSFakeComponent.GetPosition(APosition: TiOSXIBPos): integer;

var
  State: TiOSXIBPos;

  function ParseDOMElement(AnElement: TDOMNode): integer;
  var
    s,s1: string;
    p: pchar;
  begin
    s := AnElement.TextContent;
    s1 := '';
    p := @s[1];
    while p^<>#0 do
      begin
      if p^ in ['0'..'9'] then
        s1 := s1 + p^;
      if p^ =',' then
        begin
        if APosition =State then
          begin
          result := strtoint(s1);
          exit;
          end;
        s1 := '';
        inc(State);
        end;
      inc(p);
      end;
    if APosition=sHeight then
      result := StrToInt(s1)
    else
      raise Exception.create('Invalid NSFrame value');
  end;

var
  AFrameNode: TDOMNode;
begin
  result := 0;
  if assigned(XIBObjectElement) then
    begin
    AFrameNode := FindKeyNode(XIBObjectElement,'string','NSFrame');
    if assigned(AFrameNode) then
      begin
      state := sLeft;
      result := ParseDOMElement(AFrameNode);
      end
    else if APosition in [sWidth, sHeight] then
      begin
      AFrameNode := FindKeyNode(XIBObjectElement,'string','NSFrameSize');
      if assigned(AFrameNode) then
        begin
        state := sWidth;
        result := ParseDOMElement(AFrameNode);
        end;
      end;
    end;
end;

function tiOSFakeComponent.GetRef: integer;
begin
  if assigned(XIBObjectElement) then
     result := StrToIntDef(XIBObjectElement.AttribStrings['id'],0)
  else
    result := 0;
end;

function tiOSFakeComponent.GetXIBDocument: TXMLDocument;
begin
  result := GetNSObject.NIBDocument;
end;

function tiOSFakeComponent.GetXIBOrderedObjects: TDOMElement;
begin
  Result := FindKeyNode(GetXIBObjectRecords,'array','orderedObjects');
  if not assigned(result) then
    Result := FindKeyNode(GetXIBObjectRecords,'object','orderedObjects');
end;

function tiOSFakeComponent.GetXIBObjectRecords: TDOMElement;
begin
  Result := FindKeyNode(GetXIBObjects,'object','objectRecords');
end;

function tiOSFakeComponent.GetXIBObjects: TDOMElement;
var
  ArchiveNode: TDOMNode;
  DataNode: TDOMNode;
begin
  ArchiveNode := GetXIBDocument.FirstChild;
  assert(ArchiveNode.NodeName='archive');
  DataNode := ArchiveNode.FindNode('data');
  Result := FindKeyNode(DataNode,'object','IBDocument.Objects');
end;

function tiOSFakeComponent.GetXIBRootObjects: TDOMElement;
var
  ArchiveNode: TDOMNode;
  DataNode: TDOMNode;
begin
   ArchiveNode := GetXIBDocument.FirstChild;
  assert(ArchiveNode.NodeName='archive');
  DataNode := ArchiveNode.FindNode('data');
  Result := FindKeyNode(DataNode,'array','IBDocument.RootObjects');
  if not assigned(Result) then
    Result := FindKeyNode(DataNode,'object','IBDocument.RootObjects');
end;

function tiOSFakeComponent.GetXIBFlattenedProperties: TDOMElement;
var
  AnElement: TDOMElement;
begin
  AnElement := GetXIBObjects;
  result := GetKeyNode(AnElement, 'dictionary', 'flattenedProperties', 'NSMutableDictionary');
end;

function tiOSFakeComponent.GetXIBConnectionRecords: TDOMElement;
begin
  Result := GetKeyNode(GetXIBObjects, 'array', 'connectionRecords', 'NSMutableArray');
end;

function tiOSFakeComponent.FindOrderdObjectByRef(ARef: int64): TDOMElement;
var
  ANode: TDOMNode;
  ARefNode: TDOMElement;
begin
  result := nil;
  ANode := GetXIBOrderedObjects.FirstChild;
  while assigned(ANode) do
    begin
    if (ANode.NodeName='object') and (ANode is TDOMElement) and (TDOMElement(ANode).AttribStrings['class']='IBObjectRecord') then
      begin
      ARefNode := FindKeyNode(ANode,'reference','object');
      if assigned(ARefNode) and (ARefNode.AttribStrings['ref']=inttostr(ARef)) then
        begin
        result := ANode as TDOMElement;
        break;
        end;
      end;
    ANode := ANode.NextSibling;
    end;
end;

procedure tiOSFakeComponent.SetXIBObjectElement(const AValue: TDOMElement);
var
  AnElement: TDOMElement;
  ARef: integer;
begin
  FXIBObjectElement := AValue;
  if csLoading in ComponentState then
    Exit;
  if (Name<>'') then
    begin
    ARef := Ref;
    if ARef<>0 then
      begin
      AnElement := FindOrderdObjectByRef(ARef);
      AnElement := GetKeyNode(AnElement,'string','objectName');
      AnElement.TextContent:=Name;
      end;
    end;
end;

procedure tiOSFakeComponent.AddChild(const AValue: tiOSFakeComponent);
var
  AComp: UIView;
  AnElement: TDOMElement;
begin
  FChilds.Add(AValue);
  AddChildToDom(AValue);
  if (AValue is UIView) and not (csLoading in ComponentState) then
    begin
    AComp := UIView(AValue).ObtainSuperview;
    if AComp<>AValue then
      UIView(AValue).NSSuperview := AComp as UIView;

    AnElement := GetXIBConnection(GetNSObject.Ref, AValue.Ref, ctOutlet, true, -1);
    AnElement := GetKeyNode(AnElement,'string','label');
    AnElement.TextContent:=AValue.Name;
    end;

end;

procedure tiOSFakeComponent.AddChildToDom(const AValue: tiOSFakeComponent);
var
  ANode: TDOMElement;
  AOrderedObjects: TDOMElement;
  s: string;
  ARef: integer;
  IsRootObject: boolean;
begin
  if not (csLoading in ComponentState) {and assigned(FXIBObjectElement)} then
    begin
    AValue.SetXIBObjectElement(GetXIBDocument.CreateElement('object'));
    ARef := random(999999999);
    while assigned(AValue.FindOrderdObjectByRef(ARef)) do
      ARef := random(999999999);
    AValue.XIBObjectElement.AttribStrings['class'] := AValue.GetIBClassName;
    AValue.XIBObjectElement.AttribStrings['id'] := IntToStr(ARef);

    if GetNSObject.XIBUsesObjectsForArrays then
      s := 'object'
    else
      s := 'array';

    IsRootObject:=not assigned(parent);

    if IsRootObject then
      ANode := GetXIBRootObjects
    else
      ANode := GetKeyNode(XIBObjectElement,s,'NSSubviews','NSMutableArray');

    ANode.AppendChild(AValue.XIBObjectElement);

    AOrderedObjects := GetXIBOrderedObjects;
    ANode := AddElement(AOrderedObjects,'object');
    ANode.AttribStrings['class'] := 'IBObjectRecord';
    AddIBInt(ANode,'objectID',GetNextObjectID);
    if AValue.Name<>'' then
      AddIBString(ANode, 'objectName', AValue.Name);
    AddIBReference(ANode, 'object', IntToStr(ARef));
    if IsRootObject then
      AddIBReference(ANode, 'parent', '0')
    else
      AddIBReference(ANode, 'parent', Self, True);

    if not IsRootObject then
      begin
      ANode := FindOrderdObjectByRef(Ref);
      ANode := GetKeyNode(ANode, s,'children','NSMutableArray');
      ANode := AddElement(ANode,'reference');
      ANode.AttribStrings['ref']:=IntToStr(ARef);
      end;
    end;
end;

procedure tiOSFakeComponent.RemoveChildFromDom(const AValue: tiOSFakeComponent);
var
  AnElement: TDOMElement;
  AnOutletElement: TDOMElement;
  ARefElement: TDOMElement;
  ARemove: boolean;
  ARemoveElement: TDOMElement;
begin
  if assigned(XIBObjectElement) then
    begin
    // Remove possible connections
    AnElement := GetXIBConnectionRecords.FirstChild as TDOMElement;
    while assigned(AnElement) do
      begin
      ARemove:=false;
      if (AnElement.NodeName='object') and (AnElement.AttribStrings['class']='IBConnectionRecord') then
        begin
        AnOutletElement := FindKeyNode(AnElement, 'object', 'connection');
        if assigned(AnOutletElement) then
          begin
          ARefElement := FindKeyNode(AnOutletElement, 'reference', 'source');
          if assigned(ARefElement) and (ARefElement.AttribStrings['ref']=inttostr(AValue.Ref)) then
            ARemove:=true;
          ARefElement := FindKeyNode(AnOutletElement, 'reference', 'destination');
          if assigned(ARefElement) and (ARefElement.AttribStrings['ref']=inttostr(AValue.Ref)) then
            ARemove:=true;
          end;
        end;
      ARemoveElement := AnElement;
      AnElement := AnElement.NextSibling as TDOMElement;
      if ARemove then
        ARemoveElement.ParentNode.RemoveChild(ARemoveElement);
      end;

    // Remove ObjectRecord
    AnElement := FindOrderdObjectByRef(AValue.Ref);
    AnElement.ParentNode.RemoveChild(AnElement);

    // Remove from Parent's ObjectRecord, unless AValue is a RootObject
    if assigned(parent) then
      begin
      AnElement := FindOrderdObjectByRef(Ref);
      ARefElement := FindKeyNode(AnElement,'array','children');
      if not assigned(ARefElement) then
        ARefElement := FindKeyNode(AnElement,'object','children');
      ARefElement := ARefElement.FirstChild as TDOMElement;
      while assigned(ARefElement) do
        begin
        if ARefElement.AttribStrings['ref']=IntToStr(AValue.Ref) then
          begin
          ARefElement.ParentNode.RemoveChild(ARefElement);
          break;
          end;
        ARefElement := ARefElement.NextSibling as TDOMElement;
        end;
    end;

    // Remove object itself
    AValue.XIBObjectElement.ParentNode.RemoveChild(AValue.XIBObjectElement);
    AValue.FXIBObjectElement :=nil;
    end;
end;

procedure tiOSFakeComponent.RemoveChild(const AValue: tiOSFakeComponent);
begin
  FChilds.Remove(AValue);
  RemoveChildFromDom(AValue);
end;

function tiOSFakeComponent.GetHeight: integer;
begin
  result := GetPosition(sHeight);
end;

function tiOSFakeComponent.GetLeft: integer;

begin
  if StoreSizeAsFrameSize then
    result := FLeft
  else
    result := GetPosition(sLeft);
end;

function tiOSFakeComponent.GetTop: integer;
begin
  if StoreSizeAsFrameSize then
    result := FTop
  else
    result := GetPosition(sTop);
end;

function tiOSFakeComponent.GetWidth: integer;
begin
  result := GetPosition(sWidth);
end;

function tiOSFakeComponent.StringToBytes(const AString: string): string;
begin
  result:=EncodeStringBase64(AString);
  while Result[length(Result)]='=' do
    Result:=copy(Result,1,length(Result)-1);
end;

function tiOSFakeComponent.BytesToString(AString: string): string;
var
  i: integer;
begin
  if (length(AString) mod 4) <> 0 then
    for i := 0 to 3-(length(AString) mod 4) do
      AString := AString + '=';
  result:=DecodeStringBase64(AString);
end;

function tiOSFakeComponent.GetXIBInteger(index: TXIBProperties): integer;
begin
  result := StrToIntDef(GetXIBString(index,'int'),0);
end;

procedure tiOSFakeComponent.SetXIBInteger(index: TXIBProperties; AValue: integer);
begin
  SetXIBString(index, 'int', IntToStr(AValue));
end;

function tiOSFakeComponent.GetXIBInt64(index: TXIBProperties): int64;
begin
  result := StrToInt64(GetXIBString(index,'int'));
end;

procedure tiOSFakeComponent.SetXIBInt64(index: TXIBProperties; AValue: int64);
begin
  SetXIBString(index, 'int', IntToStr(AValue));
end;

function tiOSFakeComponent.GetXIBColor(index: TXIBProperties): TColor;
var
  AnElement: TDOMElement;
  l: longint;
  fr,fg,fb: single;
  b: byte;
  s,sf: string;
  p1, p2: byte;

begin
  result := clDefault;
  if Assigned(XIBObjectElement) then
    begin
    AnElement := FindKeyNode(XIBObjectElement, 'object', XIBPropertiesStrings[index].APropertyName);
    if assigned(AnElement) then
      begin
      AnElement := FindKeyNode(AnElement, 'bytes', 'NSRGB');
      if assigned(AnElement) then
        begin
        s := AnElement.TextContent;
        s := BytesToString(s);

        p1 := pos(' ',s);
        sf := copy(s,1,p1-1);
        fr := StrToFloat(sf) * $ff;

        p2 := PosEx(' ',s,p1+1);
        sf := copy(s,p1+1,p2-p1-1);
        fg := StrToFloat(sf) * $ff;

        sf := copy(s,p2+1,length(s)-p2);
        fb := StrToFloat(sf) * $ff;

        result := RGBToColor(round(fr),round(fg),round(fb));
        end;
      end
    end
end;

procedure tiOSFakeComponent.SetXIBColor(index: TXIBProperties; AValue: TColor);
var
  l: longint;
  fr,fg,fb: single;
  b: byte;
  s: string;
  AnElement: TDOMElement;
begin
  if GetXIBColor(index)=AValue then
    Exit;

  if Assigned(XIBObjectElement) then
    begin
    if (AValue = clDefault) then
      begin
      // Value is set to default, remove node
      AnElement := FindKeyNode(XIBObjectElement, 'object', XIBPropertiesStrings[index].APropertyName);
      if assigned(AnElement) then
        AnElement.ParentNode.RemoveChild(AnElement);
      end
    else
      begin
      l:=ColorToRGB(AValue);
      b := l and ($ff0000) shr 16;
      fb := b / $ff;
      b := l and ($00ff00) shr 8;
      fg := b / $ff;
      b := l and ($0000ff);
      fr := b / $ff;
      s := FloatToStr(fr)+' '+FloatToStr(fg)+' '+FloatToStr(fb);

      AnElement := FindKeyNode(XIBObjectElement, 'object', XIBPropertiesStrings[index].APropertyName);
      if assigned(AnElement) then
        begin
        AnElement := FindKeyNode(AnElement,'bytes','NSRGB');
        AnElement.TextContent:=StringToBytes(s);
        end
      else
        begin
        AnElement := GetKeyNode(XIBObjectElement, 'object', XIBPropertiesStrings[index].APropertyName);
        AnElement.AttribStrings['class'] := 'NSColor';
        AddIBInt(AnElement,'NSColorSpace',1);
        AddIBBytes(AnElement,'NSRGB',s);
        end;
      end;
    Invalidate;
    end;
end;

function tiOSFakeComponent.GetXIBEvent(index: integer): TCocoaEvent;
var
  ConnectionElement: TDOMElement;
  AnComponent: tiOSFakeComponent;
  AnElement: TDOMElement;
  s: shortstring;
  i: integer;
begin
  ConnectionElement:=GetXIBConnection(Ref, 0, ctEvent, false, index);
  if assigned(ConnectionElement) then
    begin
    AnElement:=FindKeyNode(ConnectionElement,'reference','destination');
    AnComponent:=GetNSObject.FindComponentByRef(StrToInt(AnElement.AttribStrings['ref']));
    AnElement:=FindKeyNode(ConnectionElement,'string','label');
    s := AnElement.TextContent;
    i := pos(':',s);
    if i > 0 then
      s := copy(s,1,i-1);

    i := AnComponent.FStoredEvents.IndexOf(s);
    if i > -1 then
      begin
      TMethod(Result).Code := nil;
      TMethod(Result).Data := AnComponent.FStoredEvents.Objects[i];
      end
    else
      begin
      TMethod(Result).Data := AnComponent;
      TMethod(Result).Code := AnComponent.MethodAddress(s);
      end;
    end
  else
    result := nil;
end;

procedure tiOSFakeComponent.SetXIBEvent(Index: integer; AValue: TCocoaEvent);
var
  AnElement: TDOMElement;
  AnComponent: TObject;
  AMethodName: shortstring;
  ARef: int64;
  EventOwner: tiOSFakeComponent;
  s: string;
begin
  // Lazarus's fake-events can not be compared to each other. So Lazarus always
  // sets an event to a bogus value first. Checking if the new value is different
  // from the old value is pointless in that case.
  if assigned(TMethod(AValue).Data) then
    begin
    // Check if the event is a Lazarus bogus-event. If this is the case, just
    // ignore.
    if TMethod(AValue).Code=pointer(1) then
      Exit;

    AnComponent := Tobject(TMethod(AValue).Data);
    AMethodName := GlobalDesignHook.GetMethodName(TMethod(AValue), nil);

    if (AnComponent is tiOSFakeComponent) then
      EventOwner := tiOSFakeComponent(AnComponent)
    else
      EventOwner := GetNSObject;

    ARef := EventOwner.Ref;
    EventOwner.FStoredEvents.AddObject(AMethodName,TObject(TMethod(AValue).Data));

    AnElement:=GetXIBConnection(ref, ARef, ctEvent, True, Index);
    AnElement:=GetKeyNode(AnElement, 'string', 'label');
    AnElement.TextContent:=AMethodName + ':';
    end
  else
    begin
    AnElement:=GetXIBConnection(Ref, 0, ctEvent, False, Index);
    if assigned(AnElement) then
      AnElement.ParentNode.ParentNode.RemoveChild(AnElement.ParentNode);
    end;

end;

function tiOSFakeComponent.GetXIBFont(index: TXIBProperties): TiOSFakeFontDescription;
begin
  FFont.FXIBObjectElement := XIBObjectElement;
  result := FFont;
end;

function tiOSFakeComponent.GetNSObject: NSObject;
begin
  if assigned(Parent) then
    result := Parent.GetNSObject
  else
    result := nil;
end;

procedure tiOSFakeComponent.InitializeDefaults;
begin
  InitializeDefaultChildren;
end;

procedure tiOSFakeComponent.DefineProperties(Filer: TFiler);

var
  AnElement: TDOMElement;
  AnComponent: TObject;
  AMethodName: shortstring;
  ARef: int64;
  s: string;
  Reader: TXIBReader;
  Handled: boolean;
  ConnectionElement: TDOMElement;
  SourceElement: TDOMElement;
  DestElement: TDOMElement;
  LabelElement: TDOMElement;
  EventTypeElement: TDOMElement;
  i: integer;
  IBEventType: integer;

begin
  inherited DefineProperties(Filer);
  if filer is TXIBReader then
    begin
    SetXIBObjectElement(TXIBObjectReader(TReader(Filer).Driver).FCurrentObject);

    // Explicitely set the value for all events. This because Lazarus uses
    // some sort of artificial events, which are created by setting them
    // during the component's read.
    AnElement := GetXIBConnectionRecords.FirstChild as TDOMElement;
    while assigned(AnElement) do
      begin
      ConnectionElement := FindKeyNode(AnElement, 'object', 'connection');
      if ConnectionElement.AttribStrings['class'] = 'IBCocoaTouchEventConnection' then
        begin
        SourceElement := FindKeyNode(ConnectionElement, 'reference', 'source');
        if SourceElement.AttribStrings['ref']=IntToStr(Ref) then
          begin

          DestElement:=FindKeyNode(ConnectionElement,'reference','destination');
          EventTypeElement:=FindKeyNode(ConnectionElement, 'int', 'IBEventType');
          IBEventType:=StrToIntDef(EventTypeElement.TextContent,-1);
          AnComponent:=GetNSObject.FindComponentByRef(StrToInt(DestElement.AttribStrings['ref']));
          LabelElement:=FindKeyNode(ConnectionElement,'string','label');
          s := AnElement.TextContent;
          i := pos(':',s);
          if i > 0 then
            s := copy(s,1,i-1);

          Reader  := TXIBReader(Filer);
          if IBEventType in [low(EventNames)..high(EventNames)] then
            Reader.OnSetMethodProperty(Reader, Self, GetPropInfo(self,EventNames[IBEventType]),s,Handled);
          end

        end;

      AnElement := AnElement.NextSibling as TDOMElement;
      end;
    end;
end;

function GetKeyNode(AParentNode: TDOMNode; NodeName, Key: string; AClass: string): TDOMElement;
begin
  result := FindKeyNode(AParentNode, NodeName, Key);
  if not assigned(result) then
    begin
    result := AddElement(AParentNode as TDOMElement,NodeName);
    result.AttribStrings['key']:=Key;
    if AClass<>'' then
      result.AttribStrings['class']:=AClass;
    end;
end;

function AddElement(ADomNode: TDOMElement; AName: string): TDOMElement;
begin
  result := ADomNode.OwnerDocument.CreateElement(AName);
  ADomNode.AppendChild(result);
end;

procedure tiOSFakeComponent.SetParentComponent(Value: TComponent);
begin
  if Value is tiOSFakeComponent then
    Parent:=UIView(Value);
end;

procedure tiOSFakeComponent.SetParent(const AValue: tiOSFakeComponent);
begin
  if FParent=AValue then exit;
  if FParent<>nil then begin
    Invalidate;
    FParent.RemoveChild(self);
  end;
  FParent:=AValue;
  if FParent<>nil then begin
    FParent.AddChild(Self);
  end;
  Invalidate;
end;

function tiOSFakeComponent.HasParent: Boolean;
begin
  Result:=Parent<>nil;
end;

function tiOSFakeComponent.GetParentComponent: TComponent;
begin
  Result:=Parent;
end;

procedure tiOSFakeComponent.SetTop(const AValue: integer);
begin
  SetBounds(Left,AValue,Width,Height);
end;

procedure tiOSFakeComponent.SetWidth(const AValue: integer);
begin
  SetBounds(Left,Top,AValue,Height);
end;

function tiOSFakeComponent.GetChilds(Index: integer): tiOSFakeComponent;
begin
  Result:=tiOSFakeComponent(FChilds[Index]);
end;

procedure tiOSFakeComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  for i:=0 to ChildCount-1 do
    if Children[i].Owner=Root then
      Proc(Children[i]);
end;

procedure tiOSFakeComponent.InternalInvalidateRect(ARect: TRect; Erase: boolean);
begin
  //
end;

function tiOSFakeComponent.StoreSizeAsFrameSize: boolean;
begin
  result := False;
end;

procedure tiOSFakeComponent.SetName(const NewName: TComponentName);
var
  AnElement: TDOMElement;
begin
  inherited SetName(NewName);
  if not (csLoading in ComponentState) and assigned(XIBObjectElement) then
    begin
    AnElement := FindOrderdObjectByRef(Ref);
    AnElement := GetKeyNode(AnElement, 'string','objectName');
    AnElement.TextContent := NewName;

    AnElement := GetXIBConnection(GetNSObject.Ref, Ref, ctOutlet, false, -1);
    if assigned(AnElement) then
      begin
      AnElement := GetKeyNode(AnElement, 'string', 'label');
      AnElement.TextContent:=NewName;
      end;
    end;
end;

procedure tiOSFakeComponent.AddConnectionRecord(AnObjectDomElement: TDOMElement; AConnectionType, ALabel, AEventType: string);
var
  IBConnectionRecordElement : TDOMElement;
  IBConnectionElement : TDOMElement;
begin
  if name='' then Exit;
  IBConnectionRecordElement := AddIBObject(AnObjectDomElement,'','IBConnectionRecord');

  IBConnectionElement := AddIBObject(IBConnectionRecordElement,'connection',AConnectionType);

  AddIBString(IBConnectionElement,'label',ALabel);
  if AEventType<>'' then
    begin
    AddIBReference(IBConnectionElement,'source',self,True);
    AddIBReference(IBConnectionElement,'destination','664661524');
    end
  else
    begin
    AddIBReference(IBConnectionElement,'source','664661524');
    AddIBReference(IBConnectionElement,'destination',self,True);
    end;
  //AddIBInt(IBConnectionRecordElement,'connectionID',GConnectionID);
  //inc(GConnectionID);
  if AEventType<>'' then
    AddIBInt(IBConnectionElement,'IBEventType',StrToInt64Def(AEventType,1));
end;

function tiOSFakeComponent.GetIBClassName: string;
begin
  result := '';
end;

function tiOSFakeComponent.GetDesigner: IMyWidgetDesigner;
begin
  if assigned(parent) then
    result := parent.GetDesigner
  else
    result := nil;
end;

constructor tiOSFakeComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChilds:=TFPList.Create;
  FFont := TiOSFakeFontDescription.Create;
  FStoredEvents := TStringList.Create;
  FStoredEvents.Sorted:=true;
  FStoredEvents.Duplicates:=dupIgnore;
end;

destructor tiOSFakeComponent.Destroy;
begin
  while ChildCount>0 do Children[ChildCount-1].Free;
  Parent:=nil;
  FreeAndNil(FChilds);
  FreeAndNil(FStoredEvents);
  FFont.Free;
  inherited Destroy;
end;

procedure tiOSFakeComponent.InitializeDefaultChildren;
begin
  // Do nothing
end;

procedure tiOSFakeComponent.paint(ACanvas: TCanvas);
begin
  with ACanvas do
    begin
    Brush.Style:=bsSolid;
    Brush.Color:=clLtGray;
    // outer frame
    Pen.Color:=clRed;
    Rectangle(0,0,self.Width,self.Height);
    end;
end;

procedure tiOSFakeComponent.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
var
  AFrameNode: TDOMNode;
  s: string;
begin
  if (Left=NewLeft) and (Top=NewTop) and (Width=NewWidth) and (Height=NewHeight) then
    exit;
  Invalidate;

  if assigned(XIBObjectElement) then
    begin
    if StoreSizeAsFrameSize then
      begin
      AFrameNode := GetKeyNode(XIBObjectElement,'string','NSFrameSize');
      s := Format('{%d, %d}',[NewWidth, NewHeight]);
      FLeft:=NewLeft;
      FTop:=NewTop;
      end
    else
      begin
      AFrameNode := GetKeyNode(XIBObjectElement,'string','NSFrame');
      s := Format('{{%d, %d}, {%D, %d}}',[NewLeft, NewTop, NewWidth, NewHeight]);
      end;
    AFrameNode.TextContent:=s;
    end;
  Invalidate;
end;

procedure tiOSFakeComponent.InvalidateRect(ARect: TRect; Erase: boolean);
begin
  //writeln('TMyWidget.InvalidateRect ',Name,' ',ARect.Left,',',ARect.Top);
  ARect.Left:=Max(0,ARect.Left);
  ARect.Top:=Max(0,ARect.Top);
  ARect.Right:=Min(Width,ARect.Right);
  ARect.Bottom:=Max(Height,ARect.Bottom);
  if Parent<>nil then begin
    Parent.InvalidateRect(ARect,Erase);
  end else begin
    InternalInvalidateRect(ARect,Erase);
  end;
end;

procedure tiOSFakeComponent.Invalidate;
begin
  InvalidateRect(Rect(0,0,Width,Height),false);
end;

function tiOSFakeComponent.ChildCount: integer;
begin
  Result:=FChilds.Count;
end;

function tiOSFakeComponent.AddElement(ADomNode: TDOMElement; AName: string
  ): TDOMElement;
begin
  result := ADomNode.OwnerDocument.CreateElement(AName);
  ADomNode.AppendChild(result);
end;

function tiOSFakeComponent.AddIBInt(ADomNode: TDOMElement; APropName: string;
  APropValue: integer; ADefaultValue: integer): TDOMElement;
begin
  if APropValue<>ADefaultValue then
    begin
    result := AddElement(ADomNode,'int');
    result.AttribStrings['key']:=APropName;
    result.TextContent:=inttostr(APropValue);
    end;
end;

function tiOSFakeComponent.AddIBDouble(ADomNode: TDOMElement; APropName: string; APropValue: double): TDOMElement;
begin
  result := AddElement(ADomNode,'double');
  result.AttribStrings['key']:=APropName;
  result.TextContent:=floattostr(APropValue);
end;

function tiOSFakeComponent.AddIBFloat(ADomNode: TDOMElement; APropName: string;
  APropValue: double; ADefaultValue: double): TDOMElement;
begin
  if APropValue<>ADefaultValue then
    begin
    result := AddElement(ADomNode,'double');
    result.AttribStrings['key']:=APropName;
    result.TextContent:=floattostr(APropValue);
    end;
end;

function tiOSFakeComponent.AddIBString(ADomNode: TDOMElement; APropName: string; APropValue: string): TDOMElement;
begin
  result := AddElement(ADomNode,'string');
  if APropName<>'' then
    result.AttribStrings['key']:=APropName;
  result.TextContent:=APropValue;
end;

function tiOSFakeComponent.AddIBReference(ADomNode: TDOMElement;APropName: string; APropValue: tiOSFakeComponent; ForceValue: boolean): TDOMElement;
begin
  if assigned(APropValue) then
    begin
    result := AddElement(ADomNode,'reference');
    if APropName<>'' then
      result.AttribStrings['key']:=APropName;
    if ForceValue or (APropValue<>self) then
      result.AttribStrings['ref']:=inttostr(APropValue.Ref);
    end;
end;

function tiOSFakeComponent.AddIBReference(ADomNode: TDOMElement; APropName: string; APropValue: string): TDOMElement;
begin
  result := AddElement(ADomNode,'reference');
  result.AttribStrings['key']:=APropName;
  result.AttribStrings['ref']:=APropValue;
end;

function tiOSFakeComponent.AddIBBoolean(ADomNode: TDOMElement; APropName: string; APropValue: Boolean): TDOMElement;
begin
  result := AddElement(ADomNode,'bool');
  if APropName<>'' then
    result.AttribStrings['key']:=APropName;
  if APropValue then
    result.TextContent:='YES'
  else
    result.TextContent:='NO';
end;

function tiOSFakeComponent.AddIBBoolean(ADomNode: TDOMElement; APropName: string; APropValue: Boolean; ADefaultValue: boolean): TDOMElement;
begin
  if APropValue<>ADefaultValue then
    AddIBBoolean(ADomNode,APropName,APropValue);
end;

function tiOSFakeComponent.AddIBBytes(ADomNode: TDOMElement; APropName: string; APropValue: string): TDOMElement;
begin
  if APropValue<>'' then
    begin
    APropValue:=EncodeStringBase64(APropValue);
    while APropValue[length(APropValue)]='=' do
      APropValue:=copy(APropValue,1,length(APropValue)-1);
    result := AddElement(ADomNode,'bytes');
    result.AttribStrings['key']:=APropName;
    result.TextContent:=APropValue;

    end;
end;

function tiOSFakeComponent.AddIBObject(ADomNode: TDOMElement; APropName: string; AClass: string): TDOMElement;
begin
  result := AddElement(ADomNode,'object');
  result.AttribStrings['class']:=AClass;
  if APropName<>'' then
    result.AttribStrings['key']:=APropName;
end;

function tiOSFakeComponent.AddIBObject(ADomDocument: TXMLDocument; APropName: string; AClass: string): TDOMElement;
begin
  result := ADomDocument.CreateElement('object');
  ADomDocument.AppendChild(result);
  result.AttribStrings['class']:=AClass;
  if APropName<>'' then
    result.AttribStrings['key']:=APropName;
end;

function tiOSFakeComponent.AddIBColor(ADomNode: TDOMElement; APropName: string; AColor: TColor): TDOMElement;
var
  l: longint;
  fr,fg,fb: single;
  b: byte;
  s: string;
  AnElement: TDOMElement;
begin
  if AColor=clDefault then
    Exit;

  l:=ColorToRGB(AColor);
  b := l and ($ff0000) shr 16;
  fb := b / $ff;
  b := l and ($00ff00) shr 8;
  fg := b / $ff;
  b := l and ($0000ff);
  fr := b / $ff;
  s := FloatToStr(fr)+' '+FloatToStr(fg)+' '+FloatToStr(fb);

  AnElement := AddIBObject(ADomNode,APropName,'NSColor');
  AddIBInt(AnElement,'NSColorSpace',1);
  AddIBBytes(AnElement,'NSRGB',s);
end;

function tiOSFakeComponent.AddIBFontDescription(ADomNode: TDOMElement; APropName: string; AFontDescription: TiOSFakeFontDescription): TDOMElement;
var
  l: longint;
  fr,fg,fb: single;
  b: byte;
  s: string;
  AnElement: TDOMElement;
begin
  if AFontDescription.FontType<>ftNotSet then
    begin
    AnElement := AddIBObject(ADomNode,APropName,'IBUIFontDescription');
    AddIBInt(AnElement,'type',ord(AFontDescription.FontType));
    AddIBDouble(AnElement,'pointSize',AFontDescription.pointSize);
    end;
end;

procedure UISearchBar.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  if Prompt<>'' then
    NewHeight := 75
  else
    NewHeight := 44;
  inherited SetBounds(NewLeft, NewTop, NewWidth, NewHeight);
end;

procedure UISearchBar.paint(ACanvas: TCanvas);
var
  ATop: integer;
  ts: TTextStyle;
begin
  with ACanvas do
    begin
    brush.Style := bsSolid;
    pen.Color := $dfd5cd;
    pen.style := psSolid;
    if Prompt='' then
      begin
      ATop:=0;
      Line(0,0,self.Width,0);
      GradientFill(rect(-1,ATop,self.Width+2, self.Height),$cdbcb0,$a2846d,gdVertical);
      end
    else
      begin
      brush.Color:=$cdbcb0;
      pen.Color:=$cdbcb0;
      ATop:=30;
      Rectangle(0,0,self.Width-1,ATop);
      Font.Size:=14;
      Font.Italic:=false;
      font.Bold:=false;
      ts := TextStyle;
      TS.Alignment:=taCenter;
      TS.Layout:=tlCenter;
      TextRect(rect(2,2,self.Width-3,ATop-2),2,2,Prompt,ts);
      brush.Color:=$a2846d;
      Rectangle(0,ATop,self.Width, self.Height);
      end;

    Brush.Color:=clWhite;
    pen.Style := psSolid;
    pen.Color := $2d3642;
    RoundRect(6,ATop+6,self.Width-7, self.Height-7,24,24);
    Line(0,self.Height-1,self.Width-1,Self.Height-1);
    end;
end;

procedure UISearchBar.SetPrompt(AIndex: TXIBProperties; AValue: string);
begin
  SetXIBString(AIndex,AValue);
  SetHeight(-1);
end;

{ UISearchBar }

constructor UISearchBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

procedure UISearchBar.InitializeDefaults;
begin
  inherited InitializeDefaults;
  Height:=44;
  Width:=320;
  Opaque:=true;
end;

function UISearchBar.GetIBClassName: string;
begin
  Result:='IBUISearchBar';
end;

function UITableView.GetSeparatorStyle: TiOSFakeSeparatorStyle;
begin
  result := TiOSFakeSeparatorStyle(GetXIBInteger(bvSeparatorStyle));
end;

procedure UITableView.SetSeparatorStyle(AValue: TiOSFakeSeparatorStyle);
begin
  SetXIBInteger(bvSeparatorStyle, ord(AValue));
end;

{ UITableView }

constructor UITableView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FSeparatorColor:=clDefault;
  FAcceptChildsAtDesignTime:=false;
end;

procedure UITableView.InitializeDefaults;
begin
  inherited InitializeDefaults;
  RowHeight:=44;
  SectionFooterHeight:=22;
  SectionHeaderHeight:=22;
  SeparatorStyle:=ssSingleLine;
  Opaque:=true;
  ClipSubviews:=true;
  BackgroundColor:=clWhite;
  ShowSelectionOnTouch:=true;
  BounceVertically:=true;
end;

procedure UITableView.paint(ACanvas: TCanvas);
var
  TS: TTextStyle;
  i: integer;
  Count: integer;
begin
  Inherited;
  with ACanvas do
    begin
    pen.Style:=psSolid;
    TS.Alignment:=taLeftJustify;
    TS.Layout:=tlCenter;
    font.Bold:=true;
    font.Size:=16;
    font.color:=clWhite;
    GradientFill(rect(-1,round(self.Height-SectionFooterHeight),self.Width+2,self.Height-1),clLtGray, clGray, gdVertical);
    TextRect(rect(8,round(self.Height-SectionFooterHeight),self.Width-8,Self.Height),8,round(self.Height-SectionFooterHeight),'Section Footer',TS);

    GradientFill(rect(-1,0,self.Width+2,round(SectionHeaderHeight)),clLtGray, clGray, gdVertical);
    TextRect(rect(8,0,self.Width-8,round(SectionHeaderHeight)),8,0,'Netherlands',TS);

    pen.Color:=SeparatorColor;
    font.Color:=clBlack;
    font.Size:=18;
    count := trunc((self.Height-SectionFooterHeight-SectionHeaderHeight) / RowHeight);
    for i := 0 to count-1 do
      begin
      TextRect(rect(8,round(SectionHeaderHeight+(i*RowHeight)),self.Width-8,round(SectionHeaderHeight+((i+1)*RowHeight))),8,round(SectionHeaderHeight+(i*RowHeight)),'Item',TS);
      if SeparatorStyle=ssNone then
        pen.Style:=psClear
      else if SeparatorStyle=ssSingleLine then
        pen.Style:=psSolid
      else if SeparatorStyle=ssSingleLineEtched then
        pen.Style:=psDash;

      Line(0,round(SectionHeaderHeight+((i+1)*RowHeight)),self.Width,round(SectionHeaderHeight+((i+1)*RowHeight)));
      end;

    end;
end;

function UITableView.GetIBClassName: string;
begin
  Result:='IBUITableView';
end;

{ UITextField }

function UITextField.GetTextAlignment: TiOSFakeAlignment;
begin
  result := TiOSFakeAlignment(GetXIBInteger(bvTextAlignment));
end;

procedure UITextField.SetTextAlignment(AValue: TiOSFakeAlignment);
begin
  SetXIBInteger(bvTextAlignment,ord(AValue));
end;

{
procedure UITextField.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBString(AnObjectDomElement,'IBUIText',Text);
  AddIBString(AnObjectDomElement,'IBUIPlaceholder',Placeholder);
  AddIBInt(AnObjectDomElement,'IBUITextAlignment',ord(Alignment),0)
end;
}
constructor UITextField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

function UITextField.GetIBClassName: string;
begin
  Result:='IBUITextField';
end;

procedure UITextField.paint(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Font.Color:=TextColor;
  Font.ApplyToLCLFont(ACanvas.Font);
  ACanvas.TextOut(5,2,Caption);
end;

{ TiOSFakeFontDescription }

procedure TiOSFakeFontDescription.SetFont(AFontType: TiOSFakeFontType; APointSize: double);
var
  ADescNode: TDOMNode;
  AFontNode: TDOMNode;
  ANode: TDOMNode;
begin
  if Assigned(FXIBObjectElement) then
    begin
    ADescNode := FindKeyNode(FXIBObjectElement, 'object', 'IBUIFontDescription');
    AFontNode := FindKeyNode(FXIBObjectElement, 'object', 'NSFont');
    if AFontType = ftNotSet then
      begin
      if Assigned(ADescNode) then
        ADescNode.ParentNode.RemoveChild(ADescNode);
      if Assigned(AFontNode) then
        AFontNode.ParentNode.RemoveChild(AFontNode);
      end
    else
      begin
      if not assigned(ADescNode) then
        ADescNode := GetKeyNode(FXIBObjectElement, 'object', 'IBUIFontDescription', 'IBUIFontDescription');
      if not assigned(AFontNode) then
        AFontNode := GetKeyNode(FXIBObjectElement, 'object', 'IBUIFont', 'NSFont');

      ANode := GetKeyNode(ADescNode,'double','pointSize');
      ANode.TextContent:=FloatToStr(ApointSize);
      ANode := GetKeyNode(AFontNode,'double','NSSize');
      ANode.TextContent:=FloatToStr(ApointSize);

      ANode := GetKeyNode(ADescNode, 'int', 'type');
      ANode.TextContent:=IntToStr(ord(AFontType));
      ANode := GetKeyNode(AFontNode, 'int', 'NSfFlags');
      ANode.TextContent:='16';

      ANode := GetKeyNode(AFontNode, 'string', 'NSName');
      case AFontType of
        ftSystem: ANode.TextContent:='Helvetica';
        ftSystemBold: ANode.TextContent:='Helvetica-Bold';
        ftSystemItalic: ANode.TextContent:='Helvetica-Oblique';
      end;

      end;
    end;
end;

function TiOSFakeFontDescription.GetFontType: TiOSFakeFontType;
var
  ANode: TDOMElement;
begin
  Result := ftNotSet;
  if Assigned(FXIBObjectElement) then
    begin
    ANode := FindKeyNode(FXIBObjectElement, 'object', 'IBUIFontDescription');
    if assigned(ANode) then
      begin
      ANode := FindKeyNode(ANode,'int','type');
      if assigned(ANode) then
        Result:=TiOSFakeFontType(StrToInt(ANode.TextContent))
      else
        Result:=ftCustom;
      end;
    end
end;

function TiOSFakeFontDescription.GetpointSize: double;
var
  ANode: TDOMElement;
begin
  Result := -1;
  if Assigned(FXIBObjectElement) then
    begin
    ANode := FindKeyNode(FXIBObjectElement, 'object', 'IBUIFontDescription');
    if assigned(ANode) then
      begin
      ANode := FindKeyNode(ANode,'double','pointSize');
      if assigned(ANode) then
        Result:=StrToFloat(ANode.TextContent)
      else
        Result:=-1;
      end;
    end
end;

procedure TiOSFakeFontDescription.SetFontType(AValue: TiOSFakeFontType);
begin
  SetFont(AValue, pointSize);
end;

procedure TiOSFakeFontDescription.SetpointSize(AValue: double);
begin
  SetFont(FontType, AValue);
end;

constructor TiOSFakeFontDescription.Create;
begin
  pointSize:=12;
end;

procedure TiOSFakeFontDescription.ApplyToLCLFont(AFont: TFont);
begin
  if pointSize>0 then
    AFont.Size:=round(pointSize)
  else
    AFont.Size:=12;

  AFont.Name:='Helvetica';
  case FontType of
    ftSystemBold: begin
                  AFont.Bold:=true;
                  AFont.Italic:=False;
                  end;
    ftSystem:     begin
                  AFont.Bold:=false;
                  AFont.Italic:=False;
                  end;
    ftSystemItalic: begin
                    AFont.Italic:=true;
                    AFont.Bold:=false;
                    end
  else
    begin
    AFont.Italic:=false;
    AFont.Bold:=false;
    end;
  end;

end;

{ TMyLabelButton }

function UILabel.GetLineBreaks: TLineBreaks;
begin
  result := TLineBreaks(GetXIBInteger(bvLineBreak));
end;

procedure UILabel.SetLineBreaks(AValue: TLineBreaks);
begin
  SetXIBInteger(bvLineBreak,ord(AValue));
end;

function UILabel.GetTextAlignment: TiOSFakeAlignment;
begin
  result := TiOSFakeAlignment(GetXIBInteger(bvTextAlignment));
end;

procedure UILabel.SetTextAlignment(AValue: TiOSFakeAlignment);
begin
  SetXIBInteger(bvTextAlignment,ord(AValue));
end;

{
procedure UILabel.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  AnElement: TDOMElement;
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBString(AnObjectDomElement,'IBUIText',Caption);
  AddIBInt(AnObjectDomElement,'IBUINumberOfLines',Lines,1);
  AddIBInt(AnObjectDomElement,'IBUITextAlignment',ord(TextAlignment),0);
  AddIBColor(AnObjectDomElement,'IBUITextColor',TextColor);
  AddIBFontDescription(AnObjectDomElement,'IBUIFontDescription',Font);
  AddIBBoolean(AnObjectDomElement,'IBUIEnabled',Enabled,True);
  AddIBBoolean(AnObjectDomElement,'IBUIHighlighted',Highlighted,false);
  AddIBInt(AnObjectDomElement,'IBUILineBreakMode',ord(LineBreaks),4);
end;
}

constructor UILabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

function UILabel.GetIBClassName: string;
begin
  result := 'IBUILabel';
end;

procedure UILabel.paint(ACanvas: TCanvas);
begin
  Font.ApplyToLCLFont(ACanvas.Font);
  ACanvas.Font.Color:=TextColor;
  ACanvas.TextOut(5,2,Caption);
end;


{ UIView }

function UIView.GetXIBObject(AIndex: TXIBProperties): UIView;

  function FindComponentByRef(AComp: tiOSFakeComponent;const ARef: Int64): tiOSFakeComponent;
  var
    i: Integer;
  begin
    for i := 0 to AComp.ChildCount-1 do
      begin
      if AComp.Children[i].Ref=ARef then
        begin
        result := AComp.Children[i];
        exit;
        end;
      if AComp.Children[i]=AComp then
        raise exception.create('Circular reference in components');
      result := FindComponentByRef(AComp.Children[i], ARef);
      if assigned(result) then
        exit;
      end;
    result := nil;
  end;

var
  s: string;
  ANode: TDOMElement;
begin
  ANode := FindKeyNode(XIBObjectElement, 'reference', XIBPropertiesStrings[AIndex].APropertyName);
  if assigned(ANode) then
    begin
    s  := ANode.AttribStrings['ref'];
    result := FindComponentByRef(Owner as tiOSFakeComponent, StrToIntDef(s,-1)) as UIView;
    end
  else
    result := nil;
end;

function UIView.GetFlags(AIndex: Integer): boolean;
begin
  result := (Flags and (1 shl AIndex)) = (1 shl AIndex);
end;

function UIView.ObtainSuperview: UIView;
begin
  if assigned(Parent) and (Parent is UIView) then
    result := uiview(parent).ObtainSuperView
  else
    result := self;
end;

procedure UIView.SetFlags(AIndex: Integer; AValue: boolean);
begin
  if AValue then
    Flags := flags or (1 shl AIndex)
  else
    Flags := flags and (not (1 shl AIndex));
end;

procedure UIView.SetXIBObject(AIndex: TXIBProperties; AValue: UIView);
var
  ANode: TDOMElement;
begin
  if not assigned(XIBObjectElement) then
    raise exception.create('NoObjectElement');
  if assigned(AValue) then
    begin
    ANode := GetKeyNode(XIBObjectElement, 'reference', XIBPropertiesStrings[AIndex].APropertyName);
    ANode.AttribStrings['ref'] := IntToStr(AValue.Ref);
    end
  else
    begin
    ANode := FindKeyNode(XIBObjectElement, 'reference', XIBPropertiesStrings[AIndex].APropertyName);
    if assigned(ANode) then
      ANode.ParentNode.RemoveChild(ANode);
    end;
  Invalidate;
end;

function UIView.GetPaintText: string;
begin
  result := Caption;
end;

procedure tiOSFakeComponent.SetXIBString(index: TXIBProperties; ANodeName, AValue: string);
var
  ANode: TDOMNode;
begin
  if GetXIBString(index, ANodeName)=AValue then
    Exit;
  if Assigned(XIBObjectElement) then
    begin
    if (AValue = XIBPropertiesStrings[index].ADefaultValue) then
      begin
      // Value is set to default, remove node
      ANode := FindKeyNode(XIBObjectElement, ANodeName, XIBPropertiesStrings[index].APropertyName);
      if assigned(ANode) then
        ANode.ParentNode.RemoveChild(ANode);
      end
    else
      begin
      ANode := GetKeyNode(XIBObjectElement, ANodeName, XIBPropertiesStrings[index].APropertyName);
      ANode.TextContent:=AValue;
      end;
    Invalidate;
    end;
end;

function tiOSFakeComponent.GetXIBString(index: TXIBProperties): string;
begin
  result := GetXIBString(index, 'string');
end;

procedure tiOSFakeComponent.SetXIBString(index: TXIBProperties; AValue: string);
begin
  SetXIBString(index, 'string', AValue);
end;

procedure UIView.SetName(const NewName: TComponentName);
begin
  if Name=Caption then Caption:=NewName;
  inherited SetName(NewName);
end;

procedure UIView.AddChild(const AValue: tiOSFakeComponent);
begin
  inherited AddChild(AValue);

  if (AValue is UIView) and not (csLoading in ComponentState) then
    begin
    UIView(AValue).NSNextResponder:=self;
    end;
end;

{
procedure UIView.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  b: integer;
begin
  AddIBString(AnObjectDomElement,'NSFrame','{{'+inttostr(Left)+', '+inttostr(top)+'}, {'+inttostr(width)+', '+inttostr(height)+'}}');
  AddIBReference(AnObjectDomElement,'NSSuperview',NSSuperview);
  AddIBReference(AnObjectDomElement,'NSNextResponder',NSNextResponder);
  AddIBBoolean(AnObjectDomElement,'IBUIOpaque',Opaque);
  b := 1316; //1325;
  if Hidden then
    b := b or (1 shl 31);
  AddIBInt(AnObjectDomElement,'NSvFlags',b);
  AddIBFloat(AnObjectDomElement,'IBUIAlpha',Alpha,1);
  AddIBColor(AnObjectDomElement,'IBUIBackgroundColor',BackgroundColor);
  inherited WriteToDomElement(AnObjectDomElement);
end;
}
function tiOSFakeComponent.GetXIBBoolean(index: TXIBProperties): boolean;
var
  s: string;
begin
  s := GetXIBString(index, 'bool');
  if s='YES' then
    result := true
  else
    result := false;
end;

procedure tiOSFakeComponent.SetXIBBoolean(index: TXIBProperties; AValue: boolean);
begin
  if AValue then
    SetXIBString(index,'bool','YES')
  else
    SetXIBString(index,'bool','NO')
end;

function tiOSFakeComponent.GetXIBFloat(index: TXIBProperties): double;
var
  s: string;
begin
  s := GetXIBString(index, 'float');
  result := StrToFloat(s);
end;

procedure tiOSFakeComponent.SetXIBFloat(index: TXIBProperties; AValue: double);
begin
  SetXIBString(index,'float',FloatToStr(AValue));
end;

function tiOSFakeComponent.GetXIBString(index: TXIBProperties; ANodeName: string): string;
var
  ANode: TDOMNode;
begin
  if Assigned(XIBObjectElement) then
    begin
    ANode := FindKeyNode(XIBObjectElement, ANodeName, XIBPropertiesStrings[index].APropertyName);
    if not assigned(ANode) then
      result := XIBPropertiesStrings[index].ADefaultValue
    else
      result := ANode.TextContent
    end
  else
    result := XIBPropertiesStrings[index].ADefaultValue;
end;

constructor UIView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=true;
end;

procedure UIView.InitializeDefaults;
begin
  inherited InitializeDefaults;
  SetBounds(10,30,72,37);
  Alpha:=1;
  Opaque:=False;
  Flags := 292;
end;

procedure UIView.paint(ACanvas: TCanvas);
begin
  with ACanvas do
    begin
    Brush.Color:=BackgroundColor;
    Pen.Color:=BackgroundColor;
    if (BackgroundColor = clDefault) then
      begin
      Brush.Style:=bsClear;
      pen.Style:=psClear;
      end
    else
      begin
      Brush.Style:=bsSolid;
      pen.Style:=psSolid;
      end;

    // Background
    Rectangle(0,0,self.Width,self.Height);
    end;
end;

function UIView.GetIBClassName: string;
begin
  result := 'IBUIView';
end;

{ UIButton }
{
procedure UIButton.WriteToDomElement(AnObjectDomElement: TDOMElement);

begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBReference(AnObjectDomElement,'NSNextKeyView',NSNextKeyView);
end;
}

function UIButton.GetButtonType: TiOSFakeButtonType;
begin
  result := TiOSFakeButtonType(GetXIBInteger(bvButtonType));
end;

procedure UIButton.SetButtonType(AValue: TiOSFakeButtonType);
begin
  SetXIBInteger(bvButtonType, Ord(AValue));
end;

function UIButton.GetPaintText: string;
begin
  result := NormalTitle;
end;

procedure UIButton.InitializeDefaults;
begin
  inherited InitializeDefaults;
  NormalTitleColor:=$00854F32;
  ButtonType:=RoundedRect;
end;

constructor UIButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

function UIButton.GetIBClassName: string;
begin
  result := 'IBUIButton';
end;

procedure UIButton.paint(ACanvas: TCanvas);
var
  ARadius: integer;
begin
  Inherited;
  with ACanvas do
    begin
    brush.Color:=clWhite;
    pen.Color:=clGray;
    pen.Style := psSolid;

    ARadius := min(self.Width,self.Height);
    ARadius := min(ARadius, 22);

    RoundRect(0,0,self.Width,self.Height,ARadius, ARadius);

    // caption
    Font.Color:=NormalTitleColor;
    self.Font.ApplyToLCLFont(Font);
    TextOut(5,2,GetPaintText);
    end;
end;

end.

