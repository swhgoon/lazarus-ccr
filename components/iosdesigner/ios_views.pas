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
  Classes, SysUtils, Math, types, DOM, XMLWrite, XMLRead, Graphics, strutils;

type
  TXIBProperties = (
    bvOpaque,
    bvHighlighted,
    bvAlpha,
    bvText,
    bvSuperview,
    bvLines,
    bvBackgroundColor,
    bvEnabled);

  TXIBProperty = record
    APropertyName: string;
    ADefaultValue: string;
  end;


const
  XIBPropertiesStrings : array[TXIBProperties] of TXIBProperty = (
    (APropertyName: 'IBUIOpaque'     ; ADefaultValue: 'NO'),
    (APropertyName: 'IBUIHighlighted'; ADefaultValue: 'NO'),
    (APropertyName: 'IBUIAlpha'      ; ADefaultValue: '1'),
    (APropertyName: 'IBUIText'       ; ADefaultValue: 'Label'),
    (APropertyName: 'NSSuperview'    ; ADefaultValue: ''),
    (APropertyName: 'Lines'          ; ADefaultValue: '1'),
    (APropertyName: 'IBUIBackgroundColor' ; ADefaultValue: ''),
    (APropertyName: 'IBUIEnabled'    ; ADefaultValue: 'NO'));


type
  IMyWidgetDesigner = interface(IUnknown)
    procedure InvalidateRect(Sender: TObject; ARect: TRect; Erase: boolean);
    function CreateComponent(ParentComp: TComponent;
                             TypeClass: TComponentClass;
                             const AUnitName: shortstring;
                             X,Y,W,H: Integer;
                             DisableAutoSize: boolean): TComponent;
  end;

  TiOSFakeFontType = (ftNotSet, ftSystem, ftSystemBold, ftSystemItalic);

  { TiOSFakeFontDescription }

  TiOSFakeFontDescription = class(TPersistent)
  private
    FFontType: TiOSFakeFontType;
    FpointSize: double;
  public
    constructor Create;
  published
    property FontType: TiOSFakeFontType read FFontType write FFontType;
    property pointSize: double read FpointSize write FpointSize;
  end;

  { tiOSFakeComponent }

  tiOSWriteDomMethod = function (AnObjectDomElement: TDOMElement): TDOMElement of object;
  TiOSXIBPos = (sLeft, sTop, sWidth, sHeight);

  TiOSXIBConnectionType = (ctOutlet, ctEvent);

  NSObject = class;
  tiOSFakeComponent = class(TComponent)
  private
    FAcceptChildsAtDesignTime: boolean;
    FChilds: TFPList; // list of tiOSFakeComponent
    FTop: integer;
    FLeft: integer;
    FXIBObjectElement: TDOMElement;
    FParent: tiOSFakeComponent;
    // iOS
    procedure AddChildToDom(const AValue: tiOSFakeComponent);
    function ElementToString(AWriteDomMethod: tiOSWriteDomMethod): string;
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
    function GetXIBConnection(ASourceRef, ADestinationRef: int64; ConnectionType: TiOSXIBConnectionType; CreateIfNotExists: boolean): TDOMElement;
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
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); virtual;
    function WriteToObject(AnObjectDomElement: TDOMElement): TDOMElement; virtual;
    function WriteToConnectionRecords(AnObjectDomElement: TDOMElement): TDOMElement; virtual;
    function WriteToObjectRecord(AnObjectDomElement: TDOMElement): TDOMElement; virtual;
    class function GetIBClassName: string; virtual;
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
    function GetXIBColor(index: TXIBProperties): TColor; virtual;
    procedure SetXIBColor(index: TXIBProperties; AValue: TColor);

    function GetNSObject: NSObject; virtual;
    // Streaming
    procedure DefineProperties(Filer: TFiler); override;
    function GetKeyNode(AParentNode: TDOMNode; NodeName, Key: string; AClass: string =''): TDOMElement;
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
    function getAsXIBObject: string; virtual;
    function getConnectionRecords: string; virtual;
    function getObjectRecord: string; virtual;
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

  { UIView }

  UIView = class(tiOSFakeComponent)
  private
    FNSNextResponder: UIView;
    FBackgroundColor: TColor;
    FHidden: boolean;
    function GetNSSuperview: UIView;
    function ObtainSuperview: UIView;
    procedure SetNSSuperView(AValue: UIView);
  protected
    procedure SetName(const NewName: TComponentName); override;
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitializeDefaults; override;
    procedure  paint(ACanvas: TCanvas); override;
    class function GetIBClassName: string; override;
    property NSSuperview: UIView read GetNSSuperview write SetNSSuperView;
    property NSNextResponder: UIView read FNSNextResponder write FNSNextResponder;
  published
    property Caption: string index bvText read GetXIBString write SetXIBString;
    property Opaque: boolean index bvOpaque read GetXIBBoolean write SetXIBBoolean;
    property BackgroundColor: TColor index bvBackgroundColor read GetXIBColor write SetXIBColor;
    property Alpha: double index bvAlpha read GetXIBFloat write SetXIBFloat;
    property Hidden: boolean read FHidden write FHidden;
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
    procedure GetXIBSaveParam(Sender: TObject; const ParamName: String; out AValue: String);
    function IsNIBRoot: boolean;
    function GetFilesOwnerID: string;
    procedure SetFilesOwnerOutletName(AValue: string);
  protected
    function GetFlattenedProperties: string;
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
    procedure SaveAsXIB(const Filename: string);
    function getObjectRecord: string; override;
    function getAsXIBObject: string; override;
    function getConnectionRecords: string; override;
    function GetDesigner: IMyWidgetDesigner; override;
    procedure InitializeDefaults; override;
    class function GetIBClassName: string; override;
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

  UIWindow = class(UIView)
  private
  protected
    function StoreSizeAsFrameSize: boolean; override;
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    procedure InitializeDefaults; override;
    constructor Create(AOwner: TComponent); override;
    procedure paint(ACanvas: TCanvas); override;
    class function GetIBClassName: string; override;
  published
  end;

  { UIViewController }

  UIViewController = class(tiOSFakeComponent)
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
    function GetHeight: integer; override;
    function GetWidth: integer; override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetIBClassName: string; override;
  end;

  { UINavigationBar }

  UINavigationBar = class(UIView)
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
    procedure paint(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetIBClassName: string; override;
  end;

  { UINavigationItem }

  UINavigationItem = class(UIView)
  protected
    function GetWidth: integer; override;
    procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetIBClassName: string; override;
  end;

  { UINavigationController }

  UINavigationController = class(UIViewController)
  private
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
     procedure SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitializeDefaultChildren; override;
    class function GetIBClassName: string; override;
  published
  end;

  { UIButton }
  id = ^objc_object;
  TcocoaEvent = procedure(sender: id) of object;

  UIButton = class(UIView)
  private
    FNSNextKeyView: UIView;
    FonTouchDown: TCocoaEvent;
    FTextColor: TColor;
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
    function WriteToConnectionRecords(AnObjectDomElement: TDOMElement): TDOMElement; override;
  public
    procedure InitializeDefaults; override;
    constructor Create(AOwner: TComponent); override;
    class function GetIBClassName: string; override;

    property NSNextKeyView: UIView read FNSNextKeyView write FNSNextKeyView;
  published
    property TextColor: TColor read FTextColor write FTextColor;
    property onTouchDown: TCocoaEvent read FonTouchDown write FonTouchDown;
  end;

  { UILabel }
  TLineBreaks = (lbWordWrap,lbCharacterMode,lbClip,lbTruncateHead,lbTruncateTail,lbTruncateMiddle);
  TiOSFakeAlignment = (alLeft, alCenter, alRight);

  UILabel = class(UIView)
  private
    FFont: TiOSFakeFontDescription;
    FLineBreaks: TLineBreaks;
    FTextAlignment: TiOSFakeAlignment;
    FTextColor: TColor;
    procedure SetFont(AValue: TiOSFakeFontDescription);
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetIBClassName: string; override;
    procedure paint(ACanvas: TCanvas); override;
  published
    property Lines: integer index bvLines read GetXIBInteger write SetXIBInteger;
    property TextAlignment: TiOSFakeAlignment read FTextAlignment write FTextAlignment;
    property TextColor: TColor read FTextColor write FTextColor;
    property Font: TiOSFakeFontDescription read FFont write SetFont;
    property Enabled: boolean index bvEnabled read GetXIBBoolean write SetXIBBoolean;
    property Highlighted: boolean index bvHighlighted read GetXIBBoolean write SetXIBBoolean;
    property LineBreaks: TLineBreaks read FLineBreaks write FLineBreaks;
  end;

  { UITextField }

  UITextField = class(UIView)
  private
    FPlaceholder: string;
    FText: string;
    FTextAlignment: TiOSFakeAlignment;
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetIBClassName: string; override;
    //procedure paint(ACanvas: TCanvas); override;
  published
    property Text: string read FText write FText;
    property Placeholder: string read FPlaceholder write FPlaceholder;
    property Alignment: TiOSFakeAlignment read FTextAlignment write FTextAlignment;
  end;

  TiOSFakeSeparatorStyle = (ssNone,ssSingleLine,ssSingleLineEtched);

  { UITableView }

  UITableView = class(UIView)
  private
    FClipSubviews: boolean;
    FRowHeight: float;
    FSectionFooterHeight: float;
    FSectionHeaderHeight: float;
    FSeparatorColor: TColor;
    FSeparatorStyle: TiOSFakeSeparatorStyle;
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetIBClassName: string; override;
  published
    property RowHeight: float read FRowHeight write FRowHeight;
    property SectionHeaderHeight: float read FSectionHeaderHeight write FSectionHeaderHeight;
    property SectionFooterHeight: float read FSectionFooterHeight write FSectionFooterHeight;
    property SeparatorStyle: TiOSFakeSeparatorStyle read FSeparatorStyle write FSeparatorStyle;
    property SeparatorColor: TColor read FSeparatorColor write FSeparatorColor;
    property ClipSubviews: boolean read FClipSubviews write FClipSubviews;
  end;

  { UISearchBar }

  UISearchBar = class(UIView)
  private
    FPlaceholder: string;
    FPrompt: string;
    FText: string;
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetIBClassName: string; override;
  published
    property Text: string read FText write FText;
    property Placeholder: string read FPlaceholder write FPlaceholder;
    property Prompt: string read FPrompt write FPrompt;
  end;

  { UIProgressView }

  TiOSFakeProgressViewStyle = (pvUIProgressViewStyleDefault,pvUIProgressViewStyleBar);

  UIProgressView = class(UIView)
  private
    FProgress: float;
    FprogressTintColor: TColor;
    FProgressViewStyle: TiOSFakeProgressViewStyle;
    FtrackTintColor: TColor;
  protected
    procedure WriteToDomElement(AnObjectDomElement: TDOMElement); override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetIBClassName: string; override;
  published
    property progressViewStyle: TiOSFakeProgressViewStyle read FProgressViewStyle write FprogressViewStyle;
    property progress: float read FProgress write FProgress;
    property progressTintColor: TColor read FprogressTintColor write FprogressTintColor;
    property trackTintColor: TColor read FtrackTintColor write FtrackTintColor;
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

var
  GConnectionID: integer;

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
    CompClassName:=copy(CompClassName,3,250);
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
      AnElement := FakeComp.GetKeyNode(AnElement,'string', inttostr(FakeComp.ObjectID) +'.CustomClassName');
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

{ UIProgressView }

procedure UIProgressView.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBFloat(AnObjectDomElement,'IBUIProgress',FProgress);
  AddIBInt(AnObjectDomElement,'IBUIProgressViewStyle',ord(progressViewStyle),0);
  AddIBColor(AnObjectDomElement,'IBUIProgressTintColor',progressTintColor);
  AddIBColor(AnObjectDomElement,'IBUITrackTintColor',trackTintColor);
end;

constructor UIProgressView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime := false;
  FprogressTintColor := clDefault;
  FtrackTintColor := clDefault;
end;

class function UIProgressView.GetIBClassName: string;
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

class function UINavigationItem.GetIBClassName: string;
begin
  Result:='IBUINavigationItem';
end;

{ UIViewController }

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

class function UIViewController.GetIBClassName: string;
begin
  Result:='IBUIViewController';
end;

{ UINavigationBar }

procedure UINavigationBar.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  AnElement: TDOMElement;
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AnElement := AnObjectDomElement.FindNode('NSFrame') as TDOMElement;
  if assigned(AnElement) then
    AnElement.NodeValue:='{{0, -44}, {0, 44}}';
end;

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

class function UINavigationBar.GetIBClassName: string;
begin
  Result:='IBUINavigationBar';
end;

{ UINavigationController }

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

class function UINavigationController.GetIBClassName: string;
begin
  Result:='IBUINavigationController';
end;

{ UIWindow }

function UIWindow.StoreSizeAsFrameSize: boolean;
begin
  Result:=true;
end;

procedure UIWindow.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBReference(AnObjectDomElement,'NSWindow',self);
  AddIBString(AnObjectDomElement,'targetRuntimeIdentifier','IBCocoaTouchFramework');
  AddIBBoolean(AnObjectDomElement,'IBUIResizesToFullScreen',True);
end;

procedure UIWindow.SetBounds(NewLeft, NewTop, NewWidth, NewHeight: integer);
begin
  inherited SetBounds(NewLeft, NewTop, Width, Height);
end;

procedure UIWindow.InitializeDefaults;
begin
  inherited InitializeDefaults;
  Inherited SetBounds(left, top, 320, 480);
end;

constructor UIWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NSNextResponder:=self;
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
    Pen.Color:=clRed;
    Rectangle(0,0,self.Width,self.Height);
    end;
end;

class function UIWindow.GetIBClassName: string;
begin
  Result:='IBUIWindow';
end;

{ NSObject }

procedure NSObject.GetXIBSaveParam(Sender: TObject; const ParamName: String; out AValue: String);
var
  i: Integer;
begin
  if ParamName='Objects' then
    begin
    AValue:=getAsXIBObject;
    for i := 0 to self.ChildCount-1 do
      begin
        avalue := AValue + self.Children[i].getAsXIBObject;
      end;
    end
  else if ParamName='flattenedProperties' then
    begin
    AValue := GetFlattenedProperties;
    end
  else if ParamName='ConnectionRecords' then
    begin
    avalue := getConnectionRecords;
    for i := 0 to self.ChildCount-1 do
      begin
        avalue := AValue + self.Children[i].getConnectionRecords;
      end;
    end
  else if ParamName='ObjectRecords' then
    begin
    AValue := getObjectRecord;
    for i := 0 to self.ChildCount-1 do
      begin
        avalue := AValue + self.Children[i].getObjectRecord;
      end;
    end;
end;

function NSObject.GetFilesOwnerOutletName: string;
var
  AnElement: TDOMElement;
begin
  result := '';
  AnElement := GetXIBConnection(841351856, Ref, ctOutlet, false);
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

  AnElement := GetXIBConnection(841351856, Ref, ctOutlet, True);
  AnElement := GetKeyNode(AnElement, 'string' ,'label');
  AnElement.TextContent := AValue;
end;

function NSObject.GetNSObject: NSObject;
begin
  Result:=self;
end;

function NSObject.GetFlattenedProperties: string;
var
  XMLDoc: TXMLDocument;
  AStream: TStringStream;
  FlattenedPropertiesElement: TDOMElement;
  SortedKeysElements: TDOMElement;
  ValuesElements: TDOMElement;
  AnElement: TDOMElement;
  i: integer;
begin
//  assert(self is AppDelegate);
  XMLDoc := TXMLDocument.Create;
  try
    FlattenedPropertiesElement := AddIBObject(XMLDoc,'flattenedProperties','NSMutableDictionary');
    AddIBBoolean(FlattenedPropertiesElement,'EncodedWithXMLCoder',True);

    SortedKeysElements := AddIBObject(FlattenedPropertiesElement,'dict.sortedKeys','NSArray');
    AddIBBoolean(SortedKeysElements,'EncodedWithXMLCoder',True);
    // Add props for File's Owner
    AddIBString(SortedKeysElements,'','-1.CustomClassName');
    AddIBString(SortedKeysElements,'','-1.IBPluginDependency');
    // Add props for First Responder
    AddIBString(SortedKeysElements,'','-2.CustomClassName');
    AddIBString(SortedKeysElements,'','-2.IBPluginDependency');
    AddIBString(SortedKeysElements,'','10.IBPluginDependency');
    AddIBString(SortedKeysElements,'','2.IBAttributePlaceholdersKey');
    AddIBString(SortedKeysElements,'','2.IBPluginDependency');
    if IsHiddenObject then
      begin
      AddIBString(SortedKeysElements,'','3.CustomClassName');
      AddIBString(SortedKeysElements,'','3.IBPluginDependency');
      end;
    for i := 0 to ChildCount -1 do
      AddIBString(SortedKeysElements,'',inttostr(Children[i].ObjectID)+'.IBPluginDependency');

    ValuesElements := AddIBObject(FlattenedPropertiesElement,'dict.values','NSArray');
    AddIBBoolean(ValuesElements,'EncodedWithXMLCoder',True);
    // Add props for File's Owner
    AddIBString(ValuesElements,'',FilesOwnerClass);
    AddIBString(ValuesElements,'','com.apple.InterfaceBuilder.IBCocoaTouchPlugin');
    // Add props for First Responder
    AddIBString(ValuesElements,'','UIResponder');
    AddIBString(ValuesElements,'','com.apple.InterfaceBuilder.IBCocoaTouchPlugin');
    AddIBString(ValuesElements,'','com.apple.InterfaceBuilder.IBCocoaTouchPlugin');

    AnElement := AddIBObject(ValuesElements,'','NSMutableDictionary');
    AddIBBoolean(AnElement,'EncodedWithXMLCoder',True);
    AddIBReference(AnElement,'dict.sortedKeys','0');
    AddIBReference(AnElement,'dict.values','0');

    AddIBString(ValuesElements,'','com.apple.InterfaceBuilder.IBCocoaTouchPlugin');
    if IsHiddenObject then
      begin
      AddIBString(ValuesElements,'',self.ClassName);
      AddIBString(ValuesElements,'','com.apple.InterfaceBuilder.IBCocoaTouchPlugin');
      end;
    for i := 0 to ChildCount -1 do
      AddIBString(ValuesElements,'','com.apple.InterfaceBuilder.IBCocoaTouchPlugin');

    AStream := TStringStream.Create('');
    try
      for i := 0 to XMLDoc.ChildNodes.Count-1 do
        WriteXML(XMLDoc.ChildNodes.Item[i],AStream);
      result := AStream.DataString;
    finally
      AStream.Free;
    end;
  finally
    XMLDoc.Free;
  end;
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

function NSObject.getObjectRecord: string;
begin
  if IsNIBRoot then
    begin
    // ObjectRecord's for the File's Owner and First Responder
    result :=
      '<object class="IBObjectRecord">' + LineEnding +
      '	<int key="objectID">-1</int>' +LineEnding +
      '	<reference key="object" ref="'+GetFilesOwnerID+'"/>' + LineEnding +
      '	<reference key="parent" ref="0"/>' + LineEnding +
      '	<string key="objectName">File''s Owner</string>' + LineEnding +
      '</object>' + LineEnding +
      '<object class="IBObjectRecord">' + LineEnding +
      '	<int key="objectID">-2</int>' + LineEnding +
      '	<reference key="object" ref="427554174"/>' + LineEnding +
      '	<reference key="parent" ref="0"/>' + LineEnding +
      '</object>'+ LineEnding;
    if IsHiddenObject then
      result := result +
      '<object class="IBObjectRecord">' + LineEnding +
      '	<int key="objectID">3</int>' + LineEnding +
      '	<reference key="object" ref="664661524"/>' + LineEnding +
      '	<reference key="parent" ref="0"/>' + LineEnding +
      '</object>' + LineEnding;
    end
  else
    result := inherited getObjectRecord;
end;

function NSObject.getAsXIBObject: string;
begin
  if IsNIBRoot then
    begin
    // Objects for the File's Owner and First Responder
    result :=
      // File's owner
      '<object class="IBProxyObject" id="'+GetFilesOwnerID+'">' + LineEnding +
      '	<string key="IBProxiedObjectIdentifier">IBFilesOwner</string>' + LineEnding +
      '	<string key="targetRuntimeIdentifier">IBCocoaTouchFramework</string>' + LineEnding +
      '</object>' + LineEnding +
      // First responder
      '<object class="IBProxyObject" id="427554174">' + LineEnding +
      '	<string key="IBProxiedObjectIdentifier">IBFirstResponder</string>' + LineEnding +
      '	<string key="targetRuntimeIdentifier">IBCocoaTouchFramework</string>' + LineEnding +
      '</object>' + LineEnding;
    if IsHiddenObject then
      result := result +
        '<object class="IBUICustomObject" id="664661524">' + LineEnding +
        '	<string key="targetRuntimeIdentifier">IBCocoaTouchFramework</string>' + LineEnding +
        '</object>' + LineEnding;

    end
  else
    result := inherited getObjectRecord;
end;

function NSObject.getConnectionRecords: string;
begin
  if IsNIBRoot then
    begin
    if IsHiddenObject then
      result := result +
      '<object class="IBConnectionRecord">' + LineEnding +
      '	<object class="IBCocoaTouchOutletConnection" key="connection">' + LineEnding +
      '		<string key="label">delegate</string>' + LineEnding +
      '		<reference key="source" ref="'+GetFilesOwnerID+'"/>' + LineEnding +
      '		<reference key="destination" ref="664661524"/>' + LineEnding +
      '	</object>' + LineEnding +
      '	<int key="connectionID">4</int>' + LineEnding +
      '</object>' + LineEnding;
    end
  else
    Result:=inherited getConnectionRecords;
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
  FNIBDocument.Free;
  inherited Destroy;
end;

procedure NSObject.SaveAsXIB(const Filename: string);
var
  ATemplate: TFPTemplate;
  fs: TFileStream;
  s: string;
  dir: string;

begin
  ATemplate := TFPTemplate.Create;
  try
    ATemplate.StartDelimiter:='[{';
    ATemplate.EndDelimiter:='}]';

{$ifndef OutsideIDE}
    dir := '$PkgDir(iOSDesigner)';
    IDEMacros.SubstituteMacros(dir);
{$else}
    dir :=  '/Users/joost/svn/ccr-components/iosdesigner';
{$endif OutsideIDE}
    if dir <>'' then
      ATemplate.FileName:=dir+PathDelim+ 'MainWindow_iPhone.template'
    else
      raise exception.create('iOSDesigner package could not be found.');

    ATemplate.OnGetParam :=@GetXIBSaveParam;
    s := ATemplate.GetContent;
    fs := TFileStream.Create(Filename,fmCreate);
    try
      fs.Write(s[1],length(s));
    finally
      fs.free;
    end;
  finally
    ATemplate.Free;
  end;
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

class function NSObject.GetIBClassName: string;
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
  ConnectionType: TiOSXIBConnectionType; CreateIfNotExists: boolean): TDOMElement;
var
  AnElement: TDOMElement;
  AnOutletElement: TDOMElement;
  SourceElement: TDOMElement;
  DestElement: TDOMElement;
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
        if assigned(SourceElement) and assigned(DestElement) and
           (SourceElement.AttribStrings['ref']=IntToStr(ASourceRef)) and
           (DestElement.AttribStrings['ref']=IntToStr(ADestinationRef)) then
          begin
          if ((ConnectionType=ctOutlet) and (AnOutletElement.AttribStrings['class']='IBCocoaTouchOutletConnection')) or
             ((ConnectionType=ctEvent) and (AnOutletElement.AttribStrings['class']='IBCocoaTouchEventConnection')) then
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
    result := AnOutletElement;

    GetKeyNode(AnElement,'int','connectionID').TextContent:=IntToStr(GConnectionID);
    inc(GConnectionID);
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

function tiOSFakeComponent.ElementToString(AWriteDomMethod: tiOSWriteDomMethod): string;
var
  XMLDoc: TXMLDocument;
  AStream: TStringStream;
  RootElement: TDOMElement;
  i: integer;
begin
  XMLDoc := TXMLDocument.Create;
  try
    RootElement := XMLDoc.CreateElement('root');
    XMLDoc.AppendChild(RootElement);
    AWriteDomMethod(RootElement);

    AStream := TStringStream.Create('');
    try
      for i := 0 to RootElement.ChildNodes.Count-1 do
        WriteXML(RootElement.ChildNodes.Item[i],AStream);
      result := AStream.DataString;
    finally
      AStream.Free;
    end;
  finally
    XMLDoc.Free;
  end;
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
begin
  FChilds.Add(AValue);
  AddChildToDom(AValue);
  if (AValue is UIView) and not (csLoading in ComponentState) then
    begin
    AComp := UIView(AValue).ObtainSuperview;
    if AComp<>AValue then
      UIView(AValue).NSSuperview := AComp as UIView;
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

procedure tiOSFakeComponent.RemoveChild(const AValue: tiOSFakeComponent);
begin
  FChilds.Remove(AValue);
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
  result := StrToInt(GetXIBString(index,'int'));
end;

procedure tiOSFakeComponent.SetXIBInteger(index: TXIBProperties; AValue: integer);
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
begin
  inherited DefineProperties(Filer);
  if filer is TXIBReader then
    SetXIBObjectElement(TXIBObjectReader(TReader(Filer).Driver).FCurrentObject);
end;

function tiOSFakeComponent.GetKeyNode(AParentNode: TDOMNode; NodeName, Key: string; AClass: string): TDOMElement;
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
  AddIBInt(IBConnectionRecordElement,'connectionID',GConnectionID);
  inc(GConnectionID);
  if AEventType<>'' then
    AddIBInt(IBConnectionElement,'IBEventType',StrToInt64Def(AEventType,1));
end;

procedure tiOSFakeComponent.WriteToDomElement(AnObjectDomElement: TDOMElement);
var
  ChildsMutableArray: TDOMElement;
  i: Integer;
begin
  if ChildCount>0 then
    begin
    ChildsMutableArray := AddIBObject(AnObjectDomElement,'NSSubviews','NSMutableArray');
    AddIBBoolean(ChildsMutableArray,'EncodedWithXMLCoder',true);
    for i := 0 to ChildCount-1 do
      Children[i].WriteToObject(ChildsMutableArray);
    end;
end;

function tiOSFakeComponent.WriteToObject(AnObjectDomElement: TDOMElement) : TDOMElement;
var
  ClassDomElement: TDOMElement;
  AnAttribute: TDOMNode;

begin
  ClassDomElement := AddIBObject(AnObjectDomElement,'',GetIBClassName);
  ClassDomElement.AttribStrings['id']:=IntToStr(Ref);

  WriteToDomElement(ClassDomElement);
  result := ClassDomElement;
end;

function tiOSFakeComponent.WriteToConnectionRecords(
  AnObjectDomElement: TDOMElement): TDOMElement;
var
  i: Integer;
begin
  AddConnectionRecord(AnObjectDomElement,'IBCocoaTouchOutletConnection',Name,'');
  for i := 0 to ChildCount-1 do
    Children[i].WriteToConnectionRecords(AnObjectDomElement);
end;

function tiOSFakeComponent.WriteToObjectRecord(AnObjectDomElement: TDOMElement
  ): TDOMElement;
var
  ObjectRecordElement: TDOMElement;
  ChildArray: TDOMElement;
  i: Integer;
begin
  ObjectRecordElement := AddIBObject(AnObjectDomElement,'','IBObjectRecord');
  AddIBInt(ObjectRecordElement,'objectID',ObjectID);
  AddIBReference(ObjectRecordElement,'object',self,True);

  if ChildCount>0 then
    begin
    ChildArray := AddIBObject(ObjectRecordElement,'children','NSMutableArray');
    AddIBBoolean(ChildArray,'EncodedWithXMLCoder',True);
    for i := 0 to ChildCount-1 do
      AddIBReference(ChildArray,'',Children[i]);
    end;

  if assigned(Parent) then
    AddIBReference(ObjectRecordElement,'parent',parent)
  else
    AddIBReference(ObjectRecordElement,'parent','0');
  for i := 0 to ChildCount-1 do
    Children[i].WriteToObjectRecord(AnObjectDomElement);
  result := ObjectRecordElement;
end;

class function tiOSFakeComponent.GetIBClassName: string;
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
end;

destructor tiOSFakeComponent.Destroy;
begin
  Parent:=nil;
  while ChildCount>0 do Children[ChildCount-1].Free;
  FreeAndNil(FChilds);
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

function tiOSFakeComponent.getAsXIBObject: string;
begin
  result := ElementToString(@WriteToObject);
end;

function tiOSFakeComponent.getConnectionRecords: string;
begin
  result := ElementToString(@WriteToConnectionRecords);
  inherited;
end;

function tiOSFakeComponent.getObjectRecord: string;

begin
  result := ElementToString(@WriteToObjectRecord);
end;

function tiOSFakeComponent.AddElement(ADomNode: TDomElement; AName: string): TDOMElement;
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



{ UISearchBar }

procedure UISearchBar.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBString(AnObjectDomElement,'IBText',Text);
  AddIBString(AnObjectDomElement,'IBPlaceholder',Placeholder);
  AddIBString(AnObjectDomElement,'IBPrompt',Prompt);
end;

constructor UISearchBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

destructor UISearchBar.Destroy;
begin
  inherited Destroy;
end;

class function UISearchBar.GetIBClassName: string;
begin
  Result:='IBUISearchBar';
end;

{ UITableView }

procedure UITableView.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBFloat(AnObjectDomElement,'IBUIRowHeight',RowHeight);
  AddIBFloat(AnObjectDomElement,'IBUISectionHeaderHeight',SectionHeaderHeight);
  AddIBFloat(AnObjectDomElement,'IBUISectionFooterHeight',SectionFooterHeight);
  AddIBInt(AnObjectDomElement,'IBUISeparatorStyle',ord(SeparatorStyle),0);
  AddIBColor(AnObjectDomElement,'IBUISeparatorColor',SeparatorColor);
  AddIBBoolean(AnObjectDomElement,'IBUIClipsSubviews',ClipSubviews);
end;

constructor UITableView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRowHeight:=44;
  FSectionFooterHeight:=22;
  FSectionHeaderHeight:=22;
  FSeparatorStyle:=ssSingleLine;
  FSeparatorColor:=clDefault;
  FClipSubviews:=true;
  FAcceptChildsAtDesignTime:=false;
  FBackgroundColor := clWhite;
end;

destructor UITableView.Destroy;
begin
  inherited Destroy;
end;

class function UITableView.GetIBClassName: string;
begin
  Result:='IBUITableView';
end;

{ UITextField }

procedure UITextField.WriteToDomElement(AnObjectDomElement: TDOMElement);
begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBString(AnObjectDomElement,'IBUIText',Text);
  AddIBString(AnObjectDomElement,'IBUIPlaceholder',Placeholder);
  AddIBInt(AnObjectDomElement,'IBUITextAlignment',ord(Alignment),0)
end;

constructor UITextField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

destructor UITextField.Destroy;
begin
  inherited Destroy;
end;

class function UITextField.GetIBClassName: string;
begin
  Result:='IBUITextField';
end;

{ TiOSFakeFontDescription }

constructor TiOSFakeFontDescription.Create;
begin
  pointSize:=12;
end;

{ TMyLabelButton }

procedure UILabel.SetFont(AValue: TiOSFakeFontDescription);
begin
  if FFont=AValue then Exit;
  FFont.Assign(AValue);
end;

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


constructor UILabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
  FFont := TiOSFakeFontDescription.Create;
  Enabled := true;
end;

destructor UILabel.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

class function UILabel.GetIBClassName: string;
begin
  result := 'IBUILabel';
end;

procedure UILabel.paint(ACanvas: TCanvas);
begin
  ACanvas.Font.Color:=TextColor;
  case Font.FontType of
    ftSystemBold: begin
                  ACanvas.font.Name:='Helvetica';
                  ACanvas.Font.Size:=round(Font.FpointSize);
                  ACanvas.Font.Bold:=true;
                  ACanvas.Font.Italic:=False;
                  end;
    ftSystem:     begin
                  ACanvas.font.Name:='Helvetica';
                  ACanvas.Font.Size:=round(Font.FpointSize);
                  ACanvas.Font.Bold:=false;
                  ACanvas.Font.Italic:=False;
                  end;
    ftSystemItalic: begin
                    ACanvas.font.Name:='Helvetica';
                    ACanvas.Font.Size:=round(Font.FpointSize);
                    ACanvas.Font.Italic:=true;
                    ACanvas.Font.Bold:=false;
                    end
  else
    begin
    ACanvas.font.Name:='Helvetica';
    ACanvas.Font.Size:=12;
    ACanvas.Font.Italic:=false;
    ACanvas.Font.Bold:=false;
    end;
  end;
  ACanvas.TextOut(5,2,Caption);
end;


{ UIView }

function UIView.GetNSSuperView: UIView;

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
  ANode := FindKeyNode(XIBObjectElement, 'reference', 'NSSuperview');
  if assigned(ANode) then
    begin
    s  := ANode.AttribStrings['ref'];
    result := FindComponentByRef(Owner as tiOSFakeComponent, StrToIntDef(s,-1)) as UIView;
    end
  else
    result := nil;
end;

function UIView.ObtainSuperview: UIView;
begin
  if assigned(Parent) and (Parent is UIView) then
    result := uiview(parent).ObtainSuperView
  else
    result := self;
end;

procedure UIView.SetNSSuperView(AValue: UIView);
var
  ANode: TDOMElement;
begin
  if not assigned(XIBObjectElement) then
    raise exception.create('NoObjectElement');
  if assigned(AValue) then
    begin
    ANode := GetKeyNode(XIBObjectElement, 'reference', 'NSSuperview');
    ANode.AttribStrings['ref'] := IntToStr(AValue.Ref);
    end
  else
    begin
    ANode := FindKeyNode(XIBObjectElement, 'reference', 'NSSuperview');
    if assigned(ANode) then
      ANode.ParentNode.RemoveChild(ANode);
    end;
  Invalidate;
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
  FBackgroundColor:=clDefault;
end;

procedure UIView.InitializeDefaults;
begin
  inherited InitializeDefaults;
  BackgroundColor:=clWhite;
  Alpha:=1;
end;

procedure UIView.paint(ACanvas: TCanvas);
begin
  with ACanvas do
    begin
    Brush.Style:=bsSolid;
    Brush.Color:=BackgroundColor;
    // outer frame
    Pen.Color:=clRed;
    Rectangle(0,0,self.Width,self.Height);
    // caption
    Font.Color:=clBlack;
    Font.Name:='Helvetica';
    Font.Size:=12;
    Font.Italic:=false;
    Font.Bold:=false;
    TextOut(5,2,Caption);
    end;
end;

class function UIView.GetIBClassName: string;
begin
  result := 'IBUIView';
end;

{ UIButton }

procedure UIButton.WriteToDomElement(AnObjectDomElement: TDOMElement);

begin
  inherited WriteToDomElement(AnObjectDomElement);
  AddIBReference(AnObjectDomElement,'NSNextKeyView',NSNextKeyView);
  AddIBInt(AnObjectDomElement,'IBUIButtonType',1);
  AddIBString(AnObjectDomElement,'IBUINormalTitle',Caption);
  AddIBColor(AnObjectDomElement,'IBUINormalTitleColor',TextColor);
end;

function UIButton.WriteToConnectionRecords(AnObjectDomElement: TDOMElement): TDOMElement;
var
  AMethod: TMethod;
begin
  inherited WriteToConnectionRecords(AnObjectDomElement);
  //writeln('PROPVAL: ' + vartostr(GetPropValue(self,'onTouchDown')));
  AMethod := LazGetMethodProp(self,GetPropInfo(self,'onTouchDown'));

  writeln('PROPVAL: ' + hexStr(PtrInt( AMethod.Code),8) + ' - ' + hexStr(ptrint(AMethod.Data),8));
  if AMethod.Data<>nil then
    begin
    AddConnectionRecord(AnObjectDomElement,'IBCocoaTouchEventConnection',Name+'TouchDown:','1');
    writeln(TObject(AMethod.Data).MethodName(self));
    end
  else
    writeln('AA- Geen Touchdown ' + self.Name);
end;

procedure UIButton.InitializeDefaults;
begin
  inherited InitializeDefaults;
  SetBounds(10,10,72,37);
end;

constructor UIButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAcceptChildsAtDesignTime:=false;
end;

class function UIButton.GetIBClassName: string;
begin
  result := 'IBUIButton';
end;

initialization
  GConnectionID:=50;
end.

