unit opbitmap;

{ *************************************************************************** }
{ Copyright (c) 2007 Theo Lustenberger                                        }
{                                                                             }
{ This software is provided "as-is".  This software comes without warranty    }
{ or garantee, explicit or implied.  Use this software at your own risk.      }
{ The author will not be liable for any damage to equipment, data, or         }
{ information that may result while using this software.                      }
{                                                                             }
{ By using this software, you agree to the conditions stated above.           }
{ *************************************************************************** }

{_$DEFINE INTEL_ASM}//Use ASM Code
{_$DEFINE IMPORTTGRAPHIC}//Import TGraphic Class

{$DEFINE USE_MOVE}
{$DEFINE VER_VTV} //Version for VTV.  No Resampling, no Canvas Line, Circle... needs less files


{$IFDEF FPC}
{$MODE objfpc}{$H+}
 {_$UNDEF USE_MOVE}
{$IFDEF INTEL_ASM}
{$ASMMODE intel}
{$ENDIF}
{$ENDIF}

{_$R+}
{_$S+}
{_$Q+}


interface

uses Classes, Types, Sysutils {$IFDEF IMPORTTGRAPHIC}, Graphics {$ENDIF};

type

  PColor = ^TColor;
  TColor = -$7FFFFFFF - 1..$7FFFFFFF;

  Nibble = 0..$F;

  TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pf48bit, pf64bit, pfCustom);

const
  { Raw rgb values }
  clBlack = TColor($000000);
  clMaroon = TColor($000080);
  clGreen = TColor($008000);
  clOlive = TColor($008080);
  clNavy = TColor($800000);
  clPurple = TColor($800080);
  clTeal = TColor($808000);
  clGray = TColor($808080);
  clSilver = TColor($C0C0C0);
  clRed = TColor($0000FF);
  clLime = TColor($00FF00);
  clYellow = TColor($00FFFF);
  clBlue = TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua = TColor($FFFF00);
  clLtGray = TColor($C0C0C0);
  clDkGray = TColor($808080);
  clWhite = TColor($FFFFFF);
  clNone = TColor($1FFFFFFF);
  clDefault = TColor($20000000);

const StdColors: array[0..15] of TColor = (
    clBlack,
    clMaroon,
    clGreen,
    clOlive,
    clNavy,
    clPurple,
    clTeal,
    clGray,
    clSilver,
    clRed,
    clLime,
    clYellow,
    clBlue,
    clFuchsia,
    clAqua,
    clWhite);

  AlphaOpaque = $FF;
  AlphaTransparent = 0;

const BWColors: array[0..1] of TColor = (clBlack, clWhite);

  MaxArr = (MaxLongint div Sizeof(integer)) - 1;

  TOPBitmapStreamSign = 'OPB';
  TOPBitmapStreamVersion = 1;

var WebColors: array[0..215] of TColor; //new 215
  Gray256Colors: array[0..$FF] of TColor;

type

  //Compatibility Declarations

  TRGBQuad =
    packed record
    rgbBlue: BYTE;
    rgbGreen: BYTE;
    rgbRed: BYTE;
    rgbReserved: BYTE
  end;

  pRGBQuad = ^TRGBQuad;

  TRGBQuadArray = array[Word] of TRGBQuad;
  PRGBQuadArray = ^TRGBQuadArray;

  TRGBQuadArray256 = array[0..256] of TRGBQuad;
  PRGBQuadArray256 = ^TRGBQuadArray;


  TRGBTriple =
    packed record
    rgbtBlue: BYTE;
    rgbtGreen: BYTE;
    rgbtRed: BYTE;
  end;

  pRGBTRiple = ^TRGBTriple;

  TRGBTripleArray = array[Word] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;

  //OPBitmap Declarations

  Pixel8 = Byte;
  APixel8 = array[0..MaxArr] of Pixel8;
  PAPixel8 = ^APixel8;

  Pixel16 = Word;
  APixel16 = array[0..MaxArr] of Pixel16;
  PAPixel16 = ^APixel16;

  Pixel24 = packed record
    Blue, Green, Red: Byte;
  end;
  PPixel24 = ^Pixel24;

  APixel24 = array[0..MaxArr] of Pixel24;
  PAPixel24 = ^APixel24;

  Pixel32 = packed record
    Blue, Green, Red, Alpha: Byte;
  end;
  PPixel32 = ^Pixel32;

  APixel32 = array[0..MaxArr] of Pixel32;
  PAPixel32 = ^APixel32;


  Pixel48 = packed record
    Blue, Green, Red: Word;
  end;
  PPixel48 = ^Pixel48;

  APixel48 = array[0..MaxArr div 2] of Pixel48;
  PAPixel48 = ^APixel48;


  Pixel64 = packed record
    Blue, Green, Red, Alpha: Word;
  end;
  PPixel64 = ^Pixel64;

  APixel64 = array[0..MaxArr div 3] of Pixel64;
  PAPixel64 = ^APixel64;

  TOpenColorTableArray = array of TColor;
  POpenColorTableArray = ^TOpenColorTableArray;

  TColorTableArray = array[0..$FF] of TColor;
  PColorTableArray = ^TColorTableArray;
  TColorTableArray16 = array[0..$F] of TColor;
  PColorTableArray16 = ^TColorTableArray16;

  TOPBitmapStreamHeader = packed record
    Version: Byte;
    BPP: Byte;
    Width: LongInt;
    Height: LongInt;
    Compressed: Boolean;
    PPI: LongInt;
    Transparent: Boolean;
    TransparentColor: TColor;
  end;


  EPasBitMapError = class(Exception);

  EInvalidGraphic = class(Exception);

  TReductionMode = (rmOptimized, rmFixed);

  TProgressStage = (psStarting, psRunning, psEnding);
  
    TOpRawImageLineEnd = (
    rileTight,
    rileByteBoundary,
    rileWordBoundary,
    rileDWordBoundary,
    rileQWordBoundary
    );

const CSpaceRedu = 3;

  //---------------------------------------------------------------------------

type

  TOPBitmap = class;

  TBitmapData = class
  private
    fBPP: Byte;
    fParent: TOPBitmap;
    fWidth: Integer;
    fHeight: Integer;
    fLineLength: Integer;
    function GetScanLine(Row: Integer): Pointer; virtual; abstract;
    function GetPixel(X, Y: Integer): TColor; virtual; abstract;
    procedure SetPixel(X, Y: Integer; const Value: TColor); virtual; abstract;
    procedure SetWidth(const Value: Integer); virtual;
    procedure SetHeight(const Value: Integer); virtual;
  protected
    procedure UpdateSize; virtual; abstract;
    function CheckPixelValid(X, Y: integer): Boolean;
  public
    constructor Create(Parent: TOPBitmap); virtual;
    destructor Destroy; override;
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property ScanLine[Row: Integer]: Pointer read GetScanLine;
    property BPP: byte read fBPP;
    property Width: Integer read fWidth write SetWidth;
    property Height: Integer read fHeight write SetHeight;
    property LineLength: integer read fLineLength;
  end;

 { TBitmapData1 }

  TBitmapData1 = class(TBitmapData)
  private
    fPixels: PAPixel8;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Boolean;
    procedure SetNativePixel(X, Y: Integer; const Value: Boolean);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel8 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Boolean read GetNativePixel write SetNativePixel;
  end;

 { TBitmapData4 }

  TBitmapData4 = class(TBitmapData)
  private
    fPixels: PAPixel8;
    fLastNearestColorIdx: word;
    fLastColor: TColor;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Nibble;
    procedure SetNativePixel(X, Y: Integer; const Value: Nibble);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel8 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Nibble read GetNativePixel write SetNativePixel;
  end;

 { TBitmapData8}

  TBitmapData8 = class(TBitmapData)
  private
    fPixels: PAPixel8;
    fLastNearestColorIdx: word;
    fLastColor: TColor;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Byte;
    procedure SetNativePixel(X, Y: Integer; const Value: Byte);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel8 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Byte read GetNativePixel write SetNativePixel;
  end;

 { TBitmapData15 }

  TBitmapData15 = class(TBitmapData)
  private
    fPixels: PAPixel16;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel16 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
  end;

 { TBitmapData16 }

  TBitmapData16 = class(TBitmapData)
  private
    fPixels: PAPixel16;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel16 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
  end;


  { TBitmapData24 }

  TBitmapData24 = class(TBitmapData)
  private
    fPixels: PAPixel24;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Pixel24;
    procedure SetNativePixel(X, Y: Integer; const Value: Pixel24);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
{$IFDEF USE_MOVE}
    procedure Assign(Source: TBitmapData);
{$ENDIF}
    property RawArray: PAPixel24 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Pixel24 read GetNativePixel write SetNativePixel;
  end;


  { TBitmapData32 }

  TBitmapData32 = class(TBitmapData)
  private
    fPixels: PAPixel32;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Pixel32;
    procedure SetNativePixel(X, Y: Integer; const Value: Pixel32);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
{$IFDEF USE_MOVE}
    procedure Assign(Source: TBitmapData);
{$ENDIF}
    property RawArray: PAPixel32 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Pixel32 read GetNativePixel write SetNativePixel;
  end;



 { TBitmapData48}

  TBitmapData48 = class(TBitmapData)
  private
    fPixels: PAPixel48;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Pixel48;
    procedure SetNativePixel(X, Y: Integer; const Value: Pixel48);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel48 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Pixel48 read GetNativePixel write SetNativePixel;
  end;



 { TBitmapData64 }

  TBitmapData64 = class(TBitmapData)
  private
    fPixels: PAPixel64;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Pixel64;
    procedure SetNativePixel(X, Y: Integer; const Value: Pixel64);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel64 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Pixel64 read GetNativePixel write SetNativePixel;
  end;




  //---------------------------------------------------------------------------

  //Canvas is not the point here. It's just here for code that needs Canvas.Pixels access
  //plus some basic stuff for testing.

  TBrushStyle = (bsSolid, bsClear);

  TBrush = class //basic
  private
    fColor: TColor;
    fStyle: TBrushStyle;
  public
    property Color: TColor read fColor write fColor;
    property Style: TBrushStyle read fStyle write fStyle default bsSolid;
  end;

  TPen = class //basic
  private
    fColor: TColor;
  public
    property Color: TColor read fColor write fColor;
  end;

  TPasCanvas = class(TPersistent)
  end;

  TCanvasOPBitmap = class;

  TOPBitmapCanvas = class(TPasCanvas)
  private
    fBitmap: TOPBitmap;
    fPenPos: TPoint;
    fBrush: TBrush;
    fPen: TPen;
    function GetPixel(X, Y: Integer): TColor;
    procedure SetPixel(X, Y: Integer; const Value: TColor);
  public
    constructor Create(Bitmap: TOPBitmap);
    destructor Destroy; override;
    procedure MoveTo(X, Y: Integer);
    {$IFNDEF VER_VTV}
    procedure LineTo(X, Y: Integer);
    procedure Circle(CenterX, CenterY, Radius: Integer);
    {$ENDIF}
    procedure FillRect(Rect: TRect);
    procedure Draw(X, Y: integer; Bitmap: TCanvasOPBitmap);
    {$IFNDEF VER_VTV}
    procedure Resample(NewWidth, NewHeight: integer);
    {$ENDIF}
    procedure CopyRect(const Dest: TRect; Canvas: TOPBitmapCanvas; const Source: TRect);
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property Brush: TBrush read fBrush;
    property Pen: TPen read fPen;
  end;

  TCanvas = TOPBitmapCanvas;


{$IFDEF IMPORTTGRAPHIC}
{_$I tgraphicdecl.inc}
{$ELSE}
  TGraphic = class(TPersistent)
  end;
{$ENDIF}

  TColorFinder = class;

  { TOPBitmap }

  TOPBitmap = class(TGraphic)
  private
    fData: TBitmapData;
    fMask: TBitmapData1;
    fColorTable: TColorTableArray;
    fColorTableSize: integer;
    fLastColorIndex: Byte;
    fLastColor: TColor;
    fPaletteHasAllColours: Boolean;
    fReductionMode: TReductionMode;
    fColorFinder: TColorFinder;
    fMonochrome: Boolean;
    fTransparentColor: TColor;
{$IFNDEF ____IMPORTTGRAPHIC}FTransparent: Boolean; {$ENDIF}
    fAlphaBlend: Boolean;
    Flags: array[BYTE, BYTE] of Classes.TBits;

    function GetScanLine(Row: Integer): Pointer;
    procedure SetColorTable(const AValue: PColorTableArray);

    procedure SetPixel(X, Y: Integer; const AValue: TColor);
    function GetPixel(X, Y: Integer): TColor;
    function GetColorIndex(Color: TColor): byte;
    function GetPixelFormat: TPixelFormat;
    procedure SetPixelFormat(const Value: TPixelFormat);
    function GetBPP: byte;
    function GetColorTable: PColorTableArray;
    function NearestColor(const color: TColor): cardinal;
    function GetHandle: THandle;
    procedure SetHandle(const Value: THandle);
    function GetPalette: THandle;
{$IFNDEF VER_VTV}  procedure SetPalette(const Value: THandle); {$ENDIF}
    procedure SetMonochrome(const Value: Boolean);
    procedure SetTransparentColor(const Value: TColor);
    procedure SetAlphaBlend(const Value: Boolean);


  protected
    procedure ShrinkPaletteWeb;
    procedure SetHeight(Value: Integer); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    procedure SetWidth(Value: Integer); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    function GetHeight: Integer; {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    function GetWidth: Integer; {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    function GetTransparent: Boolean; {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    procedure SetTransparent(Value: Boolean); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    procedure DoSetPixelFormat(const Value: TPixelFormat);
    function GetEmpty: Boolean; virtual;
  public
    constructor Create; {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDataSize: Cardinal;
    procedure CopyFromColorTable(AColorTable: array of TColor; Swap: Boolean = true; Size: integer = -1);
    function CountColors(Max: Integer): Integer;
    function MakePalette(Size: Byte; var ColorTable: TOpenColorTableArray): Boolean;
    procedure LoadFromStream(Stream: TStream); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    procedure SaveToStream(Stream: TStream); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    procedure LoadFromFile(const Filename: string); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    procedure SaveToFile(const Filename: string); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
{$IFNDEF IMPORTTGRAPHIC}
    procedure Progress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
      const Msg: string {; var DoContinue: Boolean});
{$ENDIF}
    procedure SetAlpha(Value: Byte);
    procedure Clear;
    function GetTransparentMask(Tolerance: Byte; var Data: PByte; ReversedBits: Boolean; Boundary:TOpRawImageLineEnd): integer;
    function GetFullMask(var Data: PByte): integer;
    property BPP: byte read GetBPP;
    property ScanLine[Row: Integer]: Pointer read GetScanLine;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property ColorTable: PColorTableArray read GetColorTable write SetColorTable;
{$IFNDEF VER_VTV} property Palette: THandle read GetPalette write SetPalette; {$ENDIF}
    property ColorTableSize: integer read fColorTableSize;
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat;
    property Monochrome: Boolean read fMonochrome write SetMonochrome;
    property Data: TBitmapData read fData write fData;
    property Handle: THandle read GetHandle write SetHandle;
    property ReductionMode: TReductionMode read fReductionMode write fReductionMode;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property TransparentColor: TColor read fTransparentColor write SetTransparentColor;
    property AlphaBlend: Boolean read fAlphaBlend write SetAlphaBlend;
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel; //Belongs to Canvas but is nice to have here
    property Empty: Boolean read GetEmpty;
  end;

  TCanvasOPBitmap = class(TOPBitmap)
  private
    fCanvas: TOPBitmapCanvas;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Canvas: TOPBitmapCanvas read fCanvas;
  end;


  TColorEntry = record
    R, G, B: Byte;
  end;

  PColorEntry = ^TColorEntry;


  TColorFinder = class
  private
    fPalette: TList;
    fSorted: Boolean;
    fBitmap: TOPBitmap;
    fMappings: array[BYTE, BYTE] of TList;
    procedure SetBitmap(const Value: TOPBitmap);
    function GetPaletteSize: integer;
  protected

    function MapColors: Integer;
    function NearestColor(R, G, B: Byte): integer;
    function GetColor(idx: integer): TColor; overload;
    procedure GetColor(idx: integer; var r, g, b: Byte); overload;
    procedure AddColor(Color: TColor); overload;
    procedure AddColor(R, G, B: Byte); overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPalette(Pal: array of TColor; {TOpenColorTableArray; tc } Size: integer = -1);
    function GetMappingColor(const R, G, B: Byte): TColor; overload;
    function GetMappingColor(const Color: TColor): TColor; overload;
    function GetMapping(const R, G, B: Byte): Integer; overload;
    function GetMapping(const Color: TColor): Integer; overload;
    procedure ClearPalette;
    procedure ClearMappings;
    property Bitmap: TOPBitmap read fBitmap write SetBitmap;
    property PaletteSize: integer read GetPaletteSize;
  published

  end;


  TOctreeNode = class; // Forward definition so TReducibleNodes can be declared

  TReducibleNodes = array[0..7] of TOctreeNode;

  TOctreeNode =
    class(TObject)
    IsLeaf: BOOLEAN;
    PixelCount: Integer;
    RedSum: Integer;
    GreenSum: Integer;
    BlueSum: Integer;
    Next: TOctreeNode;
    Child: TReducibleNodes;

    constructor Create(const Level: Integer;
      const ColorBits: Integer;
      var LeafCount: Integer;
      var ReducibleNodes: TReducibleNodes);
    destructor Destroy; override;

  end;

  TColorQuantizer =
    class(TOBject)
  private
    FTree: TOctreeNode;
    FLeafCount: Integer;
    FReducibleNodes: TReducibleNodes;
    FMaxColors: Integer;
    FColorBits: Integer;
  protected
    procedure AddColor(var Node: TOctreeNode;
      const r, g, b: BYTE;
      const ColorBits: Integer;
      const Level: Integer;
      var LeafCount: Integer;
      var ReducibleNodes: TReducibleNodes);
    procedure DeleteTree(var Node: TOctreeNode);
    procedure GetPaletteColors(const Node: TOctreeNode;
      var RGBQuadArray: TRGBQuadArray256;
      var Index: Integer);
    procedure ReduceTree(const ColorBits: Integer;
      var LeafCount: Integer;
      var ReducibleNodes: TReducibleNodes);

  public
    constructor Create(const MaxColors: Integer; const ColorBits: Integer);
    destructor Destroy; override;

    procedure GetColorTable(var RGBQuadArray: TRGBQuadArray256); overload;
    procedure GetColorTable(AColorTable: POpenColorTableArray); overload;
    function ProcessImage(Bmp: TOPBitmap): BOOLEAN;
    property ColorCount: Integer read FLeafCount;

  end;




function ByteSwapColor(Color: LongWord): LongWord;
function MulDiv(Number, Num, Den: Integer): Integer;
function PixelFormatFromBPP(inp: Byte): TPixelFormat;
function ColorInRange(col1, col2: TColor; Range: Byte): Boolean;


implementation

uses Math, {$IFDEF FPC}zstream{$ELSE}ZLib{$ENDIF}
 {$IFNDEF VER_VTV}, resample, ftbresenham, wincomp{$ENDIF};


{ TBitmapData }


function TBitmapData.CheckPixelValid(X, Y: integer): Boolean;
begin
  Result := (fWidth >= X) and (fHeight >= Y) and (X > -1) and (Y > -1);
  if not Result then raise EPasBitMapError.CreateFmt('Pixel coordinates out of range: X=%d Y=%d', [x, y]);
end;

constructor TBitmapData.Create(Parent: TOPBitmap);
begin
  fParent := Parent;
end;

destructor TBitmapData.Destroy;
begin
  fHeight := 0;
  fWidth := 0;
  UpdateSize;
  inherited;
end;

procedure TBitmapData.SetHeight(const Value: Integer);
begin
  fHeight := Value;
  UpdateSize;
end;

procedure TBitmapData.SetWidth(const Value: Integer);
begin
  fWidth := Value;
  UpdateSize;
end;


{ TBitmapData1 }


constructor TBitmapData1.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 1;
end;

function TBitmapData1.GetNativePixel(X, Y: Integer): Boolean;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := Boolean((fPixels^[((Y * fLineLength) + (X div 8))] shr (X mod 8)) and 1);
end;

function TBitmapData1.GetPixel(X, Y: Integer): TColor;
begin
  if not CheckPixelValid(X, Y) then exit;
  if Boolean((fPixels^[((Y * fLineLength) + (X div 8))] shr (X mod 8)) and 1) then
    Result := fParent.fColorTable[0] else
    Result := fParent.fColorTable[1];
end;

function TBitmapData1.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[(Row * fLineLength)];
end;

procedure TBitmapData1.SetNativePixel(X, Y: Integer; const Value: Boolean);
var Bt: PByte;
begin
  //if not CheckPixelValid(X, Y) then exit; {$message warn 'pixelcheck'}
  Bt := @fPixels^[(Y * fLineLength) + (X div 8)];
  if Value then
    bt^ := bt^ or (1 shl (X mod 8)) else
    bt^ := bt^ and not (1 shl (X mod 8));
end;

procedure TBitmapData1.SetPixel(X, Y: Integer; const Value: TColor);
var Bt: PByte;
  Gray: Byte;
begin
  if not CheckPixelValid(X, Y) then exit;
  Bt := @fPixels^[(Y * fLineLength) + (X div 8)];
  gray := (Byte(Value) * 77 + Byte(Value shr 8) * 151 + Byte(Value shr 16) * 28) shr 8;
  if gray < 110 then //little shift for the bright colors was 100
    bt^ := bt^ or (1 shl (X mod 8)) else
    bt^ := bt^ and not (1 shl (X mod 8));
end;


procedure TBitmapData1.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    if fWidth mod 8 > 0 then
      fLineLength := (fWidth div 8) + 1 else fLineLength := (fWidth div 8);

    if (fPixels <> nil) then FreeMem(fPixels);

    GetMem(fPixels, fHeight * fLineLength);
  end else

    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;

{ TBitmapData4 }


constructor TBitmapData4.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 4;
end;


function TBitmapData4.GetNativePixel(X, Y: Integer): Nibble;
var bt: Pixel8;
begin
  if not CheckPixelValid(X, Y) then exit;

  Bt := fPixels^[(Y * fLineLength) + (X div 2)];

  if (X mod 2 > 0) then
    Result := (Bt shr 4) and $F else
    Result := (Bt and $F);
end;

function TBitmapData4.GetPixel(X, Y: Integer): TColor;
var bt: Pixel8;
begin
  if not CheckPixelValid(X, Y) then exit;

  Bt := fPixels^[(Y * fLineLength) + (X div 2)];

  if (X mod 2 > 0) then
    Result := fParent.fColortable[(Bt shr 4) and $F] else
    Result := fParent.fColortable[(Bt and $F)]
end;

function TBitmapData4.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[(Row * fLineLength)];
end;

procedure TBitmapData4.SetNativePixel(X, Y: Integer; const Value: Nibble);
var Bt: PByte;
begin
  if not CheckPixelValid(X, Y) then exit;
  Bt := @fPixels^[(Y * fLineLength) + (X div 2)];
  if (X mod 2 > 0) then
    Bt^ := (Value shl 4) or (Bt^ and $F) else
    Bt^ := (((Bt^ shr 4) and $F) shl 4) or Value;
end;

procedure TBitmapData4.SetPixel(X, Y: Integer; const Value: TColor);
var Bt: PByte;
  Val: Integer;
  R, G, B: byte;
begin
  if not CheckPixelValid(X, Y) then exit;

  Val := -1;

  if fLastColor = Value then
  begin
    Val := fLastNearestColorIdx;
  end else
  begin
    if fParent.fPaletteHasAllColours then
      Val := fParent.GetColorIndex(Value) else
    begin
      if fParent.Reductionmode <> rmFixed then
        if CSpaceRedu > 0 then
        begin
          B := Byte(Value shr 16);
          G := Byte(Value shr 8);
          R := Byte(Value);
          Val := fParent.fColorFinder.GetMapping(R - (R mod CSpaceRedu), G - (G mod CSpaceRedu), B - (B mod CSpaceRedu))
        end else
          Val := fParent.fColorFinder.GetMapping(Value); //without color space reduction.
    end;

     //Not found in Mappings. This happens when painting after conversion with non-palette color, or with fixed palette
     //Then simply find NearestColor:
    if Val = -1 then Val := fParent.NearestColor(Value);

    fLastColor := Value;
    fLastNearestColorIdx := Val;
  end;

  Bt := @fPixels^[(Y * fLineLength) + (X div 2)];
  if (X mod 2 > 0) then
    Bt^ := Byte(Val shl 4) or (Bt^ and $F) else
    Bt^ := (((Bt^ shr 4) and $F) shl 4) or Val;
end;

procedure TBitmapData4.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    if fWidth mod 2 > 0 then
      fLineLength := (fWidth div 2) + 1 else
      fLineLength := (fWidth div 2);
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * fLineLength);
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;

{ TBitmapData8 }

constructor TBitmapData8.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 8;
end;

function TBitmapData8.GetNativePixel(X, Y: Integer): Byte;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := fPixels^[Y * fWidth + X];
end;

function TBitmapData8.GetPixel(X, Y: Integer): TColor;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := fParent.fColorTable[fPixels^[Y * fWidth + X]];
end;

function TBitmapData8.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;


procedure TBitmapData8.SetNativePixel(X, Y: Integer; const Value: Byte);
begin
  if not CheckPixelValid(X, Y) then exit;
  fPixels^[Y * fWidth + X] := Value;
end;



procedure TBitmapData8.SetPixel(X, Y: Integer; const Value: TColor);
var Val: integer;
  R, G, B: byte;
begin
  if not CheckPixelValid(X, Y) then exit;

  Val := -1;

  if fLastColor = Value then
  begin
    Val := fLastNearestColorIdx;
  end else
  begin
    if fParent.fPaletteHasAllColours then
      Val := fParent.GetColorIndex(Value) else
    begin
      if CSpaceRedu > 0 then
      begin
        B := Byte(Value shr 16);
        G := Byte(Value shr 8);
        R := Byte(Value);
        Val := fParent.fColorFinder.GetMapping(R - (R mod CSpaceRedu), G - (G mod CSpaceRedu), B - (B mod CSpaceRedu));
        //writeln('mapped');
      end else
        Val := fParent.fColorFinder.GetMapping(Value); //without color space reduction.
    end;

     //Not found in Mappings. This happens when painting after conversion with non-palette color.
     //Then simply find NearestColor:
    if Val = -1 then begin Val := fParent.NearestColor(Value); {writeln('nearest');} end;

    fLastColor := Value;
    fLastNearestColorIdx := Val;
  end;

  fPixels^[Y * fWidth + X] := Val;
end;


procedure TBitmapData8.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * fLineLength)
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;

{ TBitmapData15 }

constructor TBitmapData15.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 15;
end;

function TBitmapData15.GetPixel(X, Y: Integer): TColor;
var idx: Cardinal;
  R, G, B: Byte;
begin
  if not CheckPixelValid(X, Y) then exit;
  idx := Y * fWidth + X;
  R := (fPixels^[idx] and $7C00) shr 10;
  G := (fPixels^[idx] and $3E0) shr 5;
  B := (fPixels^[idx] and $1F);

  if (R = $1F) then R := $FF else if (R <> 0) then R := (R + 1) shl 3;
  if (G = $1F) then G := $FF else if (G <> 0) then G := (G + 1) shl 3;
  if (B = $1F) then B := $FF else if (B <> 0) then B := (B + 1) shl 3;

  Result := (B shl 16) + (G shl 8) + R;
end;


function TBitmapData15.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;


procedure TBitmapData15.SetPixel(X, Y: Integer; const Value: TColor);
var idx: Cardinal;
begin
  if not CheckPixelValid(X, Y) then exit;
  idx := Y * fWidth + X;
  fPixels^[idx] := ((Pixel32(Value).Blue shr 3) shl 10) or
    ((Pixel32(Value).Green shr 3) shl 5) or
    ((Pixel32(Value).Red shr 3) shl 0);
end;


procedure TBitmapData15.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth * 2;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * fLineLength)
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;

{ TBitmapData16 }

constructor TBitmapData16.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 16;
end;


function TBitmapData16.GetPixel(X, Y: Integer): TColor;
var idx: Cardinal;
  R, G, B: Byte;
begin
  if not CheckPixelValid(X, Y) then exit;
  idx := Y * fWidth + X;

  R := (fPixels^[idx] and $F800) shr 11;
  G := (fPixels^[idx] and $7E0) shr 5;
  B := (fPixels^[idx] and $1F);

  if (R = $1F) then R := $FF else if (R <> 0) then R := (R + 1) shl 3;
  if (G = $3F) then G := $FF else if (G <> 0) then G := (G + 1) shl 2;
  if (B = $1F) then B := $FF else if (B <> 0) then B := (B + 1) shl 3;

  Result := (B shl 16) + (G shl 8) + R;
end;

function TBitmapData16.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;

procedure TBitmapData16.SetPixel(X, Y: Integer; const Value: TColor);
var idx: Cardinal;
begin
  if not CheckPixelValid(X, Y) then exit;
  idx := Y * fWidth + X;
  fPixels^[idx] := ((Pixel32(Value).Blue shr 3) shl 11) or
    ((Pixel32(Value).Green shr 2) shl 5) or
    ((Pixel32(Value).Red shr 3) shl 0);
end;

procedure TBitmapData16.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth * 2;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * fLineLength)
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;


{ TBitmapData24 }

constructor TBitmapData24.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 24;
end;

{$IFDEF USE_MOVE}

procedure TBitmapData24.Assign(Source: TBitmapData);
var X: integer;
begin
  if Source is TBitmapData32 then
  begin
    Width := Source.Width;
    Height := Source.Height;
    if not Source.fParent.Empty then
      for X := 0 to (Width * Height) - 1 do
        Move(TBitmapData32(Source).RawArray^[X], RawArray^[X], 3);
  end;
end;
{$ENDIF}

function TBitmapData24.GetPixel(X, Y: Integer): TColor;
var pix: PPixel24;
begin
  if not CheckPixelValid(X, Y) then exit;
  pix := @fPixels^[Y * fWidth + X];
  Result := (pix^.Blue shl 16) + (pix^.Green shl 8) + pix^.Red;
end;

function TBitmapData24.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;

procedure TBitmapData24.SetPixel(X, Y: Integer; const Value: TColor);
var pix: PPixel24;
begin
  if not CheckPixelValid(X, Y) then exit;
  pix := @fPixels^[Y * fWidth + X];
  pix^.Blue := Byte(Value shr 16);
  pix^.Green := Byte(Value shr 8);
  pix^.Red := Byte(Value);
end;

procedure TBitmapData24.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth * 3;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * (fLineLength))
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;


function TBitmapData24.GetNativePixel(X, Y: Integer): Pixel24;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := fPixels^[Y * fWidth + X];
end;

procedure TBitmapData24.SetNativePixel(X, Y: Integer;
  const Value: Pixel24);
begin
  if not CheckPixelValid(X, Y) then exit;
  fPixels^[Y * fWidth + X] := Value
end;

{ TBitmapData32 }

constructor TBitmapData32.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 32;
end;

{$IFDEF USE_MOVE}

procedure TBitmapData32.Assign(Source: TBitmapData);
var X: integer;
  pix: PPixel32;
begin
  if Source is TBitmapData24 then
  begin
    Width := Source.Width;
    Height := Source.Height;
    if not Source.fParent.Empty then
      for X := 0 to (Width * Height) - 1 do
      begin
        pix := @RawArray^[X];
        Move(TBitmapData24(Source).RawArray^[X], pix^, 3);
        pix^.Alpha := AlphaOpaque;
      end;
  end;
end;
{$ENDIF}

function TBitmapData32.GetPixel(X, Y: Integer): TColor;
var pix: PPixel32;
begin
  if not CheckPixelValid(X, Y) then exit;
  pix := @fPixels^[Y * fWidth + X];
  Result := (pix^.Blue shl 16) + (pix^.Green shl 8) + pix^.Red;
end;

function TBitmapData32.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;


procedure TBitmapData32.SetPixel(X, Y: Integer; const Value: TColor);
var pix: PPixel32;
begin
  if not CheckPixelValid(X, Y) then exit;
  pix := @fPixels^[Y * fWidth + X];
  pix^.Blue := Byte(Value shr 16);
  pix^.Green := Byte(Value shr 8);
  pix^.Red := Byte(Value);
  pix^.Alpha := AlphaOpaque;
end;

procedure TBitmapData32.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth * 4;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * (fLineLength))
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;

function TBitmapData32.GetNativePixel(X, Y: Integer): Pixel32;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := fPixels^[Y * fWidth + X];
end;

procedure TBitmapData32.SetNativePixel(X, Y: Integer;
  const Value: Pixel32);
begin
  if not CheckPixelValid(X, Y) then exit;
  fPixels^[Y * fWidth + X] := Value;
end;


{ TBitmapData48 }


constructor TBitmapData48.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 48;
end;

function TBitmapData48.GetNativePixel(X, Y: Integer): Pixel48;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := fPixels^[Y * fWidth + X];
end;

function TBitmapData48.GetPixel(X, Y: Integer): TColor;
var Col: Pixel48;
begin
  if not CheckPixelValid(X, Y) then exit;
  Col := fPixels^[Y * fWidth + X];
  Result := ((Col.Red shr 8) and $FF)
    or (Col.Green and $FF00)
    or ((Col.Blue shl 8) and $FF0000);
end;

function TBitmapData48.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;

procedure TBitmapData48.SetNativePixel(X, Y: Integer;
  const Value: Pixel48);
begin
  if not CheckPixelValid(X, Y) then exit;
  fPixels^[Y * fWidth + X] := Value;
end;

procedure TBitmapData48.SetPixel(X, Y: Integer; const Value: TColor);
var col: Pixel48;
begin
  if not CheckPixelValid(X, Y) then exit;
  col.Red := (Value and $FF);
  col.Red := col.Red + (col.Red shl 8);
  col.Green := (Value and $FF00);
  col.Green := col.Green + (col.Green shr 8);
  col.Blue := (Value and $FF0000) shr 8;
  col.Blue := col.Blue + (col.Blue shr 8);
  fPixels^[Y * fWidth + X] := col;
end;

procedure TBitmapData48.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth * 6;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * fLineLength)
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;

end;


{ TBitmapData64 }


constructor TBitmapData64.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 64;
end;

function TBitmapData64.GetNativePixel(X, Y: Integer): Pixel64;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := fPixels^[Y * fWidth + X];
end;

function TBitmapData64.GetPixel(X, Y: Integer): TColor;
var Col: Pixel64;
begin
  if not CheckPixelValid(X, Y) then exit;
  Col := fPixels^[Y * fWidth + X];
  Result := ((Col.Red shr 8) and $FF)
    or (Col.Green and $FF00)
    or ((Col.Blue shl 8) and $FF0000);
end;

function TBitmapData64.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;

procedure TBitmapData64.SetNativePixel(X, Y: Integer;
  const Value: Pixel64);
begin
  if not CheckPixelValid(X, Y) then exit;
  fPixels^[Y * fWidth + X] := Value;
end;

procedure TBitmapData64.SetPixel(X, Y: Integer; const Value: TColor);
var col: Pixel64;
begin
  if not CheckPixelValid(X, Y) then exit;
  col.Red := (Value and $FF);
  col.Red := col.Red + (col.Red shl 8);
  col.Green := (Value and $FF00);
  col.Green := col.Green + (col.Green shr 8);
  col.Blue := (Value and $FF0000) shr 8;
  col.Blue := col.Blue + (col.Blue shr 8);
  col.Alpha := AlphaOpaque;
  fPixels^[Y * fWidth + X] := col;
end;

procedure TBitmapData64.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth * 8;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * fLineLength)
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;

end;

{ TOPBitmap }


constructor TOPBitmap.Create;
begin
  fData := TBitmapData32.Create(Self);
  fMask := TBitmapData1.Create(self);
  fColorTableSize := 0;
  fPaletteHasAllColours := false;
  fMonochrome := false;
  fColorFinder := TColorFinder.Create;
end;

destructor TOPBitmap.Destroy;
begin
  fColorFinder.free;

  if fMask <> nil then
  begin
    fMask.free;
    fMask := nil;
  end;

  if fData <> nil then
  begin
    fData.Free;
    fData := nil;
  end;
  inherited;
end;

function TOPBitmap.GetHeight: Integer;
begin
  if fData <> nil then Result := fData.Height else Result := 0;
end;

function TOPBitmap.GetScanLine(Row: Integer): Pointer;
begin
  if fData <> nil then Result := fData.ScanLine[Row] else Result := nil;
end;


function TOPBitmap.GetWidth: Integer;
begin
  if fData <> nil then Result := fData.Width else Result := 0;
end;

procedure TOPBitmap.SetHeight(Value: Integer);
begin
  if fData <> nil then fData.Height := Value;
end;

procedure TOPBitmap.SetWidth(Value: Integer);
begin
  if fData <> nil then fData.Width := Value;
end;


procedure TOPBitmap.SetPixel(X, Y: Integer; const AValue: TColor);
begin
  if fData <> nil then fData.SetPixel(X, Y, AValue);
end;

function TOPBitmap.GetPixel(X, Y: Integer): TColor;
begin
  if fData <> nil then Result := fData.GetPixel(X, Y) else Result := clNone;
end;


function TOPBitmap.GetColorIndex(Color: TColor): byte;
var i: integer;
begin
  Pixel32(Color).Alpha := 0;
  Result := 0;
  if Color = fLastColor then
  begin
    Result := fLastColorIndex;
    exit;
  end;
  for i := 0 to fColorTableSize - 1 do
    if fColorTable[i] = Color then
    begin
      Result := i;
      fLastColor := Color;
      fLastColorIndex := i;
      break;
    end;
end;

function TOPBitmap.GetPixelFormat: TPixelFormat;
begin
  Result := pfCustom;
  if fData <> nil then
  begin
    case fData.BPP of
      1: Result := pf1bit;
      4: Result := pf4bit;
      8: Result := pf8bit;
      15: Result := pf15bit;
      16: Result := pf16bit;
      24: Result := pf24bit;
      32: Result := pf32bit;
      48: Result := pf48bit;
      64: Result := pf64bit;
    end;
  end;
end;

function TOPBitmap.GetEmpty: Boolean;
begin
  Result := (Width < 1) or (Height < 1);
end;

procedure TOPBitmap.SetPixelFormat(const Value: TPixelFormat);
begin
// This is an ugly hack. Because we have only one ColorTable in current design,
// we have to make a non paletted format first in case of potential palette to palette reduction.
// Happens only in 8 to 4 or 8 to 1 or 4 to 1 bit reduction.
// But shouldn't be extremely slow and I'll try to change that later using local palettes.
  if (PixelFormat <= pf8bit) and (Value < PixelFormat) then DoSetPixelFormat(pf24bit);

  DoSetPixelFormat(Value);
end;

procedure TOPBitmap.DoSetPixelFormat(const Value: TPixelFormat);
var Temp: TBitmapData;
  X, Y: Cardinal;
  CT: TOpenColorTableArray;
  OptPalette: Boolean;
  cq: TColorQuantizer;
begin
  if Value <> PixelFormat then
  begin
    OptPalette := false;
    Temp := fData;
    fAlphaBlend := false;
    case Value of
      pf1bit: begin fData := TBitmapData1.Create(self); CopyFromColorTable(BWColors); end;
      pf4bit: begin
          if not Temp.fParent.Empty then
          begin
            SetLength(CT, 16);
          //if coming from lower bpp just copy palette
            if (Temp.fParent.PixelFormat < Value) and (Temp.fParent.ColorTableSize > 0) then
            begin
              CT[0] := Temp.fParent.fColorTable[0];
              CT[1] := Temp.fParent.fColorTable[1];
              OptPalette := True;
            end else
              //Try to make optimized palette on original Data
              OptPalette := MakePalette($F, CT);
            if OptPalette then
            begin
              CopyFromColorTable(CT, false);
              fPaletteHasAllColours := true;
            end;

            if not OptPalette then
            begin
                //If FixedPalette selected
              if fReductionMode = rmFixed then
              begin
                CopyFromColorTable(StdColors, false);
                fPaletteHasAllColours := false;
              end;
                //Make Optimal Reduction.
              if fReductionMode = rmOptimized then
              begin
                cq := TColorQuantizer.Create(16, 4);
                cq.ProcessImage(self);
                cq.GetColorTable(@CT);
                CopyFromColorTable(CT, true, cq.ColorCount);
                fColorFinder.SetPalette(fColorTable, cq.ColorCount);
                fColorFinder.Bitmap := self;
                cq.free;
                fPaletteHasAllColours := false;
              end;
            end;
          end;
          fData := TBitmapData4.Create(self);
        end;
      pf8bit: begin
          //if coming from lower bpp just copy palette
          SetLength(CT, 256);
          if not Temp.fParent.Empty then
          begin
            if (Temp.fParent.PixelFormat < Value) and (Temp.fParent.ColorTableSize > 0) then
            begin
              OptPalette := True;
             // CopyFromColorTable(CT, false, Temp.fParent.fColorTableSize); {$message warn'testen'};
              fPaletteHasAllColours := True;
            end else
            begin
              //Try to make optimized palette on original Data.
              OptPalette := MakePalette($FF, CT);
              if OptPalette then
              begin
                CopyFromColorTable(CT, false);
                fPaletteHasAllColours := True;
              end;
            end;
            if not OptPalette then
            begin
              //If FixedPalette selected
              if fReductionMode = rmFixed then
              begin
                ShrinkPaletteWeb;
                CopyFromColorTable(WebColors, false);
                fPaletteHasAllColours := true;
              end;
              //Make Optimal Reduction.
              if fReductionMode = rmOptimized then
              begin
                cq := TColorQuantizer.Create(256, 8);
                cq.ProcessImage(self);
                cq.GetColorTable(@CT);
                CopyFromColorTable(CT, true, cq.ColorCount);
                fColorFinder.SetPalette(fColorTable, cq.ColorCount);
                fColorFinder.Bitmap := self;
                cq.free;
                fPaletteHasAllColours := false;
              end;
            end;
          end else OptPalette := false;

          fData := TBitmapData8.Create(self);
        end;
      pf15bit: fData := TBitmapData15.Create(self);
      pf16bit: fData := TBitmapData16.Create(self);
      pf24bit: fData := TBitmapData24.Create(self);
      pf32bit: fData := TBitmapData32.Create(self);
      pf48bit: fData := TBitmapData48.Create(self);
      pf64bit: fData := TBitmapData64.Create(self);
      pfCustom, pfDevice: fData := TBitmapData32.Create(self);
    else raise EPasBitMapError.CreateFmt('Pixelformat not supported: Ordinal %d', [Ord(Value)]);
    end;
{$IFDEF USE_MOVE}
    if (Temp.BPP = 24) and (fData.BPP = 32) then TBitmapData32(fData).Assign(Temp) else //Max speed for these.
      if (Temp.BPP = 32) and (fData.BPP = 24) then TBitmapData24(fData).Assign(Temp) else
{$ENDIF}
      begin
        fData.Width := Temp.Width;
        fData.Height := Temp.Height;
        if not Temp.fParent.Empty then
          for y := 0 to Temp.Height - 1 do
            for x := 0 to Temp.Width - 1 do
              fData.Pixels[X, Y] := Temp.Pixels[X, Y];
      end;
    Temp.free;
  end;
  fPaletteHasAllColours := False;
end;


procedure TOPBitmap.ShrinkPaletteWeb; //Web Color Reduction
var X, Y: Cardinal;
  tpix: Pixel32;

  function _WebMatch(inp: Byte): Byte;
  var diff: byte;
  begin
    diff := (inp mod $33);
    if (diff < $19) then
      Result := inp - diff else
      Result := inp - diff + $33;
  end;

begin
  for y := 0 to fData.Height - 1 do
    for x := 0 to fData.Width - 1 do
    begin
      tpix := Pixel32(fData.Pixels[X, Y]);
      tpix.Red := _WebMatch(tpix.Red);
      tpix.Green := _WebMatch(tpix.Green);
      tpix.Blue := _WebMatch(tpix.Blue);
      tpix.Alpha := 0;
      fData.Pixels[X, Y] := Cardinal(tpix);
    end;
end;

procedure TOPBitmap.CopyFromColorTable(AColorTable: array of TColor; Swap: Boolean = true; Size: integer = -1);
var i: integer;
begin
  if Size > -1 then
    fColorTableSize := Size else
    fColorTableSize := High(AColorTable) + 1;
  if Swap then
    for i := 0 to fColorTableSize - 1 do fColorTable[i] := ByteSwapColor(AColorTable[i]) else
    for i := 0 to fColorTableSize - 1 do fColorTable[i] := AColorTable[i];
  fLastColor := clNone;
end;

function TOPBitmap.GetColorTable: PColorTableArray;
begin
  Result := @fColorTable;
end;

procedure TOPBitmap.SetColorTable(const AValue: PColorTableArray);
begin
  fColorTable := AValue^;
end;

function TOPBitmap.GetDataSize: Cardinal;
begin
  Result := Height * fData.fLineLength;
end;


function TOPBitmap.GetBPP: byte;
begin
  Result := 0;
  if fData <> nil then
    if fData.fBPP = 15 then Result := 16 else Result := fData.fBPP;
end;


function TOPBitmap.NearestColor(const color: TColor): Cardinal;

var
  DistanceSquared: INTEGER;
  B1, B2: Byte;
  G1, G2: Byte;
  i: INTEGER;
  R1, R2: Byte;
  SmallestDistanceSquared: INTEGER;
  col: TColor;
begin
  Result := 0;
  SmallestDistanceSquared := $1000000;


  R1 := Byte(Color);
  G1 := Byte(Color shr 8);
  B1 := Byte(Color shr 16);


  for i := 0 to fColorTableSize - 1 do
  begin

    col := fColorTable[i];

    R2 := Byte(col);
    G2 := Byte(col shr 8);
    B2 := Byte(col shr 16);

    DistanceSquared := (R1 - R2) * (R1 - R2) + (G1 - G2) * (G1 - G2) + (B1 - B2) * (B1 - B2);

    if DistanceSquared < SmallestDistanceSquared then
    begin
      Result := i;
      if Col = Color then exit;
      SmallestDistanceSquared := DistanceSquared;
    end
  end;
end;


function TOPBitmap.CountColors(Max: Integer): Integer;
var
  x, y: Cardinal;
  i, j: Cardinal;
  Red, Green, Blue: Byte;
begin
  RESULT := 0;
  for j := 0 to $FF do
    for i := 0 to $FF do
      Flags[i, j] := nil;

  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
    begin
      Red := Pixels[x, y];
      Green := (Pixels[x, y] shr 8);
      Blue := (Pixels[x, y] shr 16);
      if (Flags[Red, Green]) = nil then
      begin
        Flags[Red, Green] := Classes.TBits.Create;
        Flags[Red, Green].Size := 256;
      end;
      if not Flags[Red, Green].Bits[Blue] then
      begin
        Flags[Red, Green].Bits[Blue] := TRUE;
        if Result = Max - 1 then
        begin
          Result := -1;
          exit;
        end;
        Inc(Result);
      end;
    end;

  for j := 0 to $FF do
    for i := 0 to $FF do
      if Assigned(Flags[i, j]) then Flags[i, j].Free;
end;




function TOPBitmap.MakePalette(Size: Byte; var ColorTable: TOpenColorTableArray): Boolean;
var
  x, y: Cardinal;
  i, j: Cardinal;
  Red, Green, Blue: Byte;
  Cnt: word;
begin
  Result := false;

  for j := 0 to $FF do
    for i := 0 to $FF do
      Flags[i, j] := nil;

  for i := 0 to Size do ColorTable[i] := 0;

  Cnt := 0;

  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
    begin
      Red := Byte(Pixels[x, y]);
      Green := Byte(Pixels[x, y] shr 8);
      Blue := Byte(Pixels[x, y] shr 16);
      if (Flags[Red, Green]) = nil then
      begin

        Flags[Red, Green] := Classes.TBits.Create;
        Flags[Red, Green].Size := 256;
      end;

      if not Flags[Red, Green].Bits[Blue] then
      begin
        ColorTable[Cnt] := Pixels[x, y];
        if Cnt = Size then
        begin
          exit;
        end;
        inc(Cnt);

        Flags[Red, Green].Bits[Blue] := TRUE
      end;
    end;

  for j := 0 to $FF do
    for i := 0 to $FF do
      if Assigned(Flags[i, j]) then Flags[i, j].Free;

  Result := True;
end;


function TOPBitmap.GetHandle: THandle;
begin
  Result := THandle(Self);
end;

procedure TOPBitmap.SetHandle(const Value: THandle);
begin
 //just for compatibility
 // raise EPasBitMapError.Create('Attempt to SetHandle');
end;

function TOPBitmap.GetPalette: THandle;
begin
  Result := THandle(@fColorTable);
end;

{$IFNDEF VER_VTV}
procedure TOPBitmap.SetPalette(const Value: THandle);
var PaletteH: HPalette;
var i: integer;
begin
  PaletteH := Value;
  for i := 0 to PMaxLogPalette(PaletteH)^.palNumEntries - 1 do
    ColorTable^[i] := (PMaxLogPalette(PaletteH)^.palPalEntry[i].peBlue shl 16) +
      (PMaxLogPalette(PaletteH)^.palPalEntry[i].peGreen shl 8) +
      PMaxLogPalette(PaletteH)^.palPalEntry[i].peRed;
  fColorTableSize:=PMaxLogPalette(PaletteH)^.palNumEntries; //14.2.
end;
{$ENDIF}

procedure TOPBitmap.Assign(Source: TPersistent);
var x, y: integer;
begin
  if Source is TOPBitmap then
  begin
    Width := 0; //Don't convert;
    PixelFormat := TOPBitmap(Source).PixelFormat;
    if not TOPBitmap(Source).Empty then
    begin
      if TOPBitmap(Source).fColorTableSize > 0 then
        CopyFromColorTable(TOPBitmap(Source).fColorTable, false, TOPBitmap(Source).fColorTableSize);
      Width := TOPBitmap(Source).Width;
      Height := TOPBitmap(Source).Height;
      if TOPBitmap(Source).Transparent then
        TransparentColor := TOPBitmap(Source).TransparentColor else Transparent := false;

{$IFDEF USE_MOVE}
      Move(TOPBitmap(Source).Scanline[0]^, Scanline[0]^, Height * TOPBitmap(Source).fData.fLineLength); //Todo Check
{$ELSE}
      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
          Pixels[x, y] := TOPBitmap(Source).Pixels[x, y];
{$ENDIF}

    end;
  end
  else
    inherited Assign(Source);
end;

function PixelFormatFromBPP(inp: Byte): TPixelFormat;
begin
  case inp of
    64: Result := pf64bit;
    48: Result := pf48bit;
    32: Result := pf32bit;
    24: Result := pf24bit;
    16: Result := pf16bit;
    15: Result := pf15bit;
    8: Result := pf8bit;
    4: Result := pf4bit;
    1: Result := pf1bit;
  end;
end;

procedure TOPBitmap.SetMonochrome(const Value: Boolean);
var x, y: integer;
  gray: Byte;
  col: TColor;
  OrigPixelFormat:TPixelFormat;
begin
//  if not fMonochrome then
  begin
    OrigPixelFormat:=PixelFormat;
    if PixelFormat<pf15bit then PixelFormat:=pf24bit;
    fMonochrome := True;
    for y := 0 to Height - 1 do
      for x := 0 to Width - 1 do
      begin
        col := Pixels[x, y];
        gray := (Byte(col) * 77 + Byte(col shr 8) * 151 + Byte(col shr 16) * 28) shr 8;
        Pixels[x, y] := gray shl 16 + gray shl 8 + gray;
      end;
    PixelFormat:=OrigPixelFormat;
  end;
end;


{
straightforward compressed image stream format for testing
Prefer standard formats!
}

procedure TOPBitmap.SaveToStream(Stream: TStream);
var TCS: TCompressionStream;
  Header: TOPBitmapStreamHeader;
  ds: Cardinal;
  i: integer;
  Sign: array[0..2] of Char;
begin
  with Header do
  begin
    Version := TOPBitmapStreamVersion;
    BPP := fData.fBPP;
    Width := Self.Width;
    Height := Self.Height;
    PPI := 100;
    Compressed := true;
    Transparent := Self.Transparent;
    TransparentColor := Self.TransparentColor;
  end;
  Stream.Position := 0;
  Sign := TOPBitmapStreamSign;
  Stream.Write(sign, 3);
  Stream.Write(Header, SizeOf(TOPBitmapStreamHeader));

  TCS := TCompressionStream.Create({$IFDEF FPC}zstream.clDefault{$ELSE}ZLib.clDefault{$ENDIF}, Stream);
  try
    if Header.BPP <= 8 then
    begin
      TCS.Write(fColorTableSize, SizeOf(Word));

      for i := 0 to fColorTableSize - 1 do
        TCS.Write(fColorTable[i], sizeOf(TColor));
    end;

    ds := GetDataSize;
    TCS.Write(ds, SizeOf(Cardinal));
    TCS.Write(Data.ScanLine[0]^, ds);
  finally
    TCS.Free;
  end;
  Stream.Position := 0;
end;


procedure TOPBitmap.LoadFromStream(Stream: TStream);
var sign: array[0..2] of Char;
  Header: TOPBitmapStreamHeader;
  TDS: TDecompressionStream;
  cts: Word;
  ds: Cardinal;
  i: integer;
begin
  Stream.Read(sign, 3);
  if sign = TOPBitmapStreamSign then
  begin
    Stream.Read(Header, SizeOf(TOPBitmapStreamHeader));
    if Header.Version = TOPBitmapStreamVersion then
    begin
      Width := 0;
      PixelFormat := PixelFormatFromBPP(Header.BPP);
      Width := Header.width;
      Height := Header.height;
      Transparent := Header.Transparent;
      if Transparent then TransparentColor := Header.TransparentColor;

      TDS := TDecompressionStream.Create(Stream);
      try
        if Header.BPP <= 8 then
        begin
          TDS.Read(cts, SizeOf(Word));
          fColorTableSize := cts;
          for i := 0 to Min(cts - 1, High(fColorTable)) do
            TDS.Read(fColorTable[i], sizeOf(TColor));
        end;

        TDS.Read(ds, SizeOf(Cardinal));
        TDS.Read(Data.ScanLine[0]^, ds);
      finally
        TDS.Free;
      end;
      Stream.Position := 0;
    end else raise EPasBitMapError.Create('Unsupported OPBitmap Stream Version');
  end else raise EPasBitMapError.Create('Not an OPBitmap Stream');
end;




procedure TOPBitmap.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmShareDenyNone or fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TOPBitmap.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  if Empty then raise EPasBitMapError.Create('OPBitmap empty');
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{$IFNDEF IMPORTTGRAPHIC}

procedure TOPBitmap.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string {;
  var DoContinue: Boolean});
begin
  //Todo: Progress
end;

{$ENDIF}



procedure TOPBitmap.SetTransparentColor(const Value: TColor);
begin
  fTransparentColor := Value;
  fTransparent := True;
end;

function TOPBitmap.GetTransparent: Boolean;
begin
  Result := FTransparent;
end;

procedure TOPBitmap.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
{$IFDEF IMPORTTGRAPHIC}Changed(Self); {$ENDIF}
  end;
end;

procedure TOPBitmap.SetAlpha(Value: Byte);
var x, y: integer;
  Pix: PPixel32;
begin
  if PixelFormat = pf32bit then
  begin
    if Transparent then
    begin
      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
          if Pixels[X, Y] = TransparentColor then
          begin
            pix := @TBitmapData32(Self.fData).fPixels^[Y * Width + X];
            pix^.Alpha := Value;
          end

    end else //if Transparent
      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
        begin
          pix := @TBitmapData32(Self.fData).fPixels^[Y * Width + X];
          pix^.Alpha := Value;
        end;
  end;
  AlphaBlend := true;
end;

procedure TOPBitmap.SetAlphaBlend(const Value: Boolean);
begin
  if PixelFormat = pf32bit then
    fAlphaBlend := Value else
    fAlphaBlend := false;
end;

procedure TOPBitmap.Clear;
begin
  Width := 0;
  Height := 0;
  fColorTableSize := 0;
  Transparent := false;
end;



function ReverseBits(b: Byte): Byte;
var c: Byte;
begin
  c := b;
  c := ((c shr 1) and $55) or ((c shl 1) and $AA);
  c := ((c shr 2) and $33) or ((c shl 2) and $CC);
  c := ((c shr 4) and $0F) or ((c shl 4) and $F0);
  result := c;
end;


function GetBitsPerLine(Width, BitsPerPixel: cardinal;
                        LineEnd: TOpRawImageLineEnd): PtrUInt;
var
  BitsPerLine: PtrUInt;
begin
  BitsPerLine:=Width*BitsPerPixel;
  case LineEnd of
  rileTight: ;
  rileByteBoundary:  BitsPerLine:=(BitsPerLine+7) and not cardinal(7);
  rileWordBoundary:  BitsPerLine:=(BitsPerLine+15) and not cardinal(15);
  rileDWordBoundary: BitsPerLine:=(BitsPerLine+31) and not cardinal(31);
  rileQWordBoundary: BitsPerLine:=(BitsPerLine+63) and not cardinal(63);
  end;
  Result:=BitsPerLine;
end;


function TOPBitmap.GetTransparentMask(Tolerance: Byte; var Data: PByte;
  ReversedBits: Boolean; Boundary:TOpRawImageLineEnd): integer;
var x, y, i, cnt, aLineLength: integer;
begin
  if not Empty then
  begin

    fMask.Width:=GetBitsPerLine(Width,1,Boundary);

    fMask.Height := Height;

    cnt := 0;
    if Tolerance = 0 then
    begin

      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
          if Pixels[x, y] = fTransparentColor then
            fMask.SetNativePixel(x, y, false) else
            fMask.SetNativePixel(x, y, true);

    end else
    begin

      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
          if ColorInRange(Pixels[x, y], fTransparentColor, Tolerance) then
            fMask.SetNativePixel(x, y, false) else
            fMask.SetNativePixel(x, y, true);
    end;

    if ReversedBits then
      for i := 0 to fMask.fLineLength * fMask.Height do
        fMask.RawArray^[i] := ReverseBits(fMask.RawArray^[i]);

    Data := PByte(fMask.fPixels);
    Result := fMask.Height * fMask.LineLength;
  end;
end;

function TOPBitmap.GetFullMask(var Data: PByte): integer;
var x, y, i: integer;
begin
  if not Empty then
  begin
    fMask.Width := Width;
    fMask.Height := Height;

    for i := 0 to (fMask.fLineLength) * fMask.Height do fMask.RawArray^[i] := $FF;

    Data := PByte(fMask.fPixels);
    Result := fMask.Height * fMask.LineLength;
  end;
end;



{TOPBitmapCanvas}

constructor TOPBitmapCanvas.Create(Bitmap: TOPBitmap);
begin
  inherited Create;
  fBitmap := Bitmap;
  fBrush := TBrush.Create;
  fPen := TPen.Create;
end;

destructor TOPBitmapCanvas.Destroy;
begin
  fPen.free;
  fBrush.free;
  inherited;
end;


function TOPBitmapCanvas.GetPixel(X, Y: Integer): TColor;
begin
  if fBitmap.Data <> nil then Result := fBitmap.Data.Pixels[X, Y] else Result := clNone;
end;

procedure TOPBitmapCanvas.SetPixel(X, Y: Integer; const Value: TColor);
var NewCol: TColor;
begin
  if fBitmap.Data <> nil then
  begin
    fBitmap.Data.Pixels[X, Y] := Value;
  end;
end;

procedure TOPBitmapCanvas.FillRect(Rect: TRect);
var i, j: integer;
  Color: TColor;
begin
  Color := fBrush.Color;
  for i := Rect.Top to Rect.Bottom - 1 do
    for j := Rect.Left to Rect.Right - 1 do
      fBitmap.Data.Pixels[j, i] := Color;
end;

{$IFNDEF VER_VTV}
procedure TOPBitmapCanvas.LineTo(X, Y: Integer);
begin
  BresenhamLine(fPenPos.X, fPenPos.Y, X, Y, self, fPen.Color);
  MoveTo(X, Y);
end;
{$ENDIF}

procedure TOPBitmapCanvas.MoveTo(X, Y: Integer);
begin
  fPenPos.X := X;
  fPenPos.Y := Y;
end;

{$IFNDEF VER_VTV}
procedure TOPBitmapCanvas.Circle(CenterX, CenterY, Radius: Integer);
var X, Y: integer;
begin
  BresenhamCircle(CenterX, CenterY, Radius, self, fPen.Color);
end;
{$ENDIF}

procedure TOPBitmapCanvas.Draw(X, Y: integer; Bitmap: TCanvasOPBitmap);
var wid, hei: integer;
begin
  wid := Bitmap.Width;
  hei := Bitmap.Height;
  CopyRect(Rect(X, Y, X + wid, Y + hei), Bitmap.Canvas, Rect(0, 0, wid, hei));
end;


procedure BlendColors(SPix, DPix: PPixel32);
var alp1, alp2: integer;
begin
  if SPix^.Alpha = AlphaTransparent then exit else
    if SPix^.Alpha = AlphaOpaque then
      DPix^ := SPix^ else
    begin
      alp1 := SPix^.Alpha;
      alp2 := $FF - alp1;
      DPix^.Red := (DPix^.Red * alp2 + SPix^.Red * alp1) div $FF;
      DPix^.Green := (DPix^.Green * alp2 + SPix^.Green * alp1) div $FF;
      DPix^.Blue := (DPix^.Blue * alp2 + SPix^.Blue * alp1) div $FF;
    end;
end;


procedure TOPBitmapCanvas.CopyRect(const Dest: TRect; Canvas: TOPBitmapCanvas;
  const Source: TRect);
var Wid, Hei, x, y: integer;
  S, D: TRect;
  sp: TColor;


  procedure AdjustRect(var Rec: TRect; Width, Height: integer; Src: Boolean);
  begin

    if Rec.Left < 0 then
    begin
      if Src then Dec(D.Left, Rec.Left) else Dec(S.Left, Rec.Left);
      Rec.Left := 0;
    end;

    if Rec.Right > Width then Rec.Right := Width;

    if Rec.Top < 0 then
    begin
      if Src then Dec(D.Top, Rec.Top) else Dec(S.Top, Rec.Top);
      Rec.Top := 0;
    end;

    if Rec.Bottom > Height then Rec.Bottom := Height;
  end;

begin
  S := Source;
  D := Dest;

  AdjustRect(S, Canvas.fBitmap.Width, Canvas.fBitmap.Height, true);
  AdjustRect(D, fBitmap.Width, fBitmap.Height, false);

  Wid := Min(D.Right - D.Left, S.Right - S.Left);
  Hei := Min(D.Bottom - D.Top, S.Bottom - S.Top);

  if Canvas.fBitmap.fAlphaBlend then
  begin
    Assert(Canvas.fBitmap.PixelFormat = pf32bit, 'alphablend with 32 BPP only');
    fBitmap.PixelFormat := pf32bit;
    for y := 0 to Hei - 1 do
      for x := 0 to Wid - 1 do
      begin
        BlendColors(@TBitmapData32(Canvas.fBitmap.fData).fPixels^[(y + S.Top) * Canvas.fBitmap.Width + (S.Left + x)],
          @TBitmapData32(fBitmap.fData).fPixels^[(y + D.Top) * fBitmap.Width + (D.Left + x)]);
      end;
  end
  else
    if Canvas.fBitmap.Transparent then
      for y := 0 to Hei - 1 do
        for x := 0 to Wid - 1 do
        begin
          sp := Canvas.fBitmap.Pixels[S.Left + x, y + S.Top];
          if sp <> Canvas.fBitmap.TransparentColor then
            fBitmap.Pixels[D.Left + x, y + D.Top] := sp;
        end
    else
      for y := 0 to Hei - 1 do
        for x := 0 to Wid - 1 do
          fBitmap.Pixels[D.Left + x, y + D.Top] := Canvas.fBitmap.Pixels[S.Left + x, y + S.Top];
end;

{$IFNDEF VER_VTV}
procedure TOPBitmapCanvas.Resample(NewWidth, NewHeight: integer);
begin
  if NewWidth < fBitmap.Width then
    Stretch(NewWidth, NewHeight, sfHermite, DefaultFilterRadius[sfHermite], fBitmap) else
    Stretch(NewWidth, NewHeight, sfMitchell, DefaultFilterRadius[sfMitchell], fBitmap);

end;
{$ENDIF}

{ TCanvasOPBitmap }

constructor TCanvasOPBitmap.Create;
begin
  inherited;
  fCanvas := TOPBitmapCanvas.Create(Self);
end;

destructor TCanvasOPBitmap.Destroy;
begin
  fCanvas.free;
  inherited;
end;

{ TColorFinder }

procedure TColorFinder.AddColor(R, G, B: Byte);
var Col: PColorEntry;
begin
  GetMem(Col, SizeOf(TColorEntry));
  Col^.R := R;
  Col^.G := G;
  Col^.B := B;
  fPalette.Add(Col);
  fSorted := false;
end;


procedure TColorFinder.AddColor(Color: TColor);
begin
  AddColor(Byte(Color), Byte(Color shr 8), Byte(Color shr 16));
end;

procedure TColorFinder.ClearPalette;
var i: integer;
begin
  for i := 0 to fPalette.Count - 1 do
    FreeMem(fPalette[i], SizeOf(TColorEntry));
  fPalette.Clear;
  ClearMappings;
end;

constructor TColorFinder.Create;
begin
  fPalette := TList.create;
end;

destructor TColorFinder.Destroy;
begin
  ClearPalette;
  fPalette.free;
  inherited;
end;


function TColorFinder.NearestColor(R, G, B: Byte): integer;

var
  DistanceSquared: INTEGER;
  R1, G1, B1: Byte;
  i: INTEGER;
  SmallestDistanceSquared: INTEGER;
  col: TColor;
begin
  Result := 0;
  SmallestDistanceSquared := $1000000;


  for i := 0 to fPalette.Count - 1 do
  begin
    R1 := PColorEntry(fPalette[i])^.R;
    G1 := PColorEntry(fPalette[i])^.G;
    B1 := PColorEntry(fPalette[i])^.B;
    DistanceSquared := (R - R1) * (R - R1) + (G - G1) * (G - G1) + (B - B1) * (B - B1);
    if DistanceSquared < SmallestDistanceSquared then
    begin
      Result := i;
      if (R = R1) and (G = G1) and (B = B1) then exit;
      SmallestDistanceSquared := DistanceSquared;
    end
  end;
end;


function TColorFinder.GetColor(idx: integer): TColor;
var r, g, b: Byte;
begin
  GetColor(idx, r, g, b);
  Result := b shl 16 + g shl 8 + r;
end;

procedure TColorFinder.GetColor(idx: integer; var r, g, b: Byte);
begin
  if (idx < fPalette.Count) and (idx > -1) then
  begin
    R := PColorEntry(fPalette[idx])^.R;
    G := PColorEntry(fPalette[idx])^.G;
    B := PColorEntry(fPalette[idx])^.B;
  end;
end;


function TColorFinder.MapColors: Integer;
var
  x, y: Cardinal;
  i, j: Cardinal;
  Red, Green, Blue: Byte;
  Pcol: PInteger;
  Color: TColor;
begin
  Result := 0;
  ClearMappings;

  for y := 0 to fBitmap.Height - 1 do
    for x := 0 to fBitmap.Width - 1 do
    begin
      Color := fBitmap.Pixels[x, y];

      Red := Byte(Color);
      Green := Byte(Color shr 8);
      Blue := Byte(Color shr 16);

      //Small reduction of color space
      if CSpaceRedu > 0 then
      begin
        Dec(Red, Red mod CSpaceRedu);
        Dec(Green, Green mod CSpaceRedu);
        Dec(Blue, Blue mod CSpaceRedu);
      end;

      if (fMappings[Red, Green]) = nil then
      begin
        fMappings[Red, Green] := TList.Create;
        fMappings[Red, Green].Count := 256;
      end;
      if (fMappings[Red, Green].Items[Blue] = nil) then
      begin
        GetMem(Pcol, SizeOf(Integer));
        PCol^ := NearestColor(Red, Green, Blue);
        fMappings[Red, Green].Items[Blue] := PCol;
        Inc(Result);
      end;
    end;
end;

procedure TColorFinder.ClearMappings;
var i, j, k: Integer;
begin

  for j := 0 to $FF do
    for i := 0 to $FF do
    begin
      if Assigned(fMappings[i, j]) then
      begin
        for k := 0 to $FF do
          FreeMem(fMappings[i, j].Items[k], SizeOf(TColor));
        fMappings[i, j].Free;
      end;
      fMappings[i, j] := nil;
    end;
end;


function TColorFinder.GetMappingColor(const R, G, B: Byte): TColor;
begin
  Result := GetColor(GetMapping(R, G, B));
end;


function TColorFinder.GetMapping(const R, G, B: Byte): Integer;
var PCol: PInteger;
begin
  Result := -1;
  if fMappings[R, G] <> nil then
  begin
    PCol := fMappings[R, G].Items[B];
    if PCol <> nil then Result := PCol^;
  end;
end;

function TColorFinder.GetMappingColor(const Color: TColor): TColor;
begin
  Result := GetColor(GetMapping(Color));
end;

function TColorFinder.GetMapping(const Color: TColor): Integer;
begin
  Result := GetMapping(Color, Color shr 8, Color shr 16);
end;

procedure TColorFinder.SetBitmap(const Value: TOPBitmap);
begin
  if Value <> nil then
  begin
    fBitmap := Value;
    MapColors;
  end;
end;

procedure TColorFinder.SetPalette(Pal: array of TColor; Size: integer);
var PalSize, i: integer;
begin
  ClearPalette;
  if Size <> -1 then PalSize := Size else PalSize := High(Pal) + 1;
  for i := 0 to PalSize - 1 do AddColor(Pal[i]);
  if fBitmap <> nil then MapColors;
end;

function TColorFinder.GetPaletteSize: integer;
begin
  Result := fPalette.Count;
end;

{TOctreeNode}

constructor TOctreeNode.Create(const Level: Integer;
  const ColorBits: Integer;
  var LeafCount: Integer;
  var ReducibleNodes: TReducibleNodes);
var
  i: Integer;
begin
  PixelCount := 0;
  RedSum := 0;
  GreenSum := 0;
  BlueSum := 0;
  for i := Low(Child) to High(Child) do
    Child[i] := nil;

  IsLeaf := (Level = ColorBits);
  if IsLeaf
    then begin
    Next := nil;
    Inc(LeafCount);
  end
  else begin
    Next := ReducibleNodes[Level];
    ReducibleNodes[Level] := Self;
  end
end;


destructor TOctreeNode.Destroy;
var
  i: Integer;
begin
  for i := Low(Child) to High(Child) do
    Child[i].Free
end;


{TColorQuantizer}

constructor TColorQuantizer.Create(const MaxColors: Integer; const ColorBits: Integer);
var
  i: Integer;
begin
  Assert(ColorBits <= 8);

  FTree := nil;
  FLeafCount := 0;
  for i := Low(FReducibleNodes) to High(FReducibleNodes) do
    FReducibleNodes[i] := nil;

  FMaxColors := MaxColors;
  FColorBits := ColorBits
end;


destructor TColorQuantizer.Destroy;
begin
  if FTree <> nil
    then DeleteTree(FTree)
end;


procedure TColorQuantizer.GetColorTable(var RGBQuadArray: TRGBQuadArray256);
var
  Index: Integer;
begin
  Index := 0;
  GetPaletteColors(FTree, RGBQuadArray, Index)
end;



function TColorQuantizer.ProcessImage(Bmp: TOPBitmap): Boolean;
var
  col: TColor;
  i: Integer;
  j: Integer;
begin
  Result := True;
  if Bmp.GetDataSize > 0 then
  begin
    for j := 0 to Bmp.Height - 1 do
    begin
      for i := 0 to Bmp.Width - 1 do
      begin
        col := Bmp.Data.Pixels[i, j];
        AddColor(FTree, Byte(col), Byte(col shr 8), Byte(col shr 16),
          FColorBits, 0, FLeafCount, FReducibleNodes);
        while FLeafCount > FMaxColors do
          ReduceTree(FColorbits, FLeafCount, FReducibleNodes)
      end;
    end;
  end;
end;


procedure TColorQuantizer.AddColor(var Node: TOctreeNode;
  const r, g, b: Byte;
  const ColorBits: Integer;
  const Level: Integer;
  var LeafCount: Integer;
  var ReducibleNodes: TReducibleNodes);
const
  Mask: array[0..7] of Byte = ($80, $40, $20, $10, $08, $04, $02, $01);

var
  Index: Integer;
  Shift: Integer;
begin
  if Node = nil
    then Node := TOctreeNode.Create(Level, ColorBits, LeafCount, ReducibleNodes);

  if Node.IsLeaf
    then begin
    Inc(Node.PixelCount);
    Inc(Node.RedSum, r);
    Inc(Node.GreenSum, g);
    Inc(Node.BlueSum, b)
  end
  else begin
    Shift := 7 - Level;
    Index := (((r and mask[Level]) shr Shift) shl 2) or
      (((g and mask[Level]) shr Shift) shl 1) or
      ((b and mask[Level]) shr Shift);
    AddColor(Node.Child[Index], r, g, b, ColorBits, Level + 1,
      LeafCount, ReducibleNodes)
  end
end;



procedure TColorQuantizer.DeleteTree(var Node: TOctreeNode);
var
  i: Integer;
begin
  for i := Low(TReducibleNodes) to High(TReducibleNodes) do
  begin
    if Node.Child[i] <> nil
      then DeleteTree(Node.Child[i]);
  end;

  Node.Free;
  Node := nil;
end;


procedure TColorQuantizer.GetPaletteColors(const Node: TOctreeNode;
  var RGBQuadArray: TRGBQuadArray256;
  var Index: Integer);
var
  i: Integer;
begin
  if Node.IsLeaf
    then begin
    with RGBQuadArray[Index] do
    begin
      try
        rgbRed := Byte(Node.RedSum div Node.PixelCount);
        rgbGreen := Byte(Node.GreenSum div Node.PixelCount);
        rgbBlue := Byte(Node.BlueSum div Node.PixelCount);
        rgbReserved := 0;
      except
        rgbRed := 0;
        rgbGreen := 0;
        rgbBlue := 0;
        rgbReserved := 0;
      end;

      rgbReserved := 0
    end;
    INC(Index)
  end
  else begin
    for i := Low(Node.Child) to High(Node.Child) do
    begin
      if Node.Child[i] <> nil
        then GetPaletteColors(Node.Child[i], RGBQuadArray, Index)
    end
  end
end;


procedure TColorQuantizer.ReduceTree(const ColorBits: Integer;
  var LeafCount: Integer;
  var ReducibleNodes: TReducibleNodes);
var
  BlueSum: Integer;
  Children: Integer;
  GreenSum: Integer;
  i: Integer;
  Node: TOctreeNode;
  RedSum: Integer;
begin
  i := Colorbits - 1;
  while (i > 0) and (ReducibleNodes[i] = nil) do
    Dec(i);

  Node := ReducibleNodes[i];
  ReducibleNodes[i] := Node.Next;

  RedSum := 0;
  GreenSum := 0;
  BlueSum := 0;
  Children := 0;

  for i := Low(ReducibleNodes) to High(ReducibleNodes) do
  begin
    if Node.Child[i] <> nil
      then begin
      Inc(RedSum, Node.Child[i].RedSum);
      Inc(GreenSum, Node.Child[i].GreenSum);
      Inc(BlueSum, Node.Child[i].BlueSum);
      Inc(Node.PixelCount, Node.Child[i].PixelCount);
      Node.Child[i].Free;
      Node.Child[i] := nil;
      Inc(Children)
    end
  end;

  Node.IsLeaf := TRUE;
  Node.RedSum := RedSum;
  Node.GreenSum := GreenSum;
  Node.BlueSum := BlueSum;
  Dec(LeafCount, Children - 1)
end;


procedure TColorQuantizer.GetColorTable(AColorTable: POpenColorTableArray);
var
  Index: Integer;
  Qarr: TRGBQuadArray256;
var i: integer;
begin
  Index := 0;
  GetPaletteColors(FTree, QArr, Index);
  for i := 0 to ColorCount - 1 do
    AColorTable^[i] := (QArr[i].rgbRed shl 16) + (QArr[i].rgbGreen shl 8) + QArr[i].rgbBlue;
end;



{$IFDEF IMPORTTGRAPHIC}
{_$I tgraphicimpl.inc}
{$ENDIF}

{Other Functions}


procedure MakeWebPalette;
var r, g, b: integer;
  i: integer;
begin
  i := 0;
  for r := 0 to 5 do
    for g := 0 to 5 do
      for b := 0 to 5 do
      begin
        WebColors[i] := ((b * $33) shl 16) + ((g * $33) shl 8) + (r * $33);
        inc(i);
      end;
end;

procedure MakeGray256Palette;
var i: integer;
begin
  for i := 0 to $FF do Gray256Colors[i] := (i shl 16) + (i shl 8) + i;

end;

function MulDiv(Number, Num, Den: Integer): Integer;
begin
  if Den = 0 then
  begin
    Result := -1;
    Exit;
  end;
  Result := (Int64(Number) * Num) div Den;
end;

function ColorInRange(col1, col2: TColor; Range: Byte): Boolean;
begin
  Result := (abs(Byte(col1 shr 16) - Byte(col2 shr 16)) < Range - 1) and
    (abs(Byte(col1 shr 8) - Byte(col2 shr 8)) < Range - 2) and
    (abs(Byte(col1) - Byte(col2)) < Range)
end;


{$IFDEF INTEL_ASM}

function ByteSwapColor(Color: LongWord): LongWord; assembler; //about 25% faster than no asm.
asm
        BSWAP EAX
        SHR   EAX,8
end;

{$ELSE}

function ByteSwapColor(Color: LongWord): LongWord;
begin
  Pixel32(Result).Blue := Pixel32(Color).Red;
  Pixel32(Result).Green := Pixel32(Color).Green;
  Pixel32(Result).Red := Pixel32(Color).Blue;
  Pixel32(Result).Alpha := Pixel32(Color).Alpha;
end;

{$ENDIF}

initialization
  MakeWebPalette;
  MakeGray256Palette;
end.
unit opbitmap;

{ *************************************************************************** }
{ Copyright (c) 2007 Theo Lustenberger                                        }
{                                                                             }
{ This software is provided "as-is".  This software comes without warranty    }
{ or garantee, explicit or implied.  Use this software at your own risk.      }
{ The author will not be liable for any damage to equipment, data, or         }
{ information that may result while using this software.                      }
{                                                                             }
{ By using this software, you agree to the conditions stated above.           }
{ *************************************************************************** }

{_$DEFINE INTEL_ASM}//Use ASM Code
{_$DEFINE IMPORTTGRAPHIC}//Import TGraphic Class

{$DEFINE USE_MOVE}
{$DEFINE VER_VTV} //Version for VTV.  No Resampling, no Canvas Line, Circle... needs less files


{$IFDEF FPC}
{$MODE objfpc}{$H+}
 {_$UNDEF USE_MOVE}
{$IFDEF INTEL_ASM}
{$ASMMODE intel}
{$ENDIF}
{$ENDIF}

{_$R+}
{_$S+}
{_$Q+}


interface

uses Classes, Types, Sysutils {$IFDEF IMPORTTGRAPHIC}, Graphics {$ENDIF};

type

  PColor = ^TColor;
  TColor = -$7FFFFFFF - 1..$7FFFFFFF;

  Nibble = 0..$F;

  TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pf48bit, pf64bit, pfCustom);

const
  { Raw rgb values }
  clBlack = TColor($000000);
  clMaroon = TColor($000080);
  clGreen = TColor($008000);
  clOlive = TColor($008080);
  clNavy = TColor($800000);
  clPurple = TColor($800080);
  clTeal = TColor($808000);
  clGray = TColor($808080);
  clSilver = TColor($C0C0C0);
  clRed = TColor($0000FF);
  clLime = TColor($00FF00);
  clYellow = TColor($00FFFF);
  clBlue = TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua = TColor($FFFF00);
  clLtGray = TColor($C0C0C0);
  clDkGray = TColor($808080);
  clWhite = TColor($FFFFFF);
  clNone = TColor($1FFFFFFF);
  clDefault = TColor($20000000);

const StdColors: array[0..15] of TColor = (
    clBlack,
    clMaroon,
    clGreen,
    clOlive,
    clNavy,
    clPurple,
    clTeal,
    clGray,
    clSilver,
    clRed,
    clLime,
    clYellow,
    clBlue,
    clFuchsia,
    clAqua,
    clWhite);

  AlphaOpaque = $FF;
  AlphaTransparent = 0;

const BWColors: array[0..1] of TColor = (clBlack, clWhite);

  MaxArr = (MaxLongint div Sizeof(integer)) - 1;

  TOPBitmapStreamSign = 'OPB';
  TOPBitmapStreamVersion = 1;

var WebColors: array[0..215] of TColor; //new 215
  Gray256Colors: array[0..$FF] of TColor;

type

  //Compatibility Declarations

  TRGBQuad =
    packed record
    rgbBlue: BYTE;
    rgbGreen: BYTE;
    rgbRed: BYTE;
    rgbReserved: BYTE
  end;

  pRGBQuad = ^TRGBQuad;

  TRGBQuadArray = array[Word] of TRGBQuad;
  PRGBQuadArray = ^TRGBQuadArray;

  TRGBQuadArray256 = array[0..256] of TRGBQuad;
  PRGBQuadArray256 = ^TRGBQuadArray;


  TRGBTriple =
    packed record
    rgbtBlue: BYTE;
    rgbtGreen: BYTE;
    rgbtRed: BYTE;
  end;

  pRGBTRiple = ^TRGBTriple;

  TRGBTripleArray = array[Word] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;

  //OPBitmap Declarations

  Pixel8 = Byte;
  APixel8 = array[0..MaxArr] of Pixel8;
  PAPixel8 = ^APixel8;

  Pixel16 = Word;
  APixel16 = array[0..MaxArr] of Pixel16;
  PAPixel16 = ^APixel16;

  Pixel24 = packed record
    Blue, Green, Red: Byte;
  end;
  PPixel24 = ^Pixel24;

  APixel24 = array[0..MaxArr] of Pixel24;
  PAPixel24 = ^APixel24;

  Pixel32 = packed record
    Blue, Green, Red, Alpha: Byte;
  end;
  PPixel32 = ^Pixel32;

  APixel32 = array[0..MaxArr] of Pixel32;
  PAPixel32 = ^APixel32;


  Pixel48 = packed record
    Blue, Green, Red: Word;
  end;
  PPixel48 = ^Pixel48;

  APixel48 = array[0..MaxArr div 2] of Pixel48;
  PAPixel48 = ^APixel48;


  Pixel64 = packed record
    Blue, Green, Red, Alpha: Word;
  end;
  PPixel64 = ^Pixel64;

  APixel64 = array[0..MaxArr div 3] of Pixel64;
  PAPixel64 = ^APixel64;

  TOpenColorTableArray = array of TColor;
  POpenColorTableArray = ^TOpenColorTableArray;

  TColorTableArray = array[0..$FF] of TColor;
  PColorTableArray = ^TColorTableArray;
  TColorTableArray16 = array[0..$F] of TColor;
  PColorTableArray16 = ^TColorTableArray16;

  TOPBitmapStreamHeader = packed record
    Version: Byte;
    BPP: Byte;
    Width: LongInt;
    Height: LongInt;
    Compressed: Boolean;
    PPI: LongInt;
    Transparent: Boolean;
    TransparentColor: TColor;
  end;


  EPasBitMapError = class(Exception);

  EInvalidGraphic = class(Exception);

  TReductionMode = (rmOptimized, rmFixed);

  TProgressStage = (psStarting, psRunning, psEnding);

const CSpaceRedu = 3;

  //---------------------------------------------------------------------------

type

  TOPBitmap = class;

  TBitmapData = class
  private
    fBPP: Byte;
    fParent: TOPBitmap;
    fWidth: Integer;
    fHeight: Integer;
    fLineLength: Integer;
    function GetScanLine(Row: Integer): Pointer; virtual; abstract;
    function GetPixel(X, Y: Integer): TColor; virtual; abstract;
    procedure SetPixel(X, Y: Integer; const Value: TColor); virtual; abstract;
    procedure SetWidth(const Value: Integer); virtual;
    procedure SetHeight(const Value: Integer); virtual;
  protected
    procedure UpdateSize; virtual; abstract;
    function CheckPixelValid(X, Y: integer): Boolean;
  public
    constructor Create(Parent: TOPBitmap); virtual;
    destructor Destroy; override;
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property ScanLine[Row: Integer]: Pointer read GetScanLine;
    property BPP: byte read fBPP;
    property Width: Integer read fWidth write SetWidth;
    property Height: Integer read fHeight write SetHeight;
    property LineLength: integer read fLineLength;
  end;

 { TBitmapData1 }

  TBitmapData1 = class(TBitmapData)
  private
    fPixels: PAPixel8;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Boolean;
    procedure SetNativePixel(X, Y: Integer; const Value: Boolean);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel8 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Boolean read GetNativePixel write SetNativePixel;
  end;

 { TBitmapData4 }

  TBitmapData4 = class(TBitmapData)
  private
    fPixels: PAPixel8;
    fLastNearestColorIdx: word;
    fLastColor: TColor;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Nibble;
    procedure SetNativePixel(X, Y: Integer; const Value: Nibble);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel8 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Nibble read GetNativePixel write SetNativePixel;
  end;

 { TBitmapData8}

  TBitmapData8 = class(TBitmapData)
  private
    fPixels: PAPixel8;
    fLastNearestColorIdx: word;
    fLastColor: TColor;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Byte;
    procedure SetNativePixel(X, Y: Integer; const Value: Byte);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel8 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Byte read GetNativePixel write SetNativePixel;
  end;

 { TBitmapData15 }

  TBitmapData15 = class(TBitmapData)
  private
    fPixels: PAPixel16;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel16 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
  end;

 { TBitmapData16 }

  TBitmapData16 = class(TBitmapData)
  private
    fPixels: PAPixel16;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel16 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
  end;


  { TBitmapData24 }

  TBitmapData24 = class(TBitmapData)
  private
    fPixels: PAPixel24;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Pixel24;
    procedure SetNativePixel(X, Y: Integer; const Value: Pixel24);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
{$IFDEF USE_MOVE}
    procedure Assign(Source: TBitmapData);
{$ENDIF}
    property RawArray: PAPixel24 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Pixel24 read GetNativePixel write SetNativePixel;
  end;


  { TBitmapData32 }

  TBitmapData32 = class(TBitmapData)
  private
    fPixels: PAPixel32;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Pixel32;
    procedure SetNativePixel(X, Y: Integer; const Value: Pixel32);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
{$IFDEF USE_MOVE}
    procedure Assign(Source: TBitmapData);
{$ENDIF}
    property RawArray: PAPixel32 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Pixel32 read GetNativePixel write SetNativePixel;
  end;



 { TBitmapData48}

  TBitmapData48 = class(TBitmapData)
  private
    fPixels: PAPixel48;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Pixel48;
    procedure SetNativePixel(X, Y: Integer; const Value: Pixel48);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel48 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Pixel48 read GetNativePixel write SetNativePixel;
  end;



 { TBitmapData64 }

  TBitmapData64 = class(TBitmapData)
  private
    fPixels: PAPixel64;
    function GetScanLine(Row: Integer): Pointer; override;
    function GetPixel(X, Y: Integer): TColor; override;
    procedure SetPixel(X, Y: Integer; const Value: TColor); override;
    function GetNativePixel(X, Y: Integer): Pixel64;
    procedure SetNativePixel(X, Y: Integer; const Value: Pixel64);
  protected
    procedure UpdateSize; override;
  public
    constructor Create(Parent: TOPBitmap); override;
    property RawArray: PAPixel64 read fPixels write fPixels;
    property NativePixels[X, Y: Integer]: Pixel64 read GetNativePixel write SetNativePixel;
  end;




  //---------------------------------------------------------------------------

  //Canvas is not the point here. It's just here for code that needs Canvas.Pixels access
  //plus some basic stuff for testing.

  TBrushStyle = (bsSolid, bsClear);

  TBrush = class //basic
  private
    fColor: TColor;
    fStyle: TBrushStyle;
  public
    property Color: TColor read fColor write fColor;
    property Style: TBrushStyle read fStyle write fStyle default bsSolid;
  end;

  TPen = class //basic
  private
    fColor: TColor;
  public
    property Color: TColor read fColor write fColor;
  end;

  TPasCanvas = class(TPersistent)
  end;

  TCanvasOPBitmap = class;

  TOPBitmapCanvas = class(TPasCanvas)
  private
    fBitmap: TOPBitmap;
    fPenPos: TPoint;
    fBrush: TBrush;
    fPen: TPen;
    function GetPixel(X, Y: Integer): TColor;
    procedure SetPixel(X, Y: Integer; const Value: TColor);
  public
    constructor Create(Bitmap: TOPBitmap);
    destructor Destroy; override;
    procedure MoveTo(X, Y: Integer);
    {$IFNDEF VER_VTV}
    procedure LineTo(X, Y: Integer);
    procedure Circle(CenterX, CenterY, Radius: Integer);
    {$ENDIF}
    procedure FillRect(Rect: TRect);
    procedure Draw(X, Y: integer; Bitmap: TCanvasOPBitmap);
    {$IFNDEF VER_VTV}
    procedure Resample(NewWidth, NewHeight: integer);
    {$ENDIF}
    procedure CopyRect(const Dest: TRect; Canvas: TOPBitmapCanvas; const Source: TRect);
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property Brush: TBrush read fBrush;
    property Pen: TPen read fPen;
  end;

  TCanvas = TOPBitmapCanvas;


{$IFDEF IMPORTTGRAPHIC}
{_$I tgraphicdecl.inc}
{$ELSE}
  TGraphic = class(TPersistent)
  end;
{$ENDIF}

  TColorFinder = class;

  { TOPBitmap }

  TOPBitmap = class(TGraphic)
  private
    fData: TBitmapData;
    fMask: TBitmapData1;
    fColorTable: TColorTableArray;
    fColorTableSize: integer;
    fLastColorIndex: Byte;
    fLastColor: TColor;
    fPaletteHasAllColours: Boolean;
    fReductionMode: TReductionMode;
    fColorFinder: TColorFinder;
    fMonochrome: Boolean;
    fTransparentColor: TColor;
{$IFNDEF ____IMPORTTGRAPHIC}FTransparent: Boolean; {$ENDIF}
    fAlphaBlend: Boolean;
    Flags: array[BYTE, BYTE] of Classes.TBits;

    function GetScanLine(Row: Integer): Pointer;
    procedure SetColorTable(const AValue: PColorTableArray);

    procedure SetPixel(X, Y: Integer; const AValue: TColor);
    function GetPixel(X, Y: Integer): TColor;
    function GetColorIndex(Color: TColor): byte;
    function GetPixelFormat: TPixelFormat;
    procedure SetPixelFormat(const Value: TPixelFormat);
    function GetBPP: byte;
    function GetColorTable: PColorTableArray;
    function NearestColor(const color: TColor): cardinal;
    function GetHandle: THandle;
    procedure SetHandle(const Value: THandle);
    function GetPalette: THandle;
{$IFNDEF VER_VTV}  procedure SetPalette(const Value: THandle); {$ENDIF}
    procedure SetMonochrome(const Value: Boolean);
    procedure SetTransparentColor(const Value: TColor);
    procedure SetAlphaBlend(const Value: Boolean);


  protected
    procedure ShrinkPaletteWeb;
    procedure SetHeight(Value: Integer); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    procedure SetWidth(Value: Integer); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    function GetHeight: Integer; {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    function GetWidth: Integer; {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    function GetTransparent: Boolean; {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    procedure SetTransparent(Value: Boolean); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    procedure DoSetPixelFormat(const Value: TPixelFormat);
    function GetEmpty: Boolean; virtual;
  public
    constructor Create; {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDataSize: Cardinal;
    procedure CopyFromColorTable(AColorTable: array of TColor; Swap: Boolean = true; Size: integer = -1);
    function CountColors(Max: Integer): Integer;
    function MakePalette(Size: Byte; var ColorTable: TOpenColorTableArray): Boolean;
    procedure LoadFromStream(Stream: TStream); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    procedure SaveToStream(Stream: TStream); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    procedure LoadFromFile(const Filename: string); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
    procedure SaveToFile(const Filename: string); {$IFDEF IMPORTTGRAPHIC} override; {$ELSE} virtual; {$ENDIF}
{$IFNDEF IMPORTTGRAPHIC}
    procedure Progress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
      const Msg: string {; var DoContinue: Boolean});
{$ENDIF}
    procedure SetAlpha(Value: Byte);
    procedure Clear;
    function GetTransparentMask(Tolerance: Byte; var Data: PByte; ReversedBits, WordBoundary: Boolean): integer;
    function GetFullMask(var Data: PByte): integer;
    property BPP: byte read GetBPP;
    property ScanLine[Row: Integer]: Pointer read GetScanLine;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property ColorTable: PColorTableArray read GetColorTable write SetColorTable;
{$IFNDEF VER_VTV} property Palette: THandle read GetPalette write SetPalette; {$ENDIF}
    property ColorTableSize: integer read fColorTableSize;
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat;
    property Monochrome: Boolean read fMonochrome write SetMonochrome;
    property Data: TBitmapData read fData write fData;
    property Handle: THandle read GetHandle write SetHandle;
    property ReductionMode: TReductionMode read fReductionMode write fReductionMode;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property TransparentColor: TColor read fTransparentColor write SetTransparentColor;
    property AlphaBlend: Boolean read fAlphaBlend write SetAlphaBlend;
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel; //Belongs to Canvas but is nice to have here
    property Empty: Boolean read GetEmpty;
  end;

  TCanvasOPBitmap = class(TOPBitmap)
  private
    fCanvas: TOPBitmapCanvas;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Canvas: TOPBitmapCanvas read fCanvas;
  end;


  TColorEntry = record
    R, G, B: Byte;
  end;

  PColorEntry = ^TColorEntry;


  TColorFinder = class
  private
    fPalette: TList;
    fSorted: Boolean;
    fBitmap: TOPBitmap;
    fMappings: array[BYTE, BYTE] of TList;
    procedure SetBitmap(const Value: TOPBitmap);
    function GetPaletteSize: integer;
  protected

    function MapColors: Integer;
    function NearestColor(R, G, B: Byte): integer;
    function GetColor(idx: integer): TColor; overload;
    procedure GetColor(idx: integer; var r, g, b: Byte); overload;
    procedure AddColor(Color: TColor); overload;
    procedure AddColor(R, G, B: Byte); overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPalette(Pal: array of TColor; {TOpenColorTableArray; tc } Size: integer = -1);
    function GetMappingColor(const R, G, B: Byte): TColor; overload;
    function GetMappingColor(const Color: TColor): TColor; overload;
    function GetMapping(const R, G, B: Byte): Integer; overload;
    function GetMapping(const Color: TColor): Integer; overload;
    procedure ClearPalette;
    procedure ClearMappings;
    property Bitmap: TOPBitmap read fBitmap write SetBitmap;
    property PaletteSize: integer read GetPaletteSize;
  published

  end;


  TOctreeNode = class; // Forward definition so TReducibleNodes can be declared

  TReducibleNodes = array[0..7] of TOctreeNode;

  TOctreeNode =
    class(TObject)
    IsLeaf: BOOLEAN;
    PixelCount: Integer;
    RedSum: Integer;
    GreenSum: Integer;
    BlueSum: Integer;
    Next: TOctreeNode;
    Child: TReducibleNodes;

    constructor Create(const Level: Integer;
      const ColorBits: Integer;
      var LeafCount: Integer;
      var ReducibleNodes: TReducibleNodes);
    destructor Destroy; override;

  end;

  TColorQuantizer =
    class(TOBject)
  private
    FTree: TOctreeNode;
    FLeafCount: Integer;
    FReducibleNodes: TReducibleNodes;
    FMaxColors: Integer;
    FColorBits: Integer;
  protected
    procedure AddColor(var Node: TOctreeNode;
      const r, g, b: BYTE;
      const ColorBits: Integer;
      const Level: Integer;
      var LeafCount: Integer;
      var ReducibleNodes: TReducibleNodes);
    procedure DeleteTree(var Node: TOctreeNode);
    procedure GetPaletteColors(const Node: TOctreeNode;
      var RGBQuadArray: TRGBQuadArray256;
      var Index: Integer);
    procedure ReduceTree(const ColorBits: Integer;
      var LeafCount: Integer;
      var ReducibleNodes: TReducibleNodes);

  public
    constructor Create(const MaxColors: Integer; const ColorBits: Integer);
    destructor Destroy; override;

    procedure GetColorTable(var RGBQuadArray: TRGBQuadArray256); overload;
    procedure GetColorTable(AColorTable: POpenColorTableArray); overload;
    function ProcessImage(Bmp: TOPBitmap): BOOLEAN;
    property ColorCount: Integer read FLeafCount;

  end;




function ByteSwapColor(Color: LongWord): LongWord;
function MulDiv(Number, Num, Den: Integer): Integer;
function PixelFormatFromBPP(inp: Byte): TPixelFormat;
function ColorInRange(col1, col2: TColor; Range: Byte): Boolean;


implementation

uses Math, {$IFDEF FPC}zstream{$ELSE}ZLib{$ENDIF}
 {$IFNDEF VER_VTV}, resample, ftbresenham, wincomp{$ENDIF};


{ TBitmapData }


function TBitmapData.CheckPixelValid(X, Y: integer): Boolean;
begin
  Result := (fWidth >= X) and (fHeight >= Y) and (X > -1) and (Y > -1);
  if not Result then raise EPasBitMapError.CreateFmt('Pixel coordinates out of range: X=%d Y=%d', [x, y]);
end;

constructor TBitmapData.Create(Parent: TOPBitmap);
begin
  fParent := Parent;
end;

destructor TBitmapData.Destroy;
begin
  fHeight := 0;
  fWidth := 0;
  UpdateSize;
  inherited;
end;

procedure TBitmapData.SetHeight(const Value: Integer);
begin
  fHeight := Value;
  UpdateSize;
end;

procedure TBitmapData.SetWidth(const Value: Integer);
begin
  fWidth := Value;
  UpdateSize;
end;


{ TBitmapData1 }


constructor TBitmapData1.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 1;
end;

function TBitmapData1.GetNativePixel(X, Y: Integer): Boolean;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := Boolean((fPixels^[((Y * fLineLength) + (X div 8))] shr (X mod 8)) and 1);
end;

function TBitmapData1.GetPixel(X, Y: Integer): TColor;
begin
  if not CheckPixelValid(X, Y) then exit;
  if Boolean((fPixels^[((Y * fLineLength) + (X div 8))] shr (X mod 8)) and 1) then
    Result := fParent.fColorTable[0] else
    Result := fParent.fColorTable[1];
end;

function TBitmapData1.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[(Row * fLineLength)];
end;

procedure TBitmapData1.SetNativePixel(X, Y: Integer; const Value: Boolean);
var Bt: PByte;
begin
  //if not CheckPixelValid(X, Y) then exit; {$message warn 'pixelcheck'}
  Bt := @fPixels^[(Y * fLineLength) + (X div 8)];
  if Value then
    bt^ := bt^ or (1 shl (X mod 8)) else
    bt^ := bt^ and not (1 shl (X mod 8));
end;

procedure TBitmapData1.SetPixel(X, Y: Integer; const Value: TColor);
var Bt: PByte;
  Gray: Byte;
begin
  if not CheckPixelValid(X, Y) then exit;
  Bt := @fPixels^[(Y * fLineLength) + (X div 8)];
  gray := (Byte(Value) * 77 + Byte(Value shr 8) * 151 + Byte(Value shr 16) * 28) shr 8;
  if gray < 110 then //little shift for the bright colors was 100
    bt^ := bt^ or (1 shl (X mod 8)) else
    bt^ := bt^ and not (1 shl (X mod 8));
end;


procedure TBitmapData1.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    if fWidth mod 8 > 0 then
      fLineLength := (fWidth div 8) + 1 else fLineLength := (fWidth div 8);

    if (fPixels <> nil) then FreeMem(fPixels);

    GetMem(fPixels, fHeight * fLineLength);
  end else

    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;

{ TBitmapData4 }


constructor TBitmapData4.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 4;
end;


function TBitmapData4.GetNativePixel(X, Y: Integer): Nibble;
var bt: Pixel8;
begin
  if not CheckPixelValid(X, Y) then exit;

  Bt := fPixels^[(Y * fLineLength) + (X div 2)];

  if (X mod 2 > 0) then
    Result := (Bt shr 4) and $F else
    Result := (Bt and $F);
end;

function TBitmapData4.GetPixel(X, Y: Integer): TColor;
var bt: Pixel8;
begin
  if not CheckPixelValid(X, Y) then exit;

  Bt := fPixels^[(Y * fLineLength) + (X div 2)];

  if (X mod 2 > 0) then
    Result := fParent.fColortable[(Bt shr 4) and $F] else
    Result := fParent.fColortable[(Bt and $F)]
end;

function TBitmapData4.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[(Row * fLineLength)];
end;

procedure TBitmapData4.SetNativePixel(X, Y: Integer; const Value: Nibble);
var Bt: PByte;
begin
  if not CheckPixelValid(X, Y) then exit;
  Bt := @fPixels^[(Y * fLineLength) + (X div 2)];
  if (X mod 2 > 0) then
    Bt^ := (Value shl 4) or (Bt^ and $F) else
    Bt^ := (((Bt^ shr 4) and $F) shl 4) or Value;
end;

procedure TBitmapData4.SetPixel(X, Y: Integer; const Value: TColor);
var Bt: PByte;
  Val: Integer;
  R, G, B: byte;
begin
  if not CheckPixelValid(X, Y) then exit;

  Val := -1;

  if fLastColor = Value then
  begin
    Val := fLastNearestColorIdx;
  end else
  begin
    if fParent.fPaletteHasAllColours then
      Val := fParent.GetColorIndex(Value) else
    begin
      if fParent.Reductionmode <> rmFixed then
        if CSpaceRedu > 0 then
        begin
          B := Byte(Value shr 16);
          G := Byte(Value shr 8);
          R := Byte(Value);
          Val := fParent.fColorFinder.GetMapping(R - (R mod CSpaceRedu), G - (G mod CSpaceRedu), B - (B mod CSpaceRedu))
        end else
          Val := fParent.fColorFinder.GetMapping(Value); //without color space reduction.
    end;

     //Not found in Mappings. This happens when painting after conversion with non-palette color, or with fixed palette
     //Then simply find NearestColor:
    if Val = -1 then Val := fParent.NearestColor(Value);

    fLastColor := Value;
    fLastNearestColorIdx := Val;
  end;

  Bt := @fPixels^[(Y * fLineLength) + (X div 2)];
  if (X mod 2 > 0) then
    Bt^ := Byte(Val shl 4) or (Bt^ and $F) else
    Bt^ := (((Bt^ shr 4) and $F) shl 4) or Val;
end;

procedure TBitmapData4.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    if fWidth mod 2 > 0 then
      fLineLength := (fWidth div 2) + 1 else
      fLineLength := (fWidth div 2);
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * fLineLength);
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;

{ TBitmapData8 }

constructor TBitmapData8.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 8;
end;

function TBitmapData8.GetNativePixel(X, Y: Integer): Byte;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := fPixels^[Y * fWidth + X];
end;

function TBitmapData8.GetPixel(X, Y: Integer): TColor;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := fParent.fColorTable[fPixels^[Y * fWidth + X]];
end;

function TBitmapData8.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;


procedure TBitmapData8.SetNativePixel(X, Y: Integer; const Value: Byte);
begin
  if not CheckPixelValid(X, Y) then exit;
  fPixels^[Y * fWidth + X] := Value;
end;



procedure TBitmapData8.SetPixel(X, Y: Integer; const Value: TColor);
var Val: integer;
  R, G, B: byte;
begin
  if not CheckPixelValid(X, Y) then exit;

  Val := -1;

  if fLastColor = Value then
  begin
    Val := fLastNearestColorIdx;
  end else
  begin
    if fParent.fPaletteHasAllColours then
      Val := fParent.GetColorIndex(Value) else
    begin
      if CSpaceRedu > 0 then
      begin
        B := Byte(Value shr 16);
        G := Byte(Value shr 8);
        R := Byte(Value);
        Val := fParent.fColorFinder.GetMapping(R - (R mod CSpaceRedu), G - (G mod CSpaceRedu), B - (B mod CSpaceRedu));
        //writeln('mapped');
      end else
        Val := fParent.fColorFinder.GetMapping(Value); //without color space reduction.
    end;

     //Not found in Mappings. This happens when painting after conversion with non-palette color.
     //Then simply find NearestColor:
    if Val = -1 then begin Val := fParent.NearestColor(Value); {writeln('nearest');} end;

    fLastColor := Value;
    fLastNearestColorIdx := Val;
  end;

  fPixels^[Y * fWidth + X] := Val;
end;


procedure TBitmapData8.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * fLineLength)
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;

{ TBitmapData15 }

constructor TBitmapData15.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 15;
end;

function TBitmapData15.GetPixel(X, Y: Integer): TColor;
var idx: Cardinal;
  R, G, B: Byte;
begin
  if not CheckPixelValid(X, Y) then exit;
  idx := Y * fWidth + X;
  R := (fPixels^[idx] and $7C00) shr 10;
  G := (fPixels^[idx] and $3E0) shr 5;
  B := (fPixels^[idx] and $1F);

  if (R = $1F) then R := $FF else if (R <> 0) then R := (R + 1) shl 3;
  if (G = $1F) then G := $FF else if (G <> 0) then G := (G + 1) shl 3;
  if (B = $1F) then B := $FF else if (B <> 0) then B := (B + 1) shl 3;

  Result := (B shl 16) + (G shl 8) + R;
end;


function TBitmapData15.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;


procedure TBitmapData15.SetPixel(X, Y: Integer; const Value: TColor);
var idx: Cardinal;
begin
  if not CheckPixelValid(X, Y) then exit;
  idx := Y * fWidth + X;
  fPixels^[idx] := ((Pixel32(Value).Blue shr 3) shl 10) or
    ((Pixel32(Value).Green shr 3) shl 5) or
    ((Pixel32(Value).Red shr 3) shl 0);
end;


procedure TBitmapData15.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth * 2;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * fLineLength)
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;

{ TBitmapData16 }

constructor TBitmapData16.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 16;
end;


function TBitmapData16.GetPixel(X, Y: Integer): TColor;
var idx: Cardinal;
  R, G, B: Byte;
begin
  if not CheckPixelValid(X, Y) then exit;
  idx := Y * fWidth + X;

  R := (fPixels^[idx] and $F800) shr 11;
  G := (fPixels^[idx] and $7E0) shr 5;
  B := (fPixels^[idx] and $1F);

  if (R = $1F) then R := $FF else if (R <> 0) then R := (R + 1) shl 3;
  if (G = $3F) then G := $FF else if (G <> 0) then G := (G + 1) shl 2;
  if (B = $1F) then B := $FF else if (B <> 0) then B := (B + 1) shl 3;

  Result := (B shl 16) + (G shl 8) + R;
end;

function TBitmapData16.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;

procedure TBitmapData16.SetPixel(X, Y: Integer; const Value: TColor);
var idx: Cardinal;
begin
  if not CheckPixelValid(X, Y) then exit;
  idx := Y * fWidth + X;
  fPixels^[idx] := ((Pixel32(Value).Blue shr 3) shl 11) or
    ((Pixel32(Value).Green shr 2) shl 5) or
    ((Pixel32(Value).Red shr 3) shl 0);
end;

procedure TBitmapData16.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth * 2;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * fLineLength)
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;


{ TBitmapData24 }

constructor TBitmapData24.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 24;
end;

{$IFDEF USE_MOVE}

procedure TBitmapData24.Assign(Source: TBitmapData);
var X: integer;
begin
  if Source is TBitmapData32 then
  begin
    Width := Source.Width;
    Height := Source.Height;
    if not Source.fParent.Empty then
      for X := 0 to (Width * Height) - 1 do
        Move(TBitmapData32(Source).RawArray^[X], RawArray^[X], 3);
  end;
end;
{$ENDIF}

function TBitmapData24.GetPixel(X, Y: Integer): TColor;
var pix: PPixel24;
begin
  if not CheckPixelValid(X, Y) then exit;
  pix := @fPixels^[Y * fWidth + X];
  Result := (pix^.Blue shl 16) + (pix^.Green shl 8) + pix^.Red;
end;

function TBitmapData24.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;

procedure TBitmapData24.SetPixel(X, Y: Integer; const Value: TColor);
var pix: PPixel24;
begin
  if not CheckPixelValid(X, Y) then exit;
  pix := @fPixels^[Y * fWidth + X];
  pix^.Blue := Byte(Value shr 16);
  pix^.Green := Byte(Value shr 8);
  pix^.Red := Byte(Value);
end;

procedure TBitmapData24.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth * 3;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * (fLineLength))
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;


function TBitmapData24.GetNativePixel(X, Y: Integer): Pixel24;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := fPixels^[Y * fWidth + X];
end;

procedure TBitmapData24.SetNativePixel(X, Y: Integer;
  const Value: Pixel24);
begin
  if not CheckPixelValid(X, Y) then exit;
  fPixels^[Y * fWidth + X] := Value
end;

{ TBitmapData32 }

constructor TBitmapData32.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 32;
end;

{$IFDEF USE_MOVE}

procedure TBitmapData32.Assign(Source: TBitmapData);
var X: integer;
  pix: PPixel32;
begin
  if Source is TBitmapData24 then
  begin
    Width := Source.Width;
    Height := Source.Height;
    if not Source.fParent.Empty then
      for X := 0 to (Width * Height) - 1 do
      begin
        pix := @RawArray^[X];
        Move(TBitmapData24(Source).RawArray^[X], pix^, 3);
        pix^.Alpha := AlphaOpaque;
      end;
  end;
end;
{$ENDIF}

function TBitmapData32.GetPixel(X, Y: Integer): TColor;
var pix: PPixel32;
begin
  if not CheckPixelValid(X, Y) then exit;
  pix := @fPixels^[Y * fWidth + X];
  Result := (pix^.Blue shl 16) + (pix^.Green shl 8) + pix^.Red;
end;

function TBitmapData32.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;


procedure TBitmapData32.SetPixel(X, Y: Integer; const Value: TColor);
var pix: PPixel32;
begin
  if not CheckPixelValid(X, Y) then exit;
  pix := @fPixels^[Y * fWidth + X];
  pix^.Blue := Byte(Value shr 16);
  pix^.Green := Byte(Value shr 8);
  pix^.Red := Byte(Value);
  pix^.Alpha := AlphaOpaque;
end;

procedure TBitmapData32.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth * 4;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * (fLineLength))
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;
end;

function TBitmapData32.GetNativePixel(X, Y: Integer): Pixel32;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := fPixels^[Y * fWidth + X];
end;

procedure TBitmapData32.SetNativePixel(X, Y: Integer;
  const Value: Pixel32);
begin
  if not CheckPixelValid(X, Y) then exit;
  fPixels^[Y * fWidth + X] := Value;
end;


{ TBitmapData48 }


constructor TBitmapData48.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 48;
end;

function TBitmapData48.GetNativePixel(X, Y: Integer): Pixel48;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := fPixels^[Y * fWidth + X];
end;

function TBitmapData48.GetPixel(X, Y: Integer): TColor;
var Col: Pixel48;
begin
  if not CheckPixelValid(X, Y) then exit;
  Col := fPixels^[Y * fWidth + X];
  Result := ((Col.Red shr 8) and $FF)
    or (Col.Green and $FF00)
    or ((Col.Blue shl 8) and $FF0000);
end;

function TBitmapData48.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;

procedure TBitmapData48.SetNativePixel(X, Y: Integer;
  const Value: Pixel48);
begin
  if not CheckPixelValid(X, Y) then exit;
  fPixels^[Y * fWidth + X] := Value;
end;

procedure TBitmapData48.SetPixel(X, Y: Integer; const Value: TColor);
var col: Pixel48;
begin
  if not CheckPixelValid(X, Y) then exit;
  col.Red := (Value and $FF);
  col.Red := col.Red + (col.Red shl 8);
  col.Green := (Value and $FF00);
  col.Green := col.Green + (col.Green shr 8);
  col.Blue := (Value and $FF0000) shr 8;
  col.Blue := col.Blue + (col.Blue shr 8);
  fPixels^[Y * fWidth + X] := col;
end;

procedure TBitmapData48.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth * 6;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * fLineLength)
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;

end;


{ TBitmapData64 }


constructor TBitmapData64.Create(Parent: TOPBitmap);
begin
  inherited;
  fBPP := 64;
end;

function TBitmapData64.GetNativePixel(X, Y: Integer): Pixel64;
begin
  if not CheckPixelValid(X, Y) then exit;
  Result := fPixels^[Y * fWidth + X];
end;

function TBitmapData64.GetPixel(X, Y: Integer): TColor;
var Col: Pixel64;
begin
  if not CheckPixelValid(X, Y) then exit;
  Col := fPixels^[Y * fWidth + X];
  Result := ((Col.Red shr 8) and $FF)
    or (Col.Green and $FF00)
    or ((Col.Blue shl 8) and $FF0000);
end;

function TBitmapData64.GetScanLine(Row: Integer): Pointer;
begin
  Result := @fPixels^[Row * fWidth];
end;

procedure TBitmapData64.SetNativePixel(X, Y: Integer;
  const Value: Pixel64);
begin
  if not CheckPixelValid(X, Y) then exit;
  fPixels^[Y * fWidth + X] := Value;
end;

procedure TBitmapData64.SetPixel(X, Y: Integer; const Value: TColor);
var col: Pixel64;
begin
  if not CheckPixelValid(X, Y) then exit;
  col.Red := (Value and $FF);
  col.Red := col.Red + (col.Red shl 8);
  col.Green := (Value and $FF00);
  col.Green := col.Green + (col.Green shr 8);
  col.Blue := (Value and $FF0000) shr 8;
  col.Blue := col.Blue + (col.Blue shr 8);
  col.Alpha := AlphaOpaque;
  fPixels^[Y * fWidth + X] := col;
end;

procedure TBitmapData64.UpdateSize;
begin
  if (fWidth > 0) and (fHeight > 0) then
  begin
    fLineLength := fWidth * 8;
    if (fPixels <> nil) then FreeMem(fPixels);
    GetMem(fPixels, fHeight * fLineLength)
  end else
    if (fPixels <> nil) then begin
      FreeMem(fPixels);
      fPixels := nil;
    end;

end;

{ TOPBitmap }


constructor TOPBitmap.Create;
begin
  fData := TBitmapData32.Create(Self);
  fMask := TBitmapData1.Create(self);
  fColorTableSize := 0;
  fPaletteHasAllColours := false;
  fMonochrome := false;
  fColorFinder := TColorFinder.Create;
end;

destructor TOPBitmap.Destroy;
begin
  fColorFinder.free;

  if fMask <> nil then
  begin
    fMask.free;
    fMask := nil;
  end;

  if fData <> nil then
  begin
    fData.Free;
    fData := nil;
  end;
  inherited;
end;

function TOPBitmap.GetHeight: Integer;
begin
  if fData <> nil then Result := fData.Height else Result := 0;
end;

function TOPBitmap.GetScanLine(Row: Integer): Pointer;
begin
  if fData <> nil then Result := fData.ScanLine[Row] else Result := nil;
end;


function TOPBitmap.GetWidth: Integer;
begin
  if fData <> nil then Result := fData.Width else Result := 0;
end;

procedure TOPBitmap.SetHeight(Value: Integer);
begin
  if fData <> nil then fData.Height := Value;
end;

procedure TOPBitmap.SetWidth(Value: Integer);
begin
  if fData <> nil then fData.Width := Value;
end;


procedure TOPBitmap.SetPixel(X, Y: Integer; const AValue: TColor);
begin
  if fData <> nil then fData.SetPixel(X, Y, AValue);
end;

function TOPBitmap.GetPixel(X, Y: Integer): TColor;
begin
  if fData <> nil then Result := fData.GetPixel(X, Y) else Result := clNone;
end;


function TOPBitmap.GetColorIndex(Color: TColor): byte;
var i: integer;
begin
  Pixel32(Color).Alpha := 0;
  Result := 0;
  if Color = fLastColor then
  begin
    Result := fLastColorIndex;
    exit;
  end;
  for i := 0 to fColorTableSize - 1 do
    if fColorTable[i] = Color then
    begin
      Result := i;
      fLastColor := Color;
      fLastColorIndex := i;
      break;
    end;
end;

function TOPBitmap.GetPixelFormat: TPixelFormat;
begin
  Result := pfCustom;
  if fData <> nil then
  begin
    case fData.BPP of
      1: Result := pf1bit;
      4: Result := pf4bit;
      8: Result := pf8bit;
      15: Result := pf15bit;
      16: Result := pf16bit;
      24: Result := pf24bit;
      32: Result := pf32bit;
      48: Result := pf48bit;
      64: Result := pf64bit;
    end;
  end;
end;

function TOPBitmap.GetEmpty: Boolean;
begin
  Result := (Width < 1) or (Height < 1);
end;

procedure TOPBitmap.SetPixelFormat(const Value: TPixelFormat);
begin
// This is an ugly hack. Because we have only one ColorTable in current design,
// we have to make a non paletted format first in case of potential palette to palette reduction.
// Happens only in 8 to 4 or 8 to 1 or 4 to 1 bit reduction.
// But shouldn't be extremely slow and I'll try to change that later using local palettes.
  if (PixelFormat <= pf8bit) and (Value < PixelFormat) then DoSetPixelFormat(pf24bit);

  DoSetPixelFormat(Value);
end;

procedure TOPBitmap.DoSetPixelFormat(const Value: TPixelFormat);
var Temp: TBitmapData;
  X, Y: Cardinal;
  CT: TOpenColorTableArray;
  OptPalette: Boolean;
  cq: TColorQuantizer;
begin
  if Value <> PixelFormat then
  begin
    OptPalette := false;
    Temp := fData;
    fAlphaBlend := false;
    case Value of
      pf1bit: begin fData := TBitmapData1.Create(self); CopyFromColorTable(BWColors); end;
      pf4bit: begin
          if not Temp.fParent.Empty then
          begin
            SetLength(CT, 16);
          //if coming from lower bpp just copy palette
            if (Temp.fParent.PixelFormat < Value) and (Temp.fParent.ColorTableSize > 0) then
            begin
              CT[0] := Temp.fParent.fColorTable[0];
              CT[1] := Temp.fParent.fColorTable[1];
              OptPalette := True;
            end else
              //Try to make optimized palette on original Data
              OptPalette := MakePalette($F, CT);
            if OptPalette then
            begin
              CopyFromColorTable(CT, false);
              fPaletteHasAllColours := true;
            end;

            if not OptPalette then
            begin
                //If FixedPalette selected
              if fReductionMode = rmFixed then
              begin
                CopyFromColorTable(StdColors, false);
                fPaletteHasAllColours := false;
              end;
                //Make Optimal Reduction.
              if fReductionMode = rmOptimized then
              begin
                cq := TColorQuantizer.Create(16, 4);
                cq.ProcessImage(self);
                cq.GetColorTable(@CT);
                CopyFromColorTable(CT, true, cq.ColorCount);
                fColorFinder.SetPalette(fColorTable, cq.ColorCount);
                fColorFinder.Bitmap := self;
                cq.free;
                fPaletteHasAllColours := false;
              end;
            end;
          end;
          fData := TBitmapData4.Create(self);
        end;
      pf8bit: begin
          //if coming from lower bpp just copy palette
          SetLength(CT, 256);
          if not Temp.fParent.Empty then
          begin
            if (Temp.fParent.PixelFormat < Value) and (Temp.fParent.ColorTableSize > 0) then
            begin
              OptPalette := True;
             // CopyFromColorTable(CT, false, Temp.fParent.fColorTableSize); {$message warn'testen'};
              fPaletteHasAllColours := True;
            end else
            begin
              //Try to make optimized palette on original Data.
              OptPalette := MakePalette($FF, CT);
              if OptPalette then
              begin
                CopyFromColorTable(CT, false);
                fPaletteHasAllColours := True;
              end;
            end;
            if not OptPalette then
            begin
              //If FixedPalette selected
              if fReductionMode = rmFixed then
              begin
                ShrinkPaletteWeb;
                CopyFromColorTable(WebColors, false);
                fPaletteHasAllColours := true;
              end;
              //Make Optimal Reduction.
              if fReductionMode = rmOptimized then
              begin
                cq := TColorQuantizer.Create(256, 8);
                cq.ProcessImage(self);
                cq.GetColorTable(@CT);
                CopyFromColorTable(CT, true, cq.ColorCount);
                fColorFinder.SetPalette(fColorTable, cq.ColorCount);
                fColorFinder.Bitmap := self;
                cq.free;
                fPaletteHasAllColours := false;
              end;
            end;
          end else OptPalette := false;

          fData := TBitmapData8.Create(self);
        end;
      pf15bit: fData := TBitmapData15.Create(self);
      pf16bit: fData := TBitmapData16.Create(self);
      pf24bit: fData := TBitmapData24.Create(self);
      pf32bit: fData := TBitmapData32.Create(self);
      pf48bit: fData := TBitmapData48.Create(self);
      pf64bit: fData := TBitmapData64.Create(self);
      pfCustom, pfDevice: fData := TBitmapData32.Create(self);
    else raise EPasBitMapError.CreateFmt('Pixelformat not supported: Ordinal %d', [Ord(Value)]);
    end;
{$IFDEF USE_MOVE}
    if (Temp.BPP = 24) and (fData.BPP = 32) then TBitmapData32(fData).Assign(Temp) else //Max speed for these.
      if (Temp.BPP = 32) and (fData.BPP = 24) then TBitmapData24(fData).Assign(Temp) else
{$ENDIF}
      begin
        fData.Width := Temp.Width;
        fData.Height := Temp.Height;
        if not Temp.fParent.Empty then
          for y := 0 to Temp.Height - 1 do
            for x := 0 to Temp.Width - 1 do
              fData.Pixels[X, Y] := Temp.Pixels[X, Y];
      end;
    Temp.free;
  end;
  fPaletteHasAllColours := False;
end;


procedure TOPBitmap.ShrinkPaletteWeb; //Web Color Reduction
var X, Y: Cardinal;
  tpix: Pixel32;

  function _WebMatch(inp: Byte): Byte;
  var diff: byte;
  begin
    diff := (inp mod $33);
    if (diff < $19) then
      Result := inp - diff else
      Result := inp - diff + $33;
  end;

begin
  for y := 0 to fData.Height - 1 do
    for x := 0 to fData.Width - 1 do
    begin
      tpix := Pixel32(fData.Pixels[X, Y]);
      tpix.Red := _WebMatch(tpix.Red);
      tpix.Green := _WebMatch(tpix.Green);
      tpix.Blue := _WebMatch(tpix.Blue);
      tpix.Alpha := 0;
      fData.Pixels[X, Y] := Cardinal(tpix);
    end;
end;

procedure TOPBitmap.CopyFromColorTable(AColorTable: array of TColor; Swap: Boolean = true; Size: integer = -1);
var i: integer;
begin
  if Size > -1 then
    fColorTableSize := Size else
    fColorTableSize := High(AColorTable) + 1;
  if Swap then
    for i := 0 to fColorTableSize - 1 do fColorTable[i] := ByteSwapColor(AColorTable[i]) else
    for i := 0 to fColorTableSize - 1 do fColorTable[i] := AColorTable[i];
  fLastColor := clNone;
end;

function TOPBitmap.GetColorTable: PColorTableArray;
begin
  Result := @fColorTable;
end;

procedure TOPBitmap.SetColorTable(const AValue: PColorTableArray);
begin
  fColorTable := AValue^;
end;

function TOPBitmap.GetDataSize: Cardinal;
begin
  Result := Height * fData.fLineLength;
end;


function TOPBitmap.GetBPP: byte;
begin
  Result := 0;
  if fData <> nil then
    if fData.fBPP = 15 then Result := 16 else Result := fData.fBPP;
end;


function TOPBitmap.NearestColor(const color: TColor): Cardinal;

var
  DistanceSquared: INTEGER;
  B1, B2: Byte;
  G1, G2: Byte;
  i: INTEGER;
  R1, R2: Byte;
  SmallestDistanceSquared: INTEGER;
  col: TColor;
begin
  Result := 0;
  SmallestDistanceSquared := $1000000;


  R1 := Byte(Color);
  G1 := Byte(Color shr 8);
  B1 := Byte(Color shr 16);


  for i := 0 to fColorTableSize - 1 do
  begin

    col := fColorTable[i];

    R2 := Byte(col);
    G2 := Byte(col shr 8);
    B2 := Byte(col shr 16);

    DistanceSquared := (R1 - R2) * (R1 - R2) + (G1 - G2) * (G1 - G2) + (B1 - B2) * (B1 - B2);

    if DistanceSquared < SmallestDistanceSquared then
    begin
      Result := i;
      if Col = Color then exit;
      SmallestDistanceSquared := DistanceSquared;
    end
  end;
end;


function TOPBitmap.CountColors(Max: Integer): Integer;
var
  x, y: Cardinal;
  i, j: Cardinal;
  Red, Green, Blue: Byte;
begin
  RESULT := 0;
  for j := 0 to $FF do
    for i := 0 to $FF do
      Flags[i, j] := nil;

  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
    begin
      Red := Pixels[x, y];
      Green := (Pixels[x, y] shr 8);
      Blue := (Pixels[x, y] shr 16);
      if (Flags[Red, Green]) = nil then
      begin
        Flags[Red, Green] := Classes.TBits.Create;
        Flags[Red, Green].Size := 256;
      end;
      if not Flags[Red, Green].Bits[Blue] then
      begin
        Flags[Red, Green].Bits[Blue] := TRUE;
        if Result = Max - 1 then
        begin
          Result := -1;
          exit;
        end;
        Inc(Result);
      end;
    end;

  for j := 0 to $FF do
    for i := 0 to $FF do
      if Assigned(Flags[i, j]) then Flags[i, j].Free;
end;




function TOPBitmap.MakePalette(Size: Byte; var ColorTable: TOpenColorTableArray): Boolean;
var
  x, y: Cardinal;
  i, j: Cardinal;
  Red, Green, Blue: Byte;
  Cnt: word;
begin
  Result := false;

  for j := 0 to $FF do
    for i := 0 to $FF do
      Flags[i, j] := nil;

  for i := 0 to Size do ColorTable[i] := 0;

  Cnt := 0;

  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
    begin
      Red := Byte(Pixels[x, y]);
      Green := Byte(Pixels[x, y] shr 8);
      Blue := Byte(Pixels[x, y] shr 16);
      if (Flags[Red, Green]) = nil then
      begin

        Flags[Red, Green] := Classes.TBits.Create;
        Flags[Red, Green].Size := 256;
      end;

      if not Flags[Red, Green].Bits[Blue] then
      begin
        ColorTable[Cnt] := Pixels[x, y];
        if Cnt = Size then
        begin
          exit;
        end;
        inc(Cnt);

        Flags[Red, Green].Bits[Blue] := TRUE
      end;
    end;

  for j := 0 to $FF do
    for i := 0 to $FF do
      if Assigned(Flags[i, j]) then Flags[i, j].Free;

  Result := True;
end;


function TOPBitmap.GetHandle: THandle;
begin
  Result := THandle(Self);
end;

procedure TOPBitmap.SetHandle(const Value: THandle);
begin
 //just for compatibility
 // raise EPasBitMapError.Create('Attempt to SetHandle');
end;

function TOPBitmap.GetPalette: THandle;
begin
  Result := THandle(@fColorTable);
end;

{$IFNDEF VER_VTV}
procedure TOPBitmap.SetPalette(const Value: THandle);
var PaletteH: HPalette;
var i: integer;
begin
  PaletteH := Value;
  for i := 0 to PMaxLogPalette(PaletteH)^.palNumEntries - 1 do
    ColorTable^[i] := (PMaxLogPalette(PaletteH)^.palPalEntry[i].peBlue shl 16) +
      (PMaxLogPalette(PaletteH)^.palPalEntry[i].peGreen shl 8) +
      PMaxLogPalette(PaletteH)^.palPalEntry[i].peRed;
  fColorTableSize:=PMaxLogPalette(PaletteH)^.palNumEntries; //14.2.
end;
{$ENDIF}

procedure TOPBitmap.Assign(Source: TPersistent);
var x, y: integer;
begin
  if Source is TOPBitmap then
  begin
    Width := 0; //Don't convert;
    PixelFormat := TOPBitmap(Source).PixelFormat;
    if not TOPBitmap(Source).Empty then
    begin
      if TOPBitmap(Source).fColorTableSize > 0 then
        CopyFromColorTable(TOPBitmap(Source).fColorTable, false, TOPBitmap(Source).fColorTableSize);
      Width := TOPBitmap(Source).Width;
      Height := TOPBitmap(Source).Height;
      if TOPBitmap(Source).Transparent then
        TransparentColor := TOPBitmap(Source).TransparentColor else Transparent := false;

{$IFDEF USE_MOVE}
      Move(TOPBitmap(Source).Scanline[0]^, Scanline[0]^, Height * TOPBitmap(Source).fData.fLineLength); //Todo Check
{$ELSE}
      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
          Pixels[x, y] := TOPBitmap(Source).Pixels[x, y];
{$ENDIF}

    end;
  end
  else
    inherited Assign(Source);
end;

function PixelFormatFromBPP(inp: Byte): TPixelFormat;
begin
  case inp of
    64: Result := pf64bit;
    48: Result := pf48bit;
    32: Result := pf32bit;
    24: Result := pf24bit;
    16: Result := pf16bit;
    15: Result := pf15bit;
    8: Result := pf8bit;
    4: Result := pf4bit;
    1: Result := pf1bit;
  end;
end;

procedure TOPBitmap.SetMonochrome(const Value: Boolean);
var x, y: integer;
  gray: Byte;
  col: TColor;
  OrigPixelFormat:TPixelFormat;
begin
//  if not fMonochrome then
  begin
    OrigPixelFormat:=PixelFormat;
    if PixelFormat<pf15bit then PixelFormat:=pf24bit;
    fMonochrome := True;
    for y := 0 to Height - 1 do
      for x := 0 to Width - 1 do
      begin
        col := Pixels[x, y];
        gray := (Byte(col) * 77 + Byte(col shr 8) * 151 + Byte(col shr 16) * 28) shr 8;
        Pixels[x, y] := gray shl 16 + gray shl 8 + gray;
      end;
    PixelFormat:=OrigPixelFormat;
  end;
end;


{
straightforward compressed image stream format for testing
Prefer standard formats!
}

procedure TOPBitmap.SaveToStream(Stream: TStream);
var TCS: TCompressionStream;
  Header: TOPBitmapStreamHeader;
  ds: Cardinal;
  i: integer;
  Sign: array[0..2] of Char;
begin
  with Header do
  begin
    Version := TOPBitmapStreamVersion;
    BPP := fData.fBPP;
    Width := Self.Width;
    Height := Self.Height;
    PPI := 100;
    Compressed := true;
    Transparent := Self.Transparent;
    TransparentColor := Self.TransparentColor;
  end;
  Stream.Position := 0;
  Sign := TOPBitmapStreamSign;
  Stream.Write(sign, 3);
  Stream.Write(Header, SizeOf(TOPBitmapStreamHeader));

  TCS := TCompressionStream.Create({$IFDEF FPC}zstream.clDefault{$ELSE}ZLib.clDefault{$ENDIF}, Stream);
  try
    if Header.BPP <= 8 then
    begin
      TCS.Write(fColorTableSize, SizeOf(Word));

      for i := 0 to fColorTableSize - 1 do
        TCS.Write(fColorTable[i], sizeOf(TColor));
    end;

    ds := GetDataSize;
    TCS.Write(ds, SizeOf(Cardinal));
    TCS.Write(Data.ScanLine[0]^, ds);
  finally
    TCS.Free;
  end;
  Stream.Position := 0;
end;


procedure TOPBitmap.LoadFromStream(Stream: TStream);
var sign: array[0..2] of Char;
  Header: TOPBitmapStreamHeader;
  TDS: TDecompressionStream;
  cts: Word;
  ds: Cardinal;
  i: integer;
begin
  Stream.Read(sign, 3);
  if sign = TOPBitmapStreamSign then
  begin
    Stream.Read(Header, SizeOf(TOPBitmapStreamHeader));
    if Header.Version = TOPBitmapStreamVersion then
    begin
      Width := 0;
      PixelFormat := PixelFormatFromBPP(Header.BPP);
      Width := Header.width;
      Height := Header.height;
      Transparent := Header.Transparent;
      if Transparent then TransparentColor := Header.TransparentColor;

      TDS := TDecompressionStream.Create(Stream);
      try
        if Header.BPP <= 8 then
        begin
          TDS.Read(cts, SizeOf(Word));
          fColorTableSize := cts;
          for i := 0 to Min(cts - 1, High(fColorTable)) do
            TDS.Read(fColorTable[i], sizeOf(TColor));
        end;

        TDS.Read(ds, SizeOf(Cardinal));
        TDS.Read(Data.ScanLine[0]^, ds);
      finally
        TDS.Free;
      end;
      Stream.Position := 0;
    end else raise EPasBitMapError.Create('Unsupported OPBitmap Stream Version');
  end else raise EPasBitMapError.Create('Not an OPBitmap Stream');
end;




procedure TOPBitmap.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmShareDenyNone or fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TOPBitmap.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  if Empty then raise EPasBitMapError.Create('OPBitmap empty');
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{$IFNDEF IMPORTTGRAPHIC}

procedure TOPBitmap.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string {;
  var DoContinue: Boolean});
begin
  //Todo: Progress
end;

{$ENDIF}



procedure TOPBitmap.SetTransparentColor(const Value: TColor);
begin
  fTransparentColor := Value;
  fTransparent := True;
end;

function TOPBitmap.GetTransparent: Boolean;
begin
  Result := FTransparent;
end;

procedure TOPBitmap.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
{$IFDEF IMPORTTGRAPHIC}Changed(Self); {$ENDIF}
  end;
end;

procedure TOPBitmap.SetAlpha(Value: Byte);
var x, y: integer;
  Pix: PPixel32;
begin
  if PixelFormat = pf32bit then
  begin
    if Transparent then
    begin
      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
          if Pixels[X, Y] = TransparentColor then
          begin
            pix := @TBitmapData32(Self.fData).fPixels^[Y * Width + X];
            pix^.Alpha := Value;
          end

    end else //if Transparent
      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
        begin
          pix := @TBitmapData32(Self.fData).fPixels^[Y * Width + X];
          pix^.Alpha := Value;
        end;
  end;
  AlphaBlend := true;
end;

procedure TOPBitmap.SetAlphaBlend(const Value: Boolean);
begin
  if PixelFormat = pf32bit then
    fAlphaBlend := Value else
    fAlphaBlend := false;
end;

procedure TOPBitmap.Clear;
begin
  Width := 0;
  Height := 0;
  fColorTableSize := 0;
  Transparent := false;
end;



function ReverseBits(b: Byte): Byte;
var c: Byte;
begin
  c := b;
  c := ((c shr 1) and $55) or ((c shl 1) and $AA);
  c := ((c shr 2) and $33) or ((c shl 2) and $CC);
  c := ((c shr 4) and $0F) or ((c shl 4) and $F0);
  result := c;
end;


function TOPBitmap.GetTransparentMask(Tolerance: Byte; var Data: PByte;
  ReversedBits, WordBoundary: Boolean): integer;
var x, y, i, cnt, aLineLength: integer;
begin
  if not Empty then
  begin

    if Width mod 8 > 0 then
      aLineLength := (Width div 8) + 1 else aLineLength := (Width div 8);

    if WordBoundary then
      if odd(aLineLength) then
        fMask.Width := ((aLineLength + 1) * 8)
      else
        fMask.Width := Width
    else fMask.Width := Width;

    fMask.Height := Height;

    cnt := 0;
    if Tolerance = 0 then
    begin

      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
          if Pixels[x, y] = fTransparentColor then
            fMask.SetNativePixel(x, y, false) else
            fMask.SetNativePixel(x, y, true);

    end else
    begin

      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
          if ColorInRange(Pixels[x, y], fTransparentColor, Tolerance) then
            fMask.SetNativePixel(x, y, false) else
            fMask.SetNativePixel(x, y, true);
    end;

    if ReversedBits then
      for i := 0 to fMask.fLineLength * fMask.Height do
        fMask.RawArray^[i] := ReverseBits(fMask.RawArray^[i]);

    Data := PByte(fMask.fPixels);
    Result := fMask.Height * fMask.LineLength;
  end;
end;

function TOPBitmap.GetFullMask(var Data: PByte): integer;
var x, y, i: integer;
begin
  if not Empty then
  begin
    fMask.Width := Width;
    fMask.Height := Height;

    for i := 0 to (fMask.fLineLength) * fMask.Height do fMask.RawArray^[i] := $FF;

    Data := PByte(fMask.fPixels);
    Result := fMask.Height * fMask.LineLength;
  end;
end;



{TOPBitmapCanvas}

constructor TOPBitmapCanvas.Create(Bitmap: TOPBitmap);
begin
  inherited Create;
  fBitmap := Bitmap;
  fBrush := TBrush.Create;
  fPen := TPen.Create;
end;

destructor TOPBitmapCanvas.Destroy;
begin
  fPen.free;
  fBrush.free;
  inherited;
end;


function TOPBitmapCanvas.GetPixel(X, Y: Integer): TColor;
begin
  if fBitmap.Data <> nil then Result := fBitmap.Data.Pixels[X, Y] else Result := clNone;
end;

procedure TOPBitmapCanvas.SetPixel(X, Y: Integer; const Value: TColor);
var NewCol: TColor;
begin
  if fBitmap.Data <> nil then
  begin
    fBitmap.Data.Pixels[X, Y] := Value;
  end;
end;

procedure TOPBitmapCanvas.FillRect(Rect: TRect);
var i, j: integer;
  Color: TColor;
begin
  Color := fBrush.Color;
  for i := Rect.Top to Rect.Bottom - 1 do
    for j := Rect.Left to Rect.Right - 1 do
      fBitmap.Data.Pixels[j, i] := Color;
end;

{$IFNDEF VER_VTV}
procedure TOPBitmapCanvas.LineTo(X, Y: Integer);
begin
  BresenhamLine(fPenPos.X, fPenPos.Y, X, Y, self, fPen.Color);
  MoveTo(X, Y);
end;
{$ENDIF}

procedure TOPBitmapCanvas.MoveTo(X, Y: Integer);
begin
  fPenPos.X := X;
  fPenPos.Y := Y;
end;

{$IFNDEF VER_VTV}
procedure TOPBitmapCanvas.Circle(CenterX, CenterY, Radius: Integer);
var X, Y: integer;
begin
  BresenhamCircle(CenterX, CenterY, Radius, self, fPen.Color);
end;
{$ENDIF}

procedure TOPBitmapCanvas.Draw(X, Y: integer; Bitmap: TCanvasOPBitmap);
var wid, hei: integer;
begin
  wid := Bitmap.Width;
  hei := Bitmap.Height;
  CopyRect(Rect(X, Y, X + wid, Y + hei), Bitmap.Canvas, Rect(0, 0, wid, hei));
end;


procedure BlendColors(SPix, DPix: PPixel32);
var alp1, alp2: integer;
begin
  if SPix^.Alpha = AlphaTransparent then exit else
    if SPix^.Alpha = AlphaOpaque then
      DPix^ := SPix^ else
    begin
      alp1 := SPix^.Alpha;
      alp2 := $FF - alp1;
      DPix^.Red := (DPix^.Red * alp2 + SPix^.Red * alp1) div $FF;
      DPix^.Green := (DPix^.Green * alp2 + SPix^.Green * alp1) div $FF;
      DPix^.Blue := (DPix^.Blue * alp2 + SPix^.Blue * alp1) div $FF;
    end;
end;


procedure TOPBitmapCanvas.CopyRect(const Dest: TRect; Canvas: TOPBitmapCanvas;
  const Source: TRect);
var Wid, Hei, x, y: integer;
  S, D: TRect;
  sp: TColor;


  procedure AdjustRect(var Rec: TRect; Width, Height: integer; Src: Boolean);
  begin

    if Rec.Left < 0 then
    begin
      if Src then Dec(D.Left, Rec.Left) else Dec(S.Left, Rec.Left);
      Rec.Left := 0;
    end;

    if Rec.Right > Width then Rec.Right := Width;

    if Rec.Top < 0 then
    begin
      if Src then Dec(D.Top, Rec.Top) else Dec(S.Top, Rec.Top);
      Rec.Top := 0;
    end;

    if Rec.Bottom > Height then Rec.Bottom := Height;
  end;

begin
  S := Source;
  D := Dest;

  AdjustRect(S, Canvas.fBitmap.Width, Canvas.fBitmap.Height, true);
  AdjustRect(D, fBitmap.Width, fBitmap.Height, false);

  Wid := Min(D.Right - D.Left, S.Right - S.Left);
  Hei := Min(D.Bottom - D.Top, S.Bottom - S.Top);

  if Canvas.fBitmap.fAlphaBlend then
  begin
    Assert(Canvas.fBitmap.PixelFormat = pf32bit, 'alphablend with 32 BPP only');
    fBitmap.PixelFormat := pf32bit;
    for y := 0 to Hei - 1 do
      for x := 0 to Wid - 1 do
      begin
        BlendColors(@TBitmapData32(Canvas.fBitmap.fData).fPixels^[(y + S.Top) * Canvas.fBitmap.Width + (S.Left + x)],
          @TBitmapData32(fBitmap.fData).fPixels^[(y + D.Top) * fBitmap.Width + (D.Left + x)]);
      end;
  end
  else
    if Canvas.fBitmap.Transparent then
      for y := 0 to Hei - 1 do
        for x := 0 to Wid - 1 do
        begin
          sp := Canvas.fBitmap.Pixels[S.Left + x, y + S.Top];
          if sp <> Canvas.fBitmap.TransparentColor then
            fBitmap.Pixels[D.Left + x, y + D.Top] := sp;
        end
    else
      for y := 0 to Hei - 1 do
        for x := 0 to Wid - 1 do
          fBitmap.Pixels[D.Left + x, y + D.Top] := Canvas.fBitmap.Pixels[S.Left + x, y + S.Top];
end;

{$IFNDEF VER_VTV}
procedure TOPBitmapCanvas.Resample(NewWidth, NewHeight: integer);
begin
  if NewWidth < fBitmap.Width then
    Stretch(NewWidth, NewHeight, sfHermite, DefaultFilterRadius[sfHermite], fBitmap) else
    Stretch(NewWidth, NewHeight, sfMitchell, DefaultFilterRadius[sfMitchell], fBitmap);

end;
{$ENDIF}

{ TCanvasOPBitmap }

constructor TCanvasOPBitmap.Create;
begin
  inherited;
  fCanvas := TOPBitmapCanvas.Create(Self);
end;

destructor TCanvasOPBitmap.Destroy;
begin
  fCanvas.free;
  inherited;
end;

{ TColorFinder }

procedure TColorFinder.AddColor(R, G, B: Byte);
var Col: PColorEntry;
begin
  GetMem(Col, SizeOf(TColorEntry));
  Col^.R := R;
  Col^.G := G;
  Col^.B := B;
  fPalette.Add(Col);
  fSorted := false;
end;


procedure TColorFinder.AddColor(Color: TColor);
begin
  AddColor(Byte(Color), Byte(Color shr 8), Byte(Color shr 16));
end;

procedure TColorFinder.ClearPalette;
var i: integer;
begin
  for i := 0 to fPalette.Count - 1 do
    FreeMem(fPalette[i], SizeOf(TColorEntry));
  fPalette.Clear;
  ClearMappings;
end;

constructor TColorFinder.Create;
begin
  fPalette := TList.create;
end;

destructor TColorFinder.Destroy;
begin
  ClearPalette;
  fPalette.free;
  inherited;
end;


function TColorFinder.NearestColor(R, G, B: Byte): integer;

var
  DistanceSquared: INTEGER;
  R1, G1, B1: Byte;
  i: INTEGER;
  SmallestDistanceSquared: INTEGER;
  col: TColor;
begin
  Result := 0;
  SmallestDistanceSquared := $1000000;


  for i := 0 to fPalette.Count - 1 do
  begin
    R1 := PColorEntry(fPalette[i])^.R;
    G1 := PColorEntry(fPalette[i])^.G;
    B1 := PColorEntry(fPalette[i])^.B;
    DistanceSquared := (R - R1) * (R - R1) + (G - G1) * (G - G1) + (B - B1) * (B - B1);
    if DistanceSquared < SmallestDistanceSquared then
    begin
      Result := i;
      if (R = R1) and (G = G1) and (B = B1) then exit;
      SmallestDistanceSquared := DistanceSquared;
    end
  end;
end;


function TColorFinder.GetColor(idx: integer): TColor;
var r, g, b: Byte;
begin
  GetColor(idx, r, g, b);
  Result := b shl 16 + g shl 8 + r;
end;

procedure TColorFinder.GetColor(idx: integer; var r, g, b: Byte);
begin
  if (idx < fPalette.Count) and (idx > -1) then
  begin
    R := PColorEntry(fPalette[idx])^.R;
    G := PColorEntry(fPalette[idx])^.G;
    B := PColorEntry(fPalette[idx])^.B;
  end;
end;


function TColorFinder.MapColors: Integer;
var
  x, y: Cardinal;
  i, j: Cardinal;
  Red, Green, Blue: Byte;
  Pcol: PInteger;
  Color: TColor;
begin
  Result := 0;
  ClearMappings;

  for y := 0 to fBitmap.Height - 1 do
    for x := 0 to fBitmap.Width - 1 do
    begin
      Color := fBitmap.Pixels[x, y];

      Red := Byte(Color);
      Green := Byte(Color shr 8);
      Blue := Byte(Color shr 16);

      //Small reduction of color space
      if CSpaceRedu > 0 then
      begin
        Dec(Red, Red mod CSpaceRedu);
        Dec(Green, Green mod CSpaceRedu);
        Dec(Blue, Blue mod CSpaceRedu);
      end;

      if (fMappings[Red, Green]) = nil then
      begin
        fMappings[Red, Green] := TList.Create;
        fMappings[Red, Green].Count := 256;
      end;
      if (fMappings[Red, Green].Items[Blue] = nil) then
      begin
        GetMem(Pcol, SizeOf(Integer));
        PCol^ := NearestColor(Red, Green, Blue);
        fMappings[Red, Green].Items[Blue] := PCol;
        Inc(Result);
      end;
    end;
end;

procedure TColorFinder.ClearMappings;
var i, j, k: Integer;
begin

  for j := 0 to $FF do
    for i := 0 to $FF do
    begin
      if Assigned(fMappings[i, j]) then
      begin
        for k := 0 to $FF do
          FreeMem(fMappings[i, j].Items[k], SizeOf(TColor));
        fMappings[i, j].Free;
      end;
      fMappings[i, j] := nil;
    end;
end;


function TColorFinder.GetMappingColor(const R, G, B: Byte): TColor;
begin
  Result := GetColor(GetMapping(R, G, B));
end;


function TColorFinder.GetMapping(const R, G, B: Byte): Integer;
var PCol: PInteger;
begin
  Result := -1;
  if fMappings[R, G] <> nil then
  begin
    PCol := fMappings[R, G].Items[B];
    if PCol <> nil then Result := PCol^;
  end;
end;

function TColorFinder.GetMappingColor(const Color: TColor): TColor;
begin
  Result := GetColor(GetMapping(Color));
end;

function TColorFinder.GetMapping(const Color: TColor): Integer;
begin
  Result := GetMapping(Color, Color shr 8, Color shr 16);
end;

procedure TColorFinder.SetBitmap(const Value: TOPBitmap);
begin
  if Value <> nil then
  begin
    fBitmap := Value;
    MapColors;
  end;
end;

procedure TColorFinder.SetPalette(Pal: array of TColor; Size: integer);
var PalSize, i: integer;
begin
  ClearPalette;
  if Size <> -1 then PalSize := Size else PalSize := High(Pal) + 1;
  for i := 0 to PalSize - 1 do AddColor(Pal[i]);
  if fBitmap <> nil then MapColors;
end;

function TColorFinder.GetPaletteSize: integer;
begin
  Result := fPalette.Count;
end;

{TOctreeNode}

constructor TOctreeNode.Create(const Level: Integer;
  const ColorBits: Integer;
  var LeafCount: Integer;
  var ReducibleNodes: TReducibleNodes);
var
  i: Integer;
begin
  PixelCount := 0;
  RedSum := 0;
  GreenSum := 0;
  BlueSum := 0;
  for i := Low(Child) to High(Child) do
    Child[i] := nil;

  IsLeaf := (Level = ColorBits);
  if IsLeaf
    then begin
    Next := nil;
    Inc(LeafCount);
  end
  else begin
    Next := ReducibleNodes[Level];
    ReducibleNodes[Level] := Self;
  end
end;


destructor TOctreeNode.Destroy;
var
  i: Integer;
begin
  for i := Low(Child) to High(Child) do
    Child[i].Free
end;


{TColorQuantizer}

constructor TColorQuantizer.Create(const MaxColors: Integer; const ColorBits: Integer);
var
  i: Integer;
begin
  Assert(ColorBits <= 8);

  FTree := nil;
  FLeafCount := 0;
  for i := Low(FReducibleNodes) to High(FReducibleNodes) do
    FReducibleNodes[i] := nil;

  FMaxColors := MaxColors;
  FColorBits := ColorBits
end;


destructor TColorQuantizer.Destroy;
begin
  if FTree <> nil
    then DeleteTree(FTree)
end;


procedure TColorQuantizer.GetColorTable(var RGBQuadArray: TRGBQuadArray256);
var
  Index: Integer;
begin
  Index := 0;
  GetPaletteColors(FTree, RGBQuadArray, Index)
end;



function TColorQuantizer.ProcessImage(Bmp: TOPBitmap): Boolean;
var
  col: TColor;
  i: Integer;
  j: Integer;
begin
  Result := True;
  if Bmp.GetDataSize > 0 then
  begin
    for j := 0 to Bmp.Height - 1 do
    begin
      for i := 0 to Bmp.Width - 1 do
      begin
        col := Bmp.Data.Pixels[i, j];
        AddColor(FTree, Byte(col), Byte(col shr 8), Byte(col shr 16),
          FColorBits, 0, FLeafCount, FReducibleNodes);
        while FLeafCount > FMaxColors do
          ReduceTree(FColorbits, FLeafCount, FReducibleNodes)
      end;
    end;
  end;
end;


procedure TColorQuantizer.AddColor(var Node: TOctreeNode;
  const r, g, b: Byte;
  const ColorBits: Integer;
  const Level: Integer;
  var LeafCount: Integer;
  var ReducibleNodes: TReducibleNodes);
const
  Mask: array[0..7] of Byte = ($80, $40, $20, $10, $08, $04, $02, $01);

var
  Index: Integer;
  Shift: Integer;
begin
  if Node = nil
    then Node := TOctreeNode.Create(Level, ColorBits, LeafCount, ReducibleNodes);

  if Node.IsLeaf
    then begin
    Inc(Node.PixelCount);
    Inc(Node.RedSum, r);
    Inc(Node.GreenSum, g);
    Inc(Node.BlueSum, b)
  end
  else begin
    Shift := 7 - Level;
    Index := (((r and mask[Level]) shr Shift) shl 2) or
      (((g and mask[Level]) shr Shift) shl 1) or
      ((b and mask[Level]) shr Shift);
    AddColor(Node.Child[Index], r, g, b, ColorBits, Level + 1,
      LeafCount, ReducibleNodes)
  end
end;



procedure TColorQuantizer.DeleteTree(var Node: TOctreeNode);
var
  i: Integer;
begin
  for i := Low(TReducibleNodes) to High(TReducibleNodes) do
  begin
    if Node.Child[i] <> nil
      then DeleteTree(Node.Child[i]);
  end;

  Node.Free;
  Node := nil;
end;


procedure TColorQuantizer.GetPaletteColors(const Node: TOctreeNode;
  var RGBQuadArray: TRGBQuadArray256;
  var Index: Integer);
var
  i: Integer;
begin
  if Node.IsLeaf
    then begin
    with RGBQuadArray[Index] do
    begin
      try
        rgbRed := Byte(Node.RedSum div Node.PixelCount);
        rgbGreen := Byte(Node.GreenSum div Node.PixelCount);
        rgbBlue := Byte(Node.BlueSum div Node.PixelCount);
        rgbReserved := 0;
      except
        rgbRed := 0;
        rgbGreen := 0;
        rgbBlue := 0;
        rgbReserved := 0;
      end;

      rgbReserved := 0
    end;
    INC(Index)
  end
  else begin
    for i := Low(Node.Child) to High(Node.Child) do
    begin
      if Node.Child[i] <> nil
        then GetPaletteColors(Node.Child[i], RGBQuadArray, Index)
    end
  end
end;


procedure TColorQuantizer.ReduceTree(const ColorBits: Integer;
  var LeafCount: Integer;
  var ReducibleNodes: TReducibleNodes);
var
  BlueSum: Integer;
  Children: Integer;
  GreenSum: Integer;
  i: Integer;
  Node: TOctreeNode;
  RedSum: Integer;
begin
  i := Colorbits - 1;
  while (i > 0) and (ReducibleNodes[i] = nil) do
    Dec(i);

  Node := ReducibleNodes[i];
  ReducibleNodes[i] := Node.Next;

  RedSum := 0;
  GreenSum := 0;
  BlueSum := 0;
  Children := 0;

  for i := Low(ReducibleNodes) to High(ReducibleNodes) do
  begin
    if Node.Child[i] <> nil
      then begin
      Inc(RedSum, Node.Child[i].RedSum);
      Inc(GreenSum, Node.Child[i].GreenSum);
      Inc(BlueSum, Node.Child[i].BlueSum);
      Inc(Node.PixelCount, Node.Child[i].PixelCount);
      Node.Child[i].Free;
      Node.Child[i] := nil;
      Inc(Children)
    end
  end;

  Node.IsLeaf := TRUE;
  Node.RedSum := RedSum;
  Node.GreenSum := GreenSum;
  Node.BlueSum := BlueSum;
  Dec(LeafCount, Children - 1)
end;


procedure TColorQuantizer.GetColorTable(AColorTable: POpenColorTableArray);
var
  Index: Integer;
  Qarr: TRGBQuadArray256;
var i: integer;
begin
  Index := 0;
  GetPaletteColors(FTree, QArr, Index);
  for i := 0 to ColorCount - 1 do
    AColorTable^[i] := (QArr[i].rgbRed shl 16) + (QArr[i].rgbGreen shl 8) + QArr[i].rgbBlue;
end;



{$IFDEF IMPORTTGRAPHIC}
{_$I tgraphicimpl.inc}
{$ENDIF}

{Other Functions}


procedure MakeWebPalette;
var r, g, b: integer;
  i: integer;
begin
  i := 0;
  for r := 0 to 5 do
    for g := 0 to 5 do
      for b := 0 to 5 do
      begin
        WebColors[i] := ((b * $33) shl 16) + ((g * $33) shl 8) + (r * $33);
        inc(i);
      end;
end;

procedure MakeGray256Palette;
var i: integer;
begin
  for i := 0 to $FF do Gray256Colors[i] := (i shl 16) + (i shl 8) + i;

end;

function MulDiv(Number, Num, Den: Integer): Integer;
begin
  if Den = 0 then
  begin
    Result := -1;
    Exit;
  end;
  Result := (Int64(Number) * Num) div Den;
end;

function ColorInRange(col1, col2: TColor; Range: Byte): Boolean;
begin
  Result := (abs(Byte(col1 shr 16) - Byte(col2 shr 16)) < Range - 1) and
    (abs(Byte(col1 shr 8) - Byte(col2 shr 8)) < Range - 2) and
    (abs(Byte(col1) - Byte(col2)) < Range)
end;


{$IFDEF INTEL_ASM}

function ByteSwapColor(Color: LongWord): LongWord; assembler; //about 25% faster than no asm.
asm
        BSWAP EAX
        SHR   EAX,8
end;

{$ELSE}

function ByteSwapColor(Color: LongWord): LongWord;
begin
  Pixel32(Result).Blue := Pixel32(Color).Red;
  Pixel32(Result).Green := Pixel32(Color).Green;
  Pixel32(Result).Red := Pixel32(Color).Blue;
  Pixel32(Result).Alpha := Pixel32(Color).Alpha;
end;

{$ENDIF}

initialization
  MakeWebPalette;
  MakeGray256Palette;
end.
