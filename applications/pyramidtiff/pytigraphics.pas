{ Graphic functions for pyramidtiff.

  Copyright (C) 2008  Mattias Gaertner  mattias@freepascal.org

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
unit PyTiGraphics;

{$mode objfpc}{$H+}

{$inline on}

interface

uses
  Math, sysutils, Classes, FPimage,
  LazLogger, FPCanvas, FPWriteTiff, FPTiffCmn;

type
  TPTMemImgDesc = record
    Gray: boolean; // true = red=green=blue, false: a RGB image
    Depth: word; // 8 or 16 bit
    HasAlpha: boolean;
  end;

  { TPTMemImgBase }

  TPTMemImgBase = class(TFPCustomImage)
  private
    FDesc: TPTMemImgDesc;
  public
    property Desc: TPTMemImgDesc read FDesc;
  end;
  TPTMemImgBaseClass = class of TPTMemImgBase;

  { TPTMemImgGray16Bit }

  TPTMemImgGray16Bit = class(TPTMemImgBase)
  protected
    FData: PWord;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TPTMemImgGrayAlpha16BitValue = packed record
    g,a: word;
  end;
  PPTMemImgGrayAlpha16BitValue = ^TPTMemImgGrayAlpha16BitValue;

  { TPTMemImgGrayAlpha16Bit }

  TPTMemImgGrayAlpha16Bit = class(TPTMemImgBase)
  protected
    FData: PPTMemImgGrayAlpha16BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  { TPTMemImgGray8Bit }

  TPTMemImgGray8Bit = class(TPTMemImgBase)
  protected
    FData: PByte;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TPTMemImgGrayAlpha8BitValue = packed record
    g,a: byte;
  end;
  PPTMemImgGrayAlpha8BitValue = ^TPTMemImgGrayAlpha8BitValue;

  { TPTMemImgGrayAlpha8Bit }

  TPTMemImgGrayAlpha8Bit = class(TPTMemImgBase)
  protected
    FData: PPTMemImgGrayAlpha8BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TPTMemImgRGBA8BitValue = packed record
    r,g,b,a: byte;
  end;
  PPTMemImgRGBA8BitValue = ^TPTMemImgRGBA8BitValue;

  { TPTMemImgRGBA8Bit }

  TPTMemImgRGBA8Bit = class(TPTMemImgBase)
  protected
    FData: PPTMemImgRGBA8BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TPTMemImgRGB8BitValue = packed record
    r,g,b: byte;
  end;
  PPTMemImgRGB8BitValue = ^TPTMemImgRGB8BitValue;

  { TPTMemImgRGB8Bit }

  TPTMemImgRGB8Bit = class(TPTMemImgBase)
  protected
    FData: PPTMemImgRGB8BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TPTMemImgRGB16BitValue = packed record
    r,g,b: word;
  end;
  PPTMemImgRGB16BitValue = ^TPTMemImgRGB16BitValue;

  { TPTMemImgRGB16Bit }

  TPTMemImgRGB16Bit = class(TPTMemImgBase)
  protected
    FData: PPTMemImgRGB16BitValue;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  { TPTMemImgRGBA16Bit }

  TPTMemImgRGBA16Bit = class(TPTMemImgBase)
  protected
    FData: PFPColor;
    function GetInternalColor(x, y: integer): TFPColor; override;
    function GetInternalPixel({%H-}x, {%H-}y: integer): integer; override;
    procedure SetInternalColor (x, y: integer; const Value: TFPColor); override;
    procedure SetInternalPixel({%H-}x, {%H-}y: integer; {%H-}Value: integer); override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    procedure SetSize(AWidth, AHeight: integer); override;
  end;

  TCreateCompatibleMemImgEvent = procedure(Sender: TObject; Img: TFPCustomImage;
          NewWidth, NewHeight: integer; out NewImage: TFPCustomImage) of object;

  { TLinearInterpolation }

  TLinearInterpolation = class(TFPCustomInterpolation)
  private
    procedure CreatePixelWeights(OldSize, NewSize: integer;
      out Entries: Pointer; out EntrySize: integer; out Support: integer);
  protected
    procedure Execute(x,y,w,h: integer); override;
    function Filter(x: double): double; virtual;
    function MaxSupport: double; virtual;
  end;

{ Create a descriptor to select a memimg class }
function GetPTMemImgDesc(Gray: boolean; Depth: word; HasAlpha: boolean): TPTMemImgDesc;

{ Returns a memimg class that fits the descriptor }
function GetPTMemImgClass(const Desc: TPTMemImgDesc): TPTMemImgBaseClass;

{ Create a memimg with the descriptor }
function CreatePTMemImg(const Desc: TPTMemImgDesc; Width, Height: integer): TFPCustomImage;

{ Create a memimg with the same features as Img.
  If Img is a TPTMemImgBaseClass it will create that.
  Otherwise it returns a memimg that fits the Img using GetMinimumPTDesc. }
function CreateCompatiblePTMemImg(Img: TFPCustomImage; Width, Height: integer
  ): TFPCustomImage;

{ As CreateCompatiblePTMemImg, but the image has always an alpha channel. }
function CreateCompatiblePTMemImgWithAlpha(Img: TFPCustomImage;
  Width, Height: integer): TFPCustomImage;

{ Returns the smallest descriptor that allows to store the Img.
  It returns HasAlpha=false if all pixel are opaque.
  It returns Gray=true if all red=green=blue.
  It returns Depth=8 if all lo byte equals the hi byte or all lo bytes are 0.
  To ignore rounding errors you can pass a FuzzyDepth. For example a FuzzyDepth
  of 3 ignores the lower 3 bits when comparing.  }
function GetMinimumPTDesc(Img: TFPCustomImage; FuzzyDepth: word = 4): TPTMemImgDesc;

{ Create a smaller memimg with the same information as Img.
  Pass FreeImg=true to call Img.Free }
function GetMinimumPTMemImg(Img: TFPCustomImage; FreeImg: boolean;
  FuzzyDepth: word = 4): TFPCustomImage;

procedure SetFPImgExtraTiff(const Desc: TPTMemImgDesc; Img: TFPCustomImage;
  ClearTiffExtras: boolean);

function dbgs(const Desc: TPTMemImgDesc): string; overload;

implementation

procedure SetFPImgExtraTiff(const Desc: TPTMemImgDesc; Img: TFPCustomImage;
  ClearTiffExtras: boolean);
begin
  if ClearTiffExtras then
    FPTiffCmn.ClearTiffExtras(Img);
  if Desc.Gray then begin
    Img.Extra[TiffPhotoMetric]:='1';
    Img.Extra[TiffGrayBits]:=IntToStr(Desc.Depth);
  end else begin
    Img.Extra[TiffPhotoMetric]:='2';
    Img.Extra[TiffRedBits]:=IntToStr(Desc.Depth);
    Img.Extra[TiffGreenBits]:=IntToStr(Desc.Depth);
    Img.Extra[TiffBlueBits]:=IntToStr(Desc.Depth);
  end;
  if Desc.HasAlpha then
    Img.Extra[TiffAlphaBits]:=IntToStr(Desc.Depth)
  else
    Img.Extra[TiffAlphaBits]:='0';
end;

function dbgs(const Desc: TPTMemImgDesc): string;
begin
  Result:='Depth='+dbgs(Desc.Depth)
    +',Gray='+dbgs(Desc.Gray)
    +',HasAlpha='+dbgs(Desc.HasAlpha);
end;

function GetPTMemImgDesc(Gray: boolean; Depth: word; HasAlpha: boolean
  ): TPTMemImgDesc;
begin
  Result.Gray:=Gray;
  Result.Depth:=Depth;
  Result.HasAlpha:=HasAlpha;
end;

function GetPTMemImgClass(const Desc: TPTMemImgDesc): TPTMemImgBaseClass;
begin
  if Desc.Gray then begin
    if Desc.HasAlpha then begin
      // gray, alpha
      if Desc.Depth<=8 then
        Result:=TPTMemImgGrayAlpha8Bit
      else
        Result:=TPTMemImgGrayAlpha16Bit;
    end else begin
      // gray, no alpha
      if Desc.Depth<=8 then
        Result:=TPTMemImgGray8Bit
      else
        Result:=TPTMemImgGray16Bit;
    end;
  end else begin
    // RGB
    if Desc.HasAlpha then begin
      // RGB, alpha
      if Desc.Depth<=8 then
        Result:=TPTMemImgRGBA8Bit
      else
        Result:=TPTMemImgRGBA16Bit;
    end else begin
      // RGB, no alpha
      if Desc.Depth<=8 then
        Result:=TPTMemImgRGB8Bit
      else
        Result:=TPTMemImgRGB16Bit;
    end;
  end;
end;

function CreatePTMemImg(const Desc: TPTMemImgDesc; Width, Height: integer
  ): TFPCustomImage;
var
  ImgClass: TPTMemImgBaseClass;
begin
  ImgClass:=GetPTMemImgClass(Desc);
  Result:=ImgClass.Create(Width,Height);
end;

function CreateCompatiblePTMemImg(Img: TFPCustomImage; Width, Height: integer
  ): TFPCustomImage;
begin
  if Img is TPTMemImgBase then
    Result:=CreatePTMemImg(TPTMemImgBase(Img).Desc,Width,Height)
  else
    Result:=CreatePTMemImg(GetMinimumPTDesc(Img),Width,Height);
  //DebugLn(['CreateCompatibleQVMemImg '+Img.ClassName+' '+Result.ClassName]);
end;

function CreateCompatiblePTMemImgWithAlpha(Img: TFPCustomImage; Width,
  Height: integer): TFPCustomImage;
var
  Desc: TPTMemImgDesc;
begin
  if Img is TPTMemImgBase then
    Desc:=TPTMemImgBase(Img).Desc
  else
    Desc:=GetMinimumPTDesc(Img);
  Desc.HasAlpha:=true;
  Result:=CreatePTMemImg(Desc,Width,Height);
end;

function GetMinimumPTDesc(Img: TFPCustomImage; FuzzyDepth: word = 4): TPTMemImgDesc;
var
  AllLoEqualsHi, AllLoAre0: Boolean;
  FuzzyMaskLoHi: Word;

  procedure Need16Bit(c: word); inline;
  var
    l: Byte;
  begin
    c:=c and FuzzyMaskLoHi;
    l:=Lo(c);
    AllLoAre0:=AllLoAre0 and (l=0);
    AllLoEqualsHi:=AllLoEqualsHi and (l=Hi(c));
  end;

var
  TestGray: Boolean;
  TestAlpha: Boolean;
  Test16Bit: Boolean;
  BaseImg: TPTMemImgBase;
  ImgDesc: TPTMemImgDesc;
  y: Integer;
  x: Integer;
  col: TFPColor;
  FuzzyMaskWord: Word;
  FuzzyOpaque: Word;
begin
  TestGray:=true;
  TestAlpha:=true;
  Test16Bit:=FuzzyDepth<8;
  Result.HasAlpha:=false;
  Result.Gray:=true;
  Result.Depth:=8;
  if Img is TPTMemImgBase then begin
    BaseImg:=TPTMemImgBase(Img);
    ImgDesc:=BaseImg.Desc;
    if ImgDesc.Depth<=8 then Test16Bit:=false;
    if ImgDesc.Gray then TestGray:=false;
    if not ImgDesc.HasAlpha then TestAlpha:=false;
  end;

  if (not TestGray) and (not TestAlpha) and (not Test16Bit) then exit;

  FuzzyMaskWord:=Word($ffff) shl FuzzyDepth;
  FuzzyOpaque:=alphaOpaque and FuzzyMaskWord;
  FuzzyMaskLoHi:=Word(lo(FuzzyMaskWord))+(Word(lo(FuzzyMaskWord)) shl 8);
  AllLoAre0:=true;
  AllLoEqualsHi:=true;
  for y:=0 to Img.Height-1 do begin
    for x:=0 to Img.Width-1 do begin
      col:=Img.Colors[x,y];
      if TestAlpha and ((col.alpha and FuzzyMaskWord)<>FuzzyOpaque) then begin
        TestAlpha:=false;
        Result.HasAlpha:=true;
        if (not TestGray) and (not Test16Bit) then break;
      end;
      if TestGray
      and ((col.red and FuzzyMaskWord)<>(col.green and FuzzyMaskWord))
      or ((col.red and FuzzyMaskWord)<>(col.blue and FuzzyMaskWord)) then begin
        TestGray:=false;
        Result.Gray:=false;
        if (not TestAlpha) and (not Test16Bit) then break;
      end;
      if Test16Bit then begin
        Need16Bit(col.red);
        Need16Bit(col.green);
        Need16Bit(col.blue);
        Need16Bit(col.alpha);
        if (not AllLoAre0) and (not AllLoEqualsHi) then begin
          Test16Bit:=false;
          Result.Depth:=16;
          if (not TestAlpha) and (not TestGray) then break;
        end;
      end;
    end;
  end;
end;

function GetMinimumPTMemImg(Img: TFPCustomImage; FreeImg: boolean;
  FuzzyDepth: word = 4): TFPCustomImage;
var
  Desc: TPTMemImgDesc;
  ImgClass: TPTMemImgBaseClass;
  y: Integer;
  x: Integer;
begin
  Desc:=GetMinimumPTDesc(Img,FuzzyDepth);
  //debugln(['GetMinimumQVMemImg Depth=',Desc.Depth,' Gray=',Desc.Gray,' HasAlpha=',Desc.HasAlpha]);
  ImgClass:=GetPTMemImgClass(Desc);
  if Img.ClassType=ImgClass then
    exit(Img);
  Result:=CreatePTMemImg(Desc,Img.Width,Img.Height);
  for y:=0 to Img.Height-1 do
    for x:=0 to Img.Width-1 do
      Result.Colors[x,y]:=Img.Colors[x,y];
  if FreeImg then
    Img.Free;
end;

function ColorRound (c : double) : word;
begin
  if c > $FFFF then
    result := $FFFF
  else if c < 0.0 then
    result := 0
  else
    result := round(c);
end;

{ TPTMemImgGrayAlpha16Bit }

function TPTMemImgGrayAlpha16Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TPTMemImgGrayAlpha16BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=v.g;
  Result.green:=Result.red;
  Result.blue:=Result.red;
  Result.alpha:=v.a;
end;

function TPTMemImgGrayAlpha16Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TPTMemImgGrayAlpha16Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  v: TPTMemImgGrayAlpha16BitValue;
begin
  v.g:=Value.red;
  v.a:=Value.alpha;
  FData[x+y*Width]:=v;
end;

procedure TPTMemImgGrayAlpha16Bit.SetInternalPixel(x, y: integer; Value: integer
  );
begin

end;

constructor TPTMemImgGrayAlpha16Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetPTMemImgDesc(true,16,true);
  inherited Create(AWidth, AHeight);
end;

destructor TPTMemImgGrayAlpha16Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TPTMemImgGrayAlpha16Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TPTMemImgGrayAlpha16BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth, AHeight);
end;

{ TPTMemImgGrayAlpha8Bit }

function TPTMemImgGrayAlpha8Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TPTMemImgGrayAlpha8BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=(v.g shl 8)+v.g;
  Result.green:=Result.red;
  Result.blue:=Result.red;
  Result.alpha:=(v.a shl 8)+v.a;
end;

function TPTMemImgGrayAlpha8Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TPTMemImgGrayAlpha8Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  v: TPTMemImgGrayAlpha8BitValue;
begin
  v.g:=Value.red shr 8;
  v.a:=Value.alpha shr 8;
  FData[x+y*Width]:=v;
end;

procedure TPTMemImgGrayAlpha8Bit.SetInternalPixel(x, y: integer; Value: integer
  );
begin

end;

constructor TPTMemImgGrayAlpha8Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetPTMemImgDesc(true,8,true);
  inherited Create(AWidth, AHeight);
end;

destructor TPTMemImgGrayAlpha8Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TPTMemImgGrayAlpha8Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TPTMemImgGrayAlpha8BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth, AHeight);
end;

{ TPTMemImgGray16Bit }

function TPTMemImgGray16Bit.GetInternalColor(x, y: integer): TFPColor;
begin
  Result.red:=FData[x+y*Width];
  Result.green:=Result.red;
  Result.blue:=Result.red;
  Result.alpha:=alphaOpaque;
end;

function TPTMemImgGray16Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TPTMemImgGray16Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
begin
  FData[x+y*Width]:=Value.red;
end;

procedure TPTMemImgGray16Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TPTMemImgGray16Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetPTMemImgDesc(true,16,false);
  inherited Create(AWidth, AHeight);
end;

destructor TPTMemImgGray16Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TPTMemImgGray16Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(Word)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TPTMemImgGray8Bit }

function TPTMemImgGray8Bit.GetInternalColor(x, y: integer): TFPColor;
begin
  Result.red:=FData[x+y*Width];
  Result.red:=(Word(Result.red) shl 8)+Result.red;
  Result.green:=Result.red;
  Result.blue:=Result.red;
  Result.alpha:=alphaOpaque;
end;

function TPTMemImgGray8Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TPTMemImgGray8Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
begin
  FData[x+y*Width]:=Value.red shr 8;
end;

procedure TPTMemImgGray8Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TPTMemImgGray8Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetPTMemImgDesc(true,8,false);
  inherited Create(AWidth, AHeight);
end;

destructor TPTMemImgGray8Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TPTMemImgGray8Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(Byte)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TPTMemImgRGBA8Bit }

function TPTMemImgRGBA8Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TPTMemImgRGBA8BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=(v.r shl 8)+v.r;
  Result.green:=(v.g shl 8)+v.g;
  Result.blue:=(v.b shl 8)+v.b;
  Result.alpha:=(v.a shl 8)+v.a;
end;

function TPTMemImgRGBA8Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TPTMemImgRGBA8Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  v: TPTMemImgRGBA8BitValue;
begin
  v.r:=Value.red shr 8;
  v.g:=Value.green shr 8;
  v.b:=Value.blue shr 8;
  v.a:=Value.alpha shr 8;
  FData[x+y*Width]:=v;
end;

procedure TPTMemImgRGBA8Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TPTMemImgRGBA8Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetPTMemImgDesc(false,8,true);
  inherited Create(AWidth, AHeight);
end;

destructor TPTMemImgRGBA8Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TPTMemImgRGBA8Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TPTMemImgRGBA8BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TPTMemImgRGB8Bit }

function TPTMemImgRGB8Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TPTMemImgRGB8BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=(v.r shl 8)+v.r;
  Result.green:=(v.g shl 8)+v.g;
  Result.blue:=(v.b shl 8)+v.b;
  Result.alpha:=alphaOpaque;
end;

function TPTMemImgRGB8Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TPTMemImgRGB8Bit.SetInternalColor(x, y: integer; const Value: TFPColor
  );
var
  v: TPTMemImgRGB8BitValue;
begin
  v.r:=Value.red shr 8;
  v.g:=Value.green shr 8;
  v.b:=Value.blue shr 8;
  FData[x+y*Width]:=v;
end;

procedure TPTMemImgRGB8Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TPTMemImgRGB8Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetPTMemImgDesc(false,8,false);
  inherited Create(AWidth, AHeight);
end;

destructor TPTMemImgRGB8Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TPTMemImgRGB8Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TPTMemImgRGB8BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TPTMemImgRGB16Bit }

function TPTMemImgRGB16Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TPTMemImgRGB16BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=v.r;
  Result.green:=v.g;
  Result.blue:=v.b;
  Result.alpha:=alphaOpaque;
end;

function TPTMemImgRGB16Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TPTMemImgRGB16Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  v: TPTMemImgRGB16BitValue;
begin
  v.r:=Value.red;
  v.g:=Value.green;
  v.b:=Value.blue;
  FData[x+y*Width]:=v;
end;

procedure TPTMemImgRGB16Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TPTMemImgRGB16Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetPTMemImgDesc(false,16,false);
  inherited Create(AWidth, AHeight);
end;

destructor TPTMemImgRGB16Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TPTMemImgRGB16Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TPTMemImgRGB16BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TPTMemImgRGBA16Bit }

function TPTMemImgRGBA16Bit.GetInternalColor(x, y: integer): TFPColor;
begin
  Result:=FData[x+y*Width];
end;

function TPTMemImgRGBA16Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TPTMemImgRGBA16Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
begin
  FData[x+y*Width]:=Value;
end;

procedure TPTMemImgRGBA16Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TPTMemImgRGBA16Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetPTMemImgDesc(false,16,true);
  inherited Create(AWidth, AHeight);
end;

destructor TPTMemImgRGBA16Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TPTMemImgRGBA16Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPColor)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TLinearInterpolation }

procedure TLinearInterpolation.CreatePixelWeights(OldSize, NewSize: integer;
  out Entries: Pointer; out EntrySize: integer; out Support: integer);
// create an array of #NewSize entries. Each entry starts with an integer
// for the StartIndex, followed by #Support singles for the pixel weights.
// The sum of weights for each entry is 1.
var
  Entry: Pointer;

  procedure SetSupport(NewSupport: integer);
  begin
    Support:=NewSupport;
    EntrySize:=SizeOf(integer)+SizeOf(Single)*Support;
    Getmem(Entries,EntrySize*NewSize);
    Entry:=Entries;
  end;

var
  i: Integer;
  Factor: double;
  StartPos: Double;
  StartIndex: Integer;
  j: Integer;
  FirstValue: Double;
  //Sum: double;
begin
  if NewSize=OldSize then begin
    SetSupport(1);
    for i:=0 to NewSize-1 do begin
      // 1:1
      PInteger(Entry)^:=i;
      inc(Entry,SizeOf(Integer));
      PSingle(Entry)^:=1.0;
      inc(Entry,SizeOf(Single));
    end;
  end else if NewSize<OldSize then begin
    // shrink
    SetSupport(Max(2,(OldSize+NewSize-1) div NewSize));
    Factor:=double(OldSize)/double(NewSize);
    for i:=0 to NewSize-1 do begin
      StartPos:=Factor*i;
      StartIndex:=Floor(StartPos);
      PInteger(Entry)^:=StartIndex;
      inc(Entry,SizeOf(Integer));
      // first pixel
      FirstValue:=(1.0-(StartPos-double(StartIndex)));
      PSingle(Entry)^:=FirstValue/Factor;
      inc(Entry,SizeOf(Single));
      // middle pixel
      for j:=1 to Support-2 do begin
        PSingle(Entry)^:=1.0/Factor;
        inc(Entry,SizeOf(Single));
      end;
      // last pixel
      PSingle(Entry)^:=(Factor-FirstValue-(Support-2))/Factor;
      inc(Entry,SizeOf(Single));
    end;
  end else begin
    // enlarge
    if OldSize=1 then begin
      SetSupport(1);
      for i:=0 to NewSize-1 do begin
        // nothing to interpolate
        PInteger(Entry)^:=0;
        inc(Entry,SizeOf(Integer));
        PSingle(Entry)^:=1.0;
        inc(Entry,SizeOf(Single));
      end;
    end else begin
      SetSupport(2);
      Factor:=double(OldSize-1)/double(NewSize);
      for i:=0 to NewSize-1 do begin
        StartPos:=Factor*i+Factor/2;
        StartIndex:=Floor(StartPos);
        PInteger(Entry)^:=StartIndex;
        inc(Entry,SizeOf(Integer));
        // first pixel
        FirstValue:=(1.0-(StartPos-double(StartIndex)));
        // convert linear distribution
        FirstValue:=Min(1.0,Max(0.0,Filter(FirstValue/MaxSupport)));
        PSingle(Entry)^:=FirstValue;
        inc(Entry,SizeOf(Single));
        // last pixel
        PSingle(Entry)^:=1.0-FirstValue;
        inc(Entry,SizeOf(Single));
      end;
    end;
  end;
  if Entry<>Entries+EntrySize*NewSize then
    raise Exception.Create('TSimpleInterpolation.Execute inconsistency');

  {WriteLn('CreatePixelWeights Old=',OldSize,' New=',NewSize,' Support=',Support,' EntrySize=',EntrySize,' Factor=',FloatToStr(Factor));
  Entry:=Entries;
  for i:=0 to NewSize-1 do begin
    StartIndex:=PInteger(Entry)^;
    inc(Entry,SizeOf(Integer));
    write(i,' Start=',StartIndex);
    Sum:=0;
    for j:=0 to Support-1 do begin
      FirstValue:=PSingle(Entry)^;
      inc(Entry,SizeOf(Single));
      write(' ',FloatToStr(FirstValue));
      Sum:=Sum+FirstValue;
    end;
    writeln(' Sum=',FloatToStr(Sum));
  end;}
end;

procedure TLinearInterpolation.Execute(x, y, w, h: integer);
// paint Image on Canvas at x,y,w*h
var
  dy: Integer;
  dx: Integer;
  HorzResized: PFPColor;
  xEntries: Pointer; // size:integer,weight1:single,weight2:single,...
  xEntrySize: integer;
  xSupport: integer;// how many horizontal pixel are needed to created one new pixel
  yEntries: Pointer; // size:integer,weight1:single,weight2:single,...
  yEntrySize: integer;
  ySupport: integer;// how many vertizontal pixel are needed to created one new pixel
  NewSupportLines: LongInt;
  yEntry: Pointer;
  SrcStartY: LongInt;
  LastSrcStartY: LongInt;
  sy: Integer;
  xEntry: Pointer;
  sx: LongInt;
  cx: Integer;
  f: Single;
  NewCol: TFPColor;
  Col: TFPColor;
  CurEntry: Pointer;
  NewRed, NewGreen, NewBlue, NewAlpha: Single;
begin
  //WriteLn('TSimpleInterpolation.Execute Src=',image.width,'x',image.Height,' Dest=',x,',',y,',',w,'x',h);
  if (w<=0) or (h<=0) or (image.Width=0) or (image.Height=0) then exit;

  xEntries:=nil;
  yEntries:=nil;
  HorzResized:=nil;
  try
    CreatePixelWeights(image.Width,w,xEntries,xEntrySize,xSupport);
    CreatePixelWeights(image.Height,h,yEntries,yEntrySize,ySupport);
    //WriteLn('TSimpleInterpolation.Execute xSupport=',xSupport,' ySupport=',ySupport);
    // create temporary buffer for the horizontally resized pixel for the current
    // y line
    GetMem(HorzResized,w*ySupport*SizeOf(TFPColor));

    SrcStartY:=0;
    for dy:=0 to h-1 do begin
      if dy=0 then begin
        yEntry:=yEntries;
        SrcStartY:=PInteger(yEntry)^;
        NewSupportLines:=ySupport;
      end else begin
        LastSrcStartY:=SrcStartY;
        inc(yEntry,yEntrySize);
        SrcStartY:=PInteger(yEntry)^;
        NewSupportLines:=SrcStartY-LastSrcStartY;
        //WriteLn('TSimpleInterpolation.Execute dy=',dy,' SrcStartY=',SrcStartY,' LastSrcStartY=',LastSrcStartY,' NewSupportLines=',NewSupportLines);
        // move lines up
        if (NewSupportLines>0) and (ySupport>NewSupportLines) then
          System.Move(HorzResized[NewSupportLines*w],
                      HorzResized[0],
                      (ySupport-NewSupportLines)*w*SizeOf(TFPColor));
      end;

      // compute new horizontally resized line(s)
      for sy:=ySupport-NewSupportLines to ySupport-1 do begin
        xEntry:=xEntries;
        for dx:=0 to w-1 do begin
          sx:=PInteger(xEntry)^;
          inc(xEntry,SizeOf(integer));
          NewRed:=0.0;
          NewGreen:=0.0;
          NewBlue:=0.0;
          NewAlpha:=0.0;
          for cx:=sx to sx+xSupport-1 do begin
            f:=PSingle(xEntry)^;
            inc(xEntry,SizeOf(Single));
            Col:=image.Colors[cx,SrcStartY+sy];
            NewRed:=NewRed+Col.red*f;
            NewGreen:=NewGreen+Col.green*f;
            NewBlue:=NewBlue+Col.blue*f;
            NewAlpha:=NewAlpha+Col.alpha*f;
          end;
          NewCol.red:=Min(round(NewRed),$ffff);
          NewCol.green:=Min(round(NewGreen),$ffff);
          NewCol.blue:=Min(round(NewBlue),$ffff);
          NewCol.alpha:=Min(round(NewAlpha),$ffff);
          HorzResized[dx+sy*w]:=NewCol;
        end;
      end;

      // compute new vertically resized line
      for dx:=0 to w-1 do begin
        CurEntry:=yEntry+SizeOf(integer);
        NewRed:=0.0;
        NewGreen:=0.0;
        NewBlue:=0.0;
        NewAlpha:=0.0;
        for sy:=0 to ySupport-1 do begin
          f:=PSingle(CurEntry)^;
          inc(CurEntry,SizeOf(Single));
          Col:=HorzResized[dx+sy*w];
          NewRed:=NewRed+Col.red*f;
          NewGreen:=NewGreen+Col.green*f;
          NewBlue:=NewBlue+Col.blue*f;
          NewAlpha:=NewAlpha+Col.alpha*f;
        end;
        NewCol.red:=Min(round(NewRed),$ffff);
        NewCol.green:=Min(round(NewGreen),$ffff);
        NewCol.blue:=Min(round(NewBlue),$ffff);
        NewCol.alpha:=Min(round(NewAlpha),$ffff);
        Canvas.Colors[x+dx,y+dy]:=NewCol;
      end;
    end;
  finally
    if xEntries<>nil then FreeMem(xEntries);
    if yEntries<>nil then FreeMem(yEntries);
    if HorzResized<>nil then FreeMem(HorzResized);
  end;
end;

function TLinearInterpolation.Filter(x: double): double;
begin
  Result:=x;
end;

function TLinearInterpolation.MaxSupport: double;
begin
  Result:=1.0;
end;

end.

