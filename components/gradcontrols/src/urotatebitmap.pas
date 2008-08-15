unit urotatebitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Buttons, LCLType,
     IntfGraphics, fpImage, LCLIntf, Types;
  
type
    TRotateDirection = (rdRight, rdNormal, rdLeft);

    TRotatedBitmap = class( TObject )
    private
        FNormalImage, FRotatedImage : TLazIntfImage;
        FRotateTo : TRotateDirection;
        function GetBitmap : TBitmap;
        procedure SetRotateTo(const Value : TRotateDirection);
        procedure DoRotate;
    protected
        function GetWidth : Integer;
        function GetHeight : Integer;
    public
        constructor Create; virtual;
        destructor Destroy; override;
        procedure Free;
        procedure LoadBitmap(var b : TBitmap);
        //property Bitmap : TBitmap read GetBitmap;
        property Image : TLazIntfImage read FRotatedImage;
        property RotateDirection : TRotateDirection read FRotateTo write SetRotateTo;
        procedure Draw(X,Y: Integer;var b : TBitmap); virtual;
        procedure Draw(X,Y: Integer;var b : TLazIntfImage); virtual;
        procedure Draw(X,Y: Integer;var b : TBitmap;
                  TransparentColor : TColor); virtual;
        procedure Draw(X,Y: Integer;var b : TLazIntfImage;
                  TransparentColor : TFPColor); virtual;
        property Width : Integer read GetWidth;
        property Height : Integer read GetHeight;
    end;

    TRotatedGlyph = class(TRotatedBitmap)
    private
        FNormalGlyphBitmap : TBitmap;
        FNormalGlyph : TButtonGlyph;
        FTransparentColor : TColor;
        FButtonState : TButtonState;
        function GetGlyph : TBitmap;
        procedure SetGlyph(Value: TBitmap);
        procedure SetButtonState(Value: TButtonState);
        procedure SetTransparentColor(Value: TColor);
    public
        constructor Create; override;
        destructor Destroy; override;
        procedure Draw(X,Y: Integer;var b : TBitmap;
                  TransparentColor : TColor); override;
        procedure Draw(X, Y: Integer; var b: TLazIntfImage;
           TransparentColor: TFPColor); override;
        procedure Update;
        property State : TButtonState read FButtonState write SetButtonState;
        property Glyph : TBitmap read GetGlyph write SetGlyph;
        property TransparentColor: TColor read FTransparentColor write SetTransparentColor;
    end;

    TRotatedText = class(TRotatedBitmap)
    private
       FBitmap : TBitmap;
       FText : String;
       procedure SetText(const Value: String);
       function GetCanvas : TCanvas;
       procedure PaintText;
    public
       constructor Create; override;
       destructor Destroy; override;
       procedure Update;
       property Text : String read FText write SetText;
       property Canvas : TCanvas read GetCanvas;
       procedure Draw(X,Y: Integer;var b : TBitmap); override;
       procedure Draw(X,Y: Integer;var b : TLazIntfImage); override;
    end;

    operator := (const b : TBitmap) : TLazIntfImage;
    operator := (const i : TLazIntfImage) : TBitmap;

implementation

uses
    LCLProc;

constructor TRotatedGlyph.Create;
begin
    inherited;

    FNormalGlyph := TButtonGlyph.Create;
    //FNormalGlyph.SetTransparentMode(gtmTransparent);

    FTransparentColor:=clFuchsia;
    FNormalGlyphBitmap := TBitmap.Create;
    
    FButtonState := bsUp;
end;

destructor TRotatedGlyph.Destroy;
begin
    DebugLn('TRotatedGlyph.Destroy');
    DebugLn('FNormalGlyph.Free Assigned: %s',[BoolToStr(Assigned(FNormalGlyph),true)]);
    if Assigned(FNormalGlyph) then FNormalGlyph.Free;
    DebugLn('FNormalGlyphBitmap.Free Assigned: %s',[BoolToStr(Assigned(FNormalGlyphBitmap),true)]);
    if Assigned(FNormalGlyphBitmap) then FNormalGlyphBitmap.Free;

    DebugLn('Inherited');
    inherited;
end;

procedure TRotatedGlyph.SetTransparentColor(Value: TColor);
begin
    FTransparentColor:=Value;
    
    Update;
end;

function TRotatedGlyph.GetGlyph : TBitmap;
begin
    Result := FNormalGlyphBitmap;
end;

procedure TRotatedGlyph.SetGlyph(Value: TBitmap);
begin
    FNormalGlyphBitmap.Assign(Value);
    //FNormalGlyph.Glyph.TransparentMode:=tmFixed;
    //FNormalGlyph.Glyph.Transparent:=true;
    
    Update;
end;

procedure TRotatedGlyph.SetButtonState(Value: TButtonState);
begin
    FButtonState:=Value;

    Update;
end;

procedure TRotatedGlyph.Update;
var
   TempBitmap : TBitmap;
   SrcIntf, TrgIntf : TLazIntfImage;
   i, j : Integer;
begin
    TempBitmap := TBitmap.Create;
    TempBitmap.Width:=FNormalGlyphBitmap.Width;
    TempBitmap.Height:=FNormalGlyphBitmap.Height;

    TempBitmap.Canvas.Brush.Color:=clNone;
    TempBitmap.Canvas.FillRect(0,0,TempBitmap.Width,TempBitmap.Height);
    
    SrcIntf := FNormalGlyphBitmap;
    
    TrgIntf := TempBitmap;
    {TmpIntf.DataDescription := GetDescriptionFromDevice(0);
    TmpIntf.SetSize(TempBitmap.Width, TempBitmap.Height);
     }
    
    for i := 0 to TempBitmap.Width-1 do
      for j := 0 to TempBitmap.Height-1 do
        if SrcIntf.Colors[i,j] <> TColorToFPColor(FTransparentColor) then
         TrgIntf.Colors[i,j] := SrcIntf.Colors[i,j];
    
    {FNormalGlyph.Draw(TempBitmap.Canvas,Rect(0,0,TempBitmap.Width,TempBitmap.Height),
      Point(0,0), FButtonState, true, 0);
    }

    TempBitmap.Free;

    TempBitmap := TrgIntf;
    
    LoadBitmap(TempBitmap);

    TempBitmap.Free;
    TrgIntf.Free;
    SrcIntf.Free;
end;

procedure TRotatedGlyph.Draw(X,Y: Integer;var b : TBitmap;
                  TransparentColor : TColor);
var
   Temp : TLazIntfImage;
begin
    Temp := b;
    b.Free;
    Draw(X,Y,Temp, TColorToFPColor(TransparentColor));
    b := Temp;
    Temp.Free;
end;

procedure TRotatedGlyph.Draw(X, Y: Integer; var b: TLazIntfImage;
           TransparentColor: TFPColor);
var
   TempBitmap,Trg : TBitmap;
begin
   Trg := TBitmap.Create;

   //First Rotate the Glyph then Draw it with an other State
   inherited Draw(X,Y,b,TransparentColor);
   
   TempBitmap := b;
   b.Free;

   Trg.Width:=b.Width;
   Trg.Height:=b.Height;
   
   FNormalGlyph.Glyph.Assign(TempBitmap);
   
   FNormalGlyph.Draw(Trg.Canvas,Rect(0,0,Trg.Width,Trg.Height),
      Point(0,0), FButtonState, true, 0);
      
   b := Trg;
   
   Trg.Free;
   TempBitmap.Free;
end;

constructor TRotatedText.Create;
begin
    inherited;

    FBitmap := TBitmap.Create;
    FText:='X';
    SetText('');
end;

destructor TRotatedText.Destroy;
begin
    FBitmap.Free;

    inherited;
end;

procedure TRotatedText.Update;
begin
    PaintText;
end;

procedure TRotatedText.SetText(const Value: String);
begin
    if FText <> Value then
    begin
       FText:=Value;

       PaintText;
    end;
end;

function TRotatedText.GetCanvas : TCanvas;
begin
    Result := FBitmap.Canvas;
end;

procedure TRotatedText.PaintText;
var
   TextSize : TSize;
begin
    TextSize := FBitmap.Canvas.TextExtent(FText);

    {$ifdef LCLWin32}
    //win32 does not comput correct text extent when Italic style is set.
    //small workaround to this bug
    //not sure if other widgetsets alsoa have this bug. Enable it only for win32 for now
    if fsItalic in FBitmap.Canvas.Font.Style then
      Inc(TextSize.cx, 4);
    {$endif}
    FBitmap.SetSize(TextSize.cx, TextSize.cy);

    //check to allow Text with Fuchsia color
    if FBitmap.Canvas.Font.Color = clFuchsia then
      FBitmap.Canvas.Brush.Color := clWhite
    else
      FBitmap.Canvas.Brush.Color := clFuchsia;

    FBitmap.Canvas.FillRect(0,0, FBitmap.Width, FBitmap.Height);

    FBitmap.Canvas.TextOut(0,0, FText);

    Inherited LoadBitmap(FBitmap);
end;

procedure TRotatedText.Draw(X,Y: Integer;var b : TBitmap);
begin
    Inherited Draw(X,Y,b,FBitmap.Canvas.Brush.Color);
end;

procedure TRotatedText.Draw(X,Y: Integer;var b : TLazIntfImage);
begin
    Inherited Draw(X,Y,b,TColorToFPColor(FBitmap.Canvas.Brush.Color));
end;

operator := (const b : TBitmap) : TLazIntfImage;
begin
  Result := TLazIntfImage.Create(0,0);
  Result.LoadFromBitmap(b.Handle,b.MaskHandle);
end;

operator := (const i : TLazIntfImage) : TBitmap;
begin
  Result := TBitmap.Create;
  Result.LoadFromIntfImage(i);
end;

constructor TRotatedBitmap.Create;
begin
     //inherited;

     FRotateTo:=rdNormal;
end;

destructor TRotatedBitmap.Destroy;
begin
     DebugLn('Destroy');
     if Assigned(FNormalImage) then FNormalImage.Free;
     if Assigned(FRotatedImage) then FRotatedImage.Free;
end;

procedure TRotatedBitmap.Free;
begin
    inherited;
    //if Self<>nil then
       //Self.Destroy;
end;

function TRotatedBitmap.GetBitmap : TBitmap;
var
   TempIntf : TLazIntfImage;
begin
   if FRotateTo = rdNormal then
      TempIntf := FNormalImage
   else
      TempIntf := FRotatedImage;

    if Assigned(TempIntf) then
       Result := TempIntf;
end;

procedure TRotatedBitmap.SetRotateTo(const Value : TRotateDirection);
begin
    //WriteLn(FRotateTo<>Value);
    if FRotateTo<>Value then
    begin
        //WriteLn('SetRotateTo');
        FRotateTo:=Value;

        DoRotate;
    end;
end;

procedure TRotatedBitmap.LoadBitmap(var b : TBitmap);
begin
   if FNormalImage <> nil then FNormalImage.Free;

   if FRotatedImage = nil then begin
       FRotatedImage := TLazIntfImage.Create(0,0);
       FRotatedImage.DataDescription := GetDescriptionFromDevice(0);
   end;

   DebugLn('Assigned: B',BoolToStr(Assigned(FNormalImage),true));
   FNormalImage := b;
   //FNormalImage := TLazIntfImage.Create(0,0);
   DebugLn('Assigned: A',BoolToStr(Assigned(FNormalImage),true));

   if FRotateTo <> rdNormal then DoRotate;
end;

procedure TRotatedBitmap.DoRotate;
var
   px, py, iw, ih, nx, ny : Integer;
   CurColor: TFPColor;
begin
   if FRotateTo=rdNormal then Exit;

   {if Assigned(FRotatedImage) then
      FRotatedImage.Free;

   FRotatedImage := TLazIntfImage.Create(0,0);

   FRotatedImage.DataDescription := GetDescriptionFromDevice(0);
    }
   FRotatedImage.SetSize({FNormalImage.Width,FNormalImage.Height}
      FNormalImage.Height,FNormalImage.Width);

   FRotatedImage.FillPixels(colWhite);

   //WriteLn(FRotatedImage.Width, ' ', FRotatedImage.Height);

   for px := 0 to FNormalImage.Width-1 do
    for py := 0 to FNormalImage.Height-1 do
     begin
         if FRotateTo = rdRight then
         begin
             nx := FRotatedImage.Width-1-py;
             ny := px;
         end else begin
             nx := py;
             ny := FRotatedImage.Height-1-px;
         end;

         //WriteLn(nx, ' ', ny, ' ', px, ' ', py);

         CurColor := FNormalImage.Colors[px,py];

         FRotatedImage.Colors[nx,ny]:= CurColor;
     end;
end;

procedure TRotatedBitmap.Draw(X,Y: Integer;var b : TBitmap);
begin
    Draw(X,Y,b,clNone);
end;

procedure TRotatedBitmap.Draw(X,Y: Integer;var b : TLazIntfImage);
begin
    Draw(X,Y,b,TColorToFPColor(clNone));
end;

procedure TRotatedBitmap.Draw(X,Y: Integer;var b : TBitmap; TransparentColor : TColor);
var
   TempIntfImage : TLazIntfImage;
begin
   TempIntfImage := b;
   b.Free;
   Draw(X,Y, TempIntfImage, TColorToFPColor(TransparentColor));
   b := TempIntfImage;
   TempIntfImage.Free;
end;

procedure TRotatedBitmap.Draw(X,Y: Integer;var b : TLazIntfImage; TransparentColor : TFPColor);
var
   px, py : Integer;
   TempIntf : TLazIntfImage;
begin
   if FRotateTo = rdNormal then
      TempIntf := FNormalImage
   else
      TempIntf := FRotatedImage;

   //WriteLn(Assigned(FNormalImage));
   //WriteLn(Assigned(FRotatedImage));
         
   for px := 0 to TempIntf.Width-1 do
    for py := 0 to TempIntf.Height-1 do
     if (TempIntf.Colors[px,py] <> TransparentColor)
     AND ((X+px) < b.Width) AND ((Y+py) < b.Height)
     AND ((X+px) >= 0) AND ((Y+py) >= 0) then
     begin
      //WriteLn(px, ' ', py);
      b.Colors[X+px,Y+py]:=TempIntf.Colors[px,py];
     end;
end;

function TRotatedBitmap.GetWidth : Integer;
begin
   Result := -1;
   if (FRotateTo = rdNormal) AND Assigned(FNormalImage) then
      Result := FNormalImage.Width
   else if Assigned(FRotatedImage) then
      Result := FRotatedImage.Width;
end;

function TRotatedBitmap.GetHeight : Integer;
begin
   Result := -1;
   if (FRotateTo = rdNormal) AND Assigned(FNormalImage) then
      Result := FNormalImage.Height
   else if Assigned(FRotatedImage) then
      Result := FRotatedImage.Height;
end;

end.

