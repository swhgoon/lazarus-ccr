program perlin1d;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  {$ifdef fpc}
  Interfaces, // this includes the LCL widgetset
  {$endif}
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  noise;

type

  { TMainWindow }

  TMainWindow = class(TForm)
  private
    { private declarations }
    G1, G2, G3, G4: array of double;
    SelectInterpolation: TComboBox;
    procedure DoPaint(Sender: TObject);
    procedure DoRefresh(Sender: TObject);
    procedure DoPaintGraph(var Graph: array of Double; StartX, StartY, WL, A, NPoints: Integer);
    procedure DoCalculateNoise(var Graph: array of Double; WL, NPoints: Integer);
    function NormalizeNoise(x: Double; Amplitude: Integer): Integer;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  vMainWindow: TMainWindow;

{ TMainWindow }

procedure TMainWindow.DoPaint(Sender: TObject);
var
  i: Integer;
begin
  SetLength(G1, 20 * 12);
  DoCalculateNoise(G1, 20, 12);
  DoPaintGraph(G1, 25,   25,  20, 250, 12);
  Canvas.TextOut(60, 15, '1st Harmonic');

  SetLength(G2, 10 * 24);
  DoCalculateNoise(G2, 10, 24);
  DoPaintGraph(G2, 325,  25,  10, 125,  24);
  Canvas.TextOut(460, 15, '2nd Harmonic');

  SetLength(G3, 5 * 48);
  DoCalculateNoise(G3, 5, 48);
  DoPaintGraph(G3, 25,  325,  5,  62,  48);
  Canvas.TextOut(60, 315, '3rd Harmonic');

  { The 4th graphic is a the sum of the first 3, using the amplitudes to
   ponderate the values }
  SetLength(G4, 20 * 12);
  for i := 0 to 20 * 12 - 1 do G4[i] := ( G1[i] * 250 + G2[i] * 125 + G3[i] * 62 ) / (250 + 125 + 62);
  DoPaintGraph(G4, 325,  325,  20, 250, 12);
  Canvas.TextOut(460, 315, 'Perlin Noise');
end;

procedure TMainWindow.DoRefresh(Sender: TObject);
begin
  Repaint;
end;

{*******************************************************************
*  TMainWindow.DoCalculateNoise ()
*
*  DESCRIPTION:    Creates a array with a 1D Noise plus interpolation
*
*  PARAMETERS:     Graph   - Array to store the points
*                  WL      - Wavelength in units.
*                            Those are filled with interpolation
*                  NPoints - Number of Noise points to be created
*
*******************************************************************}
procedure TMainWindow.DoCalculateNoise(var Graph: array of Double; WL, NPoints: Integer);
var
  i, j: Integer;
  interpolation: Double;
begin
  for i := 0 to NPoints - 1 do
  begin
    Graph[i * WL] := IntNoise(i);

    if (i = NPoints - 1) then Continue;

    for j := 1 to WL - 1 do
    begin
      case SelectInterpolation.ItemIndex of
       0: interpolation := Linear_Interpolate(IntNoise(i), IntNoise(i + 1), j / WL);
       1: interpolation := Cosine_Interpolate(IntNoise(i), IntNoise(i + 1), j / WL);
      else
        interpolation := Cubic_Interpolate(IntNoise(i - 1), IntNoise(i),
         IntNoise(i + 1), IntNoise(i + 2), j / WL);
      end;

      Graph[i * WL + j] := interpolation;
    end;
  end;
end;

{*******************************************************************
*  TMainWindow.DoPaintGraph ()
*
*  DESCRIPTION:    Draws a graphic that represents a 1D Noise function
*
*  PARAMETERS:     Graph   - Array to store the points
*                  StartX  - Starting X position for the graphic
*                  StartY  - Starting Y position for the graphic
*                  WL      - Wavelength in pixels
*                  A       - Amplitude in pixels
*                  NPoints - Number of points to be drawn
*
*******************************************************************}
procedure TMainWindow.DoPaintGraph(var Graph: array of Double; StartX, StartY, WL, A, NPoints: Integer);
var
  i, j: Integer;
begin
  { Draws rulers }
  Canvas.MoveTo(StartX,       StartY      );
  Canvas.LineTo(StartX,       StartY + 250);
  Canvas.LineTo(StartX + 250, StartY + 250);

  { Draws NPoints points and the interpolation between them }
  for i := 0 to NPoints - 1 do
  begin
    Canvas.Ellipse(i * WL + StartX + 1, NormalizeNoise(Graph[i * WL], A) + StartY + 1,
     i * WL + StartX - 1, NormalizeNoise(Graph[i * WL], A) + StartY - 1);

    if (i = NPoints - 1) then Continue;

    for j := 1 to WL - 1 do
    begin
      Canvas.Pixels[i * WL + StartX + j, NormalizeNoise(Graph[i * WL + j], A) + StartY] := clBlack;
    end;
  end;
end;

function TMainWindow.NormalizeNoise(x: Double; Amplitude: Integer): Integer;
begin
  Result := Round( 125 + x * Amplitude / 2 );
end;

constructor TMainWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Position := poScreenCenter;
  Width := 600;
  Height := 600;
  Caption := 'Perlin Noise 1D';

  OnPaint := DoPaint;

  SelectInterpolation := TComboBox.Create(Self);
  SelectInterpolation.Parent := Self;
  SelectInterpolation.Items.Add('Linear Interpolation');
  SelectInterpolation.Items.Add('Cosine Interpolation');
  SelectInterpolation.Items.Add('Cubic Interpolation');
  SelectInterpolation.Left := 200;
  SelectInterpolation.Width := 200;
  SelectInterpolation.ItemIndex := 0;

  SelectInterpolation.OnChange := DoRefresh;
end;

destructor TMainWindow.Destroy;
begin

  inherited Destroy;
end;

begin
  Application.Initialize;
  Application.CreateForm(TMainWindow, vMainWindow);
  Application.Run;
end.

