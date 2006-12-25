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
    procedure DoPaintGraph(Graph: array of Double; StartX, StartY, WL, A, NPoints: Integer);
    procedure DoCalculateNoise(Graph: array of Double; WL, NPoints: Integer);
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
begin
  SetLength(G1, 20 * 12);
  DoCalculateNoise(G1, 20, 12);
  DoPaintGraph(G1, 25,   25,  20, 250, 12);

{  SetLength(G3, 40 * 6);
  DoPaintGraph(G2, 325,  25,  40, 125,  6);

  SetLength(G3, 80 * 3);
  DoPaintGraph(G3, 25,  325,  80,  62,  3);}
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
procedure TMainWindow.DoCalculateNoise(Graph: array of Double; WL, NPoints: Integer);
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
procedure TMainWindow.DoPaintGraph(Graph: array of Double; StartX, StartY, WL, A, NPoints: Integer);
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
    Canvas.Ellipse(i * WL + StartX + 1, NormalizeNoise(Graph[i], A) + StartY + 1,
     i * WL + StartX - 1, NormalizeNoise(Graph[i], A) + StartY - 1);

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
  SelectInterpolation.Left := 100;
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

