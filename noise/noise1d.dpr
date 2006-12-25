program noise1d;

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
    SelectInterpolation: TComboBox;
    procedure DoPaint(Sender: TObject);
    procedure DoRefresh(Sender: TObject);
    function NormalizeNoise(x: Double): Integer;
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
  i, j, interpolation: Integer;
begin
  { Draws rulers }
  Canvas.MoveTo(25,  25 );
  Canvas.LineTo(25,  275);
  Canvas.LineTo(275, 275);
  
  { Draws 12 points and the interpolation between them }
  for i := 0 to 11 do
  begin
    Canvas.Ellipse(i * 20 + 25 + 1, NormalizeNoise(IntNoise(i)) + 1,
     i * 20 + 25 - 1, NormalizeNoise(IntNoise(i)) - 1);
     
    if (i = 11) then Continue;
    
    for j := 1 to 19 do
    begin
      case SelectInterpolation.ItemIndex of
       0: interpolation := NormalizeNoise(Linear_Interpolate(IntNoise(i), IntNoise(i + 1), j / 20));
       1: interpolation := NormalizeNoise(Cosine_Interpolate(IntNoise(i), IntNoise(i + 1), j / 20));
      else
        interpolation := NormalizeNoise(Cubic_Interpolate(IntNoise(i - 1),
         IntNoise(i), IntNoise(i + 1), IntNoise(i + 2), j / 20));
      end;

      Canvas.Pixels[i * 20 + 25 + j, interpolation] := clBlack;
    end;
  end;
end;

procedure TMainWindow.DoRefresh(Sender: TObject);
begin
  Repaint;
end;

function TMainWindow.NormalizeNoise(x: Double): Integer;
begin
  Result := Round( 25 + (x + 1.0) * 125 );
end;

constructor TMainWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Position := poScreenCenter;
  Width := 300;
  Height := 300;
  Caption := 'Noise 1D';

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

