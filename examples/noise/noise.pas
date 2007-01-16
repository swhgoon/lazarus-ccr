unit noise;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils; 

function IntNoise(x: Integer): Double;

function Linear_Interpolate(a, b, x: Double): Double;
function Cosine_Interpolate(a, b, x: Double): Double;
function Cubic_Interpolate(v0, v1, v2, v3, x: Double): Double;

implementation

function IntNoise(x: Integer): Double;
var
  xl: Integer;
begin
  xl := (x shl 13) xor x;
  Result := (xl * (xl * xl * 15731 + 789221) + 1376312589) and $7fffffff;
  Result := 1.0 - (Result / 1073741824.0);
end;

function Linear_Interpolate(a, b, x: Double): Double;
begin
  Result := a * (1-x) + b * x;
end;

function Cosine_Interpolate(a, b, x: Double): Double;
var
  f, ft: Double;
begin
  ft := x * Pi;
  f := (1.0 - cos(ft)) * 0.5;
  Result := a * (1 - f) + b * f;
end;
  
function Cubic_Interpolate(v0, v1, v2, v3, x: Double): Double;
var
  P, Q, R, S: Double;
begin
  P := (v3 - v2) - (v0 - v1);
  Q := (v0 - v1) - P;
  R := v2 - v0;
  S := v1;

  Result := P * x * x * x + Q * x * x + R * x + S;
end;

end.

