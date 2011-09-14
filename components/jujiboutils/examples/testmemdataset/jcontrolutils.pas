unit jcontrolutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ReplaceChar(const s: string; ch1: char; ch2: char): string;
function CountChar(const s: string; ch: char): integer;
procedure Split(const Delimiter: char; Input: string; Strings: TStrings);
function NormalizeDate(const Value: string; theValue: TDateTime): string;
function NormalizeDateSeparator(const s: string): string;

implementation

function ReplaceChar(const s: string; ch1: char; ch2: char): string;
var
  i: integer;
begin
  Result := s;
  for i := 1 to length(Result) do
    if Result[i] = ch1 then
      Result[i] := ch2;
end;

function CountChar(const s: string; ch: char): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to length(s) do
    if s[i] = ch then
      Inc(Result);
end;

procedure Split(const Delimiter: char; Input: string; Strings: TStrings);
begin
  Assert(Assigned(Strings));
  Strings.Clear;
  Strings.Delimiter := Delimiter;
  Strings.DelimitedText := Input;
end;

function NormalizeDate(const Value: string; theValue: TDateTime): string;
var
  texto: string;
  i: integer;
  d, m, y: word;
begin
  if theValue = 0 then
    DecodeDate(Now, y, m, d)
  else
    decodedate(theValue, y, m, d);
  // normalize date
  texto := Value;
  texto:= NormalizeDateSeparator(texto);
  //texto := replacechar(texto, '.', DateSeparator);
  //texto := replacechar(texto, '-', DateSeparator);
  //texto := replacechar(texto, '/', DateSeparator);
  i := countchar(texto, DateSeparator);

  case i of
    1: texto := texto + DateSeparator + IntToStr(y);
    0: texto := texto + DateSeparator + IntToStr(m) + DateSeparator + IntToStr(y);
  end;
  Result := texto;
end;

function NormalizeDateSeparator(const s: string): string;
var
  i: integer;
begin
  Result := s;
  for i := 1 to length(Result) do
    if Result[i] in ['.', ',', '/', '-'] then  // valid date separators
      Result[i] := DateSeparator;
end;

end.

