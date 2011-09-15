unit jcontrolutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function ReplaceChar(const s: string; ch1: char; ch2: char): string;
function CountChar(const s: string; ch: char): integer;
procedure Split(const Delimiter: char; Input: string; Strings: TStrings);
function NormalizeDate(const Value: string; theValue: TDateTime;
  const theFormat: string = ''): string;
function NormalizeDateSeparator(const s: string): string;
function IsValidDateString(const Value: string): boolean;

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

function NormalizeDate(const Value: string; theValue: TDateTime;
  const theFormat: string): string;
var
  texto: string;
  i: integer;
  d, m, y: word;
  ds, ms, ys: string;
  aDate: TDateTime;
  tokens: TStringList;
begin
  if theValue = 0 then
    DecodeDate(Now, y, m, d)
  else
    decodedate(theValue, y, m, d);
  ds := IntToStr(d);
  ms := IntToStr(m);
  ys := IntToStr(y);
  texto := Value;
  texto := NormalizeDateSeparator(texto);
  Result := texto;   // default value
  i := countchar(texto, DateSeparator);
  tokens := TStringList.Create;
  Split(DateSeparator, texto, tokens);
  if tokens.Count > 0 then
  begin
    // for now only working date forma d/m/y
    // TODO add logic to make it working with m/d/y and ISO format
    // update jdbgridutils to work with this code
    if tokens[0] <> '' then
      ds := tokens[0];
    if (tokens.Count > 1) and (tokens[1] <> '') then
      ms := tokens[1];
    if (tokens.Count > 2) and (tokens[2] <> '') then
      ys := tokens[2];
    texto := ds + DateSeparator + ms + DateSeparator + ys;
    if IsValidDateString(texto) then
    begin
      Result:= texto;
    end;
  end;
  tokens.Free;
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

function IsValidDateString(const Value: string): boolean;
begin
  if StrToDateDef(Value, MaxDateTime) = MaxDateTime then
    Result := False
  else
    Result := True;
end;

end.

