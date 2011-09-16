{ jcontrolutils

  Copyright (C) 2011 Julio Jiménez Borreguero
  Contact: jujibo at gmail dot com

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file license-jujiboutils.txt and COPYING.LGPL, included in this distribution,
  for details about the license.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

unit jcontrolutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

function CountChar(const s: string; ch: char): integer;
procedure Split(const Delimiter: char; Input: string; Strings: TStrings);
function NormalizeDate(const Value: string; theValue: TDateTime;
  const theFormat: string = ''): string;
function NormalizeDateSeparator(const s: string): string;
function IsValidDateString(const Value: string): boolean;

implementation

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
  aDateFormat: string;
  aChar: char;

  procedure LittleEndianForm;
  begin
    // Note: only numeric input allowed (months names not implemented)
    if tokens[0] <> '' then
      ds := tokens[0];
    if (tokens.Count > 1) and (tokens[1] <> '') then
      ms := tokens[1];
    if (tokens.Count > 2) and (tokens[2] <> '') then
      ys := tokens[2];
    texto := ds + DateSeparator + ms + DateSeparator + ys;
  end;

  procedure MiddleEndianForm;
  begin
    if tokens[0] <> '' then
      ms := tokens[0];
    if (tokens.Count > 1) and (tokens[1] <> '') then
      ds := tokens[1];
    if (tokens.Count > 2) and (tokens[2] <> '') then
      ys := tokens[2];
    texto := ms + DateSeparator + ds + DateSeparator + ys;
  end;

  procedure BigEndianForm;
  begin
    if tokens[0] <> '' then
      ys := tokens[0];
    if (tokens.Count > 1) and (tokens[1] <> '') then
      ms := tokens[1];
    if (tokens.Count > 2) and (tokens[2] <> '') then
      ds := tokens[2];
    texto := ys + DateSeparator + ms + DateSeparator + ds;
  end;

begin
  if theFormat = '' then
    aDateFormat := ShortDateFormat
  else
    aDateFormat := theFormat;
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
    aChar := aDateFormat[1];
    case aChar of
      'd', 'D': LittleEndianForm;
      'm', 'M': MiddleEndianForm;
      'y', 'Y': BigEndianForm;
    end;

    if IsValidDateString(texto) then
    begin
      aDate := StrToDate(texto);
      Result := FormatDateTime(aDateFormat, aDate);
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

