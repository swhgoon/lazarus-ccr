{ jinpututils

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

unit jinpututils;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils;
type

 { TJCurrencyCtrl }

 TJCurrencyCtrl = class(TObject)
 private
   myEdit: TCustomEdit;
   theValue: Currency;
   fFormat : String;
   fDecimales : Integer;
   function getDecimales: integer;
   function getFormat: string;
   function getValue: Currency;
   procedure myEditEnter(Sender: TObject);
   procedure myEditExit(Sender: TObject);
   procedure formatInput;
   procedure setDecimales(const AValue: integer);
   procedure setFormat(const AValue: string);
   procedure OnKeyPress(Sender: TObject; var key : char);
   function redondear(const AValue: Currency; const NDecimales: Integer) : Currency;
   function IsValidFloat(const Value: string): Boolean;
   procedure setValue(const AValue: Currency);
 public
   property format : String read getFormat write setFormat;
   property decimales : Integer read getDecimales write setDecimales;
   property value : Currency read getValue write setValue;
   constructor Create(widget : TCustomEdit);
   constructor Create(widget : TCustomEdit ;aDecimals : Integer; aFormat : String);
 end;

 { TJIntegerCtrl }

 TJIntegerCtrl = class(TObject)
 private
   myEdit: TCustomEdit;
   theValue: Integer;
   fFormat : String;
   function getFormat: string;
   function getValue: Integer;
   procedure myEditEnter(Sender: TObject);
   procedure myEditExit(Sender: TObject);
   procedure formatInput;
   procedure setFormat(const AValue: string);
   procedure OnKeyPress(Sender: TObject; var key : char);
   function IsValidInteger(const Value: string): Boolean;
   procedure setValue(const AValue: Integer);
 public
   property format : String read getFormat write setFormat;
   property value : Integer read getValue write setValue;
   constructor Create(widget : TCustomEdit);
 end;

  { TJDateCtrl }

 TJDateCtrl = class(TObject)
 private
   myEdit: TCustomEdit;
   theValue: TDateTime;
   fFormat : String;
   function getFormat: string;
   function getValue: TDateTime;
   procedure myEditEnter(Sender: TObject);
   procedure myEditExit(Sender: TObject);
   procedure formatInput;
   procedure setFormat(const AValue: string);
   procedure OnKeyPress(Sender: TObject; var key : char);
   function IsValidDate(const Value: string): Boolean;
   procedure setValue(const AValue: TDateTime);
 public
   function isNull : Boolean;
   property format : String read getFormat write setFormat;
   property value : TDateTime read getValue write setValue;
   constructor Create(widget : TCustomEdit);
 end;

function replacechar(const s: string; ch1: char; ch2: char): string;
function countchar(const s: string; ch: char): integer;
procedure Split(const Delimiter: Char; Input: string; Strings: TStrings);

implementation

uses
 math,dateutils,Graphics,Dialogs;


{ TJCurrencyCtrl }

function TJCurrencyCtrl.getDecimales: integer;
begin
  result:= fDecimales;
end;

function TJCurrencyCtrl.getFormat: string;
begin
  result := fFormat;
end;

function TJCurrencyCtrl.getValue: Currency;
begin
  result := theValue;
end;

procedure TJCurrencyCtrl.myEditEnter(Sender: TObject);
begin
  myEdit.Caption:= FloatToStr(theValue);
  myEdit.SelectAll;
end;

procedure TJCurrencyCtrl.myEditExit(Sender: TObject);
begin
  if IsValidFloat(myEdit.Caption) then
    theValue:= StrToCurr(myEdit.Caption)
  else
  begin
    ShowMessage(myEdit.Caption + ' no es un valor válido');
    myEdit.SetFocus;
  end;
  theValue:= redondear(theValue, fDecimales);
  formatInput;
end;

procedure TJCurrencyCtrl.formatInput;
begin
  myEdit.Caption:= FormatFloat(format, theValue);
end;

procedure TJCurrencyCtrl.setDecimales(const AValue: integer);
begin
  // Poner decimales, redondear valor y mostrarlo
  if AValue >= 0 then
    fDecimales:= AValue;
end;

procedure TJCurrencyCtrl.setFormat(const AValue: string);
begin
  fFormat:= AValue;
  formatInput;
end;

procedure TJCurrencyCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if (Key in ['.',',']) then Key := Decimalseparator;
  if (key = DecimalSeparator) and (Pos(key, myEdit.Caption) >0) then key := #0;
  if not (Key in ['0'..'9',DecimalSeparator,'+','-',#8,#9]) then Key := #0;
  if (Key = DecimalSeparator) and (fDecimales = 0) then Key := #0;
end;

function TJCurrencyCtrl.redondear(const AValue: Currency;
  const NDecimales: Integer): Currency;
begin
  redondear := round(AValue * power(10, NDecimales)) / power(10, NDecimales);
end;

function TJCurrencyCtrl.IsValidFloat(const Value: string): Boolean;
begin
 if StrToCurrDef(value, MaxCurrency) = MaxCurrency then Result := False
 else
   Result := True;
end;

procedure TJCurrencyCtrl.setValue(const AValue: Currency);
begin
   theValue:= redondear(AValue, fDecimales);
   formatInput;
end;

constructor TJCurrencyCtrl.Create(widget: TCustomEdit);
begin
  myEdit:= widget;
  myEdit.OnEnter:= @myEditEnter;
  myEdit.OnExit:= @myEditExit;
  myEdit.OnKeyPress:= @OnKeyPress;
  myEdit.caption:= ''; // avoid prev input
  format:= '#,0.00 €';
  fDecimales:= 2;
end;
constructor TJCurrencyCtrl.Create(widget: TCustomEdit; aDecimals: Integer;
  aFormat: String);
begin
  myEdit:= widget;
  myEdit.OnEnter:= @myEditEnter;
  myEdit.OnExit:= @myEditExit;
  myEdit.OnKeyPress:= @OnKeyPress;
  myEdit.caption:= ''; // avoid prev input
  format:= aFormat;
  fDecimales:= aDecimals;
end;


{ TJIntegerCtrl }

function TJIntegerCtrl.getFormat: string;
begin
  Result := fFormat;
end;

function TJIntegerCtrl.getValue: Integer;
begin
  Result := theValue;
end;

procedure TJIntegerCtrl.myEditEnter(Sender: TObject);
begin
  myEdit.Caption:= IntToStr(theValue);
  myEdit.SelectAll;
end;

procedure TJIntegerCtrl.myEditExit(Sender: TObject);
begin
  if IsValidInteger(myEdit.Caption) then
    theValue:= StrToInt(myEdit.Caption)
  else
  begin
    ShowMessage(myEdit.Caption + ' no es un valor válido');
    myEdit.SetFocus;
  end;
  formatInput;
end;

procedure TJIntegerCtrl.formatInput;
begin
  myEdit.Caption:= FormatFloat(format, theValue);
end;

procedure TJIntegerCtrl.setFormat(const AValue: string);
begin
  fFormat:= AValue;
  formatInput;
end;

procedure TJIntegerCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if not (Key in ['0'..'9',#8,#9,'-']) then Key := #0;
end;

function TJIntegerCtrl.IsValidInteger(const Value: string): Boolean;
begin
 if StrToIntDef(value, MaxInt) = MaxInt then
    Result := False
 else
   Result := True;
end;

procedure TJIntegerCtrl.setValue(const AValue: Integer);
begin
  theValue:= AValue;
  formatInput;
end;

constructor TJIntegerCtrl.Create(widget: TCustomEdit);
begin
  myEdit:= widget;
  myEdit.OnEnter:= @myEditEnter;
  myEdit.OnExit:= @myEditExit;
  myEdit.OnKeyPress:= @OnKeyPress;
  myEdit.caption:= ''; // avoid prev input
  format:= '0';
end;

{ TJDateCtrl }

function TJDateCtrl.getFormat: string;
begin
  result := fFormat;
end;

function TJDateCtrl.getValue: TDateTime;
begin
  Result := theValue;
end;

procedure TJDateCtrl.myEditEnter(Sender: TObject);
begin
  if theValue <> 0 then
    myEdit.Caption:= FormatDateTime(format, theValue)
  else
    myEdit.Caption := '';
  myEdit.SelectAll;
end;

procedure TJDateCtrl.myEditExit(Sender: TObject);
begin
  if Length(myEdit.Caption) = 0 then
    theValue := 0
  else
  if IsValidDate(myEdit.Caption) then
    theValue:= StrToDate(myEdit.Caption)
  else
  begin
    ShowMessage(myEdit.Caption + ' no es una fecha válida');
    myEdit.SetFocus;
  end;
  formatInput;
end;

procedure TJDateCtrl.formatInput;
begin
  if theValue <> 0 then
    myEdit.Caption:= FormatDateTime(format, theValue);
end;

procedure TJDateCtrl.setFormat(const AValue: string);
begin
  fFormat:= AValue;
  formatInput;
end;

procedure TJDateCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if not (Key in ['0'..'9',#8,#9, '.', '-', '/']) then Key := #0;
end;

function TJDateCtrl.IsValidDate(const Value: string): Boolean;
var
 texto : string;
 i : integer;
 d, m, y : word;
begin
 Result := false;
 // normalize date
 texto:= myEdit.Caption;
 texto := replacechar(texto, '.', DateSeparator);
 texto := replacechar(texto, '-', DateSeparator);
 texto := replacechar(texto, '/', DateSeparator);
 i := countchar(texto, DateSeparator);
 decodedate(theValue, y, m, d);
 case i of
  1 : texto := texto + DateSeparator + inttostr(y);
  0 : texto := texto + DateSeparator + inttostr(m) + DateSeparator + inttostr(y);
 end;
 myEdit.Caption:= texto;
 // comprobar que la fecha es valida
 if StrToDateDef(texto, MaxDateTime) = MaxDateTime then
   Result := False
 else
   Result := True;
end;

procedure TJDateCtrl.setValue(const AValue: TDateTime);
begin
  theValue:= AValue;
  formatInput;
end;

function TJDateCtrl.isNull: Boolean;
begin
  Result := theValue = 0;
end;

constructor TJDateCtrl.Create(widget: TCustomEdit);
begin
  myEdit:= widget;
  myEdit.OnEnter:= @myEditEnter;
  myEdit.OnExit:= @myEditExit;
  myEdit.OnKeyPress:= @OnKeyPress;
  myEdit.caption:= ''; // avoid prev input
  theValue:= now;
  format:= 'dd/mm/yyyy';
end;

function replacechar(const s: string; ch1: char; ch2: char): string;
var
 i : integer;
begin
 result := s;
 for i := 1 to length(result) do
  if result[i] = ch1 then
   result[i] := ch2;
end;

function countchar(const s: string; ch: char): integer;
var
 i : integer;
begin
 result := 0;
 for i := 1 to length(s) do
  if s[i] = ch then
   inc(result);
end;

procedure Split(const Delimiter: Char; Input: string; Strings: TStrings) ;
begin
   Assert(Assigned(Strings)) ;
   Strings.Clear;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
end;

end.

