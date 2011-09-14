{ jdbutils

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

unit jdbutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBCtrls, jcontrolutils;

type

  { TJDBCurrencyCtrl }

  TJDBCurrencyCtrl = class(TObject)
  private
    myEdit: TDBEdit;
    theValue: currency;
    fDecimales: integer;
    function getDecimales: integer;
    procedure myEditOnEnter(Sender: TObject);
    procedure myEditOnEditingDone(Sender: TObject);
    procedure setDecimales(const AValue: integer);
    procedure OnKeyPress(Sender: TObject; var key: char);
    function redondear(const AValue: currency; const NDecimales: integer): currency;
    function IsValidFloat(const Value: string): boolean;
  public
    property decimales: integer read getDecimales write setDecimales;
    procedure Enter(widget: TDBEdit);
  end;

  { TJDBIntegerCtrl }

  TJDBIntegerCtrl = class(TObject)
  private
    myEdit: TDBEdit;
    theValue: integer;
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure myEditOnEditingDone(Sender: TObject);
    function IsValidInteger(const Value: string): boolean;
  public
    procedure Enter(widget: TDBEdit);
  end;

  { TJDBDateCtrl }

  TJDBDateCtrl = class(TObject)
  private
    myEdit: TDBEdit;
    theValue: TDateTime;
    fFormat: string;
    function getFormat: string;
    procedure myEditOnEditingDone(Sender: TObject);
    procedure formatInput;
    procedure setFormat(const AValue: string);
    procedure OnKeyPress(Sender: TObject; var key: char);
    function IsValidDate(const Value: string): boolean;
  public
    function isNull: boolean;
    property format: string read getFormat write setFormat;
    procedure Enter(widget: TDBEdit);
  end;

var
  dateDbControl: TJDBDateCtrl;
  integerDbControl: TJDBIntegerCtrl;
  currencyDbControl: TJDBCurrencyCtrl;


implementation

uses
  Math, dateutils, Graphics, Dialogs;


{ TJDBCurrencyCtrl }

function TJDBCurrencyCtrl.getDecimales: integer;
begin
  Result := fDecimales;
end;

procedure TJDBCurrencyCtrl.myEditOnEnter(Sender: TObject);
begin

end;

procedure TJDBCurrencyCtrl.myEditOnEditingDone(Sender: TObject);
var
  bufCaption: string;
begin
  if IsValidFloat(myEdit.Caption) then
  begin
    theValue := StrToCurr(myEdit.Caption);
    theValue := redondear(theValue, fDecimales);
    myEdit.Caption := CurrToStr(theValue);
  end
  else
  begin
    bufCaption := myEdit.Caption;
    myEdit.Caption := myEdit.Field.AsString;
    ShowMessage(bufCaption + ' no es un número válido');
    myEdit.SetFocus;
  end;
end;



procedure TJDBCurrencyCtrl.setDecimales(const AValue: integer);
begin
  if AValue >= 0 then
    fDecimales := AValue;
end;


procedure TJDBCurrencyCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if (Key in ['.', ',']) then
    Key := Decimalseparator;
  if (key = DecimalSeparator) and (Pos(key, myEdit.Caption) > 0) then
    key := #0;
  if not (Key in ['0'..'9', DecimalSeparator, '+', '-', #8, #9]) then
    Key := #0;
  if (Key = DecimalSeparator) and (fDecimales = 0) then
    Key := #0;
end;

function TJDBCurrencyCtrl.redondear(const AValue: currency;
  const NDecimales: integer): currency;
begin
  redondear := round(AValue * power(10, NDecimales)) / power(10, NDecimales);
end;

function TJDBCurrencyCtrl.IsValidFloat(const Value: string): boolean;
begin
  if StrToCurrDef(Value, MaxCurrency) = MaxCurrency then
    Result := False
  else
    Result := True;
end;


procedure TJDBCurrencyCtrl.Enter(widget: TDBEdit);
begin
  myEdit := widget;
  myEdit.OnEditingDone := @myEditOnEditingDone;
  myEdit.OnKeyPress := @OnKeyPress;
  fDecimales := 2; // default 2 decimals
  theValue := myEdit.Field.AsCurrency;
  myEdit.SelectAll;
end;


{ TJDBIntegerCtrl }

procedure TJDBIntegerCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '-']) then
    Key := #0;
end;

procedure TJDBIntegerCtrl.myEditOnEditingDone(Sender: TObject);
var
  bufCaption: string;
begin
  if IsValidInteger(myEdit.Caption) then
  begin
    theValue := StrToInt(myEdit.Caption);
    myEdit.Caption := IntToStr(theValue);
  end
  else
  begin
    bufCaption := myEdit.Caption;
    myEdit.Caption := myEdit.Field.AsString;
    ShowMessage(bufCaption + ' no es un número válido');
    myEdit.SetFocus;
  end;
end;

function TJDBIntegerCtrl.IsValidInteger(const Value: string): boolean;
begin
  if StrToIntDef(Value, MaxInt) = MaxInt then
    Result := False
  else
    Result := True;
end;

procedure TJDBIntegerCtrl.Enter(widget: TDBEdit);
begin
  myEdit := widget;
  myEdit.OnEditingDone := @myEditOnEditingDone;
  myEdit.OnKeyPress := @OnKeyPress;
  theValue := myEdit.Field.AsInteger;
  myEdit.SelectAll;
end;

{ TJDBDateCtrl }

function TJDBDateCtrl.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDBDateCtrl.myEditOnEditingDone(Sender: TObject);
var
  bufCaption: string;
begin
  bufCaption := NormalizeDate(myEdit.Caption, theValue);
  if Length(myEdit.Caption) = 0 then
    theValue := 0
  else
  if IsValidDate(bufCaption) then
  begin
    theValue := StrToDate(bufCaption);
    myEdit.Caption := bufCaption;
  end
  else
  begin
    myEdit.Caption := myEdit.Field.AsString;
    ShowMessage(bufCaption + ' no es una fecha válida');
    myEdit.SetFocus;
  end;
  //formatInput;
end;

procedure TJDBDateCtrl.formatInput;
begin
  if theValue <> 0 then
    myEdit.Caption := FormatDateTime(format, theValue);
end;

procedure TJDBDateCtrl.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJDBDateCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '.', '-', '/']) then
    Key := #0;
end;

function TJDBDateCtrl.IsValidDate(const Value: string): boolean;
begin
  // comprobar que la fecha es valida
  if StrToDateDef(Value, MaxDateTime) = MaxDateTime then
    Result := False
  else
    Result := True;
end;

function TJDBDateCtrl.isNull: boolean;
begin
  Result := theValue = 0;
end;

procedure TJDBDateCtrl.Enter(widget: TDBEdit);
begin
  myEdit := widget;
  myEdit.OnEditingDone := @myEditOnEditingDone;
  myEdit.OnKeyPress := @OnKeyPress;
  format := 'dd/mm/yyyy';
  theValue := myEdit.Field.AsDateTime;
  myEdit.SelectAll;
end;

procedure CreateResources;
begin
  dateDbControl := TJDBDateCtrl.Create;
  integerDbControl := TJDBIntegerCtrl.Create;
  currencyDbControl := TJDBCurrencyCtrl.Create;
end;

procedure CleanResources;
begin
  dateDbControl.Free;
  integerDbControl.Free;
  currencyDbControl.Free;
end;

initialization
  CreateResources;

finalization
  CleanResources;
end.

