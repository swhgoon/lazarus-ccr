{ jdbgridutils

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

unit jdbgridutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, Dialogs, LCLType, DBGrids, Controls, DB,
  jcontrolutils;

type

  { TJDbGridTimeCtrl }

  TJDbGridTimeCtrl = class(TObject)
  private
    Field: TField;
    updated: boolean;
    theValue: TTime;
    fFormat: string;
    function getFormat: string;
    procedure myEditEnter(Sender: TObject);
    procedure myEditOnEditingDone(Sender: TObject);
    procedure formatInput;
    procedure setFormat(const AValue: string);
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  public
    CellEditor: TStringCellEditor;
    theGrid: TDBGrid;
    function isNull: boolean;
    property format: string read getFormat write setFormat;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid): TStringCellEditor;
  end;

  { TJDbGridDateCtrl }

  TJDbGridDateCtrl = class(TObject)
  private
    Field: TField;
    updated: boolean;
    theValue: TDateTime;
    fFormat: string;
    function getFormat: string;
    procedure myEditEnter(Sender: TObject);
    procedure myEditOnEditingDone(Sender: TObject);
    procedure formatInput;
    procedure setFormat(const AValue: string);
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  public
    CellEditor: TStringCellEditor;
    theGrid: TDBGrid;
    function isNull: boolean;
    property format: string read getFormat write setFormat;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid): TStringCellEditor;
  end;

  { TJDbGridIntegerCtrl }

  TJDbGridIntegerCtrl = class(TObject)
  private
    theValue: integer;
    updated: boolean;
    Field: TField;
    procedure myEditOnEnter(Sender: TObject);
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure myEditOnEditingDone(Sender: TObject);
    function IsValidInteger(const Value: string): boolean;
  public
    CellEditor: TStringCellEditor;
    theGrid: TDBGrid;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid): TStringCellEditor;
  end;

  { TJDbGridDoubleCtrl }

  TJDbGridDoubleCtrl = class(TObject)
  private
    Field: TField;
    updated: boolean;
    theValue: double;
    fDecimales: integer;
    function getDecimales: integer;
    procedure myEditOnEnter(Sender: TObject);
    procedure myEditOnEditingDone(Sender: TObject);
    procedure setDecimales(const AValue: integer);
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    function IsValidFloat(const Value: string): boolean;
    function textNumber(const aValue: string): string;
  public
    CellEditor: TStringCellEditor;
    theGrid: TDBGrid;
    property decimales: integer read getDecimales write setDecimales;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid; aDecimals: integer = 2): TStringCellEditor;
  end;


implementation

uses
  Math;

{ TJDbGridTimeCtrl }

function TJDbGridTimeCtrl.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDbGridTimeCtrl.myEditEnter(Sender: TObject);
begin
  Field := theGrid.SelectedField;
  CellEditor.BoundsRect := theGrid.SelectedFieldRect;
  CellEditor.Text := Field.AsString;
  CellEditor.OnKeyPress := @OnKeyPress;  // Recuperamos el control :-p
  CellEditor.OnKeyDown := @OnKeyDown;
  theValue := Field.AsDateTime;
  updated := False;
  CellEditor.SelectAll;
end;

procedure TJDbGridTimeCtrl.myEditOnEditingDone(Sender: TObject);
begin
  CellEditor.Caption := NormalizeTime(CellEditor.Caption, theValue);
  if Length(CellEditor.Caption) = 0 then
  begin
    Field.DataSet.Edit;
    Field.Value := Null;
    theValue := 0;
    updated := True;
  end
  else
  if IsValidTimeString(CellEditor.Caption) then
  begin
    if (not updated) then
    begin
      theValue := StrToTime(CellEditor.Caption);
      Field.DataSet.Edit;
      Field.AsDateTime := theValue;
    end;
  end
  else
  begin
    ShowMessage(CellEditor.Caption + ' no es una hora válida');
    CellEditor.Text := FormatDateTime(format, theValue);
  end;
end;

procedure TJDbGridTimeCtrl.formatInput;
begin
  if theValue <> 0 then
    CellEditor.Caption := FormatDateTime(format, theValue);
end;

procedure TJDbGridTimeCtrl.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJDbGridTimeCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if not (Key in ['0'..'9', #8, #9, ':']) then
    Key := #0;
end;

procedure TJDbGridTimeCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Length(CellEditor.Caption) = 0 then
  begin
    Field.DataSet.Edit;
    Field.Value := Null;
    theValue := 0;
    updated := True;
  end
  else
    if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
      (not IsValidTimeString(NormalizeTime(CellEditor.Caption, theValue))) then
    begin
      ShowMessage(CellEditor.Caption + ' no es una hora válida');
      CellEditor.Text := FormatDateTime(format, theValue);
      CellEditor.SelectAll;
      Key := VK_UNKNOWN;
    end
    else
    if key = VK_ESCAPE then
    begin
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(format, Field.AsDateTime);
      updated := True;
      theGrid.SetFocus; // No perder el foco
    end
    else
    if Key in [VK_UP, VK_DOWN] then
    begin
      Key := VK_UNKNOWN;
    end
    else
    if Key in [VK_RETURN, VK_TAB, VK_RIGHT, VK_LEFT] then
    begin
      CellEditor.Caption := NormalizeTime(CellEditor.Caption, theValue);
      if Length(CellEditor.Caption) = 0 then
        theValue := 0
      else
      if IsValidTimeString(CellEditor.Caption) then
      begin
        theValue := StrToTime(CellEditor.Caption);
        Field.DataSet.Edit;
        Field.AsDateTime := theValue;
        CellEditor.SelectAll;
        updated := True;
      end;
    end;
end;

function TJDbGridTimeCtrl.isNull: boolean;
begin
  Result := theValue = 0;
end;

constructor TJDbGridTimeCtrl.Create;
begin
  inherited Create;
  CellEditor := TStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  CellEditor.OnKeyPress := @OnKeyPress;   // se sobreescribe por el Grid :(
  format := ShortTimeFormat;
end;

destructor TJDbGridTimeCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridTimeCtrl.Editor(aGrid: TDBGrid): TStringCellEditor;
begin
  theGrid := aGrid;
  Result := CellEditor;
end;

{ TJDbGridDateCtrl }

function TJDbGridDateCtrl.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDbGridDateCtrl.myEditEnter(Sender: TObject);
begin
  Field := theGrid.SelectedField;
  CellEditor.BoundsRect := theGrid.SelectedFieldRect;
  CellEditor.Text := Field.AsString;
  CellEditor.OnKeyPress := @OnKeyPress;  // Recuperamos el control :-p
  CellEditor.OnKeyDown := @OnKeyDown;
  theValue := Field.AsDateTime;
  updated := False;
  CellEditor.SelectAll;
end;

procedure TJDbGridDateCtrl.myEditOnEditingDone(Sender: TObject);
begin
  CellEditor.Caption := NormalizeDate(CellEditor.Caption, theValue);
  if Length(CellEditor.Caption) = 0 then
  begin
    Field.DataSet.Edit;
    Field.Value := Null;
    theValue := 0;
    updated := True;
  end
  else
  if IsValidDateString(CellEditor.Caption) then
  begin
    if (not updated) then
    begin
      theValue := StrToDate(CellEditor.Caption);
      Field.DataSet.Edit;
      Field.AsDateTime := theValue;
    end;
  end
  else
  begin
    ShowMessage(CellEditor.Caption + ' no es una fecha válida');
    CellEditor.Text := FormatDateTime(format, theValue);
  end;
  //formatInput;
end;

procedure TJDbGridDateCtrl.formatInput;
begin
  if theValue <> 0 then
    CellEditor.Caption := FormatDateTime(format, theValue);
end;

procedure TJDbGridDateCtrl.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJDbGridDateCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '.', '-', '/']) then
    Key := #0;
end;

procedure TJDbGridDateCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Length(CellEditor.Caption) <> 0 then
    if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
      (not IsValidDateString(NormalizeDate(CellEditor.Caption, theValue))) then
    begin
      ShowMessage(CellEditor.Caption + ' no es una fecha válida');
      CellEditor.Text := FormatDateTime(format, theValue);
      CellEditor.SelectAll;
      Key := VK_UNKNOWN;
    end
    else
    if key = VK_ESCAPE then
    begin
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(format, Field.AsDateTime);
      updated := True;
      theGrid.SetFocus; // No perder el foco
    end
    else
    if Key in [VK_UP, VK_DOWN] then
    begin
      Key := VK_UNKNOWN;
    end
    else
    if Key in [VK_RETURN, VK_TAB, VK_RIGHT, VK_LEFT] then
    begin
      CellEditor.Caption := NormalizeDate(CellEditor.Caption, theValue);
      if Length(CellEditor.Caption) = 0 then
        theValue := 0
      else
      if IsValidDateString(CellEditor.Caption) then
      begin
        theValue := StrToDate(CellEditor.Caption);
        Field.DataSet.Edit;
        Field.AsDateTime := theValue;
        CellEditor.SelectAll;
        updated := True;
      end;
    end;
end;


function TJDbGridDateCtrl.isNull: boolean;
begin
  Result := theValue = 0;
end;

constructor TJDbGridDateCtrl.Create;
begin
  inherited Create;
  CellEditor := TStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  CellEditor.OnKeyPress := @OnKeyPress;   // se sobreescribe por el Grid :(
  format := ShortDateFormat;
end;

destructor TJDbGridDateCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridDateCtrl.Editor(aGrid: TDBGrid): TStringCellEditor;
begin
  theGrid := aGrid;
  Result := CellEditor;
end;

{ TJDbGridDoubleCtrl }

function TJDbGridDoubleCtrl.getDecimales: integer;
begin
  Result := fDecimales;
end;

procedure TJDbGridDoubleCtrl.myEditOnEnter(Sender: TObject);
begin
  Field := theGrid.SelectedField;
  CellEditor.BoundsRect := theGrid.SelectedFieldRect;
  CellEditor.Text := Field.AsString;
  CellEditor.OnKeyPress := @OnKeyPress;  // Recuperamos el control :-p
  CellEditor.OnKeyDown := @OnKeyDown;
  theValue := Field.AsFloat;
  updated := False;
  CellEditor.SelectAll;
end;

procedure TJDbGridDoubleCtrl.myEditOnEditingDone(Sender: TObject);
begin
  if IsValidFloat(CellEditor.Caption) then
  begin
    if (not updated) then
    begin
      theValue := StrToFloat(CellEditor.Caption);
      Field.DataSet.Edit;
      //theValue := redondear(theValue, fDecimales);
      Field.AsFloat := theValue;
      // normalize value
      Field.Value := textNumber(Field.DisplayText);
    end;
  end
  else
  begin
    ShowMessage(CellEditor.Caption + ' no es un número válido');
    CellEditor.Text := FloatToStr(theValue);
  end;
end;

procedure TJDbGridDoubleCtrl.setDecimales(const AValue: integer);
begin
  if (AValue >= 0) and (AValue < 5) then
    fDecimales := AValue;
end;

procedure TJDbGridDoubleCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if (Key in ['.', ',']) then
    Key := Decimalseparator;
  if (key = DecimalSeparator) and (Pos(key, CellEditor.Caption) > 0) then
    key := #0;
  if not (Key in ['0'..'9', DecimalSeparator, '+', '-', #8, #9]) then
    Key := #0;
  if (Key = DecimalSeparator) and (fDecimales = 0) then
    Key := #0;
end;

procedure TJDbGridDoubleCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
    (not IsValidFloat(CellEditor.Caption)) then
  begin
    ShowMessage(CellEditor.Caption + ' no es número válido');
    CellEditor.Text := FloatToStr(theValue);
    CellEditor.SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if key = VK_ESCAPE then
  begin
    if Field.IsNull then
      CellEditor.Text := ''
    else
      CellEditor.Text := FloatToStr(Field.AsFloat);
    //CellEditor.Text := CurrToStr(redondear(Field.AsCurrency, fDecimales));
    updated := True;
    theGrid.SetFocus; // No perder el foco
  end
  else
  if key in [VK_UP, VK_DOWN] then
  begin
    Key := VK_UNKNOWN;
  end
  else
  if Key in [VK_RETURN, VK_TAB] then
  begin
    if IsValidFloat(CellEditor.Caption) then
    begin
      theValue := StrToFloat(CellEditor.Caption);
      Field.DataSet.Edit;
      Field.AsFloat := theValue;//redondear(theValue, fDecimales);
      // normalize value
      Field.Value := textNumber(Field.DisplayText);
      CellEditor.Text := Field.AsString;
      updated := True;
    end;
  end;
end;

function TJDbGridDoubleCtrl.IsValidFloat(const Value: string): boolean;
begin
  if StrToFloatDef(Value, MaxDouble) = MaxDouble then
    Result := False
  else
    Result := True;
end;

function TJDbGridDoubleCtrl.textNumber(const aValue: string): string;
var
  i: integer;
  aText: string;
begin
  // Because there is no way to know the number scale (or I don't know how
  // use display format to get a valid number with the desired scale (decimals)
  // so if display format is 0.00  we'll get a number rounded to two decimals
  // This is a bit tricky but works mostly...
  aText := '';
  if Length(aValue) = 0 then
    aText := aValue
  else
  begin
    if aValue[1] = '-' then
      aText := '-'; // negative number?
    for i := 1 to length(aValue) do
      if aValue[i] in ['0'..'9', DecimalSeparator] then
        aText += aValue[i];
  end;
  Result := aText;
end;

constructor TJDbGridDoubleCtrl.Create;
begin
  inherited Create;
  CellEditor := TStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditOnEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  fDecimales := 2;
end;

destructor TJDbGridDoubleCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridDoubleCtrl.Editor(aGrid: TDBGrid;
  aDecimals: integer): TStringCellEditor;
begin
  decimales := aDecimals;
  theGrid := aGrid;
  Result := CellEditor;
end;

{ TJDbGridIntegerCtrl }

procedure TJDbGridIntegerCtrl.myEditOnEnter(Sender: TObject);
begin
  Field := theGrid.SelectedField;
  CellEditor.BoundsRect := theGrid.SelectedFieldRect;
  CellEditor.Text := Field.AsString;
  CellEditor.OnKeyPress := @OnKeyPress;  // Recuperamos el control :-p
  CellEditor.OnKeyDown := @OnKeyDown;
  theValue := Field.AsInteger;
  CellEditor.SelectAll;
  updated := False;
end;

procedure TJDbGridIntegerCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '-']) then
    Key := #0;
end;

procedure TJDbGridIntegerCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
    (not IsValidInteger(CellEditor.Caption)) then
  begin
    ShowMessage(CellEditor.Caption + ' no es un número válido');
    CellEditor.Text := IntToStr(theValue);
    CellEditor.SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if (Key = VK_ESCAPE) then
  begin
    if Field.IsNull then
      CellEditor.Text := ''
    else
      CellEditor.Text := IntToStr(Field.AsInteger);
    updated := True;
    theGrid.SetFocus; // No perder el foco
  end
  else
  if key in [VK_UP, VK_DOWN] then
  begin
    Key := VK_UNKNOWN;
  end
  else
  if Key in [VK_RETURN, VK_TAB] then
  begin
    if IsValidInteger(CellEditor.Caption) then
    begin
      theValue := StrToInt(CellEditor.Caption);
      Field.DataSet.Edit;
      Field.AsInteger := theValue;
      updated := True;
    end;
  end;
end;

procedure TJDbGridIntegerCtrl.myEditOnEditingDone(Sender: TObject);
begin
  if IsValidInteger(CellEditor.Caption) then
  begin
    if (not updated) then
    begin
      theValue := StrToInt(CellEditor.Caption);
      Field.DataSet.Edit;
      Field.AsInteger := theValue;
    end;
  end
  else
  begin
    ShowMessage(CellEditor.Caption + ' no es un número válido');
    CellEditor.Text := IntToStr(theValue);
  end;
end;

function TJDbGridIntegerCtrl.IsValidInteger(const Value: string): boolean;
begin
  if StrToIntDef(Value, MaxInt) = MaxInt then
    Result := False
  else
    Result := True;
end;

constructor TJDbGridIntegerCtrl.Create;
begin
  inherited Create;
  CellEditor := TStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditOnEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  CellEditor.OnKeyPress := @OnKeyPress;   // se sobreescribe por el Grid :(
end;

destructor TJDbGridIntegerCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridIntegerCtrl.Editor(aGrid: TDBGrid): TStringCellEditor;
begin
  theGrid := aGrid;
  Result := CellEditor;
end;

end.

