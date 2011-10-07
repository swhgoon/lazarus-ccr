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
  jcontrolutils, jinputconsts;

type

  TJStringCellEditor = class(TStringCellEditor);

  { TJDbGridStringCtrl }

  TJDbGridStringCtrl = class(TObject)
  private
    Field: TField;
    updated: boolean;
    fMaxLength: integer;
    procedure myEditEnter(Sender: TObject);
    procedure myEditOnEditingDone(Sender: TObject);
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  public
    CellEditor: TStringCellEditor;
    theGrid: TDBGrid;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid; aMaxLength: integer = 0): TStringCellEditor;
  end;

  { TJDbGridDateTimeCtrl }

  TJDbGridDateTimeCtrl = class(TObject)
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
    property DisplayFormat: string read getFormat write setFormat;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid): TStringCellEditor;
  end;

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
    property DisplayFormat: string read getFormat write setFormat;
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
    property DisplayFormat: string read getFormat write setFormat;
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
    fDecimals: integer;
    function getDecimals: integer;
    procedure myEditOnEnter(Sender: TObject);
    procedure myEditOnEditingDone(Sender: TObject);
    procedure setDecimals(const AValue: integer);
    procedure OnKeyPress(Sender: TObject; var key: char);
    procedure OnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    function IsValidFloat(const Value: string): boolean;
    function ScaleTo(const AValue: double; const NDecimals: integer): double;
  public
    CellEditor: TStringCellEditor;
    theGrid: TDBGrid;
    property decimals: integer read getDecimals write setDecimals;
    constructor Create;
    destructor Destroy; override;
    function Editor(aGrid: TDBGrid; aDecimals: integer = 2): TStringCellEditor;
  end;


implementation

uses
  Math;

{ TJDbGridStringCtrl }

procedure TJDbGridStringCtrl.myEditEnter(Sender: TObject);
begin
  Field := theGrid.SelectedField;
  CellEditor.BoundsRect := theGrid.SelectedFieldRect;
  CellEditor.Text := Field.AsString;
  CellEditor.OnKeyPress := @OnKeyPress;  // Recuperamos el control :-p
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.MaxLength := fMaxLength;
  updated := False;
  CellEditor.SelectAll;
end;

procedure TJDbGridStringCtrl.myEditOnEditingDone(Sender: TObject);
begin
  if (not updated) then
  begin
    if CellEditor.Text <> Field.AsString then
    begin
      Field.DataSet.DisableControls;
      Field.DataSet.Edit;
      Field.AsString := CellEditor.Text;
      field.DataSet.EnableControls;
    end;
  end;
end;

procedure TJDbGridStringCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  // nothing right now
end;

procedure TJDbGridStringCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if key = VK_ESCAPE then
  begin
    CellEditor.Text := Field.AsString;
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
    Field.DataSet.Edit;
    Field.AsString := CellEditor.Text;
    CellEditor.SelectAll;
    updated := True;
  end;
end;

constructor TJDbGridStringCtrl.Create;
begin
  inherited Create;
  CellEditor := TJStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  CellEditor.OnKeyPress := @OnKeyPress;
end;

destructor TJDbGridStringCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridStringCtrl.Editor(aGrid: TDBGrid;
  aMaxLength: integer): TStringCellEditor;
begin
  theGrid := aGrid;
  fMaxLength := aMaxLength;
  Result := CellEditor;
end;

{ TJDbGridDateTimeCtrl }

function TJDbGridDateTimeCtrl.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDbGridDateTimeCtrl.myEditEnter(Sender: TObject);
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

procedure TJDbGridDateTimeCtrl.myEditOnEditingDone(Sender: TObject);
begin
  if Length(CellEditor.Caption) = 0 then
  begin
    if Field.Value <> Null then
    begin
      Field.DataSet.DisableControls;
      Field.DataSet.Edit;
      Field.Value := Null;
      theValue := 0;
      updated := True;
      Field.DataSet.EnableControls;
    end;
  end
  else
  begin
    CellEditor.Caption := NormalizeDateTime(CellEditor.Caption, theValue);
    if IsValidDateTimeString(CellEditor.Caption) then
    begin
      if (not updated) then
      begin
        theValue := StrToDateTime(CellEditor.Caption);
        if theValue <> Field.AsDateTime then
        begin
          Field.DataSet.DisableControls;
          Field.DataSet.Edit;
          Field.AsDateTime := theValue;
          Field.DataSet.EnableControls;
        end;
      end;
    end
    else
    begin
      ShowMessage(Format(SInvalidDateTime, [CellEditor.Caption]));
      CellEditor.Text := FormatDateTime(DisplayFormat, Field.AsDateTime);
    end;

  end;
end;

procedure TJDbGridDateTimeCtrl.formatInput;
begin
  if theValue <> 0 then
    CellEditor.Caption := FormatDateTime(DisplayFormat, theValue);
end;

procedure TJDbGridDateTimeCtrl.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJDbGridDateTimeCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '.', '-', '/', ':', ' ']) then
    Key := #0;
end;

procedure TJDbGridDateTimeCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Length(CellEditor.Caption) = 0 then
  begin
    if Field.Value <> Null then
    begin
      Field.DataSet.Edit;
      Field.Value := Null;
      theValue := 0;
      updated := True;
    end;
  end
  else
  if Length(CellEditor.Caption) <> 0 then
    if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
      (not IsValidDateTimeString(NormalizeDateTime(CellEditor.Caption, theValue))) then
    begin
      ShowMessage(Format(SInvalidDateTime, [CellEditor.Caption]));
      CellEditor.Text := FormatDateTime(DisplayFormat, theValue);
      CellEditor.SelectAll;
      Key := VK_UNKNOWN;
    end
    else
    if key = VK_ESCAPE then
    begin
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(DisplayFormat, Field.AsDateTime);
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
      CellEditor.Caption := NormalizeDateTime(CellEditor.Caption, theValue);
      if Length(CellEditor.Caption) = 0 then
        theValue := 0
      else
      if IsValidDateTimeString(CellEditor.Caption) then
      begin
        theValue := StrToDateTime(CellEditor.Caption);
        Field.DataSet.Edit;
        Field.AsDateTime := theValue;
        CellEditor.SelectAll;
        updated := True;
      end;
    end;
end;

function TJDbGridDateTimeCtrl.isNull: boolean;
begin
  Result := theValue = 0;
end;

constructor TJDbGridDateTimeCtrl.Create;
begin
  inherited Create;
  CellEditor := TJStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  CellEditor.OnKeyPress := @OnKeyPress;
  DisplayFormat := ShortDateFormat + ' ' + ShortTimeFormat;
end;

destructor TJDbGridDateTimeCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridDateTimeCtrl.Editor(aGrid: TDBGrid): TStringCellEditor;
begin
  theGrid := aGrid;
  Result := CellEditor;
end;

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
  if Length(CellEditor.Caption) = 0 then
  begin
    if Field.Value <> Null then
    begin
      Field.DataSet.DisableControls;
      Field.DataSet.Edit;
      Field.Value := Null;
      theValue := 0;
      updated := True;
      Field.DataSet.EnableControls;
    end;
  end
  else
  begin
    CellEditor.Caption := NormalizeTime(CellEditor.Caption, theValue);
    if IsValidTimeString(CellEditor.Caption) then
    begin
      if (not updated) then
      begin
        theValue := StrToTime(CellEditor.Caption);
        if theValue <> Field.AsDateTime then
        begin
          Field.DataSet.DisableControls;
          Field.DataSet.Edit;
          Field.AsDateTime := theValue;
          Field.DataSet.EnableControls;
        end;
      end;
    end
    else
    begin
      ShowMessage(Format(SInvalidTime, [CellEditor.Caption]));
      CellEditor.Text := FormatDateTime(DisplayFormat, Field.AsDateTime);
    end;
  end;
end;

procedure TJDbGridTimeCtrl.formatInput;
begin
  if theValue <> 0 then
    CellEditor.Caption := FormatDateTime(DisplayFormat, theValue);
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
    if Field.Value <> Null then
    begin
      Field.DataSet.Edit;
      Field.Value := Null;
      theValue := 0;
      updated := True;
    end;
  end
  else
  if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
    (not IsValidTimeString(NormalizeTime(CellEditor.Caption, theValue))) then
  begin
    ShowMessage(Format(SInvalidTime, [CellEditor.Caption]));
    CellEditor.Text := FormatDateTime(DisplayFormat, theValue);
    CellEditor.SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if key = VK_ESCAPE then
  begin
    if Field.IsNull then
      CellEditor.Text := ''
    else
      CellEditor.Text := FormatDateTime(DisplayFormat, Field.AsDateTime);
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
  CellEditor := TJStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  CellEditor.OnKeyPress := @OnKeyPress;   // se sobreescribe por el Grid :(
  DisplayFormat := ShortTimeFormat;
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
  if Length(CellEditor.Caption) = 0 then
  begin
    if Field.Value <> Null then
    begin
      Field.DataSet.DisableControls;
      Field.DataSet.Edit;
      Field.Value := Null;
      theValue := 0;
      updated := True;
      Field.DataSet.EnableControls;
    end;
  end
  else
  begin
    CellEditor.Caption := NormalizeDate(CellEditor.Caption, theValue);
    if IsValidDateString(CellEditor.Caption) then
    begin
      if (not updated) then
      begin
        theValue := StrToDate(CellEditor.Caption);
        if theValue <> Field.AsDateTime then
        begin
          Field.DataSet.DisableControls;
          Field.DataSet.Edit;
          Field.AsDateTime := theValue;
          field.DataSet.EnableControls;
        end;
      end;
    end
    else
    begin
      ShowMessage(Format(SInvalidDate, [CellEditor.Caption]));
      CellEditor.Text := FormatDateTime(DisplayFormat, Field.AsDateTime);
    end;
  end;
end;

procedure TJDbGridDateCtrl.formatInput;
begin
  if theValue <> 0 then
    CellEditor.Caption := FormatDateTime(DisplayFormat, theValue);
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
  if Length(CellEditor.Caption) = 0 then
  begin
    if Field.Value <> null then
    begin
      Field.DataSet.Edit;
      Field.Value := Null;
      theValue := 0;
      updated := True;
    end;
  end
  else
  if Length(CellEditor.Caption) <> 0 then
    if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
      (not IsValidDateString(NormalizeDate(CellEditor.Caption, theValue))) then
    begin
      ShowMessage(Format(SInvalidDate, [CellEditor.Caption]));
      CellEditor.Text := FormatDateTime(DisplayFormat, theValue);
      CellEditor.SelectAll;
      Key := VK_UNKNOWN;
    end
    else
    if key = VK_ESCAPE then
    begin
      if Field.IsNull then
        CellEditor.Text := ''
      else
        CellEditor.Text := FormatDateTime(DisplayFormat, Field.AsDateTime);
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
  CellEditor := TJStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  CellEditor.OnKeyPress := @OnKeyPress;   // se sobreescribe por el Grid :(
  DisplayFormat := ShortDateFormat;
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

function TJDbGridDoubleCtrl.getDecimals: integer;
begin
  Result := fDecimals;
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
      if theValue <> Field.AsFloat then
      begin
        Field.DataSet.DisableControls;
        Field.DataSet.Edit;
        if decimals > 0 then
          theValue := ScaleTo(theValue, fDecimals);
        Field.Value := theValue;
        Field.DataSet.EnableControls;
      end;
    end;
  end
  else
  begin
    ShowMessage(Format(SInvalidNumber, [CellEditor.Caption]));
    CellEditor.Text := FloatToStr(theValue);
  end;
end;

procedure TJDbGridDoubleCtrl.setDecimals(const AValue: integer);
begin
  if (AValue >= 0) and (AValue < 11) then
    fDecimals := AValue;
end;

procedure TJDbGridDoubleCtrl.OnKeyPress(Sender: TObject; var key: char);
begin
  if (Key in ['.', ',']) then
    Key := Decimalseparator;
  if (key = DecimalSeparator) and (Pos(key, CellEditor.Caption) > 0) then
    key := #0;
  if not (Key in ['0'..'9', DecimalSeparator, '+', '-', #8, #9]) then
    Key := #0;
  //if (Key = DecimalSeparator) and (fDecimals = 0) then
  //  Key := #0;    // Note: decimal=0 avoids rounding
end;

procedure TJDbGridDoubleCtrl.OnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key in [VK_RETURN, VK_TAB, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and
    (not IsValidFloat(CellEditor.Caption)) then
  begin
    ShowMessage(Format(SInvalidNumber, [CellEditor.Caption]));
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
    //CellEditor.Text := CurrToStr(redondear(Field.AsCurrency, fDecimals));
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
      if decimals > 0 then
        theValue := ScaleTo(theValue, fDecimals);
      Field.Value := theValue;
      ;
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

function TJDbGridDoubleCtrl.ScaleTo(const AValue: double;
  const NDecimals: integer): double;
begin
  Result := round(AValue * power(10, NDecimals)) / power(10, NDecimals);
end;

constructor TJDbGridDoubleCtrl.Create;
begin
  inherited Create;
  CellEditor := TJStringCellEditor.Create(nil);
  CellEditor.OnEnter := @myEditOnEnter;
  CellEditor.OnKeyDown := @OnKeyDown;
  CellEditor.OnEditingDone := @myEditOnEditingDone;
  fDecimals := 2;
end;

destructor TJDbGridDoubleCtrl.Destroy;
begin
  CellEditor.Free;
  inherited Destroy;
end;

function TJDbGridDoubleCtrl.Editor(aGrid: TDBGrid;
  aDecimals: integer): TStringCellEditor;
begin
  decimals := aDecimals;
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
    ShowMessage(Format(SInvalidNumber, [CellEditor.Caption]));
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
      if theValue <> Field.AsInteger then
      begin
        Field.DataSet.DisableControls;
        Field.DataSet.Edit;
        Field.AsInteger := theValue;
        field.DataSet.EnableControls;
        updated := True;
      end;
    end;
  end
  else
  begin
    ShowMessage(Format(SInvalidNumber, [CellEditor.Caption]));
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
  CellEditor := TJStringCellEditor.Create(nil);
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

