{ JCurrencyEdit

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

unit JCurrencyEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs;

type

  { TJCurrencyEdit }

  TJCurrencyEdit = class(TCustomEdit)
  private
    { Private declarations }
    theValue: currency;
    fFormat: string;
    fDecimals: integer;
    function getDecimals: integer;
    function getFormat: string;
    function getValue: currency;
    procedure formatInput;
    procedure setDecimals(const AValue: integer);
    procedure setFormat(const AValue: string);
    function scaleTo(const AValue: currency; const NDecimals: integer): currency;
    function IsValidFloat(const Value: string): boolean;
    procedure setValue(const AValue: currency);
  protected
    { Protected declarations }
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyPress(var Key: char); override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property DisplayFormat: string read getFormat write setFormat;
    property Decimals: integer read getDecimals write setDecimals;
    property Value: currency read getValue write setValue;

    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderStyle;
    property BorderSpacing;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentBidiMode;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
  end;

procedure Register;

implementation

uses
  Math;

procedure Register;
begin
  {$I jcurrencyedit_icon.lrs}
  RegisterComponents('Additional', [TJCurrencyEdit]);
end;

{ TJCurrencyEdit }

function TJCurrencyEdit.getDecimals: integer;
begin
  Result := fDecimals;
end;

function TJCurrencyEdit.getFormat: string;
begin
  Result := fFormat;
end;

function TJCurrencyEdit.getValue: currency;
begin
  Result := theValue;
end;

procedure TJCurrencyEdit.formatInput;
begin
  Caption := FormatFloat(DisplayFormat, theValue);
end;

procedure TJCurrencyEdit.setDecimals(const AValue: integer);
begin
  if (AValue >= 0) and (AValue < 5) then
    fDecimals := AValue;
end;

procedure TJCurrencyEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

function TJCurrencyEdit.scaleTo(const AValue: currency;
  const NDecimals: integer): currency;
begin
  Result := round(AValue * power(10, NDecimals)) / power(10, NDecimals);
end;

function TJCurrencyEdit.IsValidFloat(const Value: string): boolean;
begin
  if StrToCurrDef(Value, MaxCurrency) = MaxCurrency then
    Result := False
  else
    Result := True;
end;

procedure TJCurrencyEdit.setValue(const AValue: currency);
begin
  theValue := scaleTo(AValue, fDecimals);
  formatInput;
end;

procedure TJCurrencyEdit.DoEnter;
begin
  inherited DoEnter;
  Text := FloatToStr(theValue);
  SelectAll;
end;

procedure TJCurrencyEdit.DoExit;
begin
  inherited DoExit;
  if IsValidFloat(Text) then
    theValue := StrToCurr(Text)
  else
  begin
    ShowMessage(Text + ' no es un valor válido');
    SetFocus;
  end;
  theValue := scaleTo(theValue, fDecimals);
  formatInput;
end;

procedure TJCurrencyEdit.KeyPress(var Key: char);
begin
  if (Key in ['.', ',']) then
    Key := Decimalseparator;
  if (key = DecimalSeparator) and (Pos(key, Text) > 0) then
    key := #0;
  if not (Key in ['0'..'9', DecimalSeparator, '+', '-', #8, #9]) then
    Key := #0;
  if (Key = DecimalSeparator) and (fDecimals = 0) then
    Key := #0;
  inherited KeyPress(Key);
end;

constructor TJCurrencyEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Text := '';
  fFormat := '#,0.00';
  fDecimals := 2;
  formatInput;
end;

destructor TJCurrencyEdit.Destroy;
begin
  inherited Destroy;
end;

end.

