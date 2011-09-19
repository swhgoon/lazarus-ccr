{ jdblabeledcurrencyedit

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

unit jdblabeledcurrencyedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Controls, StdCtrls, ExtCtrls, DB, DBCtrls,
  LMessages, LCLType, Dialogs,
  SysUtils;

type

  { TJDBLabeledCurrencyEdit }

  TJDBLabeledCurrencyEdit = class(TLabeledEdit)
  private
    fFormat: string;
    FDataLink: TFieldDataLink;
    fDecimales: integer;

    procedure DataChange(Sender: TObject);
    function getDecimals: integer;
    procedure setDecimals(AValue: integer);
    procedure UpdateData(Sender: TObject);
    procedure FocusRequest(Sender: TObject);

    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;

    function IsReadOnly: boolean;

    function getFormat: string;
    procedure setFormat(const AValue: string);
    procedure formatInput;

    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;

    function IsValidCurrency(const Value: string): boolean;
    function ScaleTo(const AValue: currency; const NDecimals: integer): currency;

  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ActiveChange(Sender: TObject); virtual;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure DoEnter; override;
    function GetReadOnly: boolean; override;
    procedure SetReadOnly(Value: boolean); override;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EditingDone; override;
    property Field: TField read GetField;

  published
    property DisplayFormat: string read getFormat write setFormat;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Decimals: integer read getDecimals write setDecimals;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default False;

    // From TEdit
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
    property EchoMode;
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
  {$I lcurrencydbicon.lrs}
  RegisterComponents('Data Controls', [TJDBLabeledCurrencyEdit]);
end;

{ TJDBLabeledCurrencyEdit }

procedure TJDBLabeledCurrencyEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if not Focused then
      formatInput
    else
      Caption := FDataLink.Field.AsString;
  end
  else
    Text := '';
end;

function TJDBLabeledCurrencyEdit.getDecimals: integer;
begin
  Result := fDecimales;
end;

procedure TJDBLabeledCurrencyEdit.setDecimals(AValue: integer);
begin
  if AValue >= 0 then
    fDecimales := AValue;
end;


procedure TJDBLabeledCurrencyEdit.UpdateData(Sender: TObject);
var
  theValue: currency;
begin
  if FDataLink.Field <> nil then
  begin
    if IsValidCurrency(Text) then
    begin
      theValue := StrToCurr(Text);
      theValue := ScaleTo(theValue, fDecimales);
      Text := CurrToStr(theValue);
      FDataLink.Field.Text := Text;
    end
    else
    begin
      if FDataLink.Field <> nil then
      begin
        ShowMessage(Caption + ' no es un valor válido');
        Caption := FDataLink.Field.AsString;
        SelectAll;
        SetFocus;
      end;
    end;
  end
  else
    Text := '';
end;

procedure TJDBLabeledCurrencyEdit.FocusRequest(Sender: TObject);
begin
  SetFocus;
end;

function TJDBLabeledCurrencyEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TJDBLabeledCurrencyEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJDBLabeledCurrencyEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TJDBLabeledCurrencyEdit.IsReadOnly: boolean;
begin
  if FDatalink.Active then
    Result := not FDatalink.CanModify
  else
    Result := False;
end;

function TJDBLabeledCurrencyEdit.getFormat: string;
begin
  Result := fFormat;
end;

procedure TJDBLabeledCurrencyEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  if not Focused then
    formatInput;
end;

procedure TJDBLabeledCurrencyEdit.formatInput;
begin
  if FDataLink.Field <> nil then
    //FDataLink.Field.DisplayText -> formatted  (tdbgridcolumns/persistent field DisplayFormat
    if fFormat <> '' then
      Caption := FormatFloat(fFormat, FDataLink.Field.AsCurrency)
    else
      Caption := FDataLink.Field.DisplayText
  else
    Caption := 'nil';
end;

function TJDBLabeledCurrencyEdit.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TJDBLabeledCurrencyEdit.SetReadOnly(Value: boolean);
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

procedure TJDBLabeledCurrencyEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TJDBLabeledCurrencyEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    ChangeDataSource(Self, FDataLink, Value);
end;

procedure TJDBLabeledCurrencyEdit.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink); // Delphi dbctrls compatibility?
end;

function TJDBLabeledCurrencyEdit.IsValidCurrency(const Value: string): boolean;
begin
  if StrToCurrDef(Value, MaxCurrency) = MaxCurrency then
    Result := False
  else
    Result := True;
end;

function TJDBLabeledCurrencyEdit.ScaleTo(const AValue: currency;
  const NDecimals: integer): currency;
begin
  Result := round(AValue * power(10, NDecimals)) / power(10, NDecimals);
end;

procedure TJDBLabeledCurrencyEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

procedure TJDBLabeledCurrencyEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // clean up
  if (Operation = opRemove) then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
  end;
end;

procedure TJDBLabeledCurrencyEdit.ActiveChange(Sender: TObject);
begin
  if FDatalink.Active then
    datachange(Sender)
  else
    Text := '';
end;

procedure TJDBLabeledCurrencyEdit.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_ESCAPE then
  begin
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end
  else
  if Key in [VK_DELETE, VK_BACK] then
  begin
    if not IsReadOnly then
      FDatalink.Edit
    else
      Key := VK_UNKNOWN;
  end;
end;

procedure TJDBLabeledCurrencyEdit.KeyPress(var Key: char);
begin
  if (Key in ['.', ',']) then
    Key := Decimalseparator;
  if (key = DecimalSeparator) and (Pos(key, Text) > 0) then
    key := #0;
  if not (Key in ['0'..'9', DecimalSeparator, '+', '-', #8, #9]) then
    Key := #0;
  if (Key = DecimalSeparator) and (fDecimales = 0) then
    Key := #0;

  if (Key <> #0) and (not IsReadOnly) then
    FDatalink.Edit;
  inherited KeyPress(Key);
end;

procedure TJDBLabeledCurrencyEdit.DoEnter;
begin
  if FDataLink.Field <> nil then
    Caption := FDataLink.Field.AsString;
  inherited DoEnter;
end;

constructor TJDBLabeledCurrencyEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  FDataLInk.OnActiveChange := @ActiveChange;
  // Set default values
  //fDecimales := 2;
  //fFormat := '0.00';
end;

destructor TJDBLabeledCurrencyEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJDBLabeledCurrencyEdit.EditingDone;
begin
  inherited EditingDone;
  UpdateData(self);
end;

end.

