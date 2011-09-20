{ JIntegerEdit

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

unit JIntegerEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs;

type

  { TJIntegerEdit }

  TJIntegerEdit = class(TCustomEdit)
  private
    { Private declarations }
    theValue: integer;
    fFormat: string;
    function getFormat: string;
    function getValue: integer;
    procedure setFormat(const AValue: string);
    procedure setValue(const AValue: integer);
    function IsValidInteger(const Value: string): boolean;
    procedure FormatInput;
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
    property Value: integer read getValue write setValue;

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


procedure Register;
begin
  {$I jintegeredit_icon.lrs}
  RegisterComponents('Additional', [TJIntegerEdit]);
end;

{ TJIntegerEdit }

function TJIntegerEdit.getFormat: string;
begin
  Result := fFormat;
end;

function TJIntegerEdit.getValue: integer;
begin
  Result := theValue;
end;

procedure TJIntegerEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJIntegerEdit.setValue(const AValue: integer);
begin
  theValue := AValue;
  formatInput;
end;

function TJIntegerEdit.IsValidInteger(const Value: string): boolean;
begin
  if StrToIntDef(Value, MaxInt) = MaxInt then
    Result := False
  else
    Result := True;
end;

procedure TJIntegerEdit.FormatInput;
begin
  Text := FormatFloat(fFormat, theValue);
end;

procedure TJIntegerEdit.DoEnter;
begin
  inherited DoEnter;
  Text := IntToStr(theValue);
  SelectAll;
end;

procedure TJIntegerEdit.DoExit;
begin
  inherited DoExit;
  if IsValidInteger(Text) then
    theValue := StrToInt(Text)
  else
  begin
    ShowMessage(Text + ' no es un valor válido');
    SetFocus;
  end;
  formatInput;
end;

procedure TJIntegerEdit.KeyPress(var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '-']) then
    Key := #0;
  inherited KeyPress(Key);
end;

constructor TJIntegerEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  // Set initial values
  Text := '';
  DisplayFormat := '0';
  Value := 0;
end;

destructor TJIntegerEdit.Destroy;
begin
  inherited Destroy;
end;

end.

