{ TJDateTimeEdit

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


unit JDateTimeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics,
  Dialogs, jcontrolutils;

type
  TJDateTimeEdit = class(TCustomEdit)
  private
    { Private declarations }
    theValue: TDateTime;
    fFormat: string;
    function getFormat: string;
    function getValue: TDateTime;
    procedure formatInput;
    procedure setFormat(const AValue: string);
    procedure setValue(const AValue: TDateTime);
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
    function isNull: boolean;
    property DisplayFormat: string read getFormat write setFormat;
    property Value: TDateTime read getValue write setValue;

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
  {$I jdatetimeedit_icon.lrs}
  RegisterComponents('Additional', [TJDateTimeEdit]);
end;

function TJDateTimeEdit.getFormat: string;
begin
  Result := fFormat;
end;

function TJDateTimeEdit.getValue: TDateTime;
begin
  Result := theValue;
end;

procedure TJDateTimeEdit.formatInput;
begin
  if theValue <> 0 then
    Text := FormatDateTime(DisplayFormat, theValue);
end;

procedure TJDateTimeEdit.setFormat(const AValue: string);
begin
  fFormat := AValue;
  formatInput;
end;

procedure TJDateTimeEdit.setValue(const AValue: TDateTime);
begin
  theValue := AValue;
  formatInput;
end;

procedure TJDateTimeEdit.DoEnter;
begin
  inherited DoEnter;
  if theValue <> 0 then
    Text := FormatDateTime(DisplayFormat, theValue)
  else
    Text := '';
  SelectAll;
end;

procedure TJDateTimeEdit.DoExit;
var
  bufText: string;
begin
  inherited DoExit;
  bufText := Text;
  Text := NormalizeDateTime(Text, theValue);
  if (Length(bufText) > 0) and (Length(Text) = 0) then
  begin
    ShowMessage(bufText + ' no es una fecha-hora válida');
    SetFocus;
  end
  else
  if Length(Text) = 0 then
    theValue := 0
  else
  if IsValidDateTimeString(Text) then
    theValue := StrToDateTime(Text)
  else
  begin
    ShowMessage(Text + ' no es una fecha-hora válida');
    SetFocus;
  end;
  formatInput;
end;

procedure TJDateTimeEdit.KeyPress(var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9, '.', '-', '/', ',', ':', ' ']) then
    Key := #0;
  inherited KeyPress(Key);
end;

constructor TJDateTimeEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Text := '';
  fFormat := ShortDateFormat + ' ' + ShortTimeFormat;
  theValue := 0;
  formatInput;
end;

destructor TJDateTimeEdit.Destroy;
begin
  inherited Destroy;
end;

function TJDateTimeEdit.isNull: boolean;
begin
  Result := theValue = 0;
end;


end.

