//
// nvWidgets.h - User Interface library
//
//
// Author: Ignacio Castano, Samuel Gateau, Evan Hart
// Email: sdkfeedback@nvidia.com
//
// Copyright (c) NVIDIA Corporation. All rights reserved.
////////////////////////////////////////////////////////////////////////////////
unit nvPainter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nvBaseFont, nvTypes;

type

  UIPainter = class(TObject)
  private
    FFont: TnvBaseFont;
    procedure SetFont(AValue: TnvBaseFont);
  public
    constructor Create;

    procedure _begin(const window: Rect); virtual;
    procedure _end; virtual;

    // These methods should be called between begin/end

    procedure drawFrame(const r: Rect; margin: integer; style: integer); virtual; abstract;

    function getLabelRect(const r: Rect; const Text: string; out rt: Rect; out nbLines: integer): Rect; virtual; abstract;
    procedure drawLabel(const r: Rect; const Text: string; const rt: Rect; const nbLines: integer; isHover: boolean; style: integer); virtual; abstract;

    function getButtonRect(const r: Rect; const Text: string; out rt: Rect): Rect; virtual; abstract;
    procedure drawButton(const r: Rect; const Text: string; const rt: Rect; isDown: boolean; isHover: boolean; isFocus: boolean; style: integer); virtual; abstract;

    function getCheckRect(const r: Rect; const Text: string; out rt: Rect; out rc: Rect): Rect; virtual; abstract;
    procedure drawCheckButton(const r: Rect; const Text: string; const rt: Rect; const rr: Rect; isChecked: boolean; isHover: boolean; isFocus: boolean; style: integer); virtual; abstract;

    function getRadioRect(const r: Rect; const Text: string; out rt: Rect; out rr: Rect): Rect; virtual; abstract;
    procedure drawRadioButton(const r: Rect; const Text: string; const rt: Rect; const rr: Rect; isOn: boolean; isHover: boolean; isFocus: boolean; style: integer); virtual; abstract;

    function getHorizontalSliderRect(const r: Rect; out rs: Rect; v: double; out rc: Rect): Rect; virtual; abstract;
    procedure drawHorizontalSlider(const r: Rect; rs: Rect; v: double; rc: Rect; isHover: boolean; style: integer); virtual; abstract;

    function getItemRect(const r: Rect; const Text: string; out rt: Rect): Rect; virtual; abstract;
    procedure drawListItem(const r: Rect; const Text: string; const rt: Rect; isSelected: boolean; isHover: boolean; style: integer); virtual; abstract;

    function getListRect(const r: Rect; numOptions: integer; const options: array of string; out ri: Rect; out rt: Rect): Rect; virtual; abstract;
    procedure drawListBox(const r: Rect; numOptions: integer; const options: array of string; const ri: Rect; const rt: Rect; selected: integer; hovered: integer; style: integer); virtual; abstract;

    function getComboRect(const r: Rect; numOptions: integer; const options: array of string; selected: integer; out rt: Rect; out ra: Rect): Rect; virtual; abstract;
    function getComboOptionsRect(const rCombo: Rect; numOptions: integer; const options: array of string; out ri: Rect; out rit: Rect): Rect; virtual; abstract;
    procedure drawComboBox(const rect: Rect; numOptions: integer; const options: array of string; const rt: Rect; const ra: Rect; selected: integer; isHover: boolean; isFocus: boolean; style: integer); virtual; abstract;
    procedure drawComboOptions(const rect: Rect; numOptions: integer; const options: array of string; const ri: Rect; const rit: Rect; selected: integer; hovered: integer; isHover: boolean; isFocus: boolean; style: integer); virtual; abstract;

    function getLineEditRect(const r: Rect; const Text: string; out rt: Rect): Rect; virtual; abstract;
    procedure drawLineEdit(const r: Rect; const Text: string; const rt: Rect; caretPos: integer; isSelected: boolean; isHover: boolean; style: integer); virtual; abstract;

    function getPanelRect(const r: Rect; const Text: string; out rt: Rect; out ra: Rect): Rect; virtual; abstract;
    procedure drawPanel(const rect: Rect; const Text: string; const rt: Rect; const ra: Rect; isUnfold: boolean; isHover: boolean; isFocus: boolean; style: integer); virtual; abstract;

    function getTextureViewRect(const rect: Rect; out rt: Rect): Rect; virtual; abstract;
    procedure drawTextureView(const rect: Rect; const texID: integer; const rt: Rect; const rz: Rect; mipLevel: integer; texelScale: double; texelOffset: double; r: integer; g: integer; b: integer; a: integer; style: integer); virtual; abstract;

    // Eval widget dimensions
    function getCanvasMargin: integer; virtual;
    function getCanvasSpace: integer; virtual;
    function getFontHeight: integer; virtual;
    function getTextLineWidth(const Text: string): integer; virtual;
    function getTextSize(const Text: string; out nbLines: integer): integer; virtual;
    function getPickedCharNb(const Text: string; const at: Point): integer; virtual;

    procedure drawDebugRect(const r: Rect); virtual; abstract;

    procedure init; virtual; abstract;
  published
    property Font: TnvBaseFont read FFont write SetFont;
  end;

function EvalBool(b: boolean): byte;

implementation

function EvalBool(b: boolean): byte;
begin
  if b then
    Result := 1
  else
    Result := 0;
end;

{ UIPainter }

procedure UIPainter.SetFont(AValue: TnvBaseFont);
begin
  if FFont=AValue then Exit;
  FFont:=AValue;
end;

constructor UIPainter.Create;
begin
  inherited;
end;

procedure UIPainter._begin(const window: Rect);
begin
  init;
end;

procedure UIPainter._end;
begin

end;

function UIPainter.getCanvasMargin: integer;
begin
  Result := 0;
end;

function UIPainter.getCanvasSpace: integer;
begin
  Result := 0;
end;

function UIPainter.getFontHeight: integer;
begin
  Result := 0;
end;

function UIPainter.getTextLineWidth(const Text: string): integer;
begin
  Result := 0;
end;

function UIPainter.getTextSize(const Text: string; out nbLines: integer): integer;
begin
  Result := 0;
end;

function UIPainter.getPickedCharNb(const Text: string; const at: Point): integer;
begin
  Result := 0;
end;

end.

