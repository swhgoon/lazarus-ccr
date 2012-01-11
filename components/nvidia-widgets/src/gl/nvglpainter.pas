//
// nvGLWidgets.h - User Interface library
//
//
// Author: Ignacio Castano, Samuel Gateau, Evan Hart
// Email: sdkfeedback@nvidia.com
//
// Copyright (c) NVIDIA Corporation. All rights reserved.
////////////////////////////////////////////////////////////////////////////////
unit nvGLPainter;

{$mode objfpc}{$H+}

interface

uses
  Classes, nvPainter, nvTypes,
  GL, ftFont, FPCanvas;

const
  cBase = 0;
  cBool = 4;
  cOutline = 8;
  cFont = 12;
  cFontBack = 16;
  cTranslucent = 20;
  cNbColors = 24;

  s_colors: array [0..23, 0..3] of GLfloat =
    (
    // cBase
    (89 / 255, 89 / 255, 89 / 255, 0.7),
    (166 / 255, 166 / 255, 166 / 255, 0.8),
    (212 / 255, 228 / 255, 60 / 255, 0.5),
    (227 / 255, 237 / 255, 127 / 255, 0.5),

    // cBool
    (99 / 255, 37 / 255, 35 / 255, 1.0),
    (149 / 255, 55 / 255, 53 / 255, 1.0),
    (212 / 255, 228 / 255, 60 / 255, 1.0),
    (227 / 255, 237 / 255, 127 / 255, 1.0),

    // cOutline
    (255 / 255, 255 / 255, 255 / 255, 1.0),
    (255 / 255, 255 / 255, 255 / 255, 1.0),
    (255 / 255, 255 / 255, 255 / 255, 1.0),
    (255 / 255, 255 / 255, 255 / 255, 1.0),

    // cFont
    (255 / 255, 255 / 255, 255 / 255, 1.0),
    (255 / 255, 255 / 255, 255 / 255, 1.0),
    (255 / 255, 255 / 255, 255 / 255, 1.0),
    (255 / 255, 255 / 255, 255 / 255, 1.0),

    // cFontBack
    (79 / 255, 129 / 255, 189 / 255, 1.0),
    (79 / 255, 129 / 255, 189 / 255, 1.0),
    (128 / 255, 100 / 255, 162 / 255, 1.0),
    (128 / 255, 100 / 255, 162 / 255, 1.0),

    // cTranslucent
    (0 / 255, 0 / 255, 0 / 255, 0.0),
    (0 / 255, 0 / 255, 0 / 255, 0.0),
    (0 / 255, 0 / 255, 0 / 255, 0.0),
    (0 / 255, 0 / 255, 0 / 255, 0.0));

const
  cWidgetVSSource =
    '#version 120' + LineEnding +
    LineEnding +
    'void main()' + LineEnding +
    '{' + LineEnding +
    '    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;' + LineEnding +
    '    gl_TexCoord[0] = gl_MultiTexCoord0;' + LineEnding +
    '}';

  // @@ IC: Use standard GLSL. Do not initialize uniforms.
  cWidgetFSSource = (
    '#version 120' + LineEnding +
    'uniform vec4 fillColor /*= vec4( 1.0, 0.0,0.0,1.0)*/;' + LineEnding +
    'uniform vec4 borderColor /*= vec4( 1.0, 1.0,1.0,1.0)*/;' + LineEnding +
    'uniform vec2 zones;' + LineEnding +
    LineEnding +
    'void main()' + LineEnding +
    '{' + LineEnding +
    '    float doTurn = float(gl_TexCoord[0].y > 0);' + LineEnding +
    '    float radiusOffset = doTurn * abs( gl_TexCoord[0].z );' + LineEnding +
    '    float turnDir = sign( gl_TexCoord[0].z );' + LineEnding +
    '    vec2 uv = vec2(gl_TexCoord[0].x + turnDir*radiusOffset, gl_TexCoord[0].y);' + LineEnding +
    '    float l = abs( length(uv) - radiusOffset );' + LineEnding +
    '    float a = clamp( l - zones.x, 0.0, 2.0);' + LineEnding +
    '    float b = clamp( l - zones.y, 0.0, 2.0);' + LineEnding +
    '    b = exp2(-2.0*b*b);' + LineEnding +
    '    gl_FragColor = ( fillColor * b + (1.0-b)*borderColor );' + LineEnding +
    '    gl_FragColor.a *= exp2(-2.0*a*a);' + LineEnding +
    '}');

  cTexViewWidgetFSSource = (
    '#version 120' + LineEnding +
    'uniform float mipLevel /*= 0*/;' + LineEnding +
    'uniform float texelScale /*= 1.0*/;' + LineEnding +
    'uniform float texelOffset /*= 0.0*/;' + LineEnding +
    'uniform ivec4 texelSwizzling /*= ivec4( 0, 1, 2, 3)*/;' + LineEnding +
    'uniform sampler2D samp;' + LineEnding +
    LineEnding +
    'void main()' + LineEnding +
    '{' + LineEnding +
    '    vec4 texel;' + LineEnding +
    '    if (mipLevel > 0)' + LineEnding +
    '        texel = texture2DLod( samp, gl_TexCoord[0].xy, mipLevel);' + LineEnding +
    '    else' + LineEnding +
    '        texel = texture2D( samp, gl_TexCoord[0].xy);' + LineEnding +
    '    texel = texel * texelScale + texelOffset;' + LineEnding +
    '    gl_FragColor  = texel.x * vec4( texelSwizzling.x == 0, texelSwizzling.y == 0, texelSwizzling.z == 0, texelSwizzling.w == 0 );' + LineEnding +
    '    gl_FragColor += texel.y * vec4( texelSwizzling.x == 1, texelSwizzling.y == 1, texelSwizzling.z == 1, texelSwizzling.w == 1 );' + LineEnding +
    '    gl_FragColor += texel.z * vec4( texelSwizzling.x == 2, texelSwizzling.y == 2, texelSwizzling.z == 2, texelSwizzling.w == 2 );' + LineEnding +
    '    gl_FragColor += texel.w * vec4( texelSwizzling.x == 3, texelSwizzling.y == 3, texelSwizzling.z == 3, texelSwizzling.w == 3 );' + LineEnding +
    '}');

type

  //*************************************************************************
  // GLUIPainter

  GLUIPainter = class(UIPainter)

  public
    constructor Create;
    destructor Destroy; override;

    procedure _begin(const window: Rect); override;
    procedure _end; override;

    // These methods should be called between begin/end
    procedure drawFrame(const r: Rect; margin: integer; style: integer); override;

    function getLabelRect(const r: Rect; const Text: string; out rt: Rect; out nbLines: integer): Rect; override;
    procedure drawLabel(const r: Rect; const Text: string; const rt: Rect; const nbLines: integer; isHover: boolean; style: integer); override;

    function getButtonRect(const r: Rect; const Text: string; out rt: Rect): Rect; override;
    procedure drawButton(const r: Rect; const Text: string; const rt: Rect; isDown: boolean; isHover: boolean; isFocus: boolean; style: integer); override;

    function getCheckRect(const r: Rect; const Text: string; out rt: Rect; out rc: Rect): Rect; override;
    procedure drawCheckButton(const r: Rect; const Text: string; const rt: Rect; const rr: Rect; isChecked: boolean; isHover: boolean; isFocus: boolean; style: integer); override;

    function getRadioRect(const r: Rect; const Text: string; out rt: Rect; out rr: Rect): Rect; override;
    procedure drawRadioButton(const r: Rect; const Text: string; const rt: Rect; const rr: Rect; isOn: boolean; isHover: boolean; isFocus: boolean; style: integer); override;

    function getHorizontalSliderRect(const r: Rect; out rs: Rect; v: double; out rc: Rect): Rect; override;
    procedure drawHorizontalSlider(const r: Rect; rs: Rect; v: double; rc: Rect; isHover: boolean; style: integer); override;

    function getItemRect(const r: Rect; const Text: string; out rt: Rect): Rect; override;
    procedure drawListItem(const r: Rect; const Text: string; const rt: Rect; isSelected: boolean; isHover: boolean; style: integer); override;

    function getListRect(const r: Rect; numOptions: integer; const options: array of string; out ri: Rect; out rt: Rect): Rect; override;
    procedure drawListBox(const r: Rect; numOptions: integer; const options: array of string; const ri: Rect; const rt: Rect; selected: integer; hovered: integer; style: integer); override;

    function getComboRect(const r: Rect; numOptions: integer; const options: array of string; selected: integer; out rt: Rect; out ra: Rect): Rect; override;
    function getComboOptionsRect(const rCombo: Rect; numOptions: integer; const options: array of string; out ri: Rect; out rit: Rect): Rect; override;
    procedure drawComboBox(const aRect: Rect; numOptions: integer; const options: array of string; const rt: Rect; const ra: Rect; selected: integer; isHover: boolean; isFocus: boolean; style: integer); override;
    procedure drawComboOptions(const aRect: Rect; numOptions: integer; const options: array of string; const ri: Rect; const rit: Rect; selected: integer; hovered: integer; isHover: boolean; isFocus: boolean; style: integer); override;

    function getLineEditRect(const r: Rect; const Text: string; out rt: Rect): Rect; override;
    procedure drawLineEdit(const r: Rect; const Text: string; const rt: Rect; caretPos: integer; isSelected: boolean; isHover: boolean; style: integer); override;

    function getPanelRect(const r: Rect; const Text: string; out rt: Rect; out ra: Rect): Rect; override;
    procedure drawPanel(const r: Rect; const Text: string; const rt: Rect; const ra: Rect; isUnfold: boolean; isHover: boolean; isFocus: boolean; style: integer); override;

    function getTextureViewRect(const r: Rect; out rt: Rect): Rect; override;
    procedure drawTextureView(const aRect: Rect; const texID: integer; const rt: Rect; const rz: Rect; mipLevel: integer; texelScale: double; texelOffset: double; r: integer; g: integer; b: integer; a: integer; style: integer); override;

    // Eval widget dimensions
    function getCanvasMargin: integer; override;
    function getCanvasSpace: integer; override;
    function getFontHeight: integer; override;
    function getTextLineWidth(const Text: string): integer; override;
    function getTextSize(const Text: string; out nbLines: integer): integer; override;
    function getTextLineWidthAt(const Text: string; charNb: integer): integer;
    function getPickedCharNb(const Text: string; const at: Point): integer; override;

    procedure drawDebugRect(const r: Rect); override;

    // Draw primitive shapes
    procedure drawText(r: Rect; Text: string; nbLines: integer = 1; caretPos: integer = -1; isHover: boolean = False; isOn: boolean = False; isFocus: boolean = False);
    procedure drawFrame(aRect: Rect; corner: Point; isHover: boolean = False; isOn: boolean = False; isFocus: boolean = False);
    procedure drawBoolFrame(aRect: Rect; corner: Point; isHover: boolean = False; isOn: boolean = False; isFocus: boolean = False);
    procedure drawString(x: integer; y: integer; Text: string; nbLines: integer);
    procedure drawRect(aRect: Rect; fillColorId: integer; borderColorId: integer);
    procedure drawRoundedRect(aRect: Rect; const corner: Point; fillColorId: integer; borderColorId: integer);
    procedure drawRoundedRectOutline(aRect: Rect; corner: Point; borderColorId: integer);
    procedure drawCircle(aRect: Rect; fillColorId: integer; borderColorId: integer);
    procedure drawMinus(aRect: Rect; Width: integer; fillColorId: integer; borderColorId: integer);
    procedure drawPlus(aRect: Rect; Width: integer; fillColorId: integer; borderColorId: integer);
    procedure drawDownArrow(aRect: Rect; Width: integer; fillColorId: integer; borderColorId: integer);
    procedure drawUpArrow(aRect: Rect; Width: integer; fillColorId: integer; borderColorId: integer);

    procedure init; override;
  private
    m_setupStateDL: integer;
    m_restoreStateDL: integer;
    m_foregroundDL: integer;
    m_widgetProgram: integer;
    m_originUniform: integer;
    m_sizeUniform: integer;
    m_fillColorUniform: integer;
    m_borderColorUniform: integer;
    m_zonesUniform: integer;
    m_textureViewProgram: integer;
    m_texMipLevelUniform: integer;
    m_texelScaleUniform: integer;
    m_texelOffsetUniform: integer;
    m_texelSwizzlingUniform: integer;
  end;

implementation

uses
  GLu, GLut, GLext, Math, nvShaderUtils;

function getWidgetMargin: integer;
begin
  Result := 3; //2
end;

function getWidgetSpace: integer;
begin
  Result := 2;
end;

function getAutoWidth: integer;
begin
  Result := 100;
end;

function getAutoHeight: integer;
begin
  Result := 12 + 4;
end;

{ GLUIPainter }

constructor GLUIPainter.Create;
begin
  inherited;
  m_setupStateDL := 0;
  m_restoreStateDL := 0;
  m_foregroundDL := 0;
  m_widgetProgram := 0;
  m_fillColorUniform := 0;
  m_borderColorUniform := 0;
  m_zonesUniform := 0;
  m_textureViewProgram := 0;
  m_texMipLevelUniform := 0;
  m_texelScaleUniform := 0;
  m_texelOffsetUniform := 0;
  m_texelSwizzlingUniform := 0;
end;

destructor GLUIPainter.Destroy;
begin
  Font.Free;
  inherited Destroy;
end;

procedure GLUIPainter._begin(const window: Rect);
begin
  inherited _begin(window);

  // Cache and setup state
  glCallList(m_setupStateDL);

  // Set matrices.
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  gluOrtho2D(window.x, window.w, window.y, window.h);
end;

procedure GLUIPainter._end;
begin
  inherited _end;

  if m_foregroundDL <> 0 then
  begin
    glCallList(m_foregroundDL);
    glDeleteLists(m_foregroundDL, 1);
    m_foregroundDL := 0;
  end;

  //Restore state.
  glCallList(m_restoreStateDL);
end;

procedure GLUIPainter.drawFrame(const r: Rect; margin: integer; style: integer);
begin
  drawRoundedRectOutline(SetRect(r.x - margin, r.y - margin, r.w + 2 * margin, r.h + 2 * margin), SetPoint(margin, margin), cOutline);
end;

function GLUIPainter.getLabelRect(const r: Rect; const Text: string; out rt: Rect; out nbLines: integer): Rect;
var
  aRect: Rect;
begin
  aRect := r;
  rt.x := getWidgetMargin;
  rt.y := getWidgetMargin;

  // Eval Nblines and max line width anyway.
  rt.w := getTextSize(Text, nbLines);

  if aRect.w = 0 then
    aRect.w := rt.w + 2 * rt.x
  else
    rt.w := aRect.w - 2 * rt.x;

  if aRect.h = 0 then
  begin
    rt.h := getFontHeight * nbLines;
    aRect.h := rt.h + 2 * rt.y;
  end
  else
    rt.h := aRect.h - 2 * rt.y;

  Result := aRect;
end;

procedure GLUIPainter.drawLabel(const r: Rect; const Text: string; const rt: Rect; const nbLines: integer; isHover: boolean; style: integer);
begin
  if style > 0 then
    drawFrame(r, SetPoint(rt.x, rt.y), False, False, False);

  drawText(SetRect(r.x + rt.x, r.y + rt.y, rt.w, rt.h), Text, nbLines);
end;

function GLUIPainter.getButtonRect(const r: Rect; const Text: string; out rt: Rect): Rect;
var
  aRect: Rect;
begin
  aRect := r;
  rt.x := {4*} getWidgetMargin;
  rt.y := {4*} getWidgetMargin;

  if aRect.w = 0 then
  begin
    rt.w := getTextLineWidth(Text);
    aRect.w := rt.w + 2 * rt.x;
  end
  else
    rt.w := aRect.w - 2 * rt.x;

  if aRect.h = 0 then
  begin
    rt.h := getFontHeight;
    aRect.h := rt.h + 2 * rt.y;
  end
  else
    rt.h := aRect.h - 2 * rt.y;

  Result := aRect;
end;

procedure GLUIPainter.drawButton(const r: Rect; const Text: string; const rt: Rect; isDown: boolean; isHover: boolean; isFocus: boolean; style: integer);
begin
  drawFrame(r, SetPoint(rt.x, rt.y), isHover, isDown, isFocus);
  drawText(SetRect(r.x + rt.x, r.y + rt.y, rt.w, rt.h), Text);
end;

function GLUIPainter.getCheckRect(const r: Rect; const Text: string; out rt: Rect; out rc: Rect): Rect;
var
  aRect: Rect;
  rcOffset: integer;
begin
  aRect := r;
  rcOffset := Round(0.125 * getAutoHeight);
  rc.h := getAutoHeight - 2 * rcOffset;
  rc.w := rc.h;
  rc.x := getWidgetMargin + rcOffset;
  rc.y := getWidgetMargin + rcOffset;
  rt.x := getAutoHeight + 2 * getWidgetMargin;
  rt.y := getWidgetMargin;

  if aRect.w = 0 then
  begin
    rt.w := getTextLineWidth(Text);
    aRect.w := rt.x + rt.w + getWidgetMargin;
  end;

  if aRect.h = 0 then
  begin
    rt.h := getFontHeight;
    aRect.h := rt.h + 2 * rt.y;
  end;

  Result := aRect;
end;

procedure GLUIPainter.drawCheckButton(const r: Rect; const Text: string; const rt: Rect; const rr: Rect; isChecked: boolean; isHover: boolean; isFocus: boolean; style: integer);
begin
  if style <> 0 then
    drawFrame(r, SetPoint(rt.y, rt.y), isHover, False, isFocus);

  drawBoolFrame(SetRect(r.x + rr.x, r.y + rr.y, rr.w, rr.h), SetPoint(rr.w div 6, rr.h div 6), isHover, isChecked, False);
  drawText(SetRect(r.x + rt.x, r.y + rt.y, rt.w, rt.h), Text);
end;

function GLUIPainter.getRadioRect(const r: Rect; const Text: string; out rt: Rect; out rr: Rect): Rect;
var
  aRect: Rect;
  rrOffset: integer;
begin
  aRect := r;
  rrOffset := Round(0.125 * getAutoHeight);
  rr.h := getAutoHeight - 2 * rrOffset;
  rr.w := rr.h;
  rr.x := getWidgetMargin + rrOffset;
  rr.y := getWidgetMargin + rrOffset;
  rt.x := getAutoHeight + 2 * getWidgetMargin;
  rt.y := getWidgetMargin;

  if aRect.w = 0 then
  begin
    rt.w := getTextLineWidth(Text);
    aRect.w := rt.w + rt.x + getWidgetMargin;
  end;

  if aRect.h = 0 then
  begin
    rt.h := getFontHeight;
    aRect.h := rt.h + 2 * rt.y;
  end;

  Result := aRect;
end;

procedure GLUIPainter.drawRadioButton(const r: Rect; const Text: string; const rt: Rect; const rr: Rect; isOn: boolean; isHover: boolean; isFocus: boolean; style: integer);
begin
  if style <> 0 then
    drawFrame(r, SetPoint(rt.y, rt.y), isHover, False, isFocus);

  drawBoolFrame(SetRect(r.x + rr.x, r.y + rr.y, rr.w, rr.h), SetPoint(rr.w div 2, rr.h div 2), isHover, isOn, False);
  drawText(SetRect(r.x + rt.x, r.y + rt.y, rt.w, rt.h), Text);
end;

function GLUIPainter.getHorizontalSliderRect(const r: Rect; out rs: Rect; v: double; out rc: Rect): Rect;
var
  aRect: Rect;
begin
  aRect := r;

  if aRect.w = 0 then
    aRect.w := getAutoWidth + 2 * getWidgetMargin;

  if aRect.h = 0 then
    aRect.h := getAutoHeight + 2 * getWidgetMargin;

  // Eval the sliding & cursor aRect
  rs.y := getWidgetMargin;
  rs.h := aRect.h - 2 * rs.y;

  rc.y := rs.y;
  rc.h := rs.h;

  rs.x := 0; //getWidgetMargin;
  rc.w := rc.h;
  rs.w := aRect.w - 2 * rs.x - rc.w;
  rc.x := Round(v * rs.w);

  Result := aRect;
end;

procedure GLUIPainter.drawHorizontalSlider(const r: Rect; rs: Rect; v: double; rc: Rect; isHover: boolean; style: integer);
var
  sliderHeight: integer;
begin
  sliderHeight := rs.h div 3;
  drawFrame(SetRect(r.x + rs.x, r.y + rs.y + sliderHeight, r.w - 2 * rs.x, sliderHeight), SetPoint(sliderHeight div 2, sliderHeight div 2), isHover, False, False);
  drawFrame(SetRect(r.x + rs.x + rc.x, r.y + rc.y, rc.w, rc.h), SetPoint(rc.w div 2, rc.h div 2), isHover, True, False);
end;

function GLUIPainter.getItemRect(const r: Rect; const Text: string; out rt: Rect): Rect;
var
  aRect: Rect;
begin
  aRect := r;
  rt.x := 0;
  rt.y := 0;

  if aRect.w = 0 then
  begin
    rt.w := getTextLineWidth(Text);
    aRect.w := rt.w + 2 * rt.x;
  end
  else
    rt.w := aRect.w - 2 * rt.x;

  if aRect.h = 0 then
  begin
    rt.h := getFontHeight;
    aRect.h := rt.h + 2 * rt.y;
  end
  else
    rt.h := aRect.h - 2 * rt.y;

  Result := aRect;
end;

procedure GLUIPainter.drawListItem(const r: Rect; const Text: string; const rt: Rect; isSelected: boolean; isHover: boolean; style: integer);
begin
  //drawFrame(r, SetPoint(0, 0), isHover, isSelected, False);
  drawText(SetRect(r.x + rt.x, r.y + rt.y, rt.w, rt.h), Text, 1, -1, isHover, isSelected);
end;

function GLUIPainter.getListRect(const r: Rect; numOptions: integer; const options: array of string; out ri: Rect; out rt: Rect): Rect;
var
  aRect: Rect;
  i: integer;
  l: integer;
begin
  aRect := r;
  ri.x := getWidgetMargin;
  ri.y := getWidgetMargin;
  rt.x := getWidgetMargin;
  rt.y := getWidgetMargin;
  if aRect.w = 0 then
  begin
    rt.w := 0;
    for i := 0 to High(options) do
    begin
      l := getTextLineWidth(options[i]);
      if l > rt.w then
        rt.w := l;
    end;
    ri.w := rt.w + 2 * rt.x;
    aRect.w := ri.w + 2 * ri.x;
  end
  else
  begin
    ri.w := aRect.w - 2 * ri.x;
    rt.w := ri.w - 2 * rt.x;
  end;

  if aRect.h = 0 then
  begin
    rt.h := getFontHeight;
    ri.h := rt.h + rt.y;
    aRect.h := numOptions * ri.h + 2 * ri.y;
  end
  else
  begin
    ri.h := (aRect.h - 2 * ri.y) div numOptions;
    rt.h := ri.h - rt.y;
  end;

  Result := aRect;
end;

procedure GLUIPainter.drawListBox(const r: Rect; numOptions: integer; const options: array of string; const ri: Rect; const rt: Rect; selected: integer; hovered: integer; style: integer);
var
  ir: Rect;
  i: integer;
begin
  drawFrame(r, SetPoint(ri.x, ri.y));

  ir.Rect(r.x + ri.x, r.y + r.h - ri.y - ri.h, ri.w, ri.h);

  for i := 0 to High(options) do
  begin
    if (i = hovered) or (i = selected) then
      drawFrame(ir, SetPoint(ri.x, ri.y), False, (i = selected));

    drawText(SetRect(ir.x + rt.x, ir.y + rt.y, rt.w, rt.h), options[i]);

    ir.y := ir.y - (ir.h);
  end;
end;

function GLUIPainter.getComboRect(const r: Rect; numOptions: integer; const options: array of string; selected: integer; out rt: Rect; out ra: Rect): Rect;
var
  aRect: Rect;
  i: integer;
  l: integer;
begin
  aRect := r;
  rt.x := getWidgetMargin;
  rt.y := getWidgetMargin;
  if aRect.h = 0 then
  begin
    rt.h := getFontHeight;
    aRect.h := rt.h + 2 * rt.y;
  end
  else
    rt.h := aRect.h - 2 * rt.y;

  ra.h := rt.h;
  ra.w := ra.h;
  ra.y := rt.y;

  if aRect.w = 0 then
  begin
    rt.w := 0;

    for i := 0 to numOptions - 1 do
    begin
      l := getTextLineWidth(options[i]);
      if l > rt.w then
        rt.w := l;
    end;
    aRect.w := rt.w + 2 * rt.x;
    //Add room for drop down button
    aRect.w := aRect.w + (rt.h + rt.x);
  end
  else
    //Add room for drop down button
    rt.w := aRect.w - 3 * rt.x - rt.h;

  ra.x := 2 * rt.x + rt.w;

  Result := aRect;
end;

function GLUIPainter.getComboOptionsRect(const rCombo: Rect; numOptions: integer; const options: array of string; out ri: Rect; out rit: Rect): Rect;
var
  aRect: Rect;
begin
  //the options frame is like a list box
  aRect := getListRect(SetRect(0, 0, 0, 0), numOptions, options, ri, rit);

  //offset by the Combo box pos itself
  aRect.x := rCombo.x;
  aRect.y := rCombo.y - aRect.h;

  Result := aRect;
end;

procedure GLUIPainter.drawComboBox(const aRect: Rect; numOptions: integer; const options: array of string; const rt: Rect; const ra: Rect; selected: integer; isHover: boolean; isFocus: boolean; style: integer);
begin
  drawFrame(aRect, SetPoint(rt.x, rt.y), isHover, False, isFocus);
  drawText(SetRect(aRect.x + rt.x, aRect.y + rt.y, rt.w, rt.h), options[selected]);
  drawDownArrow(SetRect(aRect.x + ra.x, aRect.y + ra.y, ra.w, ra.h), Round(ra.h * 0.15), cBase + EvalBool(not isHover) + (EvalBool(isFocus) shl 2), cOutline);
end;

procedure GLUIPainter.drawComboOptions(const aRect: Rect; numOptions: integer; const options: array of string; const ri: Rect; const rit: Rect; selected: integer; hovered: integer; isHover: boolean; isFocus: boolean; style: integer);
begin
  m_foregroundDL := glGenLists(1);
  glNewList(m_foregroundDL, GL_COMPILE);
  drawListBox(aRect, numOptions, options, ri, rit, selected, hovered, style);
  glEndList;
end;

function GLUIPainter.getLineEditRect(const r: Rect; const Text: string; out rt: Rect): Rect;
var
  aRect: Rect;
begin
  aRect := r;
  rt.x := getWidgetMargin;
  rt.y := getWidgetMargin;

  if aRect.w = 0 then
  begin
    rt.w := max(getTextLineWidth(Text), 100);
    aRect.w := rt.w + 2 * rt.x;
  end
  else
    rt.w := aRect.w - 2 * rt.x;

  if aRect.h = 0 then
  begin
    rt.h := getFontHeight;
    aRect.h := rt.h + 2 * rt.y;
  end
  else
    rt.h := aRect.h - 2 * rt.y;

  Result := aRect;
end;

procedure GLUIPainter.drawLineEdit(const r: Rect; const Text: string; const rt: Rect; caretPos: integer; isSelected: boolean; isHover: boolean; style: integer);
begin
  drawFrame(r, SetPoint(rt.x, rt.y), True, isSelected, False);
  drawText(SetRect(r.x + rt.x, r.y + rt.y, rt.w, rt.h), Text, 1, caretPos);
end;

function GLUIPainter.getPanelRect(const r: Rect; const Text: string; out rt: Rect; out ra: Rect): Rect;
var
  aRect: Rect;
begin
  aRect := r;
  rt.x := getWidgetMargin;
  rt.y := getWidgetMargin;

  if aRect.h = 0 then
  begin
    rt.h := getFontHeight;
    aRect.h := rt.h + 2 * rt.y;
  end
  else
    rt.h := aRect.h - 2 * rt.y;

  ra.h := rt.h;
  ra.w := ra.h;
  ra.y := rt.y;

  if aRect.w = 0 then
  begin
    rt.w := getTextLineWidth(Text);
    aRect.w := rt.w + 2 * rt.x;

    // Add room for drop down button
    aRect.w := aRect.w + (ra.h + rt.x);
  end
  else
    // Add room for drop down button
    rt.w := aRect.w - 3 * rt.x - ra.h;

  ra.x := 2 * rt.x + rt.w;

  Result := aRect;
end;

procedure GLUIPainter.drawPanel(const r: Rect; const Text: string; const rt: Rect; const ra: Rect; isUnfold: boolean; isHover: boolean; isFocus: boolean; style: integer);
begin
  drawFrame(r, SetPoint(rt.x, rt.y), isHover, False, isFocus);
  drawText(SetRect(r.x + rt.x, r.y + rt.y, rt.w, rt.h), Text);

  if isUnfold then
    drawMinus(SetRect(r.x + ra.x, r.y + ra.y, ra.w, ra.h), Round(ra.h * 0.15), cBase + EvalBool(not isHover) + (EvalBool(isFocus) shl 2), cOutline)
  else
    drawPlus(SetRect(r.x + ra.x, r.y + ra.y, ra.w, ra.h), Round(ra.h * 0.15), cBase + EvalBool(not isHover) + (EvalBool(isFocus) shl 2), cOutline);
end;

function GLUIPainter.getTextureViewRect(const r: Rect; out rt: Rect): Rect;
var
  aRect: Rect;
begin
  aRect := r;
  if aRect.w = 0 then
    aRect.w := getAutoWidth;

  if aRect.h = 0 then
    aRect.h := aRect.w;

  rt.x := getCanvasMargin;
  rt.y := getCanvasMargin;
  rt.w := aRect.w - 2 * getCanvasMargin;
  rt.h := aRect.h - 2 * getCanvasMargin;

  Result := aRect;
end;

procedure GLUIPainter.drawTextureView(const aRect: Rect; const texID: integer; const rt: Rect; const rz: Rect; mipLevel: integer; texelScale: double; texelOffset: double; r: integer; g: integer; b: integer; a: integer; style: integer);
var
  lTexID: GLuint;
begin
  glPushMatrix;

  drawFrame(aRect, SetPoint(rt.x, rt.y), False, False, False);

  lTexID := texID;
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, lTexID);
  glUseProgram(m_textureViewProgram);

  glUniform1f(m_texMipLevelUniform, mipLevel);
  glUniform1f(m_texelScaleUniform, texelScale);
  glUniform1f(m_texelOffsetUniform, texelOffset);
  glUniform4i(m_texelSwizzlingUniform, r, g, b, a);

  glBegin(GL_QUADS);
    glTexCoord2f(rz.x / rt.w, rz.y / rt.h);
    glVertex2f(aRect.x + rt.x, aRect.y + rt.y);
    glTexCoord2f(rz.x / rt.w, (rz.y + rz.h) / rt.h);
    glVertex2f(aRect.x + rt.x, aRect.y + rt.y + rt.h);
    glTexCoord2f((rz.x + rz.w) / rt.w, (rz.y + rz.h) / rt.h);
    glVertex2f(aRect.x + rt.x + rt.w, aRect.y + rt.y + rt.h);
    glTexCoord2f((rz.x + rz.w) / rt.w, rz.y / rt.h);
    glVertex2f(aRect.x + rt.x + rt.w, aRect.y + rt.y);
  glEnd;
  glUseProgram(0);

  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_TEXTURE_2D);
  glPopMatrix;
end;

function GLUIPainter.getCanvasMargin: integer;
begin
  Result := 5;
end;

function GLUIPainter.getCanvasSpace: integer;
begin
  Result := 5;
end;

function GLUIPainter.getFontHeight: integer;
begin
  Result := Font.TextHeight('X') + 4;
end;

function GLUIPainter.getTextLineWidth(const Text: string): integer;
begin
  Result := Font.TextWidth(Text) + 2;
end;

function GLUIPainter.getTextSize(const Text: string; out nbLines: integer): integer;
var
  w: integer = 0;
  i: integer;
  s: TStrings;
begin
  s := TStringList.Create;

  try
    s.Text:= Text;
    nbLines := s.Count;

    for i := 0 to nbLines - 1 do
      w := Max(w, getTextLineWidth(s[i]));
  finally
    s.Free;
  end;

  Result := w;
end;

function GLUIPainter.getTextLineWidthAt(const Text: string; charNb: integer): integer;
var
  w: integer = 1;
  i: integer;
begin
  for i := 0 to charNb - 1 do
  begin
    if (Text[i] <> #13) and (Text[i + 1] <> #10) then
      w := w + Font.TextWidth(Text[i])
    else
    begin
      Result := w + 1;
      exit;
    end;
  end;

  Result := w;
end;

function GLUIPainter.getPickedCharNb(const Text: string; const at: Point): integer;
var
  textstart: byte;
  w: integer = 1;
  i: integer;
begin
  textstart := 0;

  if at.x < w then
  begin
    Result := 0;
    exit;
  end;

  i := 0;
  while (i <= Length(Text)) and (Text[i] <> #13) and (Text[i + 1] <> #10) do
  begin
    w := w + Font.TextWidth(Text[i]);
    if at.x < w then
    begin
      Result := i - textstart;
      exit;
    end;

    Inc(i);
  end;

  Result := i - textstart;
end;

procedure GLUIPainter.drawDebugRect(const r: Rect);
begin
  glBegin(GL_LINE_STRIP);
    glVertex2i(r.x, r.y);
    glVertex2i(r.x + r.w, r.y);
    glVertex2i(r.x + r.w, r.y + r.h);
    glVertex2i(r.x, r.y + r.h);
    glVertex2i(r.x, r.y);
  glEnd;
end;

procedure GLUIPainter.drawText(r: Rect; Text: string; nbLines: integer; caretPos: integer; isHover: boolean; isOn: boolean; isFocus: boolean);
var
  w: integer;
begin
  if isHover or isOn {or isFocus} then
    drawRect(r, cFontBack + EvalBool(isHover) + (EvalBool(isOn) shl 1), cOutline);

  glColor4fv(@s_colors[cFont]);

  drawString(r.x, r.y, Text, nbLines);
  if caretPos <> -1 then
  begin
    w := getTextLineWidthAt(Text, caretPos);
    drawRect(SetRect(r.x + w, r.y, 2, r.h), cOutline, cOutline);
  end;
end;

procedure GLUIPainter.drawFrame(aRect: Rect; corner: Point; isHover: boolean; isOn: boolean; isFocus: boolean);
var
  lColorNb: integer;
begin
  lColorNb := cBase + EvalBool(isHover) + (EvalBool(isOn) shl 1); { + (isFocus shl 2);}

  if corner.x + corner.y = 0 then
    drawRect(aRect, lColorNb, cOutline)
  else
    drawRoundedRect(aRect, corner, lColorNb, cOutline);
end;

procedure GLUIPainter.drawBoolFrame(aRect: Rect; corner: Point; isHover: boolean; isOn: boolean; isFocus: boolean);
var
  lColorNb: integer;
begin
  lColorNb := cBool + EvalBool(isHover) + (EvalBool(isOn) shl 1); // + (isFocus shl 2);
  drawRoundedRect(aRect, corner, lColorNb, cOutline);
end;

procedure GLUIPainter.drawString(x: integer; y: integer; Text: string; nbLines: integer);
begin
  Font.TextOut(x, y + 2, Text);
end;

procedure GLUIPainter.drawRect(aRect: Rect; fillColorId: integer; borderColorId: integer);
var
  x0, x1, y0, y1: double;
begin
  glUseProgram(m_widgetProgram);

  glUniform4fv(m_fillColorUniform, 1, @s_colors[fillColorId]);
  glUniform4fv(m_borderColorUniform, 1, @s_colors[borderColorId]);
  glUniform2f(m_zonesUniform, 0, 0);

  x0 := aRect.x;
  x1 := aRect.x + aRect.w;

  y0 := aRect.y;
  y1 := aRect.y + aRect.h;

  glBegin(GL_TRIANGLE_STRIP);
  glTexCoord2f(0, 0);

  glVertex2f(x0, y0);
  glVertex2f(x1, y0);

  glVertex2f(x0, y1);
  glVertex2f(x1, y1);
  glEnd;

  glUseProgram(0);
end;

procedure GLUIPainter.drawRoundedRect(aRect: Rect; const corner: Point; fillColorId: integer; borderColorId: integer);
var
  xb: double;
  yb: double;
  x0: double;
  x1: double;
  x2: double;
  x3: double;
  y0: double;
  y1: double;
  y2: double;
  y3: double;
begin
  glUseProgram(m_widgetProgram);
  glUniform4fv(m_fillColorUniform, 1, @s_colors[fillColorId]);
  glUniform4fv(m_borderColorUniform, 1, @s_colors[borderColorId]);
  glUniform2f(m_zonesUniform, corner.x - 1, corner.x - 2);

  xb := corner.x;
  yb := corner.y;

  x0 := aRect.x;
  x1 := aRect.x + corner.x;
  x2 := aRect.x + aRect.w - corner.x;
  x3 := aRect.x + aRect.w;

  y0 := aRect.y;
  y1 := aRect.y + corner.y;
  y2 := aRect.y + aRect.h - corner.y;
  y3 := aRect.y + aRect.h;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(xb, yb);
    glVertex2f(x0, y0);
    glTexCoord2f(0, yb);
    glVertex2f(x1, y0);

    glTexCoord2f(xb, 0);
    glVertex2f(x0, y1);
    glTexCoord2f(0, 0);
    glVertex2f(x1, y1);

    glTexCoord2f(xb, 0);
    glVertex2f(x0, y2);
    glTexCoord2f(0, 0);
    glVertex2f(x1, y2);

    glTexCoord2f(xb, yb);
    glVertex2f(x0, y3);
    glTexCoord2f(0, yb);
    glVertex2f(x1, y3);
  glEnd;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0, yb);
    glVertex2f(x2, y0);
    glTexCoord2f(xb, yb);
    glVertex2f(x3, y0);

    glTexCoord2f(0, 0);
    glVertex2f(x2, y1);
    glTexCoord2f(xb, 0);
    glVertex2f(x3, y1);

    glTexCoord2f(0, 0);
    glVertex2f(x2, y2);
    glTexCoord2f(xb, 0);
    glVertex2f(x3, y2);

    glTexCoord2f(0, yb);
    glVertex2f(x2, y3);
    glTexCoord2f(xb, yb);
    glVertex2f(x3, y3);
  glEnd;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0, yb);
    glVertex2f(x1, y0);
    glTexCoord2f(0, yb);
    glVertex2f(x2, y0);

    glTexCoord2f(0, 0);
    glVertex2f(x1, y1);
    glTexCoord2f(0, 0);
    glVertex2f(x2, y1);

    glTexCoord2f(0, 0);
    glVertex2f(x1, y2);
    glTexCoord2f(0, 0);
    glVertex2f(x2, y2);

    glTexCoord2f(0, yb);
    glVertex2f(x1, y3);
    glTexCoord2f(0, yb);
    glVertex2f(x2, y3);
  glEnd;

  glUseProgram(0);
end;

procedure GLUIPainter.drawRoundedRectOutline(aRect: Rect; corner: Point; borderColorId: integer);
var
  xb: double;
  yb: double;
  x0: double;
  x1: double;
  x2: double;
  x3: double;
  y0: double;
  y1: double;
  y2: double;
  y3: double;
begin
  glUseProgram(m_widgetProgram);
  glUniform4fv(m_fillColorUniform, 1, @s_colors[cTranslucent]);
  glUniform4fv(m_borderColorUniform, 1, @s_colors[borderColorId]);
  glUniform2f(m_zonesUniform, corner.x - 1, corner.x - 2);

  xb := corner.x;
  yb := corner.y;
  x0 := aRect.x;
  x1 := aRect.x + corner.x;
  x2 := aRect.x + aRect.w - corner.x;
  x3 := aRect.x + aRect.w;
  y0 := aRect.y;
  y1 := aRect.y + corner.y;
  y2 := aRect.y + aRect.h - corner.y;
  y3 := aRect.y + aRect.h;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(xb, yb);
    glVertex2f(x0, y0);
    glTexCoord2f(0, yb);
    glVertex2f(x1, y0);
    glTexCoord2f(xb, 0);

    glVertex2f(x0, y1);
    glTexCoord2f(0, 0);
    glVertex2f(x1, y1);
    glTexCoord2f(xb, 0);

    glVertex2f(x0, y2);
    glTexCoord2f(0, 0);
    glVertex2f(x1, y2);
    glTexCoord2f(xb, yb);

    glVertex2f(x0, y3);
    glTexCoord2f(0, yb);
    glVertex2f(x1, y3);
  glEnd;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0, yb);
    glVertex2f(x2, y0);
    glTexCoord2f(xb, yb);
    glVertex2f(x3, y0);
    glTexCoord2f(0, 0);

    glVertex2f(x2, y1);
    glTexCoord2f(xb, 0);
    glVertex2f(x3, y1);
    glTexCoord2f(0, 0);

    glVertex2f(x2, y2);
    glTexCoord2f(xb, 0);
    glVertex2f(x3, y2);
    glTexCoord2f(0, yb);
    glVertex2f(x2, y3);
    glTexCoord2f(xb, yb);
    glVertex2f(x3, y3);
  glEnd;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0, yb);
    glVertex2f(x1, y0);
    glTexCoord2f(0, yb);
    glVertex2f(x2, y0);
    glTexCoord2f(0, 0);

    glVertex2f(x1, y1);
    glTexCoord2f(0, 0);
    glVertex2f(x2, y1);
  glEnd;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0, 0);
    glVertex2f(x1, y2);
    glTexCoord2f(0, 0);
    glVertex2f(x2, y2);
    glTexCoord2f(0, yb);
    glVertex2f(x1, y3);
    glTexCoord2f(0, yb);
    glVertex2f(x2, y3);
  glEnd;

  glUseProgram(0);
end;

procedure GLUIPainter.drawCircle(aRect: Rect; fillColorId: integer; borderColorId: integer);
var
  xb: double;
  yb: double;
  x0: double;
  x1: double;
  y0: double;
  y1: double;
begin
  glUseProgram(m_widgetProgram);
  glUniform4fv(m_fillColorUniform, 1, @s_colors[fillColorId]);
  glUniform4fv(m_borderColorUniform, 1, @s_colors[borderColorId]);
  glUniform2f(m_zonesUniform, (aRect.w div 2) - 1, (aRect.w div 2) - 2);

  xb := aRect.w div 2;
  yb := aRect.w div 2;
  x0 := aRect.x;
  x1 := aRect.x + aRect.w;
  y0 := aRect.y;
  y1 := aRect.y + aRect.h;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(-xb, -yb);
    glVertex2f(x0, y0);
    glTexCoord2f(xb, -yb);
    glVertex2f(x1, y0);
    glTexCoord2f(-xb, yb);
    glVertex2f(x0, y1);
    glTexCoord2f(xb, yb);
    glVertex2f(x1, y1);
  glEnd;

  glUseProgram(0);
end;

procedure GLUIPainter.drawMinus(aRect: Rect; Width: integer; fillColorId: integer; borderColorId: integer);
var
  xb: double;
  yb: double;
  xoff: double;
  yoff: double;
  x0: double;
  x1: double;
  y1: double;
begin
  xb := Width;
  yb := Width;
  xoff := xb;
  yoff := yb;
  x0 := aRect.x + aRect.w * 0.1;
  x1 := aRect.x + aRect.w * 0.9;
  y1 := aRect.y + aRect.h * 0.5;

  glUseProgram(m_widgetProgram);
  glUniform4fv(m_fillColorUniform, 1, @s_colors[fillColorId]);
  glUniform4fv(m_borderColorUniform, 1, @s_colors[borderColorId]);
  glUniform2f(m_zonesUniform, (xb) - 1, (xb) - 2);

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord3f(-xb, -yb, 0);
    glVertex2f(x0, y1 + yoff);
    glTexCoord3f(xb, -yb, 0);
    glVertex2f(x0, y1 - yoff);
    glTexCoord3f(-xb, 0, 0);
    glVertex2f(x0 + xoff, y1 + yoff);
    glTexCoord3f(xb, 0, 0);
    glVertex2f(x0 + xoff, y1 - yoff);
    glTexCoord3f(-xb, 0, 0);

    glVertex2f(x1 - xoff, y1 + yoff);
    glTexCoord3f(xb, 0, 0);
    glVertex2f(x1 - xoff, y1 - yoff);
    glTexCoord3f(-xb, -yb, 0);

    glVertex2f(x1, y1 + yoff);
    glTexCoord3f(xb, -yb, 0);
    glVertex2f(x1, y1 - yoff);
  glEnd;

  glUseProgram(0);
end;

procedure GLUIPainter.drawPlus(aRect: Rect; Width: integer; fillColorId: integer; borderColorId: integer);
var
  xb: double;
  yb: double;
  xoff: double;
  yoff: double;
  x0: double;
  x1: double;
  x2: double;
  y0: double;
  y1: double;
  y2: double;
begin
  xb := Width;
  yb := Width;
  xoff := xb;
  yoff := yb;
  x0 := aRect.x + aRect.w * 0.1;
  x1 := aRect.x + aRect.w * 0.5;
  x2 := aRect.x + aRect.w * 0.9;
  y0 := aRect.y + aRect.h * 0.1;
  y1 := aRect.y + aRect.h * 0.5;
  y2 := aRect.y + aRect.h * 0.9;

  glUseProgram(m_widgetProgram);
  glUniform4fv(m_fillColorUniform, 1, @s_colors[fillColorId]);
  glUniform4fv(m_borderColorUniform, 1, @s_colors[borderColorId]);
  glUniform2f(m_zonesUniform, (xb) - 1, (xb) - 2);

  {
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord3f(-xb, -yb, 0);
    glVertex2f( x0, y1 + yoff);
    glTexCoord3f(xb, -yb, 0);
    glVertex2f( x0, y1 - yoff);

    glTexCoord3f(-xb, 0, 0);
    glVertex2f( x0 + xoff , y1 + yoff);
    glTexCoord3f(xb, 0, 0);
    glVertex2f( x0 + xoff, y1 - yoff);

    glTexCoord3f(-xb, 0, 0);
    glVertex2f( x1 - xoff , y1 + yoff);
    glTexCoord3f(xb, 0, 0);
    glVertex2f( x1 - xoff, y1 - yoff);

    glTexCoord3f(0, yb, 0);
    glVertex2f( x1, y1);
  glEnd;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord3f(0, yb, 0);
    glVertex2f( x1, y1);

    glTexCoord3f(-xb, 0, 0);
    glVertex2f( x1 + xoff , y1 + yoff);
    glTexCoord3f(xb, 0, 0);
    glVertex2f( x1 + xoff, y1 - yoff);

    glTexCoord3f(-xb, 0, 0);
    glVertex2f( x2 - xoff , y1 + yoff);
    glTexCoord3f(xb, 0, 0);
    glVertex2f( x2 - xoff, y1 - yoff);

    glTexCoord3f(-xb, -yb, 0);
    glVertex2f( x2, y1 + yoff);
    glTexCoord3f(xb, -yb, 0);
    glVertex2f( x2, y1 - yoff);
  glEnd;
  }

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord3f(-xb, -yb, 0);
    glVertex2f(x0, y1 + yoff);
    glTexCoord3f(xb, -yb, 0);
    glVertex2f(x0, y1 - yoff);
    glTexCoord3f(-xb, 0, 0);
    glVertex2f(x0 + xoff, y1 + yoff);
    glTexCoord3f(xb, 0, 0);
    glVertex2f(x0 + xoff, y1 - yoff);
    glTexCoord3f(-xb, 0, 0);

    glVertex2f(x2 - xoff, y1 + yoff);
    glTexCoord3f(xb, 0, 0);
    glVertex2f(x2 - xoff, y1 - yoff);
    glTexCoord3f(-xb, -yb, 0);

    glVertex2f(x2, y1 + yoff);
    glTexCoord3f(xb, -yb, 0);
    glVertex2f(x2, y1 - yoff);
  glEnd;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord3f(-xb, -yb, 0);
    glVertex2f(x1 + yoff, y0);
    glTexCoord3f(xb, -yb, 0);
    glVertex2f(x1 - yoff, y0);
    glTexCoord3f(-xb, 0, 0);
    glVertex2f(x1 + yoff, y0 + yoff);
    glTexCoord3f(xb, 0, 0);
    glVertex2f(x1 - yoff, y0 + yoff);
    glTexCoord3f(-xb, 0, 0);

    glVertex2f(x1 + yoff, y2 - yoff);
    glTexCoord3f(xb, 0, 0);
    glVertex2f(x1 - yoff, y2 - yoff);
    glTexCoord3f(-xb, -yb, 0);

    glVertex2f(x1 + yoff, y2);
    glTexCoord3f(xb, -yb, 0);
    glVertex2f(x1 - yoff, y2);
  glEnd;

  glUseProgram(0);
end;

procedure GLUIPainter.drawDownArrow(aRect: Rect; Width: integer; fillColorId: integer; borderColorId: integer);
var
  offset: double;
  xb: double;
  yb: double;
  xoff: double;
  yoff: double;
  xoff2: double;
  yoff2: double;
  x0: double;
  x1: double;
  x2: double;
  y0: double;
  y1: double;
begin
  offset := sqrt(2) / 2;
  xb := Width;
  yb := Width;
  xoff := offset * xb;
  yoff := offset * yb;
  xoff2 := offset * xb * 2.0;
  yoff2 := offset * yb * 2.0;
  x0 := aRect.x + xoff2;
  x1 := aRect.x + aRect.w * 0.5;
  x2 := aRect.x + aRect.w - xoff2;
  y0 := aRect.y + aRect.h * 0.1 + yoff2;
  y1 := aRect.y + aRect.h * 0.6;

  glUseProgram(m_widgetProgram);
  glUniform4fv(m_fillColorUniform, 1, @s_colors[fillColorId]);
  glUniform4fv(m_borderColorUniform, 1, @s_colors[borderColorId]);
  glUniform2f(m_zonesUniform, (xb) - 1, (xb) - 2);

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord3f(-xb, -yb, 0);
    glVertex2f(x0, y1 + yoff2);
    glTexCoord3f(xb, -yb, 0);
    glVertex2f(x0 - xoff2, y1);
    glTexCoord3f(-xb, 0, 0);
    glVertex2f(x0 + xoff, y1 + yoff);
    glTexCoord3f(xb, 0, 0);
    glVertex2f(x0 - xoff, y1 - yoff);
    glTexCoord3f(-xb, 0, xb);

    glVertex2f(x1, y0 + yoff2);
    glTexCoord3f(xb, 0, xb);
    glVertex2f(x1 - xoff2, y0);
    glTexCoord3f(xb, 2 * yb, xb);

    glVertex2f(x1, y0 - yoff2);
  glEnd;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord3f(xb, -yb, 0);
    glVertex2f(x2 + xoff2, y1);
    glTexCoord3f(-xb, -yb, 0);
    glVertex2f(x2, y1 + yoff2);
    glTexCoord3f(xb, 0, xb);

    glVertex2f(x2 + xoff, y1 - yoff);
    glTexCoord3f(-xb, 0, xb);
    glVertex2f(x2 - xoff, y1 + yoff);
    glTexCoord3f(xb, 0, xb);

    glVertex2f(x1 + xoff2, y0);
    glTexCoord3f(-xb, 0, xb);
    glVertex2f(x1, y0 + yoff2);
    glTexCoord3f(xb, 2 * yb, xb);

    glVertex2f(x1, y0 - yoff2);
  glEnd;

  glUseProgram(0);
end;

procedure GLUIPainter.drawUpArrow(aRect: Rect; Width: integer; fillColorId: integer; borderColorId: integer);
var
  offset: double;
  xb: double;
  yb: double;
  xoff: double;
  yoff: double;
  xoff2: double;
  yoff2: double;
  x0: double;
  x1: double;
  x2: double;
  y0: double;
  y1: double;
begin
  offset := sqrt(2) / 2.0;
  xb := Width;
  yb := Width;
  xoff := offset * xb;
  yoff := -offset * yb;
  xoff2 := offset * xb * 2.0;
  yoff2 := -offset * yb * 2.0;
  x0 := aRect.x + xoff2;
  x1 := aRect.x + aRect.w * 0.5;
  x2 := aRect.x + aRect.w - xoff2;
  y0 := aRect.y + aRect.h * 0.9 + yoff2;
  y1 := aRect.y + aRect.h * 0.4;

  glUseProgram(m_widgetProgram);
  glUniform4fv(m_fillColorUniform, 1, @s_colors[fillColorId]);
  glUniform4fv(m_borderColorUniform, 1, @s_colors[borderColorId]);
  glUniform2f(m_zonesUniform, (xb) - 1, (xb) - 2);

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord3f(-xb, -yb, 0);
    glVertex2f(x0, y1 + yoff2);
    glTexCoord3f(xb, -yb, 0);
    glVertex2f(x0 - xoff2, y1);
    glTexCoord3f(-xb, 0, 0);
    glVertex2f(x0 + xoff, y1 + yoff);
    glTexCoord3f(xb, 0, 0);
    glVertex2f(x0 - xoff, y1 - yoff);
    glTexCoord3f(-xb, 0, xb);

    glVertex2f(x1, y0 + yoff2);
    glTexCoord3f(xb, 0, xb);
    glVertex2f(x1 - xoff2, y0);
    glTexCoord3f(xb, 2 * yb, xb);

    glVertex2f(x1, y0 - yoff2);
  glEnd;

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord3f(xb, -yb, 0);
    glVertex2f(x2 + xoff2, y1);
    glTexCoord3f(-xb, -yb, 0);
    glVertex2f(x2, y1 + yoff2);
    glTexCoord3f(xb, 0, xb);

    glVertex2f(x2 + xoff, y1 - yoff);
    glTexCoord3f(-xb, 0, xb);
    glVertex2f(x2 - xoff, y1 + yoff);
    glTexCoord3f(xb, 0, xb);

    glVertex2f(x1 + xoff2, y0);
    glTexCoord3f(-xb, 0, xb);
    glVertex2f(x1, y0 + yoff2);
    glTexCoord3f(xb, 2 * yb, xb);

    glVertex2f(x1, y0 - yoff2);
  glEnd;

  glUseProgram(0);
end;

procedure GLUIPainter.init;
var
  vShader: GLuint = 0;
  fShader: GLuint = 0;
  fShaderTex: GLuint = 0;
begin
  if m_widgetProgram = 0 then
  begin
    vShader := CompileGLSLShader(GL_VERTEX_SHADER, cWidgetVSSource);
    if vShader = 0 then
      writeln(stderr, 'Vertex shader compile failed');

    fShader := CompileGLSLShader(GL_FRAGMENT_SHADER, cWidgetFSSource);
    if fShader = 0 then
      writeln(stderr, 'Fragment shader compile failed');

    m_widgetProgram := LinkGLSLProgram(vShader, fShader);

    m_fillColorUniform := glGetUniformLocation(m_widgetProgram, 'fillColor');
    m_borderColorUniform := glGetUniformLocation(m_widgetProgram, 'borderColor');
    m_zonesUniform := glGetUniformLocation(m_widgetProgram, 'zones');

    if m_textureViewProgram = 0 then
    begin
      fShaderTex := CompileGLSLShader(GL_FRAGMENT_SHADER, cTexViewWidgetFSSource);
      if fShaderTex = 0 then
        writeln(stderr, 'Fragment shader compile failed');

      m_textureViewProgram := LinkGLSLProgram(vShader, fShaderTex);
      m_texMipLevelUniform := glGetUniformLocation(m_textureViewProgram, 'mipLevel');
      m_texelScaleUniform := glGetUniformLocation(m_textureViewProgram, 'texelScale');
      m_texelOffsetUniform := glGetUniformLocation(m_textureViewProgram, 'texelOffset');
      m_texelSwizzlingUniform := glGetUniformLocation(m_textureViewProgram, 'texelSwizzling');
    end;
  end;

  if m_setupStateDL = 0 then
  begin
    m_setupStateDL := glGenLists(1);
    glNewList(m_setupStateDL, GL_COMPILE);
    begin
      //Cache previous state
      glPushAttrib(GL_STENCIL_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);

      //fill mode always
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

      //Stencil / Depth buffer and test disabled
      glDisable(GL_STENCIL_TEST);
      glStencilMask(0);
      glDisable(GL_DEPTH_TEST);
      glDepthMask(GL_FALSE);

      // Blend on for alpha
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

      // Color active
      glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

      // Modelview is identity
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix;
      glLoadIdentity;
    end;
    glEndList;
  end;

  if m_restoreStateDL = 0 then
  begin
    m_restoreStateDL := glGenLists(1);
    glNewList(m_restoreStateDL, GL_COMPILE);
    begin
      // Restore state.
      glPopAttrib;

      // Restore matrices.
      glMatrixMode(GL_PROJECTION);
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix;
    end;
    glEndList;
  end;
end;

end.

