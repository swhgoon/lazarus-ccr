{ HtmlCode unit

  Copyright (C) 2012 by Bart Broersma

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit HtmlCode;

{$mode objfpc}{$H+}

interface

uses {CopyLeft,} Classes, SysUtils, Graphics, fpimage{, LCL_Misc};

type
  THtmlDocType = (DocType_Html_401_Strict, DocType_Html_401_TR, DocType_Html_5, DocType_None);
  THtmlDocTypeStrings = Array[THtmlDocType] of String;

const



   SDocType_Html401_Strict = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"' + LineEnding +
                            '       "http://www.w3.org/TR/html4/strict.dtd">';
   SDocType_Html401_TR = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"'+ LineEnding +
                        '"http://www.w3.org/TR/html4/loose.dtd">';

   SDocType_Html_5 = '<!DOCTYPE html>';
   SDocType_None = '';
   HtmlDocTypeStrings: THtmlDocTypeStrings = (SDocType_Html401_Strict, SDocType_Html401_TR, SDocType_Html_5, SDocType_None);

   html_start = '<html>';
   html_end = '</html>';
   head_start = '<head>';
   head_end = '</head>';

   meta_content_charset_utf8 = '<meta http-equiv="content-type" content="text/html; charset=utf-8">';
   meta_content_style_type = '<meta http-equiv="content-style-type" content="text/css">';
   meta_content_script_type = '<meta http-equiv="content-script-type" content="text/javascript">';
   meta_name_author = '<meta name="author" content="%s">';
   meta_name_generator = '<meta name="generator" content="%s">';

   title_start = '<title>';
   title_end = '</title>';


   anchor_end = '</a>';

   bold_start= '<b>';
   bold_end= '</b>';
   italic_start= '<i>';
   italic_end= '</i>';
   underline_start= '<span style="text-decoration: underline;">';
   underline_end= '</span>';
   sub_start= '<sub>';
   sub_end= '</sub>';
   sup_start= '<sup>';
   sup_end= '</sup>';
   emphasis_start = '<em>';
   emphasis_end = '</em>';
   Strong_start = '<strong>';
   Strong_end = '</strong>';
   Code_start = '<code>';
   Code_end = '</code>';
   Quote_start = '<q>';
   Quote_end = '</q>';
   BlockQuote_start = '<blockquote>';
   BlockQuote_end = '</blockquote>';
   PRE_start = '<pre>';
   PRE_end = '</pre>';

   Comment_start= '<!-- ';
   Comment_end= '//  -->';

   Script_start = '<script type="text/JavaScript">' + LineEnding ;
   Script_end = LineEnding + '</script>';
   Style_start = '<style type="text/css">' ;
   Style_end = '</style>';

   Font_end='</span>';

   CenterAlign_start= '<div style="text-align: center;">';
   CenterAlign_end= '</div>';
   LeftAlign_start= '<div style="text-align: left;">';
   LeftAlign_end= '</div>';
   RightAlign_start= '<div style="text-align: right;">';
   RightAlign_end= '</div>';
   JustifyAlign_start = '<div style="text-align: justify;">';
   JustifyAlign_end= '</div>';

   Row_start = '<tr>';
   Row_end = '</tr>';
   Col_start = '<td>';
   Col_end = '</td>';
   ColHeading_start = '<th>';
   ColHeading_end = '</th>';

   Table_end = '</table>';


   UnnumberedList_start= '<ul>';    UnnumberedList_end= '</ul>';
   NumberedList_start= '<ol>';    NumberedList_end= '</ol>';
   ListItem_start= '<li>';    ListItem_end= '</li>';
   WordList_start= '<dl>';    WordList_end= '</dl>';
   WordTerm_start= '<dt>';    WordTerm_end= '</dt>';
   WordDef_start= '<dd>';    WordDef_end= '</dd>';



   Red_start     = '<span style="color: red;">';
   Green_start   = '<span style="color: green;">';
   Blue_start    = '<span style="color: blue;">';
   Purple_start  = '<span style="color: purple;">';
   Orange_start  = '<span style="color: orange;">';
   Yellow_start  = '<span style="color: yellow;">';
   Black_start   = '<span style="color: black;">';
   White_start   = '<span style="color: white;">';
   Gray_start    = '<span style="color: gray;">';
   Maroon_start  = '<span style="color: maroon;">';
   Olive_start   = '<span style="color: olive;">';
   Fuchsia_start = '<span style="color: fuchsia;">';
   Lime_start    = '<span style="color: lime;">';
   Navy_start    = '<span style="color: navy;">';
   Aqua_start    = '<span style="color: aqua;">';
   Teal_start    = '<span style="color: teal;">';
   Silver_start  = '<span style="color: silver;">';


   Paragraph_start= '<p>';    Paragraph_end= '</p>';
   Div_start = '<div>';  Div_end = '</div>';
   Span_start = '<span>';  Span_end = '</span>';
   LineBreak = '<br>' ;

   CopyRight         = '&copy;';
   Registered        = '&reg;';
   Trademark         = '&#8482;';
   NbSpace           = '&nbsp;';
   Lesser            = '&lt;';
   Greater           = '&gt;';
   Ampersand         = '&amp;';


function Favicon(const ASource: String): String;
function StyleSheet(const ASource: String): String;
function StyleSheet(const ASource, AMedia: String): String;
function Table_Start(const AID, ACLass, ASummary: String): String;
function CreateTable(const AID, ACLass, ASummary: String; const ColCount, RowCount: Integer): String;
function TabbedTextToHtmlTableContent(const AText: String): String;
function Img(const ASource, AId, AClass, AFloatStyle, AWidth, AHeight, AAlt, ATitle: String): String;
function CreateHtml(const ADocType: THtmlDocType; const AAuthor, ATitle, AFavicon, AStyleSheet, AInlineStyle: String;
                    const AGeneratorName: String = ''): String;

function HtmlToColor(const Value: String): TColor;
function ColorToHtml(const AColor: TColor): String;

Procedure DoHtmlDirSeparators (Var FileName : String);
function UrlEscape(const Url: String): String;


implementation

const
   HtmlDirectorySeparator = '/';

function Favicon(const ASource: String): String;
begin
  Result := Format('<link rel="icon" href="%s" type="image/x-icon">',[ASource]) + LineEnding +
  Format('<link rel="shortcut icon" href="%s" type="image/x-icon">',[ASource]);
end;

function StyleSheet(const ASource: String): String;
begin
  Result := Format('<link rel="stylesheet" href="%s" type="text/css">',[ASource]);
end;

function StyleSheet(const ASource, AMedia: String): String;
begin
  Result := Format('<link rel="stylesheet" href="%s" type="text/css" media="%s">',[ASource, AMedia]);
end;



function Table_Start(const AID, ACLass, ASummary: String): String;
begin
  Result := '<table';
  if AID <> '' then Result := Result + ' id="' + AID + '"';
  if AClass <> '' then Result := Result + ' class="' + AClass + '"';
  if ASummary <> '' then Result := Result + ' summary="' + ASummary + '"';
  Result := Result + '>';
end;

function CreateTable(const AID, ACLass, ASummary: String; const ColCount,
  RowCount: Integer): String;
const
  Ident = #32#32;
var
  Row, Col: Integer;
begin
  Result := Table_Start(AID, ACLass, ASummary) + LineEnding;
  for Row := 1 to RowCount do
  begin
    Result := Result + row_start + LineEnding;
    for Col := 1 to ColCount do
    begin
      if (Row = 1) then
        Result := Result + Ident + ColHeading_start + LineEnding
      else
        Result := Result + Ident + Col_start + LineEnding;

      if (Row = 1) then
        Result := Result + Ident + ColHeading_end + LineEnding
      else
        Result := Result + Ident + Col_end + LineEnding;
    end;
    Result := Result + row_end + LineEnding;
  end;
  Result := Result +  table_end;
end;

function TabbedTextToHtmlTableContent(const AText: String): String;
const
  Tab = #9;
var
  SL: TStringList;
  i, p: Integer;
  Current, S: String;
begin
  Result := '';
  //workaround for bug in ClipBoard.GetAsText
  if (Length(AText) = 0) then exit;
  SL := TStringList.Create;
  try
    p := Pos(#0, AText);
    if p > 0 then
    begin
      SL.Text := Copy(AText,1,p-1);
    end
    else SL.Text := AText;
    for i := 0 to SL.Count - 1 do
    begin
      Current := SL.Strings[i];
      //debugln('Current = "',Current,'" (',DbgS(Length(Current)),')');
      if (Length({Trim}(Current)) > 0) then
      begin
        Result := Result + row_start + LineEnding;
        repeat
          p := Pos(Tab, Current);
          if p = 0 then
            S := Current
          else
            S := Copy(Current,1,p-1);
          if (p > 0) then System.Delete(Current,1,p);
          //debugln('S = "',S,'" p = ',DbgS(p));
          Result := Result + '  ' + col_start + S + col_end + LineEnding;
        until (p = 0) or (Length(Current) = 0);
        Result := Result + row_end + LineEnding;
      end;//Length(Current) > 0
    end;//for
  finally
    Sl.Free;
  end;
end;


function Img(const ASource, AId, AClass, AFloatStyle, AWidth, AHeight, AAlt,
  ATitle: String): String;
var
  _Alt, _Title: String;
begin
  //<img src="_lazarus.gif" ID="id" class="class" style="float: left;" width="24" height="24" ALT="alt" title="title">
  _Alt := AAlt;
  _Title := ATitle;
  if _Alt = '' then _Alt := _Title;
  if (_Title = '') and (_Alt <> '') then _Title := _Alt;
  Result := '<img src="' + ASource + '"';
  if AId <> '' then Result := Result + ' id="' + AId + '"';
  if AClass <> '' then Result := Result + ' class="' + AClass + '"';
  if AFloatStyle <> '' then Result := Result + ' style="float:' + AFloatStyle + ';"';
  if AWidth <> '' then Result := Result + ' width="' + AWidth + '"';
  if AHeight <> '' then Result := Result + ' height="' + AHeight + '"';
  if _Alt <> '' then Result := Result + ' alt="' + _Alt + '"';
  if _Title <> '' then Result := Result + ' title ="' + _Title + '"';
  Result := Result + '>';
end;

function CreateHtml(const ADocType: THtmlDocType; const AAuthor, ATitle, AFavicon, AStyleSheet, AInlineStyle: String;
                    const AGeneratorName: String = ''): String;
begin
  Result := HtmlDocTypeStrings[ADocType];
  if (Result <> '') then Result := Result + LineEnding;
  Result := Result + Html_Start + LineEnding;
  Result := Result + Head_Start + LineEnding + LineEnding;


  Result := Result + meta_content_charset_utf8 + LineEnding;
  Result := Result + meta_content_style_type + LineEnding;
  Result := Result + meta_content_script_type + LineEnding;
  if (AAuthor <> '') then Result := Result + Format(meta_name_author,[AAuthor]) + LineEnding;
  if (AGeneratorName <> '') then
    Result := Result + Format(meta_name_generator,[AGeneratorName]) + LineEnding;
  Result := Result + LineEnding;

  Result := Result + Title_Start + ATitle + Title_End + LineEnding + LineEnding;

{
<link rel="icon" href="pictures/favicon.ico" type="image/x-icon">
<link rel="shortcut icon" href="pictures/favicon.ico" type="image/x-icon">

<link rel="stylesheet" href="standaard.css" type="text/css">
<link rel="stylesheet" href="print.css" type="text/css" media="print">
}

  if (AFavicon <> '') then Result := Result + Favicon(AFavicon) + LineEnding;
  if (AStyleSheet <> '') then Result := Result + AStyleSheet + LineEnding;

  if (AInlineStyle <> '') then Result := Result + LineEnding + AInlineStyle + LineEnding + LineEnding;

  Result := Result + LineEnding + LineEnding + Head_End + LineEnding;
  Result := Result + LineEnding + Html_End;
end;

function HtmlToColor(const Value: String): TColor;
begin
  //ToDo
  Result := 0;
end;

{

}

function ColorToHtml(const AColor: TColor): String;
const
  //uses unthemed original values for this
  clBlack   = TColor($000000);
  clMaroon  = TColor($000080);
  clGreen   = TColor($008000);
  clOlive   = TColor($008080);
  clNavy    = TColor($800000);
  clPurple  = TColor($800080);
  clTeal    = TColor($808000);
  clGray    = TColor($808080);
  clSilver  = TColor($C0C0C0);
  clRed     = TColor($0000FF);
  clLime    = TColor($00FF00);
  clYellow  = TColor($00FFFF);
  clBlue    = TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua    = TColor($FFFF00);
  clLtGray  = TColor($C0C0C0); // clSilver alias
  clDkGray  = TColor($808080); // clGray alias
  clWhite   = TColor($FFFFFF);
var
  RgbTriple: TFPColor;
begin
  case AColor of
    clRed:     Result := 'red';
    clGreen:   Result := 'green';
    clBlue:    Result := 'blue';
    clPurple:  Result := 'purple';
    clYellow:  Result := 'yellow';
    clBlack:   Result := 'black';
    clWhite:   Result := 'white';
    clGray:    Result := 'gray';
    clMaroon:  Result := 'maroon';
    clFuchsia: Result := 'fuchsia';
    clLime:    Result := 'lime';
    clNavy:    Result := 'navy';
    clAqua:    Result := 'aqua';
    clTeal:    Result := 'teal';
    clSilver:  Result := 'silver';
  else
    begin
      RgbTriple := TColorToFPColor(AColor);
      Result := '#'+ IntToHex(RgbTriple.Red div $FF,2) + IntToHex(RgbTriple.Green div $FF,2) + IntToHex(RgbTriple.Blue div $FF,2);
    end;
  end;
end;

Procedure DoHtmlDirSeparators (Var FileName : String);
Var I : longint;
begin
  For I:=1 to Length(FileName) do
    If FileName[I] in AllowDirectorySeparators then
      FileName[i] := HtmlDirectorySeparator;
end;

function UrlEscape(const Url: String): String;
//see: http://en.wikipedia.org/wiki/Percent-encoding
begin
  Result := StringReplace(Url, #32, '%20', [rfReplaceAll]);
end;

end.
