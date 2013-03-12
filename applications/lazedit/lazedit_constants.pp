unit lazedit_constants;

{
  LazEdit: a text editor with built-in features for HTML editing and
  Syntax Highlighting for several text formats
  (html, xml, css, javascript, pascal, c/c++, perl, python, php, bat, ini, diff)

  Copyright (C) 2011, 2012 by Bart Broersma & Flying Sheep Inc. and
  Felipe Monteiro de Carvalho
  http://wiki.lazarus.freepascal.org/LazEdit

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


{$mode objfpc}{$H+}

interface

Uses Classes, SysUtils, LCLProc;

const
  AppName = 'LazEdit';
  AboutTitle = 'About ' + AppName;
  AppVersion = 'Version 2.0';
  CopyLeftStatement = 'Copyright (c) 2011, 2012 by Bart Broersma, FlyingSheep Inc. and'+ LineEnding +
                      'Felipe Monteiro de Carvalho';
  MetaGeneratorName = AppName + #32 + AppVersion;
  AuthorWebName = 'LazEdit homepage';
  AuthorWebUrl = 'http://wiki.lazarus.freepascal.org/LazEdit';
  {$I license.inc}
  LicenseText = LGPL_Text;
  LicenseUrl = LGPL_Url;
  LicenseName = 'Gnu LGPL';

Type

  TEditorFileType = (eftNone, eftHtml, eftXml, eftCSS, eftJS, eftFpc, eftLfm, eftC, eftPy, eftPhp,
                     eftPerl, eftUnixShell, eftBat, eftDiff, eftIni, eftPo);
  TFileTypeMaskList = array[TEditorFileType] of string;

const
  HtmlTemplateExt = '.htmlt;.hks;';
  CssTemplateExt = '.csst';
  JavaTemplateExt = '.jst';
  STemplate = 'Sjablonen';

  eftNames: array[TEditorFileType] of string = ('eftNone', 'eftHtml', 'eftXml', 'eftCSS',
                     'eftJS', 'eftFpc', 'eftLfm', 'eftC' , 'eftPy', 'eftPhp',
                     'eftPerl', 'eftUnixShell', 'eftBat', 'eftDiff', 'eftIni', 'eftPo');


  eftFilterNames: array[TEditorFileType] of string = ('', 'Html bestanden', 'XML bestanden', 'CSS bestanden',
                     'Javascript bestanden', 'Pascal bronbestanden', 'Lazarus en Delphi forms', 'C en C++ bronbestanden' ,
                     'Python bronbestanden', 'PHP bronbestanden',
                     'Perl bronbestanden', 'Unix shellscripts', 'Batch bestanden', 'Diff''s en patches',
                     'Configuratie bestanden', 'po language files');



  {$IFDEF WINDOWS}
  AllFilesMask = '*.*';
  {$ENDIF}
  FilterText = 'Tekst bestanden (*.txt)|*.txt';
  FilterAll = 'Alle bestanden ('+ AllFilesMask + ')|' + AllFilesMask;
  fiMaskText = '*.txt';
  fiNameText = 'Tekst bestanden';
  fiNameAll = 'Alle bestanden';
  fiNameAllSupported = 'Alle ondersteunde bestandstypen';

  //FilterIndexes all start with FilterAllSupported, then all individually supported filetypes followed by .txt, templates and *
  //Filter indexes
  fiAllSupported = 1; //FilterIndexes are 1-based!
  fiTemplates = 1; //for use with Open/SaveAsTemplate dialog only!

  fiEftFirst = Ord(Low(TEditorFileType)) + 2; //skip eftNone, add 1 for fiAllSupported
  fiEftLast = Ord(High(TEditorFileType)) + 1; //Add 1 for fiAllSupported

  fiText = fiEftLast + 1;
  fiTemplate = fiText + 1;
  fiAll = fiTemplate + 1;

  DefaultFiletypeMaskList: TFileTypeMaskList = ('.txt',//eftNone  (no need to guess syntax for .txt files)
         '.htm;.html;.xhtml;.xhtm;' + HtmlTemplateExt, //eftHtml
         '.xml;.adfx',                                 //eftXml
         '.css;' + CssTemplateExt,                     //eftCss
         '.js;' + JavaTemplateExt,                     //eftJava
         '.pp;.pas;.p;.inc;.lpr;.dpr;.lrs;.lpk;.dpk',  //eftFpc
         '.lfm;.dfm',                                  //eftLfm
         '.c;.cpp;.h;.hpp;.hh;.gcc;.cc;.c++',          //eftC
         '.py',                                        //eftPython
         '.php',                                       //eftPhp
         '.pl',                                        //eftPerl
         '.sh',                                        //eftUnixShell
         '.bat',                                       //eftBat
         '.diff;.patch',                               //eftDiff
         '.ini;.conf;.cfg',                            //eftIni
         '.po');                                       //eftPo

  DefaultTemplateMaskList =  HtmlTemplateExt + ';' + CssTemplateExt + ';' + JavaTemplateExt;

  MruEntries = 6;

function ExtToFileType(const Ext: String; AFileTypeMaskList: TFileTypeMaskList): TEditorFileType;
function FindInMaskList(const Ext, MaskList: String): Boolean;

implementation

function FindInMaskList(const Ext, MaskList: String): Boolean;
var
  SL: TStringList;
  i: Integer;
begin
  Result := False;
  if (Length(Ext) = 0) or (Length(MaskList) = 0) then Exit;
  SL := TStringList.Create;
  try
    SL.StrictDelimiter := True;
    SL.Delimiter := ';';
    SL.CaseSensitive := False;
    SL.Duplicates := dupAccept;
    SL.DelimitedText := Trim(MaskList);
    for i := 0 to SL.Count - 1 do
    begin
      if CompareText(Ext, SL.Strings[i]) = 0 then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    SL.Free;
  end;
end;

function ExtToFileType(const Ext: String; AFileTypeMaskList: TFileTypeMaskList): TEditorFileType;
var
  Index: TEditorFileType;
begin
  //DebugLn('ExtToFileType: Ext = "',Ext,'"');
  Result := eftNone;
  if (Length(Ext) = 0) then Exit;
  for Index := Low(TEditorFileType) to High(TEditorFileType) do
  begin
    if FindInMaskList(Ext, AFileTypeMaskList[Index]) then
    begin
      Result := Index;
      Exit;
    end;
  end;
  //Debugln('ExtToFileType: Result = ',eftNames[Result]);
  //DebugLn('ExtToFileType: End');
end;

end.

