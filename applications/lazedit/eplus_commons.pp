unit EPlus_Commons;

{
  EPlus: a text editor with built-in features for HTML editing and
  Syntax Highlighting for several text formats
  (html, xml, css, javascript, pascal, c/c++, perl, python, php, bat, ini, diff)

  Copyright (C) 2011 by Bart Broersma & Flying Sheep Inc.
  http://home.tiscali.nl/~knmg0017/software.htm

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

{ *********************************************************************

  Common type and variable definitions for the EPlus program
  Some of these can be configurable at runtime

*********************************************************************** }

interface



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

  //Filter indexes
  fiEftFirst = Ord(Low(TEditorFileType)) + 1;
  fiHtml = Ord(eftHtml);
  fiXml = Ord(eftXml);
  fiCss = Ord(eftCss);
  fiJS = Ord(eftJS);
  fiFpc = Ord(eftFpc);
  fiLfm = Ord(eftLfm);
  fiC = Ord(eftC);
  fiPy = Ord(eftPy);
  fiPhp = Ord(eftPhp);
  fiPerl = Ord(eftPerl);
  fiUnixShell = Ord(eftUnixShell);
  fiBat = Ord(eftBat);
  fiDiff = Ord(eftDiff);
  fiIni = Ord(eftIni);
  fiPo = Ord(eftPo);
  fiEftLast = Ord(High(TEditorFileType));

  fiText = fiEftLast + 1;
  fiAll = fiEftLast + 2;

  DefaultFiletypeMaskList: TFileTypeMaskList = ('.txt',//eftNone  (no need to guess syntax for .txt files)
         '.htm;.html;.xhtml;.xhtm;' + HtmlTemplateExt, //eftHtml
         '.xml;.adfx',                                 //eftXml
         '.css;' + CssTemplateExt,                     //eftCss
         '.js;' + JavaTemplateExt,                     //eftJava
         '.pp;.pas;.inc;.lpr;.dpr;.lrs;.lpk;.dpk',     //eftFpc
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

implementation

end.

