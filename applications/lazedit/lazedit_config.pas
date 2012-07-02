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

unit lazedit_config;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, EditorPageControl, lazedit_constants, lazedit_translations,
  Forms, IniFiles, LCLProc;

type
  //globale type for all configurable options
  TLazEditOptions = record
    EditorOptions: TEditorOptions;
    MainForm: record
      Position: TRect; //used as Left, Top, Widht, Height
      InitialDir: String;
      ToolbarHTML: Boolean;
    end;
    HtmlCharMapDlg: record
      Position: TPoint;
    end;
    NewHtmlDlg: record
      Position: TPoint;
      InitialDir: String;
    end;
    AnchorDlg: record
      Position: TPoint;
    end;
    PictureDlg: record
      Position: TPoint;
      InitialDir: String;
      DefaultFolderPrefix: String;
    end;
    TableDlg: record
      Position: TPoint;
    end;
    FileTypeMaskList: TFileTypeMaskList;
    TemplateMaskList: String;
    RecentFiles: Array[0..MruEntries - 1] of String;
    Translation: TLanguageIds;
  end;

function LoadOptions(var Options: TLazEditOptions; FileName: String): Boolean;
function SaveOptions(const Options: TLazEditOptions; FileName: String): Boolean;
function GetDefaultIniNameOnly: String;
function GetDefaultIniDir: String;


implementation

const
  scGeneral = 'General';
    idTranslation = 'Translation';
  scMainForm = 'MainForm';
    idTop = 'Top';
    idLeft = 'Left';
    idWidth = 'Width';
    idHeight = 'Height';
    idInitialDir = 'InitialDir';  //Last opened folder
    idToolbarHTML = 'HTML toolbar';
  scHtmlCharMapDlg = 'CharMapDlg';
  scNewHtmlDlg = 'NewHtmlDlg';
  scAnchorDlg = 'AnchorDlg';
  scPictureDlg = 'PictureDlg';
    idDefaultFolderPrefix = 'DefaultFolderPrefix';  //prefix for <.. src=SomeFolder/name.jpg ...>
  scTableDlg = 'TableDlg';
  scFileTypes = 'FileTypes';
  scEditor = 'Editor';
    idFontSize = 'FontSize';
    idFontName = 'FontName';
  scRecentFiles = 'RecentFiles';
    idFilePrefix = 'File_%d';


function LoadOptions(var Options: TLazEditOptions; FileName: String): Boolean;
var
  Ini: TIniFile;
  ftIndex: TEditorFileType;
  S: String;
  i: Integer;
  Lang: LongInt;
begin
  Result := False;
  try
    Ini := Nil;
    Ini := TIniFile.Create(FileName);
    try
      Result := True; //Return true if opening the Ini file is successfull

      Options.EditorOptions.FontName := Ini.ReadString(scEditor, idFontName, '');
      Options.EditorOptions.FontSize := Ini.ReadInteger(scEditor, idFontSize, 0);

      Options.MainForm.Position.Left := Ini.ReadInteger(scMainForm, idLeft, -1);
      Options.MainForm.Position.Top := Ini.ReadInteger(scMainForm, idTop, -1);
      Options.MainForm.Position.Right := Ini.ReadInteger(scMainForm, idWidth, -1);
      Options.MainForm.Position.Bottom := Ini.ReadInteger(scMainForm, idHeight, -1);
      Options.MainForm.InitialDir := Ini.ReadString(scMainForm, idInitialDir, '');
      Options.MainForm.ToolbarHTML := Ini.ReadBool(scMainForm, idToolbarHTML, False);

      //Dialogs
      Options.NewHtmlDlg.Position.y := Ini.ReadInteger(scNewHtmlDlg, idTop, -1);
      Options.NewHtmlDlg.Position.x := Ini.ReadInteger(scNewHtmlDlg, idLeft, -1);
      Options.NewHtmlDlg.InitialDir := Ini.ReadString(scNewHtmlDlg, idInitialDir, '');

      Options.AnchorDlg.Position.y := Ini.ReadInteger(scAnchorDlg, idTop, -1);
      Options.AnchorDlg.Position.x := Ini.ReadInteger(scAnchorDlg, idLeft, -1);

      Options.HtmlCharMapDlg.Position.y := Ini.ReadInteger(scHtmlCharmapDlg, idTop, -1);
      Options.HtmlCharMapDlg.Position.x := Ini.ReadInteger(scHtmlCharmapDlg, idLeft, -1);

      Options.PictureDlg.Position.y := Ini.ReadInteger(scPictureDlg, idTop, -1);
      Options.PictureDlg.Position.x := Ini.ReadInteger(scPictureDlg, idLeft, -1);
      Options.PictureDlg.InitialDir := Ini.ReadString(scPictureDlg, idInitialDir, '');
      Options.PictureDlg.DefaultFolderPrefix := Ini.ReadString(scPictureDlg, idDefaultFolderPrefix, '');

      Options.TableDlg.Position.y := Ini.ReadInteger(scTableDlg, idTop, -1);
      Options.TableDlg.Position.x := Ini.ReadInteger(scTableDlg, idLeft, -1);


      //Recent files
      for i := 0 to MruEntries -1 do
      begin
        Options.RecentFiles[i] := Ini.ReadString(scRecentFiles, Format(idFilePrefix,[i]), '');
      end;


      //Filters
      for ftIndex := Low(TEditorFileType) to High(TEditorFileType) do
      begin
        S := Ini.ReadString(scFileTypes, eftNames[ftIndex], '');
        if (S <> '') then Options.FileTypeMaskList[ftIndex] := S;
      end;

      // Translation and other general
      Lang := Ini.ReadInteger(scGeneral, idTranslation, 0);
      if (Lang >= Ord(Low(TLanguageIds))) and (Lang <= Ord(High(TLanguageIds))) then
        Options.Translation := TLanguageIds(Lang)
      else
        Options.Translation := lidEnglish;

    finally
      Ini.Free;
      Ini := Nil;
    end;
  except
    //Only handle read errors here!
    on E: EStreamError do
    begin
      Result := false;
      if Assigned(Ini) then Ini.Free;
      Exit;
    end;
  end;
end;

function SaveOptions(const Options: TLazEditOptions; FileName: String): Boolean;
var
  Ini: TIniFile;
  ftIndex: TEditorFileType;
  i: Integer;
begin
  Result := False;
  try
    Ini := Nil;
    Ini := TIniFile.Create(FileName);
    try
      Ini.CacheUpdates := True;

      Ini.WriteString(scEditor, idFontName, Options.EditorOptions.FontName);
      Ini.WriteInteger(scEditor, idFontSize, Options.EditorOptions.FontSize);

      Ini.WriteInteger(scMainForm, idLeft, Options.MainForm.Position.Left);
      Ini.WriteInteger(scMainForm, idTop, Options.MainForm.Position.Top);
      Ini.WriteInteger(scMainForm, idWidth, Options.MainForm.Position.Right);
      Ini.WriteInteger(scMainForm, idHeight, Options.MainForm.Position.Bottom);
      Ini.WriteString(scMainForm, idInitialDir, Options.MainForm.InitialDir);
      Ini.WriteBool(scMainForm, idToolbarHTML, Options.MainForm.ToolbarHTML);

      //Dialogs
      Ini.WriteInteger(scNewHtmlDlg, idTop, Options.NewHtmlDlg.Position.y);
      Ini.WriteInteger(scNewHtmlDlg, idLeft, Options.NewHtmlDlg.Position.x);
      Ini.WriteString(scNewHtmlDlg, idInitialDir, Options.NewHtmlDlg.InitialDir);

      Ini.WriteInteger(scAnchorDlg, idTop, Options.AnchorDlg.Position.y);
      Ini.WriteInteger(scAnchorDlg, idLeft, Options.AnchorDlg.Position.x);

      Ini.WriteInteger(scHtmlCharMapDlg, idTop, Options.HtmlCharMapDlg.Position.y);
      Ini.WriteInteger(scHtmlCharMapDlg, idLeft, Options.HtmlCharMapDlg.Position.x);

      Ini.WriteInteger(scPictureDlg, idTop, Options.PictureDlg.Position.y);
      Ini.WriteInteger(scPictureDlg, idLeft, Options.PictureDlg.Position.x);
      Ini.WriteString(scPictureDlg, idInitialDir, Options.PictureDlg.InitialDir);
      Ini.WriteString(scPictureDlg, idDefaultFolderPrefix, Options.PictureDlg.DefaultFolderPrefix);

      Ini.WriteInteger(scTableDlg, idTop, Options.TableDlg.Position.y);
      Ini.WriteInteger(scTableDlg, idLeft, Options.TableDlg.Position.x);

      //Recent Files
      for i := 0 to MruEntries - 1 do
      begin
        Ini.WriteString(scRecentFiles, Format(idFilePrefix,[i]), Options.RecentFiles[i]);
      end;


      //Filters
      for ftIndex := Low(TEditorFileType) to High(TEditorFileType) do
      begin
        Ini.WriteString(scFileTypes, eftNames[ftIndex], Options.FileTypeMaskList[ftIndex]);
      end;

      // Translation and other general
      Ini.WriteInteger(scGeneral, idTranslation, Ord(Options.Translation));

      try
        Ini.UpdateFile;
        Result := True;
      except
        Result := false;
      end;

    finally
      Ini.Free;
      Ini := Nil;
    end;
  except
    //Only handle write errors
    on E: EStreamError do
    begin
      debugln('Error creating/writing configfile: ',FileName);
      Result := false;
      if Assigned(Ini) then Ini.Free;
      Exit;
    end;
  end;
end;

function GetDefaultIniNameOnly: String;
begin
  Result :=  AppName + '.ini';//IniExt;
end;

function GetDefaultIniDir: String;
begin
  Result := ExcludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
end;

end.

