{ EditorPageControl unit

  Copyright (C) 2012 by Bart Broersma & Flying Sheep Inc.
  http://home.tiscali.nl/~knmg0017/

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
unit EditorPageControl;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, {StdCtrls,} ComCtrls, Types, LCLProc, LclType, StrUtils,
  FileUtil, Forms {for Application object needed in TEditorPageControl.ClosePage()}, Menus,
  SynEdit, {SynMemo,} SynEditTypes, SynEditHighlighter,
  SynGutter, {SynGutterMarks,} SynGutterChanges, SynGutterLineNumber, {SynGutterCodeFolding,}
  SynHighlighterPas,  SynHighlighterCpp, SynHighlighterPerl, SynHighlighterHTML, SynHighlighterXML,
  SynHighlighterLFM, SynHighlighterDiff, SynHighlighterCss, SynHighlighterPHP,
  SynHighlighterPython, SynHighlighterBat, SynHighlighterIni, SynHighlighterJava,
  SynHighlighterUnixShellScript, SynHighLighterPo,
  SynEditMouseCmds, SynEditKeyCmds,
  EPlus_Commons;





type

  TEditorPageControl = class;  //forward declaration
  TEditor = class;

  { TEditorOptions }

  TEditorOptions = record
    FontName: String;
    FontSize: Integer;
  end;

  TEditorCharsetChangedEvent = procedure(Sender: TEditor; const OldCharSet, NewCharSet: String; const LineNr: Integer) of object;

  TEditor = class(TSynEdit)
  private
    FFileName: String;
    FPage: TTabSheet;  //the designated parent and owner of a TEditor
    FEditorPageControl: TEditorPageControl;
    FEditorOptions: TEditorOptions;
    FFileType: TEditorFileType;
    FFileMaskList: TFileTypeMaskList;
    FAutoFiletypeDetection: Boolean;
    FNoFileTypeChangeOnSave: Boolean;
    FOnCharsetChanged: TEditorCharsetChangedEvent;
    procedure SetAutoFiletypeDetection(AValue: Boolean);
    procedure SetEditorOptions(AValue: TEditorOptions);
    procedure UpdateEditorOptions(Sender: TObject);
    procedure SetFileName(const Utf8Fn: String; const UpdateFileType: Boolean);
    function ExtToFileType(const Ext: String): TEditorFileType;
    function GuessFileType: TEditorFileType;
    function GuessSyntaxFromString(S: String): TEditorFileType;
    procedure SetFileType(AFileType: TEditorFileType);
    procedure AdjustEncoding;
    procedure SetDefaultGutterParts;
    procedure SetDefaultMouseActions;
  protected
    procedure DoCharsetChanged(const OldCharset, NewCharset: String; const LineNr: Integer);
    property OnCharsetChanged: TEditorCharsetChangedEvent read FOnCharsetChanged write FOnCharsetChanged;
    property FileMaskList: TFileTypeMaskList read FFileMaskList write FFileMaskList;
    property EditorOptions: TEditorOptions read FEditorOptions write SetEditorOptions;
  public
    procedure SetDefaultKeyStrokes; override; // is public in parent class
    procedure LoadFromFileAnsi(const AnsiFn: String; const AsTemplate: Boolean = False);
    procedure LoadFromFileUtf8(const Utf8Fn: String; const AsTemplate: Boolean = False);
    procedure SaveToFileAnsi(const AnsiFn: String);
    procedure SaveToFileUtf8(const Utf8Fn: String);
    procedure SetHighlighterByFileType(const AFileType: TEditorFileType; const Permanent: Boolean = False);
    procedure MarkSelection(const Pre, Post: String);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property FileName: String read FFileName;  //in UTF-8 encoding
    property FileType: TEditorFileType read FFileType write SetFileType;
    property AutoFileTypeDetection: Boolean read FAutoFiletypeDetection write SetAutoFiletypeDetection default True;

  end;

const
  scFileName = scReadOnly; //Use this for FileName changes
  scAll = [scCaretX,scCaretY,scModified,scInsertMode,scFileName];

type

  { TEditorPageControl }

  TCloseEditorEvent = procedure(Sender: TTabSheet; var Cancel: Boolean) of object;

  THighLighterArray = Array[TEditorFileType] of TSynCustomHighlighter;

  TEditorPageControl = class(TPageControl)
  private
    FCounter: Cardinal;
    FHighLighters: THighlighterArray;
    FFileTypeMaskList: TFileTypeMaskList;
    FEditorOptions: TEditorOptions;
    FEditorPopupMenu: TPopupMenu;
    FOnStatusChange: TStatusChangeEvent;
    FOnBeforeCloseEditor: TCloseEditorEvent;
    FOnEditorCharsetChanged: TEditorCharsetChangedEvent;
    function GetCurrentEditor: TEditor;
    function GetHighLighter(Index: TEditorFileType): TSynCustomHighlighter;
    function GetFileTypeMaskLists(Index: TEditorFileType): String;
    procedure SetEditorOptions(AValue: TEditorOptions);
    function GetEditorOptions: TEditorOptions;
    procedure SetFileTypeMaskLists(Index: TEditorFileType; AValue: string);
    procedure SetPopupMenu(AValue: TPopupMenu);
    procedure UpdateEditorFileMasks;
    procedure UpdateEditorOptions(Sender: TObject);
    procedure UpdateEditorPopupMenu;
    //Called internally by the editor, by AddPage, DoChange and if PageCount becomes 0
    procedure InternalEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure InternalEditorCharsetChange(Sender: TEditor; const OldCharSet, NewCharSet: String; const LineNr: Integer);
  protected
    procedure DoChange; override;

    //procedure RemovePage(Index: Integer); override;
    //procedure DoCloseTabClicked(APage: TCustomPage); override;
    property HighLighters[Index: TEditorFileType]: TSynCustomHighlighter read GetHighLighter;
    //property FileTypeMaskList: TFileTypeMaskList read FFileTypeMaskList write SetFiletypeMaskList;
  public
    function AddPage: TEditor;
    function ClosePage(Index: Integer): Boolean;
    function EditorAtPage(const Index: Integer): TEditor;
    function EditorAtPage(const APage: TTabSheet): TEditor;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property CurrentEditor: TEditor read GetCurrentEditor;
    //setting FileTypeMaskLists[eftNone] results in any file with such an extension treated as eftNone (and no therefore Highlighting)
    property FileTypeMaskLists[Index: TEditorFileType]: string read GetFileTypeMaskLists write SetFileTypeMaskLists;
    property EditorOptions: TEditorOptions read GetEditorOptions write SetEditorOptions;
    property OnStatusChange: TStatusChangeEvent read FonStatusChange write FOnStatusChange;
    property OnBeforeCloseEditor: TCloseEditorEvent read FOnBeforeCloseEditor write FOnBeforeCloseEditor;
    property OnEditorCharsetChange: TEditorCharsetChangedEvent read FOnEditorCharsetChanged write FOnEditorCharsetChanged;
    property EditorPopUpmenu: TPopupMenu read FEditorPopupMenu write SetPopupMenu;
  end;



const
  EmptyStr = '';
  NoName = 'Naamloos';

implementation


function DefaultEditorOptions: TEditorOptions;
begin
  Result.FontName := '';
  Result.FontSize := 0;
end;

function EditorOptionsAreDifferent(const New, Old: TEditorOptions): Boolean;
begin
  Result := (New.FontName <> Old.FontName) or
            (New.FontSize <> Old.FontSize);
end;

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



{TEditor}

procedure TEditor.SetFileName(const Utf8Fn: String; const UpdateFileType: Boolean);
begin
  //debugln('TEditor.SetFileName: Utf8Fn = ',Utf8ToSys(Utf8Fn));
  if (FFileName = Utf8Fn) then Exit;
  FFileName := Utf8Fn;
  if Assigned(FPage) then
  begin
    if (Utf8Fn <> EmptyStr) then
      FPage.Caption := ExtractFileName(Utf8Fn)
    else
      FPage.Caption := NoName;
    //Debugln('TEditor.SetFileName: setting FPageCaption to ',FPage.Caption);
  end;
  //debugln('TEditor.SetFileName: calling DoOnStatusChange(scAll)');
  //Unless you change ReadOnly, the scFileName will be removed from Changes in TSynEdit.DoOnStatuschange
  ReadOnly := True;
  DoOnStatusChange(scAll);
  ReadOnly := false;
  //debugln('TEditor.SetFileName: setting FileType');
  if UpdateFileType then FileType := GuessFileType
  else FileType := eftNone;
  //debugln('TEditor.SetFileName: End');
end;

procedure TEditor.SetAutoFiletypeDetection(AValue: Boolean);
begin
  if FAutoFiletypeDetection = AValue then exit;
  FAutoFiletypeDetection := AValue;
  if AValue then FNoFileTypeChangeOnSave := False;
end;

procedure TEditor.SetEditorOptions(AValue: TEditorOptions);
begin
  if EditorOptionsAreDifferent(AValue, FEditorOptions) then
  begin
    FEditorOptions := AValue;
    UpdateEditorOptions(Self);
  end;
end;

procedure TEditor.UpdateEditorOptions(Sender: TObject);
begin
  //ToDo
  if FEditorOptions.FontName <> '' then
  begin
    Font.Name := FEditorOptions.FontName;
    Font.Pitch := fpFixed;
  end;
  if (FEditorOptions.FontSize <> Font.Size) and (FEditorOptions.FontSize <> 0) then Font.Size := FEditorOptions.FontSize;
end;

function TEditor.ExtToFileType(const Ext: String): TEditorFileType;
var
  Index: TEditorFileType;
begin
  //DebugLn('TEditor.ExtToFileType: Ext = "',Ext,'"');
  Result := eftNone;
  if (Length(Ext) = 0) then Exit;
  for Index := Low(TEditorFileType) to High(TEditorFileType) do
  begin
    if FindInMaskList(Ext, FileMaskList[Index]) then
    begin
      Result := Index;
      Exit;
    end;
  end;
  //Debugln('TEditor.ExtToFileType: Result = ',eftNames[Result]);
  //DebugLn('TEditor.ExtToFileType: End');
end;

function TEditor.GuessSyntaxFromString(S: String): TEditorFileType;
const
  Shebang = '#!';
begin
  //Debugln('TEditor.GuessSyntaxFromString, S = "',S,'"');
  Result := eftNone;
  S := TrimLeft(S);
  if (Pos(Shebang,S) = 1) and ((Pos('/bin/bash',S) > 0) or (Pos('/bin/sh',S) > 0)) then Result := eftUnixShell
  else if (Pos(Shebang,S) = 1) and (Pos('/perl',S) > 0) then Result := eftPerl
  else if Pos('<!DOCTYPE HTML',UpperCase(S)) = 1 then Result := eftHtml
  else if Pos('<?XML VERSION',S) = 1 then Result := eftXml
  else if Pos('#INCLUDE <',UpperCase(S)) = 1 then Result := eftC
  {
  else if Pos('',S) = 0 then Result := eft
  else if Pos('',S) = 0 then Result := eft
  else if Pos('',S) = 0 then Result := eft
  };
  //Debugln('TEditor.GuessSyntaxFromString: Result = ',eftNames[Result]);
  //Debugln('TEditor.GuessSyntaxFromString End');
end;

function TEditor.GuessFileType: TEditorFileType;
const
  MaxLines = 50; //Max number of lines to scan
var
  Ext, S: String;
  i: Integer;
begin
  //DebugLn('TEditor.GuessFileType');
  Ext := ExtractFileExt(FFileName);
  //DebugLn('TEditor.GuessFileType: Ext = "',Ext,'"');
  if (Ext = FFileName) then Ext := '';  //filenames starting with a period (on linux mostly)
  Result := ExtToFileType(Ext);
  if (Result = eftNone) and (FindInMaskList(Ext, FileMaskList[eftNone])) then
  begin
    //Debugln('TEditor.GuessFileType: explicitely defined eftNone extension found: ',Ext);
  end;
  if (Result = eftNone) and not (FindInMaskList(Ext, FileMaskList[eftNone])) then
  begin
    i := 0;
    while (i < Lines.Count) and (i < MaxLines) do
    begin
      S := Lines[i];
      Result := GuessSyntaxFromString(S);
      if (Result <> eftNone) then Break;
      Inc(i);
    end;
  end;
  //DebugLn('TEditor.GuessFileType End');
end;

procedure TEditor.SetFileType(AFileType: TEditorFileType);
begin
  //DebugLn('TEditor.SetFileType A');
  //DebugLn(Format('FileName = %s, FileType is set to %s',[ExtractFileName(FFileName),eftNames[AFileType]]));
  if AFileType <> FFileType then
  begin
    FFileType := AFileType;
    SetHighlighterByFileType(AFileType);
  end;
  //DebugLn('TEditor.SetFileType End');
end;

procedure TEditor.AdjustEncoding;
//Search first 50 lines for character encosing metat tag and change to UTF-8 if necessary
//the tag is in the form of: <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
//This only works if the meta tag is the first meta-tag on the line
var
  LineNr: Integer;
  S: String;
  p, CharsetStart, CharsetEnd: SizeInt;
  OldCharset: String;
const
  MaxLinesToScan = 5000;
begin
  //DebugLn('TEditor.AdjustEncoding A');
  LineNr := 0;
  while (LineNr < Lines.Count)  and (LineNr < MaxLinesToScan) do
  begin
    //DbgOut('[',Dbgs(LineNr),'] ');
    S := {Utf8}UpperCase(Lines[LineNr]);  //no need for utf8, this meta-tag can only contain lower ascii
    p := Pos('<META ',S);
    if (p > 0) then
    begin
      //DbgOut('meta ');
      //check for closing tag, but do not store value in p, we need the current value of p later on
      if (PosEx('>',S, p + 1) > 0) then
      begin
        //DbgOut('">" ');
        p := PosEx('"CONTENT-TYPE"',S, p + 1);
        if (p > 0) then
        begin
          //DbgOut('content-type ');
          p := PosEx('CONTENT',S, p + 1);
          if (p > 0) then
          begin
            //DbgOut('content ');
            p := PosEx('CHARSET',S, p + 1);
            if (p > 0) then
            begin
              //DbgOut('charset ');
              p := PosEx('=',S, p + 1);
              if (p > 0) then
              begin
                //DbgOut('=');
                CharsetStart := p + 1;
                p := PosEx('"',S, CharsetStart);
                if p = 0 then p := PosEx(';',S, CharSetStart);
                if (p > 0) then
                begin
                  CharsetEnd := p;
                  OldCharset := Copy(S,CharsetStart,CharSetEnd - CharSetStart);
                  //DebugLn('Current charset is ',OldCharSet,' (LineNr = ',DbgS(LineNr),')');
                  if CompareText(Trim(OldCharSet),'utf-8') <> 0 then
                  begin
                    //DebugLn('Changing charset to utf-8');
                    TextBetweenPoints[Point(CharsetStart,LineNr+1),Point(CharSetEnd,LineNr+1)] := 'utf-8';
                    DoCharsetChanged(OldCharSet, 'utf-8', LineNr);
                  end;
                  Break;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    Inc(LineNr);
    //DebugLn;
  end;
  //DebugLn('TEditor.AdjustEncoding End');
end;

procedure TEditor.SetDefaultKeyStrokes;

  procedure AddKey(const ACmd: TSynEditorCommand; const AKey: word;
     const AShift: TShiftState);
  begin
    with KeyStrokes.Add do
    begin
      Key := AKey;
      Shift := AShift;
      Command := ACmd;
    end;
  end;

begin
  KeyStrokes.Clear;

  AddKey(ecUp, VK_UP, []);
  AddKey(ecSelUp, VK_UP, [ssShift]);
  AddKey(ecScrollUp, VK_UP, [ssCtrl]);
  AddKey(ecDown, VK_DOWN, []);
  AddKey(ecSelDown, VK_DOWN, [ssShift]);
  AddKey(ecScrollDown, VK_DOWN, [ssCtrl]);
  AddKey(ecLeft, VK_LEFT, []);
  AddKey(ecSelLeft, VK_LEFT, [ssShift]);
  AddKey(ecWordLeft, VK_LEFT, [ssCtrl]);
  AddKey(ecSelWordLeft, VK_LEFT, [ssShift,ssCtrl]);
  AddKey(ecRight, VK_RIGHT, []);
  AddKey(ecSelRight, VK_RIGHT, [ssShift]);
  AddKey(ecWordRight, VK_RIGHT, [ssCtrl]);
  AddKey(ecSelWordRight, VK_RIGHT, [ssShift,ssCtrl]);
  AddKey(ecPageDown, VK_NEXT, []);
  AddKey(ecSelPageDown, VK_NEXT, [ssShift]);
  AddKey(ecPageBottom, VK_NEXT, [ssCtrl]);
  AddKey(ecSelPageBottom, VK_NEXT, [ssShift,ssCtrl]);
  AddKey(ecPageUp, VK_PRIOR, []);
  AddKey(ecSelPageUp, VK_PRIOR, [ssShift]);
  AddKey(ecPageTop, VK_PRIOR, [ssCtrl]);
  AddKey(ecSelPageTop, VK_PRIOR, [ssShift,ssCtrl]);
  AddKey(ecLineStart, VK_HOME, []);
  AddKey(ecSelLineStart, VK_HOME, [ssShift]);
  AddKey(ecEditorTop, VK_HOME, [ssCtrl]);
  AddKey(ecSelEditorTop, VK_HOME, [ssShift,ssCtrl]);
  AddKey(ecLineEnd, VK_END, []);
  AddKey(ecSelLineEnd, VK_END, [ssShift]);
  AddKey(ecEditorBottom, VK_END, [ssCtrl]);
  AddKey(ecSelEditorBottom, VK_END, [ssShift,ssCtrl]);
  AddKey(ecToggleMode, VK_INSERT, []);
  AddKey(ecCopy, VK_INSERT, [ssCtrl]);
  AddKey(ecPaste, VK_INSERT, [ssShift]);
  AddKey(ecDeleteChar, VK_DELETE, []);
  AddKey(ecCut, VK_DELETE, [ssShift]);
  AddKey(ecDeleteLastChar, VK_BACK, []);
  AddKey(ecDeleteLastChar, VK_BACK, [ssShift]);                                 //jr 2000-09-23
  AddKey(ecDeleteLastWord, VK_BACK, [ssCtrl]);
  AddKey(ecUndo, VK_BACK, [ssAlt]);
  AddKey(ecRedo, VK_BACK, [ssAlt,ssShift]);
  AddKey(ecLineBreak, VK_RETURN, []);
  AddKey(ecSelectAll, ord('A'), [ssCtrl]);
  AddKey(ecCopy, ord('C'), [ssCtrl]);
  AddKey(ecBlockIndent, ord('I'), [ssCtrl,ssShift]);
  AddKey(ecLineBreak, ord('M'), [ssCtrl]);
  //AddKey(ecInsertLine, ord('N'), [ssCtrl]);
  AddKey(ecDeleteWord, ord('T'), [ssCtrl]);
  AddKey(ecBlockUnindent, ord('U'), [ssCtrl,ssShift]);
  AddKey(ecPaste, ord('V'), [ssCtrl]);
  AddKey(ecCut, ord('X'), [ssCtrl]);
  AddKey(ecDeleteLine, ord('Y'), [ssCtrl]);
  AddKey(ecDeleteEOL, ord('Y'), [ssCtrl,ssShift]);
  AddKey(ecUndo, ord('Z'), [ssCtrl]);
  AddKey(ecRedo, ord('Z'), [ssCtrl,ssShift]);
  //AddKey(ecGotoMarker0, ord('0'), [ssCtrl]);
  //AddKey(ecGotoMarker1, ord('1'), [ssCtrl]);
  //AddKey(ecGotoMarker2, ord('2'), [ssCtrl]);
  //AddKey(ecGotoMarker3, ord('3'), [ssCtrl]);
  //AddKey(ecGotoMarker4, ord('4'), [ssCtrl]);
  //AddKey(ecGotoMarker5, ord('5'), [ssCtrl]);
  //AddKey(ecGotoMarker6, ord('6'), [ssCtrl]);
  //AddKey(ecGotoMarker7, ord('7'), [ssCtrl]);
  //AddKey(ecGotoMarker8, ord('8'), [ssCtrl]);
  //AddKey(ecGotoMarker9, ord('9'), [ssCtrl]);
  //AddKey(ecSetMarker0, ord('0'), [ssCtrl,ssShift]);
  //AddKey(ecSetMarker1, ord('1'), [ssCtrl,ssShift]);
  //AddKey(ecSetMarker2, ord('2'), [ssCtrl,ssShift]);
  //AddKey(ecSetMarker3, ord('3'), [ssCtrl,ssShift]);
  //AddKey(ecSetMarker4, ord('4'), [ssCtrl,ssShift]);
  //AddKey(ecSetMarker5, ord('5'), [ssCtrl,ssShift]);
  //AddKey(ecSetMarker6, ord('6'), [ssCtrl,ssShift]);
  //AddKey(ecSetMarker7, ord('7'), [ssCtrl,ssShift]);
  //AddKey(ecSetMarker8, ord('8'), [ssCtrl,ssShift]);
  //AddKey(ecSetMarker9, ord('9'), [ssCtrl,ssShift]);
  //AddKey(ecFoldLevel1, ord('1'), [ssAlt,ssShift]);
  //AddKey(ecFoldLevel2, ord('2'), [ssAlt,ssShift]);
  //AddKey(ecFoldLevel3, ord('3'), [ssAlt,ssShift]);
  //AddKey(ecFoldLevel4, ord('4'), [ssAlt,ssShift]);
  //AddKey(ecFoldLevel5, ord('5'), [ssAlt,ssShift]);
  //AddKey(ecFoldLevel6, ord('6'), [ssAlt,ssShift]);
  //AddKey(ecFoldLevel7, ord('7'), [ssAlt,ssShift]);
  //AddKey(ecFoldLevel8, ord('8'), [ssAlt,ssShift]);
  //AddKey(ecFoldLevel9, ord('9'), [ssAlt,ssShift]);
  //AddKey(ecFoldLevel0, ord('0'), [ssAlt,ssShift]);
  //AddKey(ecFoldCurrent, ord('-'), [ssAlt,ssShift]);
  //AddKey(ecUnFoldCurrent, ord('+'), [ssAlt,ssShift]);
  AddKey(EcToggleMarkupWord, ord('M'), [ssAlt]);
  AddKey(ecNormalSelect, ord('N'), [ssAlt,ssShift]); //changed Ctrl -> Alt
  AddKey(ecColumnSelect, ord('C'), [ssAlt,ssShift]); //changed Ctrl -> Alt
  AddKey(ecLineSelect, ord('L'), [ssCtrl,ssShift]);
  AddKey(ecTab, VK_TAB, []);
  AddKey(ecShiftTab, VK_TAB, [ssShift]);
  AddKey(ecMatchBracket, ord('B'), [ssCtrl,ssShift]);

  AddKey(ecColSelUp, VK_UP,    [ssAlt, ssShift]);
  AddKey(ecColSelDown, VK_DOWN,  [ssAlt, ssShift]);
  AddKey(ecColSelLeft, VK_LEFT, [ssAlt, ssShift]);
  AddKey(ecColSelRight, VK_RIGHT, [ssAlt, ssShift]);
  AddKey(ecColSelPageDown, VK_NEXT, [ssAlt, ssShift]);
  AddKey(ecColSelPageBottom, VK_NEXT, [ssAlt, ssShift,ssCtrl]);
  AddKey(ecColSelPageUp, VK_PRIOR, [ssAlt, ssShift]);
  AddKey(ecColSelPageTop, VK_PRIOR, [ssAlt, ssShift,ssCtrl]);
  AddKey(ecColSelLineStart, VK_HOME, [ssAlt, ssShift]);
  AddKey(ecColSelLineEnd, VK_END, [ssAlt, ssShift]);
  AddKey(ecColSelEditorTop, VK_HOME, [ssAlt, ssShift,ssCtrl]);
  AddKey(ecColSelEditorBottom, VK_END, [ssAlt, ssShift,ssCtrl]);
end;

procedure TEditor.SetDefaultGutterParts;
begin
  Gutter.Parts.Clear;
  with TSynGutterLineNumber.Create(Gutter.Parts) do
    Name := 'SynGutterLineNumber1';
  with TSynGutterChanges.Create(Gutter.Parts) do
    Name := 'SynGutterChanges1';
  with TSynGutterSeparator.Create(Gutter.Parts) do
    Name := 'SynGutterSeparator1';
end;

procedure TEditor.SetDefaultMouseActions;
var
  i: Integer;
  MA: TSynEditMouseAction;
begin
  //Remove SourceLink MouseAction
  for i := 0 to MouseActions.Count - 1 do
  begin
    MA := MouseActions.Items[i];
    if (MA.Command = emcMouseLink) then
    begin
      //DebugLn('Deleting MouseAction:',MA.DisplayName);
      MouseActions.Delete(i);
      Break;
    end;
  end;
end;

procedure TEditor.DoCharsetChanged(const OldCharset, NewCharset: String; const LineNr: Integer);
begin
  if Assigned(FOnCharsetChanged) then FOnCharsetChanged(Self, OldCharset, NewCharset, LineNr);
end;


//SynEdit deafults to UTF8 filenames when using LoadFromFile !!
procedure TEditor.LoadFromFileAnsi(const AnsiFn: String; const AsTemplate: Boolean = False);
begin
  LoadFromFileUtf8(SysToUtf8(AnsiFn), AsTemplate);
end;

procedure TEditor.LoadFromFileUtf8(const Utf8Fn: String; const AsTemplate: Boolean = False);
begin
  //DebugLn('TEditor.LoadFromFile: Utf8Fn = ',Utf8ToSys(Utf8Fn));
  try
    Lines.LoadFromFile(Utf8Fn);
    Modified := False;
    FNoFileTypeChangeOnSave := False;
    SetFileName(Utf8Fn, AutoFileTypeDetection);
    if AsTemplate then
    begin//blank out internal filename and update caption
      FFileName := EmptyStr;
      if Assigned(FPage) then FPage.Caption := NoName;
      DoOnStatusChange([scFileName]);
    end;
    //DebugLn('TEditor.LoadFromFile: FileType = ',eftNames[FileType]);
    if (FileType = eftHtml) then AdjustEncoding;
  except
    SetFileName(EmptyStr, AutoFileTypeDetection);
    Modified := True;
    Raise;
  end;
  //DebugLn('TEditor.LoadFromFile End');
end;


procedure TEditor.SaveToFileAnsi(const AnsiFn: String);
begin
  SaveToFileAnsi(SysToUtf8(AnsiFn));
end;

procedure TEditor.SaveToFileUtf8(const Utf8Fn: String);
begin
  try
    Lines.SaveToFile(Utf8Fn);
    Modified := False;
    SetFileName(Utf8Fn, AutoFileTypeDetection and (not FNoFileTypeChangeOnSave));
  except
    Modified := True;
    SetFileName(EmptyStr, AutoFileTypeDetection and (not FNoFileTypeChangeOnSave));
    Raise;
  end;
end;

procedure TEditor.SetHighlighterByFileType(const AFileType: TEditorFileType; const Permanent: Boolean = False);
begin
  if Assigned(FEditorPageControl) then
  begin
    Highlighter := FEditorPageControl.HighLighters[AFileType];
    if Permanent then FNoFileTypeChangeOnSave := True;
  end;
end;

procedure TEditor.MarkSelection(const Pre, Post: String);
var
  SLen: Integer;
  OldSelMode: TSynSelectionMode;
begin
  {
  //This method makes no sense when we are in Colomn selection mode
  if SelAvail and (SelectionMode = smColumn) then Exit;
  }
  //this only works with SelectionMode := scNormal
  OldSelMode := SelectionMode;
  SelectionMode := smNormal;
  //Using SelEnd - SelStart doesn't work correctly when selecting across lines
  SLen := Utf8Length(Seltext); //SelStart - SelEnd;
  SelText := Pre + SelText + Post;
  //SelStart now is after Post, place it before the original selection
  SelStart := SelStart - Utf8Length(Post) - SLen;
  SelEnd := SelStart + SLen;
  SelectionMode :=  OldSelMode;
  SetFocus;
end;

constructor TEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FFileName := '';
  FPage := nil;
  FEditorPageControl := nil;
  FFileType := eftNone;
  FAutoFiletypeDetection := True;
  FNoFileTypeChangeOnSave := False;
  FEditorOptions := DefaultEditorOptions;
  SetDefaultKeyStrokes;
  SetDefaultMouseActions;
  SetDefaultGutterParts;
  //UpdateEditorOptions(FEditorOptions);
end;

destructor TEditor.Destroy;
begin
  //Debugln('TEditor.Destroy: ',Name);
  //self.OnStatusChange := nil;
  //self.OnChange := nil;
  //self.OnCharsetChanged := nil;
  inherited Destroy;
  //Debugln('TEditor.Destroy: After inherited Destroy');
end;

{ **************************************************************************** }

{ TEditorPageControl }

type
  THighlighterClassesArray = array[TEditorFileType] of TSynCustomHighlighterClass;
  //dummy class for eftNone, needed for eftHighlighterClassesArray
  TDummyHighlighter = Class(TSynCustomHighlighter)
  end;

//static array used to instatiate all suppoterd highlighters
//will give compile-time error if we change definition of TEditorFileType
const
  eftHighlighterClassesArray: THighlighterClassesArray = (TDummyHighlighter,
    TSynHtmlSyn, TSynXmlSyn, TSynCssSyn, TSynJavaSyn, TSynFreePascalSyn,
    TSynLfmSyn, TSynCppSyn, TSynPythonSyn, TSynPhpSyn, TSynPerlSyn,
    TSynUNIXShellScriptSyn, TSynBatSyn, TSynDiffSyn, TSynIniSyn, TSynPoSyn);

function TEditorPageControl.GetCurrentEditor: TEditor;
var
  Pg: TTabSheet;
begin
  //DebugLn('TEditorPageControl.GetCurrentEditor: PageCount = ',DbgS(PageCount),' ActivePageIndex = ',DbgS(ActivePageIndex));
  Result := nil;
  //ActivePageIndex = -1 when you remove the last (as in no more pages available) page
  //PageCount will still be 1 at this time
  if (PageCount > 0) and (ActivePageIndex >= 0) then
  begin
    Pg := Pages[ActivePageIndex];
    Result := EditorAtPage(Pg);
  end;
end;

function TEditorPageControl.GetHighLighter(Index: TEditorFileType): TSynCustomHighlighter;
begin
  Result := FHighLighters[Index];
end;

function TEditorPageControl.GetFileTypeMaskLists(Index: TEditorFileType): String;
begin
  Result := FFileTypeMaskList[Index];
end;

procedure TEditorPageControl.SetEditorOptions(AValue: TEditorOptions);
begin
  if EditorOptionsAreDifferent(AValue, FEditorOptions) then
  begin
    FEditorOptions := AValue;
    UpdateEditorOptions(Self);
  end;
end;

function TEditorPageControl.GetEditorOptions: TEditorOptions;
var
  Ed: TEditor;
begin
  Ed := CurrentEditor;
  if Assigned(Ed) then
  begin
    //Get actual fon used by editor and update internal field
    FEditorOptions.FontName := Ed.Font.Name;
    FEditorOptions.FontSize := Ed.Font.Size;
  end;
  Result := FEditorOptions;
end;

procedure TEditorPageControl.SetFileTypeMaskLists(Index: TEditorFileType; AValue: string);
begin
  if (AValue = FFileTypeMaskList[Index]) then Exit;
  AValue := Trim(AValue);
  if (Length(AValue) > 0) and (AValue[Length(AValue)] = ';') then System.Delete(AValue,Length(AValue),1);
  FFileTypeMaskList[Index] := AValue;
  UpdateEditorFileMasks;
end;

procedure TEditorPageControl.SetPopupMenu(AValue: TPopupMenu);
begin
  if FEditorPopupMenu = AValue then Exit;
  FEditorPopupMenu := AValue;
  UpdateEditorPopupMenu;
end;

procedure TEditorPageControl.UpdateEditorFileMasks;
var
  i: Integer;
  Ed: TEditor;
begin
  for i := 0 to PageCount - 1 do
  begin
    Ed := EditorAtPage(i);
    if Assigned(Ed) then Ed.FileMaskList := Self.FFileTypeMaskList;
  end;
end;

procedure TEditorPageControl.UpdateEditorOptions(Sender: TObject);
var
  i: Integer;
  Ed: TEditor;
begin
  for i := 0 to PageCount - 1 do
  begin
    Ed := EditorAtPage(i);
    if Assigned(Ed) then Ed.EditorOptions := Self.EditorOptions;
  end;
end;

procedure TEditorPageControl.UpdateEditorPopupMenu;
var
  i: Integer;
  Ed: TEditor;
begin
  for i := 0 to PageCount - 1 do
  begin
    Ed := EditorAtPage(i);
    if Assigned(Ed) then Ed.PopupMenu := Self.EditorPopUpmenu;
  end;
end;

procedure TEditorPageControl.InternalEditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if Assigned(FOnStatusChange) then FOnStatusChange(Sender, Changes);
end;

procedure TEditorPageControl.InternalEditorCharsetChange(Sender: TEditor;
  const OldCharSet, NewCharSet: String; const LineNr: Integer);
begin
  if Assigned(FOnEditorCharsetChanged) then FOnEditorCharsetChanged(Sender, OldCharSet, NewCharSet, LineNr);
end;


procedure TEditorPageControl.DoChange;
var
  Ed: TEditor;
begin
  inherited DoChange;
  Ed := GetCurrentEditor;
  InternalEditorStatusChange(Ed, [scCaretX,scCaretY,scModified,scInsertMode,scFileName]);
  if Assigned(Ed) then
  begin
    try
      Ed.SetFocus;
    except;
      debugln('TEditorPageControl.DoChange: could not set focus to current editor');
    end;
  end;
end;



{
procedure TEditorPageControl.RemovePage(Index: Integer);
begin
  DebugLn('TEditorPageControl.RemovePage: Index = ',DbgS(Index),' PageCount = ',DbgS(PageCount));
//  Pages[Index].DestroyComponents;
  DebugLn('TEditorPageControl.RemovePage A' );
  inherited RemovePage(Index);
  DebugLn('TEditorPageControl.RemovePage A' );
  if PageCount = 0 then InternalEditorStatusChange(nil, scAll);
  DebugLn('TEditorPageControl.RemovePage A' );
end;
}


constructor TEditorPageControl.Create(TheOwner: TComponent);
var
  Index: TEditorFileType;
begin
  inherited Create(TheOwner);
  FCounter := 0;
  FFileTypeMaskList := DefaultFileTypeMaskList;
  FEditorOptions := DefaultEditorOptions;

  //{$Hint Todo: use array of TSynCustomHighlighterClass, so new highlightrs are created if we add them in TEditorFileType}
  FHighLighters[eftNone] := nil;
  { The follwing method of instatiating has the advantage that,
    when we change TEditorFiltype (adding or removing highlighters) we get a compiletime error
    on the definition of eftHighlighterClassesArray automatically.
  }
  //Do not instantiate FHighLighters[Low(TeditorFileType)], because it must remain nil!!
  for Index := Succ(Low(TEditorFileType)) to High(TEditorFileType) do
  begin
    FHighLighters[Index] := eftHighlighterClassesArray[Index].Create(Self);
  end;

  {
  FHighLighters[eftHtml] := TSynHtmlSyn.Create(Self);
  FHighLighters[eftXml] := TSynXmlSyn.Create(Self);
  FHighLighters[eftCss] := TSynCssSyn.Create(Self);
  FHighLighters[eftJS] := TSynJavaSyn.Create(Self);
  FHighLighters[eftFpc] := TSynFreePascalSyn.Create(Self);
  FHighLighters[eftLfm] := TSynLfmSyn.Create(Self);
  FHighLighters[eftC] := TSynCppSyn.Create(Self);
  FHighLighters[eftPy] := TSynPythonSyn.Create(Self);
  FHighLighters[eftPhp] := TSynPhpSyn.Create(Self);
  FHighLighters[eftPerl] := TSynPerlSyn.Create(Self);
  FHighLighters[eftUnixShell] := TSynUNIXShellScriptSyn.Create(Self);
  FHighLighters[eftBat] := TSynBatSyn.Create(Self);
  FHighLighters[eftDiff] := TSynDiffSyn.Create(Self);
  FHighLighters[eftIni] := TSynIniSyn.Create(Self);
  FHighLighters[eftPo] := TSynPoSyn.Create(Self);
  }

  //these colors are clNone by default which results in no colors at all, just black and white
  (FHighlighters[eftFpc] as TSynFreePascalSyn).CommentAttri.Foreground := clFuchsia;
  (FHighlighters[eftFpc] as TSynFreePascalSyn).CommentAttri.Style := [fsItalic];
  (FHighlighters[eftFpc] as TSynFreePascalSyn).StringAttri.Foreground := clBlue;
  (FHighlighters[eftFpc] as TSynFreePascalSyn).DirectiveAttri.Foreground := clRed;
  (FHighlighters[eftFpc] as TSynFreePascalSyn).DirectiveAttri.Style := [fsBold];
  (FHighlighters[eftFpc] as TSynFreePascalSyn).SymbolAttri.Foreground := clRed;
  //Attribute for Html entities like '&amp;'
  (FHighLighters[eftHtml] as TSynHtmlSyn).AndAttri.Foreground := $000080FF;  //Orange
end;

destructor TEditorPageControl.Destroy;
begin
  inherited Destroy;
end;

function TEditorPageControl.AddPage: TEditor;
var
  TS: TTabSheet;
  E: TEditor;
  PgIdx: Integer;
  i, NrOfNoNames: Integer;
  Suffix: String;
begin
  Result := nil;
  Inc(FCounter);
  NrOfNoNames := 0;
  Suffix := '';
  for i := 0 to PageCount - 1 do
    if Pos(NoName, Pages[i].Caption) = 1 then Inc(NrOfNoNames);
  if NrOfNoNames > 0 then Suffix := ' [' + IntToStr(NrOfNoNames + 1) + ']';

  TS := TTabSheet.Create(Self);
  TS.Name := 'TS' + IntToStr(FCounter);
  TS.PageControl := Self;
  PgIdx := TS.PageIndex;

  TS.Caption := NoName + Suffix;

//  exit;//<-------------------------------------------------------------

  E := TEditor.Create(TS);
  Result := E;
  E.FileMaskList := FFileTypeMaskList;
  E.Parent := TS;
  E.FPage := TS;
  E.FEditorPageControl := Self;
  E.Align := alClient;



  E.SetFileName(EmptyStr, E.AutoFileTypeDetection);

  //E.Lines.Clear;
  E.Modified := False;
  E.OnStatusChange :=  @InternalEditorStatusChange;
  E.OnCharsetChanged := @InternalEditorCharsetChange;



  E.Gutter.AutoSize := True;
  //E.Gutter.DigitCount := 4;
  //E.Gutter.LeadingZeros := False;
  E.Gutter.LeftOffset := 0;
  E.Gutter.RightOffset := 2;
  //We only want LineNumber, Changes and Separator




  //E.Font.Name := 'Courier New';
  E.Font.Pitch := fpFixed;
  //E.Font.Size := EditorFontSize;
  //E.HighLighter := SynHTMLSyn;
  E.Options := [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoGroupUndo, eoScrollPastEol, eoEnhanceHomeKey,
                eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoBracketHighlight, eoTabsToSpaces, eoTrimTrailingSpaces];
  E.RightEdge := 0;
  E.RightEdgeColor := clWhite;
  E.TabWidth := 2;
  E.WantTabs := True;

  //DebugLn('TEditorPageContro.AddPage: before applying E.EditorOptions');
  //Debugln('  E.Font.Name = ',E.Font.Name,' E.Font.Size = ',DbgS(E.Font.Size));
  E.EditorOptions := Self.EditorOptions;
  //DebugLn('TEditorPageContro.AddPage: after applying E.EditorOptions');
  //Debugln('  E.Font.Name = ',E.Font.Name,' E.Font.Size = ',DbgS(E.Font.Size));

  E.PopUpMenu := FEditorPopUpMenu;
  {
  E.OnKeyPress := EditorKeyPress;
  E.OnKeyDown := EditorKeyDown;
  }
  E.Modified := False;
  InternalEditorStatusChange(E, [scCaretX,scCaretY,scModified,scInsertMode,scFileName]);

  ActivePage := Pages[PgIdx];
  try
    E.SetFocus;
  except
    on AnExc: Exception do
    Debugln('SetFocus failed for Editor "',E.Name,'" with message ',AnExc.Message);
  end;
end;

function TEditorPageControl.ClosePage(Index: Integer): Boolean;
var
  Cancel: Boolean;
  Pg: TTabSheet;
  Ed: TEditor;
begin
  //Debugln('TEditorPageControl.ClosePage: Index = ',DbgS(Index));
  Result := False;
  if (Index > PageCount - 1) then Exit;
  Pg := Pages[Index];
  Cancel := False;
  if Assigned(FOnBeforeCloseEditor) then FOnBeforeCloseEditor(Pg, Cancel);
  if Not Cancel then
  begin
    //If you do Pg.Free, then the Editor gets destroyed when it may be
    //processing an event (keystrokes for instance).
    //In order to prevent this we first have to queue the destruction of teh Editor
    //and then queue the destruction of the page, or else we may get an error from TEditor:
    //WARNING: TLCLComponent.Destroy with LCLRefCount>0. Hint: Maybe the component is processing an event?

    Ed := EditorAtPage(Pg);
    if Assigned(Ed) then
    begin
      Application.ReleaseComponent(Ed);
      //Application.ProcessMessages;
    end;

    Pg.PageControl := nil;
    Application.ReleaseComponent(Pg);
    //Application.ProcessMessages;
    //Pg.Free;
    Result := True;
    if PageCount = 0 then InternalEditorStatusChange(nil, scAll);
  end;
  //Debugln('TEditorPageControl.ClosePage End.');
end;

function TEditorPageControl.EditorAtPage(const Index: Integer): TEditor;
var
  Pg: TTabSheet;
begin
  Result := nil;
  if (Index <= PageCount - 1) then
  begin
    Pg := Pages[Index];
    Result := EditorAtPage(Pg);
  end;
end;

function TEditorPageControl.EditorAtPage(const APage: TTabSheet): TEditor;
var
  cc: Integer;
begin
  Result := nil;
  if not Assigned(APage) then Exit;
  for cc := 0 to APage.ComponentCount - 1 do
  begin
    if APage.Components[cc] is TEditor then
    begin
      Result := TEditor(APage.Components[cc]);
      Exit;
    end;
  end;
end;


end.

