unit main;

{
  EPlus: a text editor with built-in features for HTML editing and
  Syntax Highlighting for several text formats
  (html, xml, css, javascript, pascal, c/c++, perl, python, php, bat, ini, diff, po)

  Copyright (C) 2011, 2012 by Bart Broersma & Flying Sheep Inc.
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


{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}Win9xWSManager,{$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  LCLProc, Menus, ActnList, ClipBrd, LclIntf,
  LMessages, {for overridden IsShortCut}
  SynEdit, SynEditTypes,
  EditorPageControl,
  EPlus_Commons, EPlus_Config, HtmlCode, HtmlDialogs, lazedit_constants,
  lazedit_translations{
  MyFileUtils, Fcl_Misc, MruLists, MyGetOpt, ExtAbout, NlAutoTranslation};

type

  TIoResult = (ioSuccess, ioFail, ioCancel);

  { TEPlusForm }

  TEPlusForm = class(TForm)
    HtmlActionList: TActionList;
      acInsertAnchor: TAction;
      acInsertPicture: TAction;
      acInsertUList: TAction;
      acInsertNlist: TAction;
      acInsertListItem: TAction;
      acInsertTable: TAction;
      acInsertLineBreak: TAction;
      acInsertNbSpace: TAction;
      acInsertAmpersand: TAction;
      acInsertLesser: TAction;
      acInsertGreater: TAction;
      acInsertCopyright: TAction;
      acInsertTradeMark: TAction;
      acLayoutBold: TAction;
      acLayoutItalic: TAction;
      acLayoutUnderline: TAction;
      acLayoutSub: TAction;
      acLayoutSup: TAction;
      acLayoutEmphasis: TAction;
      acLayoutStrong: TAction;
      acLayoutAlignLeft: TAction;
      acLayoutAlignRight: TAction;
      acLayoutAlignCenter: TAction;
      acLayoutAlignJustify: TAction;
      acLayoutH1: TAction;
      acLayoutH2: TAction;
      acLayoutH3: TAction;
      acLayoutH4: TAction;
      acLayoutH5: TAction;
      acLayoutH6: TAction;
      acGroupingParagraph: TAction;
      acGroupingDiv: TAction;
      acGroupingSpan: TAction;
    FileActionList: TActionList;
      acFileOpen: TAction;
      acFileNewPlain: TAction;
      acFileNewFromTemplate: TAction;
      acFileSave: TAction;
      acFileSaveAll: TAction;
      acAbout: TAction;
    EditActionList: TActionList;
      acEditCopy: TAction;
      acEditPaste: TAction;
      acEditFind: TAction;
      acEditFindPrevious: TAction;
      acEditFindNext: TAction;
    FindDialog: TFindDialog;
    HtmlToolbarImageList: TImageList;
    MainToolbarImageList: TImageList;
    MainMenu: TMainMenu;
    mnuInsertSep2: TMenuItem;
    mnuLanguage: TMenuItem;
    mnuEnglish: TMenuItem;
    mnuDutch: TMenuItem;
    mnuPortuguese: TMenuItem;
    mnuTools: TMenuItem;
    mnuEditPasteTableContentTab: TMenuItem;
    mnuEditPasteSpecial: TMenuItem;
    mnuAbout: TMenuItem;
    mnuViewFont: TMenuItem;
    //File menu
    mnuFile: TMenuItem;
      mnuFileOpen: TMenuItem;
      mnuFileNewFromTemplate: TMenuItem;
      mnuFileNew: TMenuItem;
        mnuFileNewText: TMenuItem;
        mnuFileNewHtml: TMenuItem;
        mnuFileNewXml: TMenuItem;
        mnuFileNewCss: TMenuItem;
        mnuFileNewJS: TMenuItem;
        mnuFileNewFpc: TMenuItem;
        mnuFileNewC: TMenuItem;
        mnuFileNewPy: TMenuItem;
        mnuFileNewPhp: TMenuItem;
        mnuFileNewPerl: TMenuItem;
        mnuFileNewShellScript: TMenuItem;
        mnuFileNewBat: TMenuItem;
        mnuFileNewIni: TMenuItem;
      mnuFileSave: TMenuItem;
      mnuFileSaveAs: TMenuItem;
      mnuFileSaveAll: TMenuItem;
      mnuFileCloseCurrent: TMenuItem;
      mnuSep1: TMenuItem;
      mnuFileOpenInBrowser: TMenuItem;
      mnuSepAboveMru: TMenuItem;
      mnuFileMru0: TMenuItem;
      mnuFileMru1: TMenuItem;
      mnuFileMru2: TMenuItem;
      mnuFileMru3: TMenuItem;
      mnuFileMru4: TMenuItem;
      mnuFileMru5: TMenuItem;
      mnuSep3: TMenuItem;
      mnuFileCloseApp: TMenuItem;
    //Edit menu
    mnuEdit: TMenuItem;
      mnuEditUndo: TMenuItem;
      mnuEditRedo: TMenuItem;
      mnuSep11: TMenuItem;
      mnuEditCopy: TMenuItem;
      mnuEditCut: TMenuItem;
      mnuEditPaste: TMenuItem;
      mnuEditSelectAll: TMenuItem;
      mnuSep12: TMenuItem;
      mnuEditReplace: TMenuItem;
      mnuEditFindNext: TMenuItem;
      mnuEditFind: TMenuItem;
    //Insert menu
    mnuHTMLTools: TMenuItem;
    mnuInsertAnchor: TMenuItem;
    mnuInsertList: TMenuItem;
      mnuInsertUList: TMenuItem;
      mnuInsertNList: TMenuItem;
      mnuInsertWordList: TMenuItem;
      mnuInsetListItem: TMenuItem;
      mnuInsertWordTerm: TMenuItem;
      mnuInsertWordDefinition: TMenuItem;
    mnuInsertTable: TMenuItem;
      mnInsertNewTable: TMenuItem;
      mnuInsertTableCell: TMenuItem;
      mnuInsertTableRow: TMenuItem;
    mnuInsertPicture: TMenuItem;
    mnuInsertSpecialChars: TMenuItem;
    mnuInsertLineBreak: TMenuItem;
    mnuInsertSep1: TMenuItem;
    mnuInsertHtmlComment: TMenuItem;
    mnuInsertJS: TMenuItem;
    mnuInsertCssStyle: TMenuItem;
    //Layout menu
    mnuLayout: TMenuItem;
      mnuLayoutBold: TMenuItem;
      mnuLayoutAlignJustify: TMenuItem;
      mnuLayoutItalic: TMenuItem;
      mnuLayoutUnderline: TMenuItem;
      mnuLayoutSub: TMenuItem;
      mnuLayoutSup: TMenuItem;
      mnuLayoutEmphasis: TMenuItem;
      mnuLayoutStrong: TMenuItem;
      mnuLayoutHeadings: TMenuItem;
        mnuLayoutH1: TMenuItem;
        mnuLayoutH2: TMenuItem;
        mnuLayoutH3: TMenuItem;
        mnuLayoutH4: TMenuItem;
        mnuLayoutH5: TMenuItem;
        mnuLayoutH6: TMenuItem;
      mnuLayoutAlign: TMenuItem;
        mnuLayoutAlignLeft: TMenuItem;
        mnuLayoutAlignRight: TMenuItem;
        mnuLayoutAlignCenter: TMenuItem;
      mnuLayoutCode: TMenuItem;
      mnuLayoutQuote: TMenuItem;
      mnuLayoutBlockQuote: TMenuItem;
      mnuLayoutPreformatted: TMenuItem;
    //Grouping menu
    mnuGrouping: TMenuItem;
      mnuGroupingParagraph: TMenuItem;
      mnuGroupingDiv: TMenuItem;
      mnuGroupingSpan: TMenuItem;
    //View menu
    mnuView: TMenuItem;
      mnuViewFontsize: TMenuItem;
        mnuViewFontSizeUp: TMenuItem;
        mnuViewFontsizeDown: TMenuItem;
      mnuViewHighlighter: TMenuItem;
        //these menu items MUST have names that are built like this:
        //'mnuViewHL' + eftNames[SomeIndex]
        mnuViewHLeftNone: TMenuItem;
        mnuViewHLeftHtml: TMenuItem;
        mnuViewHLeftXml: TMenuItem;
        mnuViewHLeftCss: TMenuItem;
        mnuViewHLeftJS: TMenuItem;
        mnuViewHLeftFpc: TMenuItem;
        mnuViewHLeftLfm: TMenuItem;
        mnuViewHLeftC: TMenuItem;
        mnuViewHLeftPy: TMenuItem;
        mnuViewHLeftPhp: TMenuItem;
        mnuViewHLeftPerl: TMenuItem;
        mnuViewHLeftUNIXShell: TMenuItem;
        mnuViewHLeftBat: TMenuItem;
        mnuViewHLeftDiff: TMenuItem;
        mnuViewHLeftIni: TMenuItem;
        mnuViewHLeftPo: TMenuItem;
    //Popup menus
    //Popup menu for editor
    EditorPopupMenu: TPopupMenu;
      mnuEditPopupSelectAll: TMenuItem;
      mnuEditPopupPaste: TMenuItem;
      mnuEditPopupCut: TMenuItem;
      mnuEditPopupCopy: TMenuItem;
    //Dropdownmenu for HeadingBtn
    HeadingDropDownMenu: TPopupMenu;
      mnuPopupLayoutH6: TMenuItem;
      mnuPopupLayoutH5: TMenuItem;
      mnuPopupLayoutH4: TMenuItem;
      mnuPopupLayoutH3: TMenuItem;
      mnuPopupLayoutH2: TMenuItem;
      mnuPopupLayoutH1: TMenuItem;


    MainToolbar: TToolBar;
      NewFromTemplateBtn: TToolButton;
      NewPlainBtn: TToolButton;
      OpenBtn: TToolButton;
      ReplaceDialog: TReplaceDialog;
      SaveBtn: TToolButton;
      SaveAllBtn: TToolButton;
      tbSep1: TToolButton;
      CopyBtn: TToolButton;
      PasteBtn: TToolButton;
      tbSep2: TToolButton;
      FindBtn: TToolButton;
      tbSep3: TToolButton;
      InfoBtn: TToolButton;

    HtmlToolbar: TToolBar;
      AnchorBtn: TToolButton;
      ImageBtn: TToolButton;
      tbSep5: TToolButton;
      UListBtn: TToolButton;
      NListBtn: TToolButton;
      ListItemBtn: TToolButton;
      tbSep4: TToolButton;
      TableBtn: TToolButton;
      tbSep6: TToolButton;
      BoldBtn: TToolButton;
      ItalicBtn: TToolButton;
      UnderlineBtn: TToolButton;
      EmBtn: TToolButton;
      StrongBtn: TToolButton;
      SubBtn: TToolButton;
      SupBtn: TToolButton;
      tbSep7: TToolButton;
      HeadingBtn: TToolButton;
      tbSep8: TToolButton;
      LeftAlignBtn: TToolButton;
      RightAlignBtn: TToolButton;
      CenterAlignBtn: TToolButton;
      JustifyAlignBtn: TToolButton;
      tbSep9: TToolButton;
      ParaBtn: TToolButton;
      DivBtn: TToolButton;
      SpanBtn: TToolButton;

    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    FontDialog: TFontDialog;
    StatusBar: TStatusBar;

    //FileActions
    procedure acAboutExecute(Sender: TObject);
    procedure acFileNewFromTemplateExecute(Sender: TObject);
    procedure acFileNewPlainExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFileSaveAllExecute(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    //EditActions
    procedure acEditCopyExecute(Sender: TObject);
    procedure acEditFindExecute(Sender: TObject);
    procedure acEditPasteExecute(Sender: TObject);
    procedure acEditFindNextExecute(Sender: TObject);
    procedure acEditFindPreviousExecute(Sender: TObject);
    procedure acInsertAmpersandExecute(Sender: TObject);
    //HtmlActions
    procedure acInsertAnchorExecute(Sender: TObject);
    procedure acInsertCopyrightExecute(Sender: TObject);
    procedure acInsertGreaterExecute(Sender: TObject);
    procedure acInsertLesserExecute(Sender: TObject);
    procedure acInsertLineBreakExecute(Sender: TObject);
    procedure acInsertListItemExecute(Sender: TObject);
    procedure acInsertNbSpaceExecute(Sender: TObject);
    procedure acInsertOlistExecute(Sender: TObject);
    procedure acInsertPictureExecute(Sender: TObject);
    procedure acInsertTableExecute(Sender: TObject);
    procedure acInsertTradeMarkExecute(Sender: TObject);
    procedure acInsertUListExecute(Sender: TObject);
    procedure acLayoutAlignCenterExecute(Sender: TObject);
    procedure acLayoutAlignJustifyExecute(Sender: TObject);
    procedure acLayoutAlignLeftExecute(Sender: TObject);
    procedure acLayoutAlignRightExecute(Sender: TObject);
    procedure acLayoutBoldExecute(Sender: TObject);
    procedure acGroupingDivExecute(Sender: TObject);
    procedure acLayoutEmphasisExecute(Sender: TObject);
    procedure acLayoutH1Execute(Sender: TObject);
    procedure acLayoutH2Execute(Sender: TObject);
    procedure acLayoutH3Execute(Sender: TObject);
    procedure acLayoutH4Execute(Sender: TObject);
    procedure acLayoutH5Execute(Sender: TObject);
    procedure acLayoutH6Execute(Sender: TObject);
    procedure acLayoutItalicExecute(Sender: TObject);
    procedure acGroupingParagraphExecute(Sender: TObject);
    procedure acGroupingSpanExecute(Sender: TObject);
    procedure acLayoutStrongExecute(Sender: TObject);
    procedure acLayoutSubExecute(Sender: TObject);
    procedure acLayoutSupExecute(Sender: TObject);
    procedure acLayoutUnderlineExecute(Sender: TObject);

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure mnuLanguageChangeClick(Sender: TObject);

    //Main menu events
    procedure TopLevelMenuClick(Sender: TObject);
    //Menu File
    procedure mnuFileCloseAppClick(Sender: TObject);
    procedure mnuFileCloseCurrentClick(Sender: TObject);
    procedure mnuFileSaveAsClick(Sender: TObject);
    procedure mnuFileNewBatClick(Sender: TObject);
    procedure mnuFileNewHtmlClick(Sender: TObject);
    procedure mnuFileNewCClick(Sender: TObject);
    procedure mnuFileNewCssClick(Sender: TObject);
    procedure mnuFileNewFpcClick(Sender: TObject);
    procedure mnuFileNewIniClick(Sender: TObject);
    procedure mnuFileNewJSClick(Sender: TObject);
    procedure mnuFileNewPerlClick(Sender: TObject);
    procedure mnuFileNewPhpClick(Sender: TObject);
    procedure mnuFileNewPyClick(Sender: TObject);
    procedure mnuFileNewShellScriptClick(Sender: TObject);
    procedure mnuFileNewXmlClick(Sender: TObject);
    procedure mnuFileMruClick(Sender: TObject);
    procedure mnuFileOpenInBrowserClick(Sender: TObject);
    //Menu Edit
    procedure mnuEditUndoClick(Sender: TObject);
    procedure mnuEditRedoClick(Sender: TObject);
    procedure mnuEditCutClick(Sender: TObject);
    procedure mnuEditSelectAllClick(Sender: TObject);
    procedure mnuFindNextClick(Sender: TObject);
    procedure mnuEditReplaceClick(Sender: TObject);
    procedure mnuEditPasteTableContentTabClick(Sender: TObject);
    //Menu Insert
    procedure mnuInsertCssStyleClick(Sender: TObject);
    procedure mnuInsertHtmlCommentClick(Sender: TObject);
    procedure mnuInsertJSClick(Sender: TObject);
    procedure mnuInsertSpecialCharsClick(Sender: TObject);
    procedure mnuInsertTableCellClick(Sender: TObject);
    procedure mnuInsertTableRowClick(Sender: TObject);
    procedure mnuInsertWordDefinitionClick(Sender: TObject);
    procedure mnuInsertWordListClick(Sender: TObject);
    procedure mnuInsertWordTermClick(Sender: TObject);

    //Menu Layout
    procedure mnuLayoutBlockQuoteClick(Sender: TObject);
    procedure mnuLayoutCodeClick(Sender: TObject);
    procedure mnuLayoutPreformattedClick(Sender: TObject);
    procedure mnuLayoutQuoteClick(Sender: TObject);

    //Menu View
    procedure mnuViewFontClick(Sender: TObject);
    procedure mnuViewFontsizeDownClick(Sender: TObject);
    procedure mnuViewFontSizeUpClick(Sender: TObject);
    procedure mnuSetHighlighterClick(Sender: TObject);

    procedure AboutEPlus;

    procedure DoFind(Sender: TObject);   //callback for FindDialog
    procedure DoReplace(Sender: TObject); //callback for ReplaceDialog

  public
    //overridden public form methods
    //prevent propagating shortcuts from non-modal windows (menu's, actions etc.)
    function IsShortcut(var Message: TLMKey): boolean; override;
  private
    { private declarations }
    NoteBook: TEditorPageControl;
    OpenSaveFilter: String;
    TemplateFilter: String;
    ConfigFileName: String;
    ConfigFileDir: String;

    FindText: String;
    ReplaceText: String;
    FindOptions: TSynSearchOptions;
    ReplaceOptions: TSynSearchOptions;

    AppOptions: TEPlusOptions;
    MruList: TStringList;
    MruMenuItems: Array[0..MruEntries-1] of TMenuItem;

    procedure SetUpAndConfigureLazEdit;
    procedure DoTranslate;
    procedure SaveEplusConfiguration;
    procedure CleanUp;

    function GetDefaultAppOptions: TEPlusOptions;
    procedure ApplyAppOptions(const Options: TEplusOptions);
    procedure GatherAppOptions(var Options: TEplusOptions);

    procedure TagMenuItemsAndActions;
    procedure UpdateMenuItems;
    procedure CreateMruMenuItemsArray;
    function TryHlMenuTagToFileType(ATag: PtrInt; out AFileType: TEditorFileType): Boolean;

    procedure ConstructOpenDialogFileFilters;
    procedure ShowError(const Msg: String);

    //Helper functions for editor;
    function TryMarkSelection(const Pre, Post: String): Boolean;
    //File procedures
    function AskFileNameOpen: String;
    function AskFileNameOpenTemplate: String;
    function AskFileNameSave(const Fn: String; const FileType: TEditorFileType): String;
    function AskFileNameSaveTemplate: String;
    function TryFileOpen(const Fn: String; const AsTemplate: Boolean = False): Boolean;
    function TryFileSave(Editor: TEditor; Fn: String): TIoResult;
    function CloseCurrentEditor: Boolean;
    function TryFileSaveAll(out Failures: String): Boolean;
    procedure DoFileOpen;
    procedure DoMruOpen(const Index: Integer);
    procedure DoTemplateOpen;
    procedure DoFileSave(Editor: TEditor);
    procedure DoFileSaveAs(Editor: TEditor);
    procedure DoFileSaveAsTemplate(Editor: TEditor);
    procedure DoFileSaveAll;
    procedure DoFileNewByType(const AFileType: TEditorFileType; const InitialText: String = '');
    procedure DoFileNewHtml;
    procedure FileOpenInBrowser;

    //Edit procedures
    procedure EditUndo;
    procedure EditRedo;
    procedure EditCopy;
    procedure EditPaste;
    procedure EditCut;
    procedure EditSelectAll;
    procedure EditFind;
    procedure EditFindNext;
    procedure EditFindPrevious;
    procedure EditReplace;
    procedure EditPasteTableContentTab;


    //Insert and layout procedures
    procedure InsertAnchor;
    procedure InsertPicture;
    procedure InsertTable;
    procedure InsertSpecialChars;
    procedure InsertSpecial(const AValue: String);

    procedure LayoutHeading(const Level: Integer);

    //View procedures
    procedure SetHighlighter(const AFileType: TEditorFileType);
    procedure EditorSelectFont;
    procedure EditorFontSizeUp;
    procedure EditorFontSizeDown;


    procedure ParseCommandlineFilenames(Dummy: PtrInt); //Dummy is needed for QueueAsyncCall()
    procedure ParseCommandLineSwitches;

    //NoteBook and Editor Events
    procedure OnEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure OnBeforeCloseEditor(Sender: TTabSheet; var Cancel: Boolean);
    procedure OnCharsetChange(Sender: TEditor; const OldCharset, NewCharset: String; const LineNr: Integer);
    procedure OnMruListChange(Sender: TObject);

    //CharMap events
    procedure OnHtmlCharMapInsertText(AValue: String);
  public
    { public declarations }
  end; 

var
  EPlusForm: TEPlusForm;

implementation

{$R *.lfm}


const pXY  = 0;   //Panels constanten
      pMod = 1;
      pIns = 2;
      pName = 3;
      SLine = 'Rg';
      SCol = 'Kol';
      SModified = 'Gewijzigd';
      SIns = 'INS';
      SOvr = 'OVR';

      tgNeedsEditor     = $01;
      tgNeedsSelection  = $02;
      tgNeedsClipPaste  = $04;


      msgOpenError = 'Fout bij openen van bestand:'^m'%s';
      msgSaveError = 'Fout bij opslaan van bestand:'^m'%s';
      msgSaveAllError = 'De volgende bestanden zijn niet opgeslagen:'^m'%s';
      msgFileIsNotText = 'Dit bestand lijkt geen tekstbestand te zijn'^m'%s'^m'Wilt u het toch openen?';
      msgFileNotFound = 'Bestand niet gevonden:'^m'%s';
      msgModifiedSave = 'Bestand is gewijzigd:'^m'%s'^m'Bestand opslaan?';
      msgMruIndexOutOfBound = 'Index voor recent geopende bestanden ligt buiten de grenzen [%d]'^m+
                              'Dit is uiteraard een fout van de programmeur';
      msgFileTypeNotForBrowser = 'Dit bestandstype lijkt niet geschikt om te openen in een browser.'^m+
                                  'Wilt u toch doorgaan?';
      msgFileHasNoName = 'Dit bestand heeft nog geen naam.'^m +
                         'U moet het bestand eerst opslaan om het in de browser te openen.';
      msgErrorBrowser = 'Er is een fout opgetreden tijdens het openen van'^m+
                        '%s'^m'in de browser.';
      msgTextNotFound = 'Tekst niet gevonden:'^m'"%s"';

      //Initial text for FileNew commands
      itXml = '<?xml version="1.0"?>';
      itBat = '@echo off';
      itUnixShellScript = '#!/bin/bash';

      //Commandline options
      opt_long_PCP = 'pcp';  //--pcp=path/to/configfile
      opt_short_blankpage = 'n';


{ TEPlusForm }

{ ************************** [  Form event handlers] ***********************************}

procedure TEPlusForm.FormCreate(Sender: TObject);
begin
  SetUpAndConfigureLazEdit;
  Self.DoTranslate();
end;

procedure TEPlusForm.FormDestroy(Sender: TObject);
begin
  CleanUp;
end;

procedure TEPlusForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveEplusConfiguration;
end;

procedure TEPlusForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  While NoteBook.PageCount > 0 do
  begin
    if not NoteBook.ClosePage(NoteBook.PageCount - 1) then
    //ClosePage invokes OnBeforeCloseEditor, which sets Cancel = True if file is not saved
    //this in turn will make ClosePage()  return false
    begin
      CanClose := False;
      Break;
    end;
  end;
end;

procedure TEPlusForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i: Integer;
begin
  for i := Low(FileNames) to High(FileNames) do
  begin
    if FileExistsUtf8(FileNames[i]) then
    begin
      if not TryFileOpen(FileNames[i]) then ShowError(Format('Fout bij openen van bestand'^m,[FileNames[i]]));
    end
    else ShowError(Format(msgFileNotFound,[FileNames[i]]));
  end;
end;

procedure TEPlusForm.FormShow(Sender: TObject);
begin
  //This needs to be done only after the mainform has become visible on the screen
  Application.QueueAsyncCall(@ParseCommandLineFileNames, 0);
end;

procedure TEPlusForm.mnuLanguageChangeClick(Sender: TObject);
begin
  vTranslations.TranslateToLanguageID(Abs(TMenuItem(Sender).Tag));
  DoTranslate();
end;


{ ********************** [ Menu OnClick Handlers ] ************************ }

procedure TEPlusForm.mnuFileOpenInBrowserClick(Sender: TObject);
begin
  FileOpenInBrowser;
end;

procedure TEPlusForm.mnuEditPasteTableContentTabClick(Sender: TObject);
begin
  EditPasteTableContentTab;
end;


procedure TEPlusForm.mnuInsertCssStyleClick(Sender: TObject);
begin
  TryMarkSelection(style_start, style_end);
end;

procedure TEPlusForm.mnuInsertHtmlCommentClick(Sender: TObject);
begin
  TryMarkSelection(comment_start, comment_end);
end;

procedure TEPlusForm.mnuInsertJSClick(Sender: TObject);
begin
 TryMarkSelection(script_start, script_end);
end;

procedure TEPlusForm.mnuInsertSpecialCharsClick(Sender: TObject);
begin
  InsertSpecialChars;
end;

procedure TEPlusForm.mnuInsertTableCellClick(Sender: TObject);
begin
  TryMarkSelection(col_start, col_end);
end;

procedure TEPlusForm.mnuInsertTableRowClick(Sender: TObject);
begin
  TryMarkSelection(row_start, row_end);
end;

procedure TEPlusForm.mnuInsertWordDefinitionClick(Sender: TObject);
begin
  TryMarkSelection(worddef_start, worddef_end);
end;

procedure TEPlusForm.mnuInsertWordListClick(Sender: TObject);
begin
  TryMarkSelection(wordlist_start, wordlist_end);
end;

procedure TEPlusForm.mnuInsertWordTermClick(Sender: TObject);
begin
  TryMarkSelection(wordterm_start, wordterm_end);
end;

procedure TEPlusForm.mnuLayoutBlockQuoteClick(Sender: TObject);
begin
  TryMarkSelection(blockquote_start, blockquote_end);
end;

procedure TEPlusForm.mnuLayoutCodeClick(Sender: TObject);
begin
  TryMarkSelection(code_start, code_end);
end;

procedure TEPlusForm.mnuLayoutPreformattedClick(Sender: TObject);
begin
  TryMarkSelection(pre_start, pre_end);
end;

procedure TEPlusForm.mnuLayoutQuoteClick(Sender: TObject);
begin
  TryMarkSelection(quote_start, quote_end);
end;

procedure TEPlusForm.mnuViewFontClick(Sender: TObject);
begin
  EditorSelectFont;
end;


procedure TEPlusForm.mnuViewFontsizeDownClick(Sender: TObject);
begin
  EditorFontSizeDown;
end;

procedure TEPlusForm.mnuViewFontSizeUpClick(Sender: TObject);
begin
  EditorFontSizeUp;
end;


procedure TEPlusForm.TopLevelMenuClick(Sender: TObject);
begin
  UpdateMenuItems;
end;

procedure TEPlusForm.mnuEditCutClick(Sender: TObject);
begin
  EditCut;
end;

procedure TEPlusForm.mnuFileCloseAppClick(Sender: TObject);
begin
  Close;
end;

procedure TEPlusForm.mnuFileCloseCurrentClick(Sender: TObject);
begin
  CloseCurrentEditor;
end;

procedure TEPlusForm.mnuFileSaveAsClick(Sender: TObject);
begin
  DoFileSaveAs(NoteBook.CurrentEditor);
end;



procedure TEPlusForm.mnuFindNextClick(Sender: TObject);
begin
  EditFindNext;
end;

procedure TEPlusForm.mnuFileNewBatClick(Sender: TObject);
begin
  DoFileNewByType(eftBat, itBat);
end;

procedure TEPlusForm.mnuFileNewHtmlClick(Sender: TObject);
begin
  DoFileNewHtml;
end;

procedure TEPlusForm.mnuFileNewCClick(Sender: TObject);
begin
    DoFileNewByType(eftC);
end;

procedure TEPlusForm.mnuFileNewCssClick(Sender: TObject);
begin
    DoFileNewByType(eftCss);
end;

procedure TEPlusForm.mnuFileNewFpcClick(Sender: TObject);
begin
    DoFileNewByType(eftFpc);
end;

procedure TEPlusForm.mnuFileNewIniClick(Sender: TObject);
begin
  DoFileNewByType(eftIni);
end;

procedure TEPlusForm.mnuFileNewJSClick(Sender: TObject);
begin
    DoFileNewByType(eftJS);
end;

procedure TEPlusForm.mnuFileNewPerlClick(Sender: TObject);
begin
  DoFileNewByType(eftPerl);
end;

procedure TEPlusForm.mnuFileNewPhpClick(Sender: TObject);
begin
  DoFileNewByType(eftPerl);
end;

procedure TEPlusForm.mnuFileNewPyClick(Sender: TObject);
begin
  DoFileNewByType(eftPy);
end;

procedure TEPlusForm.mnuFileNewShellScriptClick(Sender: TObject);
begin
  DoFileNewByType(eftUnixShell, itUnixShellScript);
end;

procedure TEPlusForm.mnuFileNewXmlClick(Sender: TObject);
begin
  DoFileNewByType(eftXml, itXml);
end;

procedure TEPlusForm.mnuFileMruClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := ((Sender as TMenuItem).Tag)  shr 4;
  //debugln('Menu->File->',(Sender as TMenuItem).Caption,' Index = ',Dbgs(Index));
  DoMruOpen(Index);
end;

procedure TEPlusForm.mnuEditRedoClick(Sender: TObject);
begin
  EditRedo;
end;

procedure TEPlusForm.mnuEditReplaceClick(Sender: TObject);
begin
  EditReplace;
end;

procedure TEPlusForm.mnuSetHighlighterClick(Sender: TObject);
var
  AFileType: TEditorFileType;
begin
  if TryHlMenuTagToFileType((Sender as TMenuItem).Tag, AFileType) then
    SetHighlighter(AFileType)
  else
    debugln(Format('Error: Invalid tag for MenuItem %s: %x',[(Sender as TMenuItem).Name,(Sender as TMenuItem).Tag]));
end;



procedure TEPlusForm.mnuEditSelectAllClick(Sender: TObject);
begin
  EditSelectAll;
end;

procedure TEPlusForm.mnuEditUndoClick(Sender: TObject);
begin
  EditUndo;
end;


{ ******************** [Action OnExecute Handlers ] *********************** }

procedure TEPlusForm.acFileOpenExecute(Sender: TObject);
begin
  DoFileOpen;
end;

procedure TEPlusForm.acFileSaveAllExecute(Sender: TObject);
begin
  DoFileSaveAll;
end;

procedure TEPlusForm.acFileNewFromTemplateExecute(Sender: TObject);
begin
  DoTemplateOpen;
end;

procedure TEPlusForm.acAboutExecute(Sender: TObject);
begin
  AboutEplus;
end;

procedure TEPlusForm.acEditFindNextExecute(Sender: TObject);
begin
  EditFindNext;
end;

procedure TEPlusForm.acEditFindPreviousExecute(Sender: TObject);
begin
  EditFindPrevious;
end;

procedure TEPlusForm.acInsertAmpersandExecute(Sender: TObject);
begin
  InsertSpecial(Ampersand);
end;

procedure TEPlusForm.acInsertAnchorExecute(Sender: TObject);
begin
  InsertAnchor;
end;

procedure TEPlusForm.acInsertCopyrightExecute(Sender: TObject);
begin
  InsertSpecial(CopyRight);
end;

procedure TEPlusForm.acInsertGreaterExecute(Sender: TObject);
begin
  InsertSpecial(Greater);
end;

procedure TEPlusForm.acInsertLesserExecute(Sender: TObject);
begin
  InsertSpecial(Lesser);
end;

procedure TEPlusForm.acInsertLineBreakExecute(Sender: TObject);
begin
  InsertSpecial(LineBreak);
end;

procedure TEPlusForm.acInsertListItemExecute(Sender: TObject);
begin
  TryMarkSelection(listitem_start, listitem_end);
end;

procedure TEPlusForm.acInsertNbSpaceExecute(Sender: TObject);
begin
  InsertSpecial(NbSpace);
end;

procedure TEPlusForm.acInsertOlistExecute(Sender: TObject);
begin
  TryMarkSelection(numberedlist_start, numberedlist_end);
end;

procedure TEPlusForm.acInsertPictureExecute(Sender: TObject);
begin
  InsertPicture;
end;

procedure TEPlusForm.acInsertTableExecute(Sender: TObject);
begin
  InsertTable;
end;

procedure TEPlusForm.acInsertTradeMarkExecute(Sender: TObject);
begin
  InsertSpecial(TradeMark);
end;

procedure TEPlusForm.acInsertUListExecute(Sender: TObject);
begin
  TryMarkSelection(unnumberedlist_start, unnumberedlist_end);
end;

procedure TEPlusForm.acLayoutAlignCenterExecute(Sender: TObject);
begin
  TryMarkSelection(centeralign_start, centeralign_end);
end;

procedure TEPlusForm.acLayoutAlignJustifyExecute(Sender: TObject);
begin
  TryMarkSelection(justifyalign_start, justifyalign_end);
end;

procedure TEPlusForm.acLayoutAlignLeftExecute(Sender: TObject);
begin
  TryMarkSelection(leftalign_start, leftalign_end);
end;

procedure TEPlusForm.acLayoutAlignRightExecute(Sender: TObject);
begin
  TryMarkSelection(rightalign_start, rightalign_end);
end;

procedure TEPlusForm.acLayoutBoldExecute(Sender: TObject);
begin
  TryMarkSelection(bold_start, bold_end);
end;

procedure TEPlusForm.acGroupingDivExecute(Sender: TObject);
begin
  TryMarkSelection(div_start, div_end);
end;

procedure TEPlusForm.acLayoutEmphasisExecute(Sender: TObject);
begin
  TryMarkSelection(emphasis_start, emphasis_end);
end;

procedure TEPlusForm.acLayoutH1Execute(Sender: TObject);
begin
  LayoutHeading(1);
end;

procedure TEPlusForm.acLayoutH2Execute(Sender: TObject);
begin
  LayoutHeading(2);
end;

procedure TEPlusForm.acLayoutH3Execute(Sender: TObject);
begin
  LayoutHeading(3);
end;

procedure TEPlusForm.acLayoutH4Execute(Sender: TObject);
begin
  LayoutHeading(4);
end;

procedure TEPlusForm.acLayoutH5Execute(Sender: TObject);
begin
  LayoutHeading(5);
end;

procedure TEPlusForm.acLayoutH6Execute(Sender: TObject);
begin
  LayoutHeading(6);
end;

procedure TEPlusForm.acLayoutItalicExecute(Sender: TObject);
begin
  TryMarkSelection(italic_start, italic_end);
end;

procedure TEPlusForm.acGroupingParagraphExecute(Sender: TObject);
begin
  TryMarkSelection(paragraph_start, paragraph_end);
end;

procedure TEPlusForm.acGroupingSpanExecute(Sender: TObject);
begin
  TryMarkSelection(span_start, span_end);
end;

procedure TEPlusForm.acLayoutStrongExecute(Sender: TObject);
begin
  TryMarkSelection(strong_start, strong_end);
end;

procedure TEPlusForm.acLayoutSubExecute(Sender: TObject);
begin
  TryMarkSelection(sub_start, sub_end);
end;

procedure TEPlusForm.acLayoutSupExecute(Sender: TObject);
begin
  TryMarkSelection(sup_start, sup_end);
end;

procedure TEPlusForm.acLayoutUnderlineExecute(Sender: TObject);
begin
  TryMarkSelection(underline_start, underline_end);
end;

procedure TEPlusForm.acEditCopyExecute(Sender: TObject);
begin
  EditCopy;
end;

procedure TEPlusForm.acEditFindExecute(Sender: TObject);
begin
  EditFind;
end;

procedure TEPlusForm.acEditPasteExecute(Sender: TObject);
begin
  EditPaste;
end;

procedure TEPlusForm.acFileNewPlainExecute(Sender: TObject);
begin
  DoFileNewByType(eftNone);
end;

procedure TEPlusForm.acFileSaveExecute(Sender: TObject);
begin
  DoFileSave(NoteBook.CurrentEditor);
end;


{ ********************** [ Form Methods ] ***************************** }

function TEPlusForm.IsShortcut(var Message: TLMKey): boolean;
begin
  if Active then
    Result := inherited IsShortcut(Message)
  else
    Result := False;
end;

{ ********************** [ Initializing and cleaning up ] ************* }


procedure TEPlusForm.SetUpAndConfigureLazEdit;
begin
  ConfigFileDir := GetDefaultIniDir;
  ParseCommandLineSwitches;
  if (not DirectoryExists(ConfigFileDir)) and (ConfigFileDir <> '') then
  begin
    if not ForceDirectories(ConfigFileDir) then
    begin
      if (CompareFilenames(ConfiGFileDir, GetDefaultIniDir) <> 0) then
      begin
        debugln('Error creating directory: ',ConfigFileDir);
        debugln('Trying ',GetDefaultIniDir,' instead.');
        ConfigFileDir := GetDefaultIniDir;
      end;
    end;
  end;

  ConfigFileName := IncludeTrailingPathDelimiter(ConfigFileDir) + GetDefaultIniNameOnly;
  //DebugLn('ConfigFileName = ',ConfigFileName);


  // REMOVE !!
  //ConfigFileName := 'F:\LazarusProjecten\EPlus\EPlus.ini';
  //debugln('Temporarily using: ',ConfigFileName);
  //^^^^^^^^^^^^^^

  Caption := AppName;

  TagMenuItemsAndActions;
  CreateMruMenuItemsArray;

  //Create the notebook
  NoteBook := TEditorPageControl.Create(Self);
  NoteBook.Align := alClient;
  NoteBook.Parent := Self;
  NoteBook.OnStatusChange := @OnEditorStatusChange;
  NoteBook.OnBeforeCloseEditor := @OnBeforeCloseEditor;
  NoteBook.OnEditorCharsetChange := @OnCharsetChange;

  //Creating dialogs
  HtmlCharMapDlg := THtmlCharMapDlg.Create;
  HtmlCharMapDlg.OnHtmlCharClick := @OnHtmlCharMapInsertText;
  NewHtmlDlg := TNewHtmlDlg.Create;
  AnchorDlg := TAnchorDlg.Create;
  PictureDlg := TPictureDlg.Create;
  TableDlg := TTableDlg.Create;

  //MruList
  MruList := TStringList.Create();
  //MruList.MaxEntries := MruEntries;

  //Configurable options
  AppOptions := GetDefaultAppOptions;
  if not LoadOptions(AppOptions, ConfigFileName) then
    DebugLn('Fout bij laden van opties:',LineEnding,'  ',ConfigFileName)
  else
    ApplyAppOptions(AppOptions);
  ConstructOpenDialogFileFilters;
  //Attach the OnChange handler after filling the list (in LoadOptions)
  //MruList.OnChange := @OnMruListChange;
  //Update the MRU menu entries
  OnMruListChange(Self);


end;

procedure TEPlusForm.DoTranslate;
begin
{  mnuEditPasteTableContentTab: TMenuItem;
  mnuEditPasteSpecial: TMenuItem;
  mnuAbout: TMenuItem;
  mnuViewFont: TMenuItem;}
  //File menu
  mnuFile.Caption := vTranslations.mnuFile;
    mnuFileOpen.Caption := vTranslations.mnuFileOpen;
    mnuFileNewFromTemplate.Caption := vTranslations.mnuFileNewFromTemplate;
    mnuFileNew.Caption := vTranslations.mnuFileNew;
      mnuFileNewText.Caption := vTranslations.mnuFileNewText;
{      mnuFileNewHtml: TMenuItem;
      mnuFileNewXml: TMenuItem;
      mnuFileNewCss: TMenuItem;
      mnuFileNewJS: TMenuItem;
      mnuFileNewFpc: TMenuItem;
      mnuFileNewC: TMenuItem;
      mnuFileNewPy: TMenuItem;
      mnuFileNewPhp: TMenuItem;
      mnuFileNewPerl: TMenuItem;
      mnuFileNewShellScript: TMenuItem;
      mnuFileNewBat: TMenuItem;
      mnuFileNewIni: TMenuItem;}
    mnuFileSave.Caption := vTranslations.mnuFileSave;
    mnuFileSaveAs.Caption := vTranslations.mnuFileSaveAs;
    mnuFileSaveAll.Caption := vTranslations.mnuFileSaveAll;
    mnuFileCloseCurrent.Caption := vTranslations.mnuFileCloseCurrent;
    //mnuSep1: TMenuItem;
    mnuFileOpenInBrowser.Caption := vTranslations.mnuFileOpenInBrowser;
    //mnuSepAboveMru: TMenuItem;
    mnuFileCloseApp.Caption := vTranslations.mnuFileCloseApp;
  //Edit menu
  mnuEdit.Caption := vTranslations.mnuEdit;
    mnuEditUndo.Caption := vTranslations.mnuEditUndo;
    mnuEditRedo.Caption := vTranslations.mnuEditRedo;
    //mnuSep11: TMenuItem;
    mnuEditCopy.Caption := vTranslations.mnuEditCopy;
    mnuEditCut.Caption := vTranslations.mnuEditCut;
    mnuEditPaste.Caption := vTranslations.mnuEditPaste;
    mnuEditSelectAll.Caption := vTranslations.mnuEditSelectAll;
    //mnuSep12: TMenuItem;
    mnuEditReplace.Caption := vTranslations.mnuEditReplace;
    mnuEditFindNext.Caption := vTranslations.mnuEditFindNext;
    mnuEditFind.Caption := vTranslations.mnuEditFind;
  //Insert menu
  mnuHTMLTools.Caption := vTranslations.mnuHTMLTools;
  {  mnuInsertAnchor: TMenuItem;
    mnuInsertList: TMenuItem;
      mnuInsertUList: TMenuItem;
      mnuInsertNList: TMenuItem;
      mnuInsertWordList: TMenuItem;
      mnuInsetListItem: TMenuItem;
      mnuInsertWordTerm: TMenuItem;
      mnuInsertWordDefinition: TMenuItem;
    mnuInsertTable: TMenuItem;
      mnInsertNewTable: TMenuItem;
      mnuInsertTableCell: TMenuItem;
      mnuInsertTableRow: TMenuItem;
    mnuInsertPicture: TMenuItem;
    mnuInsertSpecialChars: TMenuItem;
    mnuInsertLineBreak: TMenuItem;
    mnuInsertSep1: TMenuItem;
    mnuInsertHtmlComment: TMenuItem;
    mnuInsertJS: TMenuItem;
    mnuInsertCssStyle: TMenuItem;}
    //Layout menu
    mnuLayout.Caption := vTranslations.mnuLayout;
      mnuLayoutBold.Caption := vTranslations.mnuLayoutBold;
      mnuLayoutAlignJustify.Caption := vTranslations.mnuLayoutAlignJustify;
      mnuLayoutItalic.Caption := vTranslations.mnuLayoutItalic;
      mnuLayoutUnderline.Caption := vTranslations.mnuLayoutUnderline;
      mnuLayoutSub.Caption := vTranslations.mnuLayoutSub;
      mnuLayoutSup.Caption := vTranslations.mnuLayoutSup;
      mnuLayoutEmphasis.Caption := vTranslations.mnuLayoutEmphasis;
      mnuLayoutStrong.Caption := vTranslations.mnuLayoutStrong;
      mnuLayoutHeadings.Caption := vTranslations.mnuLayoutHeadings;
  {      mnuLayoutH1: TMenuItem;
        mnuLayoutH2: TMenuItem;
        mnuLayoutH3: TMenuItem;
        mnuLayoutH4: TMenuItem;
        mnuLayoutH5: TMenuItem;
        mnuLayoutH6: TMenuItem;
      mnuLayoutAlign: TMenuItem;
        mnuLayoutAlignLeft: TMenuItem;
        mnuLayoutAlignRight: TMenuItem;
        mnuLayoutAlignCenter: TMenuItem;
      mnuLayoutCode: TMenuItem;
      mnuLayoutQuote: TMenuItem;
      mnuLayoutBlockQuote: TMenuItem;
      mnuLayoutPreformatted: TMenuItem;}
    //Grouping menu
    mnuGrouping.Caption := vTranslations.mnuGrouping;
      mnuGroupingParagraph.Caption := vTranslations.mnuGroupingParagraph;
      mnuGroupingDiv.Caption := vTranslations.mnuGroupingDiv;
      mnuGroupingSpan.Caption := vTranslations.mnuGroupingSpan;
  //View menu
  mnuView.Caption := vTranslations.mnuView;
    mnuViewFontsize.Caption := vTranslations.mnuViewFontsize;
{      mnuViewFontSizeUp: TMenuItem;
      mnuViewFontsizeDown: TMenuItem;
    mnuViewHighlighter: TMenuItem;
      //these menu items MUST have names that are built like this:
      //'mnuViewHL' + eftNames[SomeIndex]
      mnuViewHLeftNone: TMenuItem;
      mnuViewHLeftHtml: TMenuItem;
      mnuViewHLeftXml: TMenuItem;
      mnuViewHLeftCss: TMenuItem;
      mnuViewHLeftJS: TMenuItem;
      mnuViewHLeftFpc: TMenuItem;
      mnuViewHLeftLfm: TMenuItem;
      mnuViewHLeftC: TMenuItem;
      mnuViewHLeftPy: TMenuItem;
      mnuViewHLeftPhp: TMenuItem;
      mnuViewHLeftPerl: TMenuItem;
      mnuViewHLeftUNIXShell: TMenuItem;
      mnuViewHLeftBat: TMenuItem;
      mnuViewHLeftDiff: TMenuItem;
      mnuViewHLeftIni: TMenuItem;
      mnuViewHLeftPo: TMenuItem;
  //Popup menus
  //Popup menu for editor
  EditorPopupMenu: TPopupMenu;
    mnuEditPopupSelectAll: TMenuItem;
    mnuEditPopupPaste: TMenuItem;
    mnuEditPopupCut: TMenuItem;
    mnuEditPopupCopy: TMenuItem;
  //Dropdownmenu for HeadingBtn
  HeadingDropDownMenu: TPopupMenu;
    mnuPopupLayoutH6: TMenuItem;
    mnuPopupLayoutH5: TMenuItem;
    mnuPopupLayoutH4: TMenuItem;
    mnuPopupLayoutH3: TMenuItem;
    mnuPopupLayoutH2: TMenuItem;
    mnuPopupLayoutH1: TMenuItem;   }
end;

procedure TEPlusForm.SaveEplusConfiguration;
begin
  GatherAppOptions(AppOptions);
  if not EPlus_Config.SaveOptions(AppOptions, ConfigFileName) then
    DebugLn('Fout bij opslaan van opties:',LineEnding,'  ',ConfigFileName);
end;

procedure TEPlusForm.CleanUp;
begin
  HtmlCharMapDlg.OnHtmlCharClick := nil;
  HtmlCharMapDlg.Free;
  NewHtmlDlg.Free;
  AnchorDlg.Free;
  PictureDlg.Free;
  TableDlg.Free;
end;

{ ********************** [ Configuration ] ************************** }

function TEPlusForm.GetDefaultAppOptions: TEPlusOptions;
var
  i: Integer;
begin
  Result.FileTypeMaskList := DefaultFileTypeMaskList;
  Result.MainForm.Position := Rect(-1, -1, -1, -1);
  Result.MainForm.InitialDir := '';
  Result.AnchorDlg.Position := Point(-1,-1);
  Result.HtmlCharMapDlg.Position := Point(-1,-1);
  Result.NewHtmlDlg.Position := Point(-1,-1);
  Result.PictureDlg.Position := Point(-1,-1);
  Result.PictureDlg.InitialDir := '';
  Result.TableDlg.Position := Point(-1,-1);
  for i := 0 to MruEntries - 1 do Result.RecentFiles[i] := '';
  Result.EditorOptions := NoteBook.EditorOptions;
  Result.TemplateMaskList := DefaultTemplateMaskList;
end;

procedure TEPlusForm.ApplyAppOptions(const Options: TEplusOptions);
  function ValidPos(const New, Old: Integer): Integer;
  begin
    if (New < 0) then Result := Old else Result := New;
  end;
var
  ALeft, ATop, AWidth, AHeight: Integer;
  //ftIndex: TEditorFileType;
  i: Integer;
  EdOpt: TEditorOptions;
begin
  EdOpt := NoteBook.EditorOptions;
  if (Options.EditorOptions.FontName <> '') or (Options.EditorOptions.FontSize <> 0) then
  begin
    //debugln('NoteBook.EditorOptions <> default');
    EdOpt.FontName := Options.EditorOptions.FontName;
    EdOpt.FontSize := Options.EditorOptions.FontSize;
    NoteBook.EditorOptions := EdOpt;
  end;
  ALeft := ValidPos(Options.MainForm.Position.Left,Left);
  ATop := ValidPos(Options.MainForm.Position.Top,Top);
  AWidth := ValidPos(Options.MainForm.Position.Right,Width);
  AHeight := ValidPos(Options.MainForm.Position.Bottom,Height);
  SetBounds(ALeft, ATop, AWidth, AHeight);
  {
  for ftIndex := Low(TEditorFileType) to High(TEditorFileType) do
  begin
    DebugLn(eftNames[ftIndex],'=',Options.FileTypeMaskList[ftIndex]);
  end;
  }
  //FileTypeMaskList := Options.FileTypeMaskList;
  if Options.MainForm.InitialDir <> '' then OpenDialog.InitialDir := Options.MainForm.InitialDir;


  //Dialogs
  NewHtmlDlg.Top := Options.NewHtmlDlg.Position.y;
  NewHtmlDlg.Left := Options.NewHtmlDlg.Position.x;
  NewHtmlDlg.InitialDir := Options.NewHtmlDlg.InitialDir;

  AnchorDlg.Top := Options.AnchorDlg.Position.y;
  AnchorDlg.Left := Options.AnchorDlg.Position.x;

  HtmlCharMapDlg.Top := Options.HtmlCharMapDlg.Position.y;
  HtmlCharMapDlg.Left := Options.HtmlCharMapDlg.Position.x;

  PictureDlg.Top := Options.PictureDlg.Position.y;
  PictureDlg.Left := Options.PictureDlg.Position.x;
  PictureDlg.InitialDir := Options.PictureDlg.InitialDir;
  PictureDlg.DefaultFolderPrefix := Options.PictureDlg.DefaultFolderPrefix;

  TableDlg.Top := Options.TableDlg.Position.y;
  TableDlg.Left := Options.TableDlg.Position.x;

  //uses downto loop because TMruList.Add stores item at index = 0
  for i := MruEntries - 1 downto 0 do
  begin
    if (Options.RecentFiles[i] <> '') then
      MruList.Add(Options.RecentFiles[i]);
  end;

  // Translation
  vTranslations.TranslateToLanguageID(Options.Translation);
end;

procedure TEPlusForm.GatherAppOptions(var Options: TEplusOptions);
var
  i: Integer;
begin
  if (Self.WindowState = wsNormal) then Options.MainForm.Position := Rect(Left, Top, Width, Height);
  OpenDialog.InitialDir := Options.MainForm.InitialDir;
  SaveDialog.InitialDir := Options.MainForm.InitialDir;

  Options.EditorOptions := NoteBook.EditorOptions;
  //Options.FileTypeMaskList := FileTypeMaskList;

  //Dialogs
  Options.NewHtmlDlg.Position.y := NewHtmlDlg.Top;
  Options.NewHtmlDlg.Position.x := NewHtmlDlg.Left;
  Options.NewHtmlDlg.InitialDir := NewHtmlDlg.InitialDir;

  Options.AnchorDlg.Position.y  := AnchorDlg.Top;
  Options.AnchorDlg.Position.x  := AnchorDlg.Left;

  Options.HtmlCharMapDlg.Position.y := HtmlCharMapDlg.Top;
  Options.HtmlCharMapDlg.Position.x := HtmlCharMapDlg.Left;

  Options.PictureDlg.Position.y := PictureDlg.Top;
  Options.PictureDlg.Position.x := PictureDlg.Left;
  Options.PictureDlg.InitialDir := PictureDlg.InitialDir;
  Options.PictureDlg.DefaultFolderPrefix := PictureDlg.DefaultFolderPrefix;

  Options.TableDlg.Position.y  := TableDlg.Top;
  Options.TableDlg.Position.x  := TableDlg.Left;

  for i := 0 to MruEntries - 1 do
  begin
    if MruList.Count > i then
      Options.RecentFiles[i] := MruList.Strings[i]
    else
      Options.RecentFiles[i] := '';
  end;
end;


procedure TEPlusForm.ConstructOpenDialogFileFilters;
  function AddWilds(const S: String): String;
  //S has the form of '.pp;.pas;.inc'
  //This function will add '*' in front of every extension in the list
  var
    SL: TStringList;
    i: Integer;
  begin
    Result := '';
    SL := TStringList.Create;
    try
      SL.Delimiter := ';';
      SL.StrictDelimiter := True;
      SL.DelimitedText := S;
      for i := 0 to SL.Count - 1 do
      begin
        if (Length(SL.Strings[i]) > 0) then Result := Result + '*' + SL.Strings[i] + ';';
      end;
      if (Length(Result) > 0) and (Result[Length(Result)] = ';') then System.Delete(Result,Length(Result),1);
    finally
      SL.Free;
    end;
  end;
var
  Filter, SubFilter, WildFilter: String;
  Index: Integer;
  FileType: TEditorFileType;
begin
  //First filter for OpenSave
  Filter := '';
  for Index := fiEftFirst to fiEftLast do
  begin
    FileType := TEditorFileType(Index);
    //DbgOut(eftNames[FileType],' -> ');
    WildFilter := AddWilds(AppOptions.FileTypeMaskList[FileType]);
    //DebugLn(WildFilter);
    SubFilter := eftFilterNames[FileType] + ' (' + WildFilter + ')|'+ WildFilter;
    Filter := Filter + '|' + SubFilter;
  end;
  Filter := Filter + '|' + FilterText;
  WildFilter := AddWilds(AppOptions.TemplateMaskList);
  //Also add template filter to Open/SaveFilter
  Filter :=  Filter + '|' + STemplate + ' (' + WildFilter + ')|' + WildFilter;
  Filter := Filter + '|' + FilterAll;
  while (Length(Filter) > 0) and (Filter[1] = '|') do System.Delete(Filter,1,1);
  OpenSaveFilter := Filter;

  //Now filter for Open/Save as Template
  TemplateFilter := STemplate + ' (' + WildFilter + ')|' + WildFilter + '|' + FilterAll;
end;

{ ********************** [ Commandline options ] ******************************** }

procedure TEPlusForm.ParseCommandlineFilenames(Dummy: PtrInt);
var
  i: Integer;
  S: String;
begin
  //debugln('ParseCommandlineFilenames, FileNamesCount = ',DbgS(FileNamesCount));
  if Dummy = 12345 then Exit; //Get rid of annoying hint
{  for i := 1 to MyGetOpt.FileNamesCount do
  begin
    S := MyGetOpt.FileNameStr(i);
    if (S <> EmptyStr) then
    begin
      //it is a file to open
      S := ExpandFileNameUtf8(S); //we want full filename here, e.g. for filename in statusbar
      if FileExistsUtf8(S) then
      begin
        if not TryFileOpen(S) then ShowError(Format(msgOpenError,[S]));
      end
      else ShowError(Format(msgFileNotFound,[S]));
    end
  end;
  if (MyGetOpt.FileNamesCount = 0) and MyGetOpt.HasOption(opt_short_blankpage,False) then
    DoFileNewByType(eftNone);   }
end;

procedure TEPlusForm.ParseCommandLineSwitches;
var
  S: String;
begin
  //debugln('ParseCommandlineSwitches');
{  if MyGetOpt.HasOption(opt_long_PCP, False, S) then
  begin
    if (S <> EmptyStr) then
    begin
      S := ExcludeTrailingPathdelimiter(ExpandFileName(S));
      //MyGetOpt returns parameters as UTF8
      //inifiles uses system-encoding
      ConfigFileDir := Utf8ToSys(S);
    end;
  end;}
end;



{ ***************** [ Menu-related Helper functions/procedures ] ************************ }


procedure TEPlusForm.CreateMruMenuItemsArray;
var
  i, Nr: Integer;
  C: TComponent;
  N: TComponentName;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    C := Components[i];
    if (C is TMenuItem) then
    begin
      N := UpperCase(C.Name);
      if Pos('MNUFILEMRU',N) = 1 then
      begin
        System.Delete(N,1,Length('mnuFileMru'));
        if TryStrToInt(N, Nr) then
        begin
          if (Nr in [0..MruEntries - 1]) then
          begin
            MruMenuItems[Nr] := TMenuItem(C);
            MruMenuItems[Nr].Tag := MruMenuItems[Nr].Tag or (Nr shl 4);
            //Debugln('Menu: ',C.Name,' -> MruMenuItems[',DbgS(Nr),']');
          end;
        end;
      end;
    end;
  end;
end;

function TEPlusForm.TryHlMenuTagToFileType(ATag: PtrInt; out
  AFileType: TEditorFileType): Boolean;
begin
  AFileType := eftNone;
  //dbgout('Tg = ',IntToHex(Tg,8),' -> ');
  ATag := ATag and $FF00;
  //dbgout('Tg = ',IntToHex(Tg,8),' -> ');
  ATag := ATag shr 8;
  //debugln('Tg = ',IntToHex(Tg,8));
  Result := not (ATag < Ord(Low(TEditorFileType))) or (ATag > Ord(High(TEditorFileType)));
  if Result then AFileType := TEditorFileType(ATag);
end;



procedure TEPlusForm.TagMenuItemsAndActions;
var
  i: Integer;
  C: TComponent;
  N: TComponentName;
  //NE: Boolean;
  //NS: Boolean;
  Index: TEditorFileType;
begin
  for i := 0 to self.ComponentCount - 1 do
  begin
    C := Self.Components[i];
    N := C.Name;
    if (C is TAction) or (C is TMenuItem) then
    begin
      //Things that need an open editor
      if Pos('LAYOUT',UpperCase(N)) > 0 then C.Tag := C.Tag or tgNeedsEditor;
      if Pos('EDIT',UpperCase(N)) > 0 then C.Tag := C.Tag or tgNeedsEditor;
      if Pos('INSERT',UpperCase(N)) > 0 then C.Tag := C.Tag or tgNeedsEditor;
      if Pos('FILESAVE',UpperCase(N)) > 0 then C.Tag := C.Tag or tgNeedsEditor;
      if Pos('CURRENT',UpperCase(N)) > 0 then C.Tag := C.Tag or tgNeedsEditor;
      if Pos('GROUPING',UpperCase(N)) > 0 then C.Tag := C.Tag or tgNeedsEditor;
      if Pos('HIGHLIGHTER',UpperCase(N)) > 0 then C.Tag := C.Tag or tgNeedsEditor;
      if Pos('FONT',UpperCase(N)) > 0 then C.Tag := C.Tag or tgNeedsEditor;
      if Pos('VIEWHL',UpperCase(N)) > 0 then
      begin
        C.Tag := C.Tag or tgNeedsEditor;
        for Index := Low(TEditorFileType) to High(TEditorFileType) do
        begin
          //need exact match!
          if ('MNUVIEWHL' + UpperCase(eftNames[Index]) = UpperCase(N)) then
          begin
            C.Tag := C.Tag or (Ord(Index) shl 8);
            //debugln(Format('%-25s',[N]),' ->', IntToHex(C.Tag,8));
          end;
        end;
      end;
      //Things that need text on the clipboard
      if Pos('PASTE',UpperCase(N)) > 0 then C.Tag := C.Tag or tgNeedsClipPaste;
      //Things that need a selection in the editor
      if Pos('COPY', UpperCase(N)) > 0 then C.Tag := C.Tag or tgNeedsSelection;
      if Pos('CUT', UpperCase(N)) > 0 then C.Tag := C.Tag or tgNeedsSelection;
      //Things that don't need an open editor
      if Pos('FILENEW',UpperCase(N)) > 0 then C.Tag := C.Tag and (not tgNeedsEditor);
      if Pos('FILEOPEN',UpperCase(N)) > 0 then C.Tag := C.Tag and (not tgNeedsEditor);
      if Pos('APP',UpperCase(N)) > 0 then C.Tag := C.Tag and (not tgNeedsEditor);

      //Exceptions and fixes aftre the applied rules above
      //needs to be _after_ 'OPEN'
      if Pos('BROWSER',UpperCase(N)) > 0 then C.Tag := C.Tag or tgNeedsEditor;
    end;
    //Top level menus
    mnuFile.Tag := 0;
    mnuEdit.Tag := 0;
    mnuHTMLTools.Tag := 0;
    mnuGrouping.Tag := 0;
    mnuView.Tag := 0;
    mnuAbout.Tag := 0;

    {
    if (C is TMenuItem) or (C is TAction) then
    begin
      NE := (C.Tag and tgNeedsEditor) > 0;
      NS := (C.Tag and tgNeedsSelection) > 0;
      debugln(C.Name,' -> NeedsEditor = ',DbgS(NE),'  NeedsSelection = ',DbgS(NS));
    end;
    }
  end;
  //Doesn't work in OI on Win9x
  acEditCopy.Caption := 'Kopiëren';
end;

procedure TEPlusForm.UpdateMenuItems;
var
  i: Integer;
  C: TComponent;
  HasEditor, HasSelection, HasClipPaste: Boolean;
  NeedsEditor, NeedsSelection, NeedsClipPaste: Boolean;
begin
  HasEditor := Assigned(NoteBook.CurrentEditor);
  HasSelection := HasEditor and (NoteBook.CurrentEditor.SelAvail);
  HasClipPaste := (ClipBoard.AsText <> EmptyStr);
  for i := 0 to ComponentCount - 1 do
  begin
    C := Components[i];
    if (C is TMenuItem) then
    begin
      NeedsEditor := (C.Tag and tgNeedsEditor) > 0;
      NeedsSelection := (C.Tag and tgNeedsSelection) > 0;
      NeedsClipPaste := (C.Tag and tgNeedsClipPaste) > 0;
      TMenuItem(C).Enabled := ((NeedsEditor and HasEditor) or (not NeedsEditor)) and
                              ((NeedsSelection and HasSelection) or (not NeedsSelection)) and
                              ((NeedsClipPaste and HasClipPaste) or (not NeedsClipPaste));
    end;
  end;
end;




{ ******************** [ NoteBook / Editor event handlers ] ************************ }


procedure TEPlusForm.OnBeforeCloseEditor(Sender: TTabSheet; var Cancel: Boolean);
var
  Ed: TEditor;
  Fn: String;
  Res: TModalResult;
begin
  Cancel := False;
  Ed := NoteBook.EditorAtPage(Sender);
  if Assigned(Ed) then
  begin
    if (Ed.Modified) {or (Ed.FileName = EmptyStr)} then
    begin
      Fn := Ed.FileName;
      if Fn = EmptyStr then Fn := Sender.Caption;  //this will differentiate between Noname and Noname [2]
      Res :=  MessageDlg(AppName, Format(msgModifiedSave,[Fn]), mtConfirmation, [mbYes,mbNo,mbCancel], 0);
      case Res of
        mrNo: Cancel := False;
        mrCancel: Cancel := True;
        mrOk: Cancel := (TryFileSave(Ed, Ed.FileName) <> IoSuccess);
      end;
    end;
  end;
end;


procedure TEPlusForm.OnEditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  Line, Col: LongInt;
begin
  if (Sender = nil) or (not (Sender is TEditor)) then
  begin
    StatusBar.Panels[pXY].Text := '';
    StatusBar.Panels[pMod].Text := '';
    StatusBar.Panels[pIns].Text := '';
    StatusBar.Panels[pName].Text := '';
    Exit;
  end;
  with (Sender as TEditor) do
  begin
    if (scCaretX in Changes) or (scCaretY in Changes) then
    begin
      Col := CaretX;
      Line := CaretY;
      StatusBar.Panels[pXY].Text := Format('%s %-3d  %s %-3d',[SLine,Line,SCol,Col]);
    end;
    if (scModified in Changes) then
    begin
      if Modified then StatusBar.Panels[pMod].Text := SModified
      else StatusBar.Panels[pMod].Text := '';
    end;
    if (scInsertMode in Changes) then
    begin
      if InsertMode then StatusBar.Panels[pIns].Text := SIns
      else StatusBar.Panels[pIns].Text := SOvr;
    end;
    if (scFileName in Changes) then
    begin
      StatusBar.Panels[pName].Text := FileName;
    end;
  end;
end;

procedure TEPlusForm.OnCharsetChange(Sender: TEditor; const OldCharset, NewCharset: String; const LineNr: Integer);
begin
  MessageDlg(Format('Let op: html charset is gewijzigd van %s naar %s'^m'(Regel %d)',[OldCharset, NewCharset, LineNr]),
             mtInformation, [mbOk], 0);
end;

procedure TEPlusForm.OnMruListChange(Sender: TObject);
var
  i: Integer;
  MnuItem: TMenuItem;
  Fn: String;
begin
{  for i := 0 to MruList.Count - 1 do
  begin
    MnuItem := MruMenuItems[i];
    Fn := ExtractFileName(MruList.Items[i]);
    MnuItem.Caption := Format('&%d %s',[i+1,Fn]);
    MnuItem.Hint := MruList.Items[i];
    MnuItem.Enabled := True;
    MnuItem.Visible := True;
  end;
  //Empty entries
  for i := MruList.Count to MruEntries - 1 do
  begin
    MnuItem := MruMenuItems[i];
    MnuItem.Caption := '';
    MnuItem.Hint := '';
    MnuItem.Enabled := False;
    MnuItem.Visible := False;
  end;
  mnuSepAboveMru.Enabled := MruList.Count > 0;
  mnuSepAboveMru.Visible := MruList.Count > 0;}
end;



{ ********************* [ CharMap event handlers ] ***************************** }

procedure TEPlusForm.OnHtmlCharMapInsertText(AValue: String);
var
  Ed: TEditor;
begin
  //Debugln('TForm1.HtmlCharMapInsertText: AValue = ',AValue);
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) and (AValue <> '') then
  begin
    Ed.SelText := AValue;
  end;
end;





{ ******************** [ General procedures/functions ] ************************ }


procedure TEPlusForm.ShowError(const Msg: String);
begin
  MessageDlg(AppName, Msg, mtError, [mbOk], 0);
end;

function TEPlusForm.TryMarkSelection(const Pre, Post: String): Boolean;
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    Ed.MarkSelection(Pre, Post);
    Result := True;
  end
  else Result := False;
end;




{ *********************** [ File handling ] *************************** }





function TEPlusForm.AskFileNameOpen: String;
begin
  OpenDialog.Filter := OpenSaveFilter;
  OpenDialog.FilterIndex := 0;
  if OpenDialog.Execute then
  begin
    Result := OpenDialog.FileName;
    AppOptions.MainForm.InitialDir := ExtractFileDir(OpenDialog.FileName);
  end
  else Result := EmptyStr;
end;

function TEPlusForm.AskFileNameOpenTemplate: String;
begin
  OpenDialog.Filter := TemplateFilter;
  OpenDialog.FilterIndex := 0;
  if OpenDialog.Execute then
  begin
    Result := OpenDialog.FileName;
    AppOptions.MainForm.InitialDir := ExtractFileDir(OpenDialog.FileName);
  end
  else Result := EmptyStr;
end;

function TEPlusForm.AskFileNameSave(const Fn: String; const FileType: TEditorFileType): String;
begin
  SaveDialog.Filter := OpenSaveFilter;
  SaveDialog.FilterIndex := 0;
  if (Fn <> EmptyStr) then
  begin
    SaveDialog.FileName := ExtractFileName(Fn);
    SaveDialog.InitialDir := ExtractFileDir(Fn);
    //f/SaveDialog.FilterIndex := GetFilterIndexFromFileName(SaveDialog.Filter, Fn);
    //f/SaveDialog.DefaultExt := GetExtensionFromFilterAtIndex(SaveDialog.Filter, SaveDialog.FilterIndex);
  end
  else
  begin
    if (FileType <> eftNone) then SaveDialog.FilterIndex := Ord(FileType);
    //f/SaveDialog.DefaultExt := GetExtensionFromFilterAtIndex(SaveDialog.Filter, SaveDialog.FilterIndex);
  end;
  //debugln('SaveDialog.FilterIndex = ',dbgs(SaveDialog.FilterIndex));
  //debugln('SaveDialog.DefaultExt = ',SaveDialog.DefaultExt);
  if SaveDialog.Execute then
  begin
    Result := SaveDialog.FileName;
    AppOptions.MainForm.InitialDir := ExtractFileDir(SaveDialog.FileName);
  end
  else Result := EmptyStr;
end;

function TEPlusForm.AskFileNameSaveTemplate: String;
begin
  SaveDialog.Filter := TemplateFilter;
  SaveDialog.FilterIndex := 0;
  if SaveDialog.Execute then
  begin
    Result := SaveDialog.FileName;
    AppOptions.MainForm.InitialDir := ExtractFileDir(SaveDialog.FileName);
  end
  else Result := EmptyStr;
end;

function TEPlusForm.TryFileOpen(const Fn: String; const AsTemplate: Boolean = False): Boolean;
var
  Ed: TEditor;
begin
  //Return False only on Errors
  {if not IsASCIIFileUtf8(Fn) then
  begin
    if MessageDlg(AppName, Format(msgFileIsNotText,[Fn]),
         mtConfirmation, [mbYes, mbNo], 0, mbNo) <> mrYes then
    begin
      Result := True; //not an Error
      Exit;
    end;
  end;}
  Ed := NoteBook.AddPage;
  if Assigned(Ed) then
  begin
    try
      Ed.LoadFromFileUtf8(Fn, AsTemplate);
      //OnEditorStatusChange(Ed, scAll);
      MruList.Add(Fn);
      Result := True;
    except
      Result := False;
      //OnEditorStatusChange(nil, scAll);
    end;
  end
  else Result := False;
end;

function TEPlusForm.TryFileSave(Editor: TEditor; Fn: String): TIoResult;
begin
  if Assigned(Editor) then
  begin
    if (Fn = EmptyStr) then
    begin
      Fn := AskFileNameSave(EmptyStr, Editor.FileType);
      if (Fn = EmptyStr) then Exit(IoCancel);
    end;
    try
      Editor.SaveToFileUtf8(Fn);
      //OnEditorStatusChange(Editor, scAll);
      MruList.Add(Fn);
      Result := IoSuccess;
    except
      Result := IoFail;
      //OnEditorStatusChange(nil, scAll);
    end;
  end
  else Result := IoFail;
end;

function TEPlusForm.CloseCurrentEditor: Boolean;
var
  Idx: Integer;
begin
  if (NoteBook.PageCount = 0) then Exit(True);

  Idx := NoteBook.ActivePageIndex;
  Result := NoteBook.ClosePage(Idx);
end;

function TEPlusForm.TryFileSaveAll(out Failures: String): Boolean;
var
  i: Integer;
  Ed: TEditor;
  Success: Boolean;
begin
  Result := True;
  Failures := '';
  for i := 0 to NoteBook.PageCount - 1 do
  begin
    Ed := NoteBook.EditorAtPage(i);
    if Assigned(Ed) then
    begin
      if Ed.Modified or (Ed.FileName = EmptyStr) then
        //TryFileSave will prompt for a filename if necessary
      begin
        Success := TryFileSave(Ed, Ed.FileName) = IoSuccess;
        //Debugln('TryFileSaveAll: i = ',dbgs(i),' Caption = ',NoteBook.Pages[i].Caption,' Success = ',dbgs(success));
        if not Success then Failures := Failures + NoteBook.Pages[i].Caption + LineEnding;
        Result := Result and Success;
      end;
    end;
  end;
end;

procedure TEPlusForm.DoFileOpen;
var
  Fn: String;
begin
  Fn := AskFileNameOpen;
  if (Fn = EmptyStr) then Exit;
  if FileExistsUtf8(Fn) then
  begin
    if not TryFileOpen(Fn, False) then ShowError(Format(msgOpenError,[Fn]));
  end
  else
    ShowError(Format(msgFileNotFound,[Fn]));
end;

procedure TEPlusForm.DoMruOpen(const Index: Integer);
var
  Fn: String;
begin
  if (Index < 0) or (Index > MruList.Count - 1) then
  begin
    ShowError(Format(msgMruIndexOutOfBound,[Index]));
  end;
  Fn := MruList.Strings[Index];
  if (Fn <> '') then
  begin
    if not TryFileOpen(Fn, False) then ShowError(Format(msgOpenError,[Fn]));
  end
end;

procedure TEPlusForm.DoTemplateOpen;
var
  Fn: String;
begin
  Fn := AskFileNameOpenTemplate;
  if (Fn = EmptyStr) then Exit;
  if FileExistsUtf8(Fn) then
  begin
    if not TryFileOpen(Fn, True) then ShowError(Format(msgOpenError,[Fn]));
  end
  else
    ShowError(Format(msgFileNotFound,[Fn]));
end;

procedure TEPlusForm.DoFileSave(Editor: TEditor);
begin
  if Assigned(Editor) then
  begin
    if (TryFileSave(Editor, Editor.FileName) = IoFail) then ShowError(Format(msgSaveError,[Editor.FileName]));
  end;
end;

procedure TEPlusForm.DoFileSaveAs(Editor: TEditor);
var
  Fn: String;
begin
  if Assigned(Editor) then
  begin
    Fn := AskFileNameSave(Editor.FileName, Editor.FileType);
    if (Fn <> EmptyStr) then
    begin
      if (TryFileSave(Editor, Fn) = IoFail) then ShowError(Format(msgSaveError,[Fn]));
    end;
  end;
end;

procedure TEPlusForm.DoFileSaveAsTemplate(Editor: TEditor);
var
  Fn: String;
begin
  if Assigned(Editor) then
  begin
    Fn := AskFileNameSaveTemplate;
    if (Fn <> EmptyStr) then
    begin
      if (TryFileSave(Editor, Fn) = IoFail) then ShowError(Format(msgSaveError,[Fn]));
    end;
  end;
end;

procedure TEPlusForm.DoFileSaveAll;
var
  S: String;
begin
  if NoteBook.PageCount = 0 then Exit;
  if not TryFileSaveAll(S) then
  begin
    S := Trim(S);   //get rid of trailing LineEndings
    ShowError(Format(msgSaveAllError,[S]));
  end;
end;

procedure TEPlusForm.DoFileNewByType(const AFileType: TEditorFileType; const InitialText: String = '');
var
  Ed: TEditor;
begin
  Ed := NoteBook.AddPage;
  if Assigned(Ed) then
  begin
    Ed.FileType := AFileType;
    if (InitialText <> EmptyStr) then
      Ed.TextBetweenPoints[Point(0,0),Point(0,0)] := InitialText
    else
      Ed.Modified := False;
  end;
end;

procedure TEPlusForm.DoFileNewHtml;
begin
  if NewHtmlDlg.Execute then DoFileNewByType(eftHtml, NewHtmlDlg.Text);
end;


procedure TEPlusForm.FileOpenInBrowser;
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    if (Ed.FileName = EmptyStr) then
    begin
      ShowError(msgFileHasNoName);
      Exit;
    end;
    if not (Ed.FileType in [eftHtml, eftXml]) then
    begin
      if (MessageDlg(AppName, msgFileTypeNotForBrowser, mtConfirmation, [mbYes,mbNo], 0, mbNo) <> mrYes) then
        Exit;
    end;
    if not OpenUrl(Ed.FileName) then ShowError(Format(msgErrorBrowser, [Ed.FileName]));
  end;
end;



{ ********************* [ Edit ] ********************************* }


procedure TEPlusForm.EditUndo;
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    if Ed.CanUndo then Ed.Undo;
  end;
end;

procedure TEPlusForm.EditRedo;
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    if Ed.CanRedo then Ed.Redo;
  end;
end;

procedure TEPlusForm.EditCopy;
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    if not Ed.SelAvail then Ed.SelectWord;
    Ed.CopyToClipboard;
  end;
end;

procedure TEPlusForm.EditPaste;
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    Ed.PasteFromClipboard;
  end;
end;


procedure TEPlusForm.EditCut;
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    if Ed.SelAvail then Ed.CutToClipboard;
  end;
end;

procedure TEPlusForm.EditSelectAll;
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    Ed.SelectAll;
  end;
end;

procedure TEPlusForm.EditFind;
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    FindDialog.Execute;
  end;
end;

procedure TEPlusForm.EditFindNext;
var
  sOpt: TSynSearchOptions;
  Ed: TEditor;
begin
  if (FindText = EmptyStr) then Exit;
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    sOpt := FindOptions;
    sOpt := sOpt - [ssoBackWards];
    if Ed.SearchReplace(FindText,'',sOpt) = 0 then
      ShowError(Format(msgTextNotFound,[FindText]));
  end;
end;

procedure TEPlusForm.EditFindPrevious;
var
  sOpt: TSynSearchOptions;
  Ed: TEditor;
begin
  if (FindText = EmptyStr) then Exit;
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    sOpt := FindOptions;
    sOpt := sOpt + [ssoBackWards];
    if Ed.SearchReplace(FindText,'',sOpt) = 0 then
      ShowError(Format(msgTextNotFound,[FindText]));
  end;
end;

procedure TEPlusForm.EditReplace;
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    ReplaceDialog.Execute;
  end;
end;

procedure TEPlusForm.EditPasteTableContentTab;
var
  Ed: TEditor;
  TabbedText: String;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    TabbedText := ClipBoard.AsText;
    if (TabbedText <> EmptyStr) then Ed.SelText := TabbedTextToHtmlTableContent(TabbedText);
  end;
end;






procedure TEPlusForm.DoFind(Sender: TObject);
var
  Ed: TEditor;
  Dlg: TFindDialog;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    Dlg := (Sender as TFindDialog);
    ReplaceOptions := [];
    if not (frDown in Dlg.Options) then ReplaceOptions := ReplaceOptions + [ssoBackWards];
    //if frFindNext         in Options then sOptions := sOptions + [];
    if frMatchCase in Dlg.Options then ReplaceOptions := ReplaceOptions + [ssoMatchCase];
    //if frReplace          in Options then sOptions := sOptions + [];
    //if frReplaceAll       in Options then sOptions := sOptions + [];
    if frWholeWord in Dlg.Options then ReplaceOptions := ReplaceOptions + [ssoWholeWord];

    FindText := Dlg.FindText;

    if Ed.SearchReplace(FindText,'',FindOptions) = 0 then
      ShowError(Format(msgTextNotFound,[FindText]));

  end;
end;

procedure TEPlusForm.DoReplace(Sender: TObject);
var
  Ed: TEditor;
  Dlg: TReplaceDialog;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    Dlg := (Sender as TReplaceDialog);
    FindOptions := [];
    if not (frDown in Dlg.Options) then FindOptions := FindOptions + [ssoBackWards];
    if frMatchCase in Dlg.Options then FindOptions := FindOptions + [ssoMatchCase];
    if frWholeWord in Dlg.Options then FindOptions := FindOptions + [ssoWholeWord];
    if frReplace in Dlg.Options then ReplaceOptions := ReplaceOptions + [ssoReplace];
    if frReplaceAll in Dlg.Options then ReplaceOptions := ReplaceOptions + [ssoReplaceAll];
    if frFindNext in Dlg.Options then ReplaceOptions := ReplaceOptions - [ssoReplace,ssoReplaceAll];
    if Ed.SelAvail then ReplaceOptions := ReplaceOptions + [ssoSelectedOnly];

    FindText := Dlg.FindText;
    ReplaceText := Dlg.ReplaceText;

    if Ed.SearchReplace(FindText,ReplaceText,ReplaceOptions) = 0 then
      ShowError(Format(msgTextNotFound,[FindText]))
    else
    if (ssoReplace in ReplaceOptions) and not (ssoReplaceAll in ReplaceOptions) then
    begin
      ReplaceOptions := ReplaceOptions - [ssoReplace];
      Ed.SearchReplace(FindText,'',ReplaceOptions); //Search and select next occurence
    end;
  end;
end;


{ **************************** [ Insert ] ************************** }



procedure TEPlusForm.InsertAnchor;
var
  Ed: TEditor;
  SStart, SLen: Integer;
  OldSelMode: TSynSelectionMode;
  OldDescr, NewDescr: String;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    OldDescr := Ed.SelText;
    AnchorDlg.Description := OldDescr;
    if AnchorDlg.Execute then
    begin
      NewDescr := AnchorDlg.Description;
      if (OldDescr <> NewDescr) then
      begin
        OldSelMode := Ed.SelectionMode;
        Ed.SelectionMode := smNormal;
        //save SelStart
        SStart := Ed.SelStart;
        Ed.SelText := NewDescr;
        SLen := Utf8Length(NewDescr);
        //Select inserted text
        Ed.SelStart := SStart;
        Ed.SelEnd := SStart + SLen;
        Ed.SelectionMode := OldSelMode;
      end;
      //Add html tags
      Ed.MarkSelection(AnchorDlg.LinkRef, anchor_end)

    end;
  end;
end;

procedure TEPlusForm.InsertPicture;
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    if PictureDlg.Execute then
    begin
      Ed.BlockEnd := Ed.BlockBegin;
      Ed.SelText := Img(PictureDlg.FileName, PictureDlg.DomId, PictureDlg.DomClass, PictureDlg.FloatStyle,
                        PictureDlg.PicWidth, PictureDlg.PicHeight, PictureDlg.Alt, PictureDlg.Title);
    end;

  end;

end;


procedure TEPlusForm.InsertTable;
var
  Ed: TEditor;
  TableText: String;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    if TableDlg.Execute then
    begin
      TableText := CreateTable(TableDlg.DomId, TableDlg.DomClass, TableDlg.Summary,
                               TableDlg.ColCount, TableDlg.RowCount);
      Ed.SelEnd := Ed.SelStart;
      Ed.Seltext := TableText;
    end;
  end;
end;


procedure TEPlusForm.InsertSpecialChars;
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    HtmlCharMapDlg.Execute;
  end;
end;


procedure TEPlusForm.InsertSpecial(const AValue: String);
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    Ed.SelText := AValue;
  end;
end;

{ *************************** [ Layout ] **************************** }



procedure TEPlusForm.LayoutHeading(const Level: Integer);
begin
  if not (Level in [1..6]) then Exit;
  TryMarkSelection(Format('<h%d>',[Level]),Format('</h%d>',[Level]));
end;


{ ************************** [ View ] ********************************* }

procedure TEPlusForm.SetHighlighter(const AFileType: TEditorFileType);
var
  Ed: TEditor;
begin
  Ed := NoteBook.CurrentEditor;
  if Assigned(Ed) then
  begin
    //debugln('Setting Highlighter to ',eftNames[AFileType]);
    Ed.SetHighlighterByFileType(AFileType, True);
  end;
end;

procedure TEPlusForm.EditorSelectFont;
var
  EO: TEditorOptions;
begin
  if Assigned(NoteBook.CurrentEditor) then
  begin
    EO := NoteBook.EditorOptions;
    FontDialog.Font.Name := EO.FontName;
    FontDialog.Font.Size := EO.FontSize;
    if FontDialog.Execute then
    begin
      EO.FontName := FontDialog.Font.Name;
      EO.FontSize := FontDialog.Font.Size;
      NoteBook.EditorOptions := EO;
    end;
  end;
end;

procedure TEPlusForm.EditorFontSizeUp;
var
  EO: TEditorOptions;
begin
  if Assigned(NoteBook.CurrentEditor) then
  begin
    EO := NoteBook.EditorOptions;
    EO.FontSize := EO.FontSize + 2;
    NoteBook.EditorOptions := EO;
  end;
end;

procedure TEPlusForm.EditorFontSizeDown;
var
  EO: TEditorOptions;
begin
  if Assigned(NoteBook.CurrentEditor) then
  begin
    EO := NoteBook.EditorOptions;
    EO.FontSize := EO.FontSize - 2;
    if EO.FontSize < 0 then EO.FontSize := 2;
    NoteBook.EditorOptions := EO;
  end;
end;


{ ************************** [ About ] *********************** }

procedure TEPlusForm.AboutEPlus;
{var
  Dlg: TExtAboutDlg;}
begin
{  Dlg := TExtAboutDlg.Create;
  try
    Dlg.Caption := AboutTitle;
    Dlg.ProductName := AppName;
    Dlg.Version := AppVersion;
    Dlg.CopyrightStatement := CopyLeftStatement;
    Dlg.WebsiteCaption := AuthorWebName;
    Dlg.WebsiteURL := AuthorWebUrl;
    Dlg.LicenseText := LicenseText;
    Dlg.LicenseWebsiteCaption := LicenseName;
    Dlg.LicenseWebsiteURL := LicenseUrl;
    Dlg.Top := Top + 20;
    Dlg.Left := Left + 20;
    Dlg.Execute;
  finally
    Dlg.Free;
  end;}
end;


end.

