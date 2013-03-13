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

unit lazedit_translations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTranslations }

  TLanguageIds = (lidEnglish, lidDutch, lidPortuguese);

const
  MenuLangNameSuffixes: Array[TLanguageIds] of string = ('English','Dutch','Portuguese');

type

  TTranslations = class(TObject)
  private
    FLanguageId: TLanguageIds;
  public
    { Main form }
    {  mnuEditPasteTableContentTab: TMenuItem;
      mnuEditPasteSpecial: TMenuItem;
      mnuAbout: TMenuItem;
      mnuViewFont: TMenuItem;}
    //File menu
    mnuFile,
      mnuFileOpen,
      mnuFileNewFromTemplate,
      mnuFileNew,
        mnuFileNewText,
{        mnuFileNewHtml: TMenuItem;
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
      mnuFileSave,
      mnuFileSaveAs,
      mnuFileSaveAll,
      mnuFileCloseCurrent,
      //mnuSep1: TMenuItem;
      mnuFileOpenInBrowser,
      //mnuSepAboveMru: TMenuItem;
      mnuFileCloseApp: string;
    //Edit menu
    mnuEdit,
      mnuEditUndo,
      mnuEditRedo,
      //mnuSep11: TMenuItem;
      mnuEditCopy,
      mnuEditCut,
      mnuEditPaste,
      mnuEditPasteSpecial,
        mnuEditPasteTableContentTab,
      mnuEditSelectAll,
      //mnuSep12: TMenuItem;
      mnuEditReplace,
      mnuEditFindNext,
      mnuEditFind: string;
    //Insert menu
    mnuHTMLTools,
      mnuInsertAnchor,
      mnuInsertList,
        mnuInsertUList,
        mnuInsertNList,
        mnuInsertWordList,
        mnuInsetListItem,
        mnuInsertWordTerm,
        mnuInsertWordDefinition,
      mnuInsertTable,
        mnInsertNewTable,
        mnuInsertTableCell,
        mnuInsertTableRow,
      mnuInsertPicture,
      mnuInsertSpecialChars,
      mnuInsertLineBreak,
      //mnuInsertSep1: TMenuItem;
      mnuInsertHtmlComment,
      mnuInsertJS,
      mnuInsertCssStyle: string;
    //Layout menu
    mnuLayout,
      mnuLayoutBold,
      mnuLayoutAlignJustify,
      mnuLayoutItalic,
      mnuLayoutUnderline,
      mnuLayoutSub,
      mnuLayoutSup,
      mnuLayoutEmphasis,
      mnuLayoutStrong,
      mnuLayoutHeadings,
{        mnuLayoutH1: TMenuItem;
        mnuLayoutH2: TMenuItem;
        mnuLayoutH3: TMenuItem;
        mnuLayoutH4: TMenuItem;
        mnuLayoutH5: TMenuItem;
        mnuLayoutH6: TMenuItem;}
      mnuLayoutAlign,
        mnuLayoutAlignLeft,
        mnuLayoutAlignRight,
        mnuLayoutAlignCenter,
      mnuLayoutCode,
      mnuLayoutQuote,
      mnuLayoutBlockQuote,
      mnuLayoutPreformatted: string;
    //Grouping menu
    mnuGrouping,
      mnuGroupingParagraph,
      mnuGroupingDiv,
      mnuGroupingSpan: string;
    //View menu
    mnuView,
      mnuViewFontsize,
        mnuViewFontSizeUp,
        mnuViewFontsizeDown,
//      mnuViewHighlighter: TMenuItem;
        //these menu items MUST have names that are built like this:
        //'mnuViewHL' + eftNames[SomeIndex]
        mnuViewHLeftNone: string;
{        mnuViewHLeftHtml: TMenuItem;
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
        mnuViewHLeftPo: TMenuItem;}
    mnuTools,
      mnuToolsLanguage,
      mnuToolsToolbars,
        mnuToolbarsMain,
        mnuToolbarsHTML: string;
{    //Popup menus
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
    // Other main.pp constants
    SLine, SCol, SModified, SIns, SOvr,
     msgOpenError, msgSaveError, msgSaveAllError, msgFileIsNotText,
     msgFileNotFound, msgModifiedSave, msgMruIndexOutOfBound,
     msgFileTypeNotForBrowser, msgFileHasNoName, msgErrorBrowser,
     msgTextNotFound: string;
    // main.pp hints in toolbar buttons
    NewFromTemplateBtn, NewPlainBtn, OpenBtn, SaveBtn, SaveAllBtn,
     CopyBtn, PasteBtn, FindBtn, InfoBtn,
     AnchorBtn, ImageBtn, UListBtn, NListBtn, ListItemBtn, TableBtn,
     BoldBtn, ItalicBtn, UnderlineBtn, EmBtn, StrongBtn, SupBtn, SubBtn,
     HeadingBtn, LeftAlignBtn, RightAlignBtn, CenterAlignBtn, JustifyAlignBtn,
     ParaBtn, DivBtn, SpanBtn: string;
    // Other constants
    NoName: string;
    { About box strings }
//    lpSupport, lpSupportInfo, lpLicense, lpLicenseInfo, lpAuthors,
//     lpContributorsTitle, lpAboutWindow, lpClose, lpInformation: string;
    { Methods }
    procedure TranslateToEnglish;
    procedure TranslateToDutch;
    procedure TranslateToPortuguese;
    procedure TranslateToLanguageID(ALangId: TLanguageIds);
    function GetCurrentLanguageID: TLanguageIds;
  end;

var
  vTranslations: TTranslations;

implementation

{ TTranslations }

procedure TTranslations.TranslateToEnglish;
begin
  mnuFile := '&File';
    mnuFileOpen := '&Open ...';
    mnuFileNewFromTemplate := 'New from template ...';
    mnuFileNew := '&New';
          mnuFileNewText := 'Plain text';
  {   mnuFileNewHtml: TMenuItem;
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
    mnuFileSave := '&Save';
    mnuFileSaveAs := 'Save &As';
    mnuFileSaveAll := 'Save all';
    mnuFileCloseCurrent := '&Close current';
    //mnuSep1: TMenuItem;
    mnuFileOpenInBrowser := 'Open in browser';
    //mnuSepAboveMru: TMenuItem;
    mnuFileCloseApp := 'Close application';
  //Edit menu
  mnuEdit := '&Edit';
    mnuEditUndo := '&Undo';
    mnuEditRedo := '&Redo';
    //mnuSep11: TMenuItem;
    mnuEditCopy := '&Copy';
    mnuEditCut := 'Cu&t';
    mnuEditPaste := '&Paste';
    mnuEditPasteSpecial := 'Paste special';
      mnuEditPasteTableContentTab := 'Paste table content';
    mnuEditSelectAll := 'Select &All';
    //mnuSep12: TMenuItem;
    mnuEditReplace := '&Replace';
    mnuEditFindNext := 'Find &Next';
    mnuEditFind := '&Find';
  //HTML Tools menu
  mnuHTMLTools := '&HTML Tools';
    mnuInsertAnchor := 'Insert Hyperlink';
      mnuInsertList := 'Insert list...';
      mnuInsertUList := 'Bullets list';
      mnuInsertNList := 'Numeric list';
      mnuInsertWordList := 'List of words';
      mnuInsetListItem := 'List item';
      mnuInsertWordTerm := 'Word term';
      mnuInsertWordDefinition := 'Word definition';
    mnuInsertTable := 'Insert Table';
      mnInsertNewTable := 'New table';
      mnuInsertTableCell := 'Table cell';
      mnuInsertTableRow := 'Table row';
    mnuInsertPicture := 'Insert Picture';
    mnuInsertSpecialChars := 'Insert special chars';
    mnuInsertLineBreak := 'Insert line break';
    //mnuInsertSep1: TMenuItem;
    mnuInsertHtmlComment := 'Insert HTML comment';
    mnuInsertJS := 'Insert Javascript';
    mnuInsertCssStyle := 'Insert CSS Style';
    //Layout menu
    mnuLayout := '&Layout';
      mnuLayoutBold := '&Bold';
      mnuLayoutItalic := '&Italic';
      mnuLayoutUnderline := '&Underline';
      mnuLayoutSub := 'Subscript';
      mnuLayoutSup := 'Superscript';
      mnuLayoutEmphasis := 'Emphasis';
      mnuLayoutStrong := 'Strong';
      mnuLayoutHeadings := 'Headings';
  {        mnuLayoutH1: TMenuItem;
        mnuLayoutH2: TMenuItem;
        mnuLayoutH3: TMenuItem;
        mnuLayoutH4: TMenuItem;
        mnuLayoutH5: TMenuItem;
        mnuLayoutH6: TMenuItem;}
      mnuLayoutAlign := 'Alignment';
        mnuLayoutAlignLeft := '&Left aligned';
        mnuLayoutAlignRight := '&Right aligned';
        mnuLayoutAlignCenter := '&Centered';
        mnuLayoutAlignJustify := '&Justified';
      mnuLayoutCode := 'Code';
      mnuLayoutQuote := 'Quote';
      mnuLayoutBlockQuote := 'Block Quote';
      mnuLayoutPreformatted := 'Preformatted';
    //Grouping menu
    mnuGrouping := '&Grouping';
      mnuGroupingParagraph := 'Paragraph';
      mnuGroupingDiv := 'Div';
      mnuGroupingSpan := 'Span';
  //View menu
  mnuView := '&View';
    mnuViewFontsize := '&Font Size';
      mnuViewFontSizeUp := 'Bigger';
      mnuViewFontsizeDown := 'Smaller';
    //mnuViewHighlighter: TMenuItem;
      mnuViewHLeftNone := 'None';
  mnuTools := '&Tools';
    mnuToolsLanguage := 'Language';
    mnuToolsToolbars := 'Toolbars';
    mnuToolbarsMain := 'Main Toolbar';
    mnuToolbarsHTML := 'HTML Toolbar';

  //messages
  SLine := 'Line';
  SCol := 'Col';
  SModified := 'Modified';
  SIns := 'INS';
  SOvr := 'OVR';
  msgOpenError := 'The following open file error has occured:'^m'%s';
  msgSaveError := 'The following save file error has occured:'^m'%s';
  msgSaveAllError := 'The following save all error has occured:'^m'%s';
  msgFileIsNotText := 'The selected file '^m'%s'^m' does not seam to be a text file.';
  msgFileNotFound := 'File not found:'^m'%s';
  msgModifiedSave := 'The following file was modified:'^m'%s'^m'Should it be saved?';
  msgMruIndexOutOfBound := 'Index out of bounds [%d]'^m;
  msgFileTypeNotForBrowser := 'The file type is not suited for a browser.'^m+'Continue anyway?';
  msgFileHasNoName := 'The file has no name.'^m +
                     'You must first save the file in order to open it in the browser.';
  msgErrorBrowser := 'An error has occured while opening'^m+
                    '%s'^m'in the browser.';
  msgTextNotFound := 'Text not found:'^m'"%s"';

  // main.pp hints in toolbar buttons
  NewFromTemplateBtn := mnuFileNewFromTemplate;
  NewPlainBtn := 'New plain text file' {mnuFileNew};
  OpenBtn := 'Open' {mnuFileOpen};
  SaveBtn := 'Save' {mnuFileSave};
  SaveAllBtn := mnuFileSaveAll;
  CopyBtn := 'Copy'{mnuEditCopy};
  PasteBtn := 'Paste';
  FindBtn := 'Find';
  InfoBtn := 'Info';
  AnchorBtn := 'Insert Hyperlink';
  ImageBtn := 'Image';
  UListBtn := 'Bullets List';
  NListBtn := 'Numeric List';
  ListItemBtn := 'List item';
  TableBtn := 'Table';
  BoldBtn := 'Bold' {mnuLayoutBold};
  ItalicBtn := 'Italic' {mnuLayoutItalic};
  UnderlineBtn := 'Underline' {mnuLayoutUnderline};
  EmBtn := mnuLayoutEmphasis;
  StrongBtn := mnuLayoutStrong;
  SupBtn := mnuLayoutSub;
  SubBtn := mnuLayoutSup;
  HeadingBtn := 'Heading (H1..H6)';
  LeftAlignBtn := 'Align Left' {mnuLayoutAlignLeft};
  RightAlignBtn:= 'Align Right' {mnuLayoutAlignRight};
  CenterAlignBtn:= 'Centered' {mnuLayoutAlignCenter};
  JustifyAlignBtn:= 'Justified' {mnuLayoutAlignJustify};
  ParaBtn := mnuGroupingParagraph;
  DivBtn := mnuGroupingDiv;
  SpanBtn := mnuGroupingSpan;

  // Other constants
  NoName := 'Untitled';
end;

procedure TTranslations.TranslateToDutch;
begin
  mnuFile := 'Bestand';
    mnuFileOpen := 'Open';
    mnuFileNewFromTemplate := 'Nieuw van sjabloon ...';
    mnuFileNew := 'Open';
      mnuFileNewText := 'Leeg blad';
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
    mnuFileSave := 'Op&slaan';
    mnuFileSaveAs := 'Opslaan &als ...';
    mnuFileSaveAll := '&Alles opslaan';
    mnuFileCloseCurrent := 'Sl&uiten';
    //mnuSep1: TMenuItem;
    mnuFileOpenInBrowser := 'Open in &browser';
    //mnuSepAboveMru: TMenuItem;
    mnuFileCloseApp := 'Afsluiten';
  //Edit menu
  mnuEdit := 'Be&werken';
    mnuEditUndo := '&Ongedaan maken';
    mnuEditRedo := '&Herhalen';
    //mnuSep11: TMenuItem;
    mnuEditCopy := '&Kopiëren';
    mnuEditCut := 'K&nippen';
    mnuEditPaste := '&Plakken';
    mnuEditPasteSpecial := 'Plakken speciaal';
      mnuEditPasteTableContentTab := 'Tabelinhoud (Tab-gescheiden)';
    mnuEditSelectAll := '&Alles selecteren';
    //mnuSep12: TMenuItem;
    mnuEditReplace := 'Ve&rvangen';
    mnuEditFindNext := '&Volgende zoeken';
    mnuEditFind := '&Zoeken';
  //Insert menu
  mnuHTMLTools := '&HTML Tools';
    mnuInsertAnchor := 'Hyperlink invoegen';
  mnuHTMLTools := '&HTML Tools';
    mnuInsertAnchor := 'Insert Hyperlink';
      mnuInsertList := 'Lijst';
      mnuInsertUList := 'Ongenummerde lijst';
      mnuInsertNList := 'Genummerde lijst';
      mnuInsertWordList := '&Woordenlijst';
      mnuInsetListItem := 'Lijst item invoegen';
      mnuInsertWordTerm := 'Woordenlijst term';
      mnuInsertWordDefinition := 'Woordenlijst definitie';
    mnuInsertTable := 'Tabel';
      mnInsertNewTable := 'Tabel invoegen ...';
      mnuInsertTableCell := 'Cel';
      mnuInsertTableRow := 'Rij';
    mnuInsertPicture := 'Plaatje invoegen';
    mnuInsertSpecialChars := 'Speciale tekens';
    mnuInsertLineBreak := 'Nieuwe regel';
    //mnuInsertSep1: TMenuItem;
    mnuInsertHtmlComment := 'Commentaar';
    mnuInsertJS := 'JavaScript';
    mnuInsertCssStyle := 'Css stijl';
    //Layout menu
    mnuLayout := '&Opmaak';
      mnuLayoutBold := 'Vet';
      mnuLayoutItalic := 'Cursief';
      mnuLayoutUnderline := 'Onderstreept';
      mnuLayoutSub := 'Subscript';
      mnuLayoutSup := 'Superscript';
      mnuLayoutEmphasis := 'Nadruk';
      mnuLayoutStrong := 'Sterke nadruk';
      mnuLayoutHeadings := 'Kop';
  {        mnuLayoutH1: TMenuItem;
        mnuLayoutH2: TMenuItem;
        mnuLayoutH3: TMenuItem;
        mnuLayoutH4: TMenuItem;
        mnuLayoutH5: TMenuItem;
        mnuLayoutH6: TMenuItem;
      mnuLayoutAlign: TMenuItem;
        mnuLayoutAlignLeft: TMenuItem;
        mnuLayoutAlignRight: TMenuItem;
        mnuLayoutAlignCenter: TMenuItem;
        mnuLayoutAlignJustify := 'Volledig uitlijnen';
      mnuLayoutCode: TMenuItem;
      mnuLayoutQuote: TMenuItem;
      mnuLayoutBlockQuote: TMenuItem;
      mnuLayoutPreformatted: TMenuItem;}
    //Grouping menu
    mnuGrouping := 'In&deling';
      mnuGroupingParagraph := 'Alinea';
      mnuGroupingDiv := 'Div';
      mnuGroupingSpan := 'Span';
  //View menu
  mnuView := 'Bee&ld';
    mnuViewFontsize := '&Tekengrootte';
      mnuViewFontSizeUp := '&Groter';
      mnuViewFontsizeDown := '&Kleiner';
    //mnuViewHighlighter: TMenuItem;
      mnuViewHLeftNone := 'Geen';

  //messages
  SLine := 'Rg';
  SCol := 'Kol';
  SModified := 'Gewijzigd';
  SIns := 'INS';
  SOvr := 'OVR';
  msgOpenError := 'Fout bij openen van bestand:'^m'%s';
  msgSaveError := 'Fout bij opslaan van bestand:'^m'%s';
  msgSaveAllError := 'De volgende bestanden zijn niet opgeslagen:'^m'%s';
  msgFileIsNotText := 'Dit bestand lijkt geen tekstbestand te zijn'^m'%s'^m'Wilt u het toch openen?';
  msgFileNotFound := 'Bestand niet gevonden:'^m'%s';
  msgModifiedSave := 'Bestand is gewijzigd:'^m'%s'^m'Bestand opslaan?';
  msgMruIndexOutOfBound := 'Index voor recent geopende bestanden ligt buiten de grenzen [%d]'^m+
                          'Dit is uiteraard een fout van de programmeur';
  msgFileTypeNotForBrowser := 'Dit bestandstype lijkt niet geschikt om te openen in een browser.'^m+
                              'Wilt u toch doorgaan?';
  msgFileHasNoName := 'Dit bestand heeft nog geen naam.'^m +
                     'U moet het bestand eerst opslaan om het in de browser te openen.';
  msgErrorBrowser := 'Er is een fout opgetreden tijdens het openen van'^m+
                    '%s'^m'in de browser.';
  msgTextNotFound := 'Tekst niet gevonden:'^m'"%s"';

  // main.pp hints in toolbar buttons
  NewFromTemplateBtn := mnuFileNewFromTemplate;
  NewPlainBtn := mnuFileNew;
  OpenBtn := mnuFileOpen;
  SaveBtn := mnuFileSave;
  SaveAllBtn := mnuFileSaveAll;
  CopyBtn := mnuEditCopy;
  PasteBtn := 'Paste';
  FindBtn := 'Find';
//  InfoBtn := 'Help'; //set to 'Info' in English
  AnchorBtn := 'Hyperlink invoegen';
  ImageBtn := 'Plaatje invoegen';
  UListBtn := 'Ongenummerde lijst';
  NListBtn := 'Genummerde ljst';
  ListItemBtn := 'Lijstitem invoegen ((on)genummerde lijst)';
  TableBtn := 'Tabel invoegen';
  BoldBtn := mnuLayoutBold;
  ItalicBtn := mnuLayoutItalic;
  UnderlineBtn := mnuLayoutUnderline;
  EmBtn := mnuLayoutEmphasis;
  StrongBtn := mnuLayoutStrong;
  SupBtn := mnuLayoutSub;
  SubBtn := mnuLayoutSup;
  HeadingBtn := 'Heading (H1..H6)';
  LeftAlignBtn := mnuLayoutAlignLeft;
  RightAlignBtn:= mnuLayoutAlignRight;
  CenterAlignBtn:= mnuLayoutAlignCenter;
  JustifyAlignBtn:= mnuLayoutAlignJustify;
  ParaBtn := mnuGroupingParagraph;
  DivBtn := mnuGroupingDiv;
  SpanBtn := mnuGroupingSpan;

  // Other constants
  NoName := 'Naamloos';
end;

procedure TTranslations.TranslateToPortuguese;
begin
  mnuFile := '&Arquivo';
    mnuFileOpen := 'Abrir';
    mnuFileNewFromTemplate := 'Novo do modelo ...';
    mnuFileNew := 'Novo';
      mnuFileNewText := 'Texto';
      {mnuFileNewHtml: TMenuItem;
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
    mnuFileSave := 'Salvar';
    mnuFileSaveAs := 'Salvar como';
    mnuFileSaveAll := 'Salvar todos';
    mnuFileCloseCurrent := 'Fechar arquivo';
    //mnuSep1: TMenuItem;
    mnuFileOpenInBrowser := 'Abrir num navegador';
    //mnuSepAboveMru: TMenuItem;
    mnuFileCloseApp := 'Fechar o programa';
  //Edit menu
  mnuEdit := 'Editar';
    mnuEditUndo := 'Desfazer';
    mnuEditRedo := 'Refazer';
    //mnuSep11: TMenuItem;
    mnuEditCopy := 'Copiar';
    mnuEditCut := 'Cortar';
    mnuEditPaste := 'Colar';
    //mnuEditPasteSpecial := 'Plakken speciaal';
    mnuEditSelectAll := 'Selecionar tudo';
    //mnuSep12: TMenuItem;
    mnuEditReplace := '&Substituir';
    mnuEditFindNext := 'Procurar Próximo';
    mnuEditFind := '&Procurar';
  //HTML Tools menu
  mnuHTMLTools := 'Ferramentas &HTML';
    mnuInsertAnchor := 'Inserir Hyperlink';
  {    mnuInsertList,
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
    mnuLayout := '&Layout';
      mnuLayoutBold := 'Negrito';
      mnuLayoutItalic := 'Italico';
      mnuLayoutUnderline := 'Sublinhado';
      mnuLayoutSub := 'Subscrito';
      mnuLayoutSup := 'Subrescrito';
      mnuLayoutEmphasis := 'Ênfase';
      mnuLayoutStrong := 'Forte';
      mnuLayoutHeadings := 'Título';
  {        mnuLayoutH1: TMenuItem;
        mnuLayoutH2: TMenuItem;
        mnuLayoutH3: TMenuItem;
        mnuLayoutH4: TMenuItem;
        mnuLayoutH5: TMenuItem;
        mnuLayoutH6: TMenuItem;
      mnuLayoutAlign: TMenuItem;
        mnuLayoutAlignLeft: TMenuItem;
        mnuLayoutAlignRight: TMenuItem;
        mnuLayoutAlignCenter: TMenuItem;
        mnuLayoutAlignJustify := 'Volledig uitlijnen';
      mnuLayoutCode: TMenuItem;
      mnuLayoutQuote: TMenuItem;
      mnuLayoutBlockQuote: TMenuItem;
      mnuLayoutPreformatted: TMenuItem;}
    //Grouping menu
    mnuGrouping := 'A&grupamento';
      mnuGroupingParagraph := 'Paragrafo';
      mnuGroupingDiv := 'Div';
      mnuGroupingSpan := 'Span';
  //View menu
  mnuView := '&Vizualização';
    mnuViewFontsize := 'Tamanho da &fonte';
end;

procedure TTranslations.TranslateToLanguageID(ALangId: TLanguageIds);
begin
  FLanguageID := ALangId;
  case ALangId of
  lidDutch: TranslateToDutch;
  lidPortuguese: TranslateToPortuguese;
  else
    TranslateToEnglish;
  end;
end;

function TTranslations.GetCurrentLanguageID: TLanguageIds;
begin
  Result := FLanguageID;
end;

initialization

vTranslations := TTranslations.Create;
vTranslations.TranslateToEnglish

finalization

FreeAndNil(vTranslations);

end.

