unit lazedit_translations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTranslations }

  TTranslations = class(TObject)
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
      mnuEditSelectAll,
      //mnuSep12: TMenuItem;
      mnuEditReplace,
      mnuEditFindNext,
      mnuEditFind: string;
    //Insert menu
    mnuInsert,
    mnuInsertAnchor,
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
    mnuGrouping,
      mnuGroupingParagraph,
      mnuGroupingDiv,
      mnuGroupingSpan,
    //View menu
    mnuView,
      mnuViewFontsize: string;
{        mnuViewFontSizeUp: TMenuItem;
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
    { About box strings }
//    lpSupport, lpSupportInfo, lpLicense, lpLicenseInfo, lpAuthors,
//     lpContributorsTitle, lpAboutWindow, lpClose, lpInformation: string;
    { Methods }
    procedure TranslateToEnglish;
    procedure TranslateToDutch;
    procedure TranslateToPortuguese;
    procedure TranslateToLanguageID(AID: Integer);
  end;

var
  vTranslations: TTranslations;

implementation

{ TTranslations }

procedure TTranslations.TranslateToEnglish;
begin
  mnuFile := 'File';
    mnuFileOpen := 'Open';
    mnuFileNewFromTemplate := 'New from template ...';
    mnuFileNew := 'New';
  {        mnuFileNewText,
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
      mnuFileNewIni: TMenuItem;}
    mnuFileSave := 'Save';
    mnuFileSaveAs := 'Save as';
    mnuFileSaveAll := 'Save all';
    mnuFileCloseCurrent := 'Close current';
    //mnuSep1: TMenuItem;
    mnuFileOpenInBrowser := 'Open in browser';
    //mnuSepAboveMru: TMenuItem;
    mnuFileCloseApp := 'Close application';
  //Edit menu
  mnuEdit := 'Edit';
    mnuEditUndo := 'Undo';
    mnuEditRedo := 'Redo';
    //mnuSep11: TMenuItem;
    mnuEditCopy := 'Copy';
    mnuEditCut := 'Cut';
    mnuEditPaste := 'Paste';
    //mnuEditPasteSpecial := 'Plakken speciaal';
    mnuEditSelectAll := 'Select all';
    //mnuSep12: TMenuItem;
    mnuEditReplace := '&Replace';
    mnuEditFindNext := 'Find &Next';
    mnuEditFind := '&Find';
  //Insert menu
  mnuInsert := '&Insert';
  mnuInsertAnchor := 'Insert Hyperlink';
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
    mnuLayoutBold := 'Bold';
    mnuLayoutItalic := 'Italic';
    mnuLayoutUnderline := 'Underline';
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
  mnuGrouping := '&Grouping';
    mnuGroupingParagraph := 'Paragraph';
    mnuGroupingDiv := 'Div';
    mnuGroupingSpan := 'Span';
  //View menu
  mnuView := '&View';
    mnuViewFontsize := '&Font Size';
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
    //mnuEditPasteSpecial := 'Plakken speciaal';
    mnuEditSelectAll := '&Alles selecteren';
    //mnuSep12: TMenuItem;
    mnuEditReplace := 'Ve&rvangen';
    mnuEditFindNext := '&Volgende zoeken';
    mnuEditFind := '&Zoeken';
  //Insert menu
  mnuInsert := '&Invoegen';
  mnuInsertAnchor := 'Hyperlink invoegen';
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
end;

procedure TTranslations.TranslateToPortuguese;
begin
  mnuFile := '&Arquivo';
    mnuFileOpen := 'Open';
    mnuFileNewFromTemplate := 'Nieuw van sjabloon ...';
    mnuFileNew := 'Open';
  {        mnuFileNewText,
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
      mnuFileNewIni: TMenuItem;}
    mnuFileSave := 'Open';
    mnuFileSaveAs := 'Open';
    mnuFileSaveAll := 'Open';
    mnuFileCloseCurrent := 'Open';
    //mnuSep1: TMenuItem;
    mnuFileOpenInBrowser := 'Open';
    //mnuSepAboveMru: TMenuItem;
    mnuFileCloseApp := 'Open';
  //Edit menu
  mnuEdit := 'Open';
    mnuEditUndo := 'Open';
    mnuEditRedo := 'Open';
    //mnuSep11: TMenuItem;
    mnuEditCopy := 'Open';
    mnuEditCut := 'Open';
    mnuEditPaste := 'Open';
    mnuEditSelectAll := 'Open';
end;

procedure TTranslations.TranslateToLanguageID(AID: Integer);
begin
  case AID of
  1: TranslateToDutch;
  2: TranslateToPortuguese;
  else
    TranslateToEnglish;
  end;
end;

initialization

vTranslations := TTranslations.Create;
vTranslations.TranslateToEnglish

finalization

FreeAndNil(vTranslations);

end.

