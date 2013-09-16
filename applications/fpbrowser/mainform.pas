unit mainform;


interface

uses
  // RTL
  SysUtils, Classes, fpimage,
  {$IFDEF MSWINDOWS} ShellAPI, {$ELSE} Unix, {$ENDIF}
  // LCL
  LclIntf, LMessages, LclType, LResources,
  Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Menus, StdCtrls, Clipbrd,
  PrintersDlgs, buttons, ComCtrls,
  // Custom Drawn
  customdrawnextras, customdrawncontrols,  customdrawndrawers, customdrawn_kde,
  //
  dlgabout, browserconstants,
  pageloader, browserviewer, browsermodules, dlgconfig;

type

  { TformBrowser }

  TformBrowser = class(TForm)
    mnuPreferences: TMenuItem;
    pageBrowser: TCDPageControl;
    labelProgress: TLabel;
    menuToolsModules: TMenuItem;
    menuViewDebug: TMenuItem;
    N1: TMenuItem;
    OpenDialog: TOpenDialog;   
    MainMenu: TMainMenu;
    panelBottom: TPanel;
    File1: TMenuItem;
    Open: TMenuItem;
    menuOptions: TMenuItem;
    ShowImages: TMenuItem;
    Fonts: TMenuItem;
    HistoryMenuItem: TMenuItem;
    Exit1: TMenuItem;
    PrintDialog: TPrintDialog;
    About1: TMenuItem;
    Edit2: TMenuItem;
    Find1: TMenuItem;
    FindDialog: TFindDialog;
    CopyItem: TMenuItem;
    N2: TMenuItem;
    SelectAllItem: TMenuItem;
    OpenTextFile: TMenuItem;
    OpenImageFile: TMenuItem;
    PopupMenu: TPopupMenu;
    CopyImageToClipboard: TMenuItem;
    Viewimage: TMenuItem;
    N3: TMenuItem;
    OpenInNewWindow: TMenuItem;
    MetaTimer: TTimer;
    Print1: TMenuItem;
    Printpreview: TMenuItem;
    Timer1: TTimer;
    ProgressBar: TProgressBar;
    PrinterSetupDialog: TPrinterSetupDialog;
    PrinterSetup1: TMenuItem;
    procedure editURLKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure menuViewDebugClick(Sender: TObject);
    procedure mnuPreferencesClick(Sender: TObject);
    procedure OpenFileClick(Sender: TObject);
    procedure pageBrowserChange(Sender: TObject);
    procedure ShowImagesClick(Sender: TObject);
    procedure buttonReloadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HistoryClick(Sender: TObject);
    procedure HistoryChange(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FontColorsClick(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SubmitEvent(Sender: TObject; Const AnAction, Target, EncType, Method: String;
      Results: TStringList);
    procedure Find1Click(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
    procedure CopyItemClick(Sender: TObject);
    procedure Edit2Click(Sender: TObject);
    procedure SelectAllItemClick(Sender: TObject);
    procedure OpenTextFileClick(Sender: TObject);
    procedure OpenImageFileClick(Sender: TObject);
    procedure CopyImageToClipboardClick(Sender: TObject);
    procedure ObjectClick(Sender, Obj: TObject; const OnClick: String);
    procedure ViewimageClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ViewerInclude(Sender: TObject; const Command: String;
      Params: TStrings; var S: string);
    procedure OpenInNewWindowClick(Sender: TObject);
    procedure MetaTimerTimer(Sender: TObject);
    procedure MetaRefreshEvent(Sender: TObject; Delay: Integer;
      const URL: String);
    procedure PrintpreviewClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PrinterSetup1Click(Sender: TObject);
    procedure HandleUserAddedPage(Sender: TObject; APage: TCDTabSheet);
  private
    { Private declarations }
{$IFDEF LCLCarbon}
    AppMenu : TMenuItem;
{$ENDIF}
    History: TStringList;
    HistoryIndex: Integer;
    MediaCount: integer;
    NewWindowFile: string;
    NextFile, PresentFile: string;
    TimerCount: integer;
    OldTitle: string;
    HintWindow: THintWindow;
    HintVisible: boolean;
    //
    procedure DropFiles(      Sender   : TObject;
                        const FileNames: array of string);
    procedure CloseAll;
    procedure ForwardClick(Sender: TObject);
    procedure BackClick(Sender: TObject);
    procedure InitializeForm();
    procedure UpdateModulesMenu();
    procedure HandleModuleMenuItemClick(Sender: TObject);
    procedure HandleModuleCommandMenuItemClick(Sender: TObject);
    function  GetCurrentPageLoader: TPageLoader;
    procedure HandlePageChanged(Sender: TObject);
  public
    { Public declarations }
    CurrentTab: Integer;
    procedure LoadURL(AURL: string);
    procedure AddBrowserTab(AURL: string; AGoToTab: Boolean);
    procedure AddBrowserControlsToTab(ATabSheet: TCDTabSheet);
    procedure AddMemoTab(AText: TStringList; ACaption: string; AGoToTab: Boolean);
    procedure AddURLToHistory(AURL: string);
    procedure HandlePageLoaderProgress(APercent: Integer);
    procedure HandlePageLoaderTerminated(Sender: TObject);
  end;

var
  formBrowser: TformBrowser;

implementation

uses
  Submit, ImgForm;//, FontDlg;

procedure TformBrowser.FormCreate(Sender: TObject);
begin
  InitializeForm();
end;

procedure TformBrowser.FormShow(Sender: TObject);
var
  S: string;
  I: integer;
begin
  // With OS X app, ParamStr not meaningful unless launched with --args switch.
  if (ParamCount >= 1) {$IFDEF DARWIN} and (Copy(ParamStr(1), 1, 4) <> '-psn') {$ENDIF} then
  begin            {Parameter is file to load}
    S := ParamStr(1);
    GetCurrentBrowserViewer.LoadFromFile(S);
  end;
end;

procedure TformBrowser.OpenFileClick(Sender: TObject);
begin
//  if Viewer.CurrentFile <> '' then
//    OpenDialog.InitialDir := ExtractFilePath(Viewer.CurrentFile);
  OpenDialog.Filter := 'HTML Files (*.htm,*.html)|*.htm;*.html';  //might have changed
  if OpenDialog.Execute then
  begin
    Update;
    GetCurrentBrowserViewer().LoadFromFile(OpenDialog.Filename);
    Caption := GetCurrentBrowserViewer().GetDocumentTitle();
  end;
end;

procedure TformBrowser.pageBrowserChange(Sender: TObject);
begin
  SetCurrentBrowserViewer(pageBrowser.PageIndex);
end;

procedure TformBrowser.editURLKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lEdit: TEdit;
begin
  lEdit := Sender as TEdit;
  if Key = VK_RETURN then
  begin
    LoadURL(lEdit.Text);
  end;
end;

procedure TformBrowser.menuViewDebugClick(Sender: TObject);
var
  lContents, lDebugInfo: TStringList;
begin
  lContents := GetCurrentBrowserViewer().MyPageLoader.ContentsList;
  lDebugInfo := GetCurrentBrowserViewer().MyPageLoader.DebugInfo;

  AddMemoTab(lContents, 'Source', False);
  AddMemoTab(lDebugInfo, 'Debug', False);
end;

procedure TformBrowser.mnuPreferencesClick(Sender: TObject);
begin
  formConfig.ShowModal;
end;

{The Show Images menu item was clicked}
procedure TformBrowser.ShowImagesClick(Sender: TObject);
begin
  GetCurrentBrowserViewer().SetShowImages((Sender as TMenuItem).Checked);
end;

procedure TformBrowser.buttonReloadClick(Sender: TObject);
{the Reload button was clicked}
begin
  GetCurrentBrowserViewer().Reload();
end;

{Forward button was clicked}
procedure TformBrowser.ForwardClick(Sender: TObject);
var
  lButton: TWinControl;
begin
  lButton := Sender as TWinControl;
  {$ifdef FPBROWSER_THTMLCOMP}
{  if Sender = buttonBack then
    Viewer.HistoryIndex := Viewer.HistoryIndex +1
  else
    Viewer.HistoryIndex := Viewer.HistoryIndex -1;
  Self.Caption := Viewer.DocumentTitle;}
  {$endif}
  LoadURL(History.Strings[HistoryIndex]);

  HistoryIndex := HistoryIndex+1;
  if HistoryIndex >= History.Count then lButton.Enabled := False;
end;

{Back button was clicked}
procedure TformBrowser.BackClick(Sender: TObject);
var
  lButton, lForwardButton: TWinControl;
begin
  lButton := Sender as TWinControl;

  LoadURL(History.Strings[HistoryIndex]);

  HistoryIndex := HistoryIndex-1;
  if HistoryIndex < 0 then lButton.Enabled := False;
  //buttonForward.Enabled := True;
end;

procedure TformBrowser.InitializeForm;
var
  I: integer;
begin
  History := TStringList.Create;

  AddBrowserTab('', True);

  Position := poScreenCenter;

  {$IFDEF DARWIN} //Don't default to within app bundle.
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0)) + '../../../';
  {$ELSE}
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  {$ENDIF}

  Caption := 'fpBrowser '+BrowserVersion;

  (*for I := 0 to MaxHistories-1 do
  begin      {create the MenuItems for the history list}
    Histories[I] := TMenuItem.Create(HistoryMenuItem);
    HistoryMenuItem.Insert(I, Histories[I]);
    with Histories[I] do
    begin
    Visible := False;
    OnClick := HistoryClick;
    Tag := I;
    end;
  end;*)

  {$IFDEF LCLCarbon}
  AppMenu := TMenuItem.Create(Self);  //Application menu
  AppMenu.Caption := #$EF#$A3#$BF;  //Unicode Apple logo char
  MainMenu.Items.Insert(0, AppMenu);
  MainMenu.Items.Remove(About1);  //Remove About as separate menu
  AppMenu.Add(About1);  //Add About as item in application menu

  File1.Remove(File1.Items[File1.Count-2]);
  File1.Remove(Exit1);  //Remove Exit since have Quit

  Find1.ShortCut := ShortCut(VK_F, [ssMeta]);
  CopyItem.ShortCut := ShortCut(VK_C, [ssMeta]);
  SelectAllItem.ShortCut := ShortCut(VK_A, [ssMeta]);
  {$ENDIF}

  AllowDropFiles := True;
  OnDropFiles := DropFiles;

  HintWindow := THintWindow.Create(Self);
  HintWindow.Color := $C0FFFF;

  // Add the modules menu
  UpdateModulesMenu();
end;

procedure TformBrowser.UpdateModulesMenu;
var
  lItem, lSubItem: TMenuItem;
  lModule: TBrowserModule;
  i, j: Integer;
  lModuleUIElements: TBrowserModuleUIElements;
begin
  // First remove any existing modules
  menuToolsModules.Clear;

  // Now add all modules and their state
  for i := 0 to GetBrowserModuleCount() - 1 do
  begin
    lModule := GetBrowserModule(i);

    lModuleUIElements := lModule.GetModuleUIElements();

    // ----
    if bmueEnabledDisableMenu in lModuleUIElements then
    begin
      lItem := TMenuItem.Create(nil);
      lItem.Caption := lModule.ShortDescription;
      lItem.Checked := lModule.Activated;
      lItem.OnClick := HandleModuleMenuItemClick;
      menuToolsModules.Add(lItem);
    end;
    // ----
    if bmueCommandsSubmenu in lModuleUIElements then
    begin
      lItem := TMenuItem.Create(nil);
      lItem.Caption := Format('Commands from module %s', [lModule.ShortDescription]);

      for j := 0 to lModule.GetCommandCount()-1 do
      begin
        lSubItem := TMenuItem.Create(nil);
        lSubItem.Caption := lModule.GetCommandName(j);
        lSubItem.OnClick := HandleModuleCommandMenuItemClick;
        lSubItem.Tag := PtrInt(lModule);
        lItem.Add(lSubItem);
      end;

      menuToolsModules.Add(lItem);
    end;
  end;
end;

procedure TformBrowser.HandleModuleMenuItemClick(Sender: TObject);
var
  lItem: TMenuItem;
  lModuleNum: Integer;
  lModule: TBrowserModule;
  lNewState: Boolean;
begin
  lItem := Sender as TMenuItem;
  lNewState := not lItem.Checked;
  lItem.Checked := lNewState;
  lModuleNum := lItem.Parent.IndexOf(lItem);
  lModule := GetBrowserModule(lModuleNum);
  lModule.Activated := lNewState;
end;

procedure TformBrowser.HandleModuleCommandMenuItemClick(Sender: TObject);
var
  lItem: TMenuItem;
  lCommandNum: Integer;
  lModule: TBrowserModule;
begin
  lItem := Sender as TMenuItem;
  lModule := TBrowserModule(lItem.Tag);
  lCommandNum := lItem.Parent.IndexOf(lItem);
  lModule.ExecuteCommand(lCommandNum);
end;

function TformBrowser.GetCurrentPageLoader: TPageLoader;
var
  lViewer: TBrowserViewer;
begin
  lViewer := GetCurrentBrowserViewer();
  Result := lViewer.MyPageLoader;
end;

procedure TformBrowser.HandlePageChanged(Sender: TObject);
var
  lIndex: Integer;
begin
  lIndex := pageBrowser.TabIndex;
  SetCurrentBrowserViewer(lIndex);
end;

procedure TformBrowser.HistoryChange(Sender: TObject);
{This event occurs when something changes history list}
var
  I: integer;
  Cap: string[80];
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  with Sender as ThtmlViewer do
  begin
  {check to see which buttons are to be enabled}
  buttonForward.Enabled := HistoryIndex > 0;
  buttonBack.Enabled := HistoryIndex < History.Count-1;

  {Enable and caption the appropriate history menuitems}
  HistoryMenuItem.Visible := History.Count > 0;
  for I := 0 to MaxHistories-1 do
    with Histories[I] do
      if I < History.Count then
        Begin
        Cap := History.Strings[I];
        if TitleHistory[I] <> '' then
          Cap := Cap + '--' + TitleHistory[I];
        Caption := Cap;    {Cap limits string to 80 char}
        Visible := True;
        Checked := I = HistoryIndex;
        end
      else Histories[I].Visible := False;
  Caption := DocumentTitle;    {keep the caption updated}
  Viewer.SetFocus;  
  end;
  {$endif}
end;

{A history list menuitem got clicked on}
procedure TformBrowser.HistoryClick(Sender: TObject);
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  {Changing the HistoryIndex loads and positions the appropriate document}
  Viewer.HistoryIndex := (Sender as TMenuItem).Tag;
  {$endif}
end;

procedure TformBrowser.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TformBrowser.FontColorsClick(Sender: TObject);
{var
  FontForm: TFontForm;}
begin
(*FontForm := TFontForm.Create(Self);
try
  with FontForm do
    begin
    FontName := Viewer.DefFontName;
    FontColor := Viewer.DefFontColor;
    FontSize := Viewer.DefFontSize;
    HotSpotColor := Viewer.DefHotSpotColor;
    Background := Viewer.DefBackground;
    if ShowModal = mrOK then
      begin
      Viewer.DefFontName := FontName;
      Viewer.DefFontColor := FontColor;
      Viewer.DefFontSize := FontSize;
      Viewer.DefHotSpotColor := HotSpotColor;
      Viewer.DefBackground := Background; 
      ReloadButtonClick(Self);    {reload to see how it looks}
      end;
    end;
finally
  FontForm.Free;
 end;*)
end;

procedure TformBrowser.Print1Click(Sender: TObject);
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  with PrintDialog do
  if Execute then
    if PrintRange = prAllPages then
      viewer.Print(1, 9999)
    else
      Viewer.Print(FromPage, ToPage);
  {$endif}
end;

procedure TformBrowser.PrinterSetup1Click(Sender: TObject);
begin
  {$IFNDEF LCLCarbon}
  PrinterSetupDialog.Execute;
  {$ELSE}
  MessageDlg('Not yet supported with Carbon widgetset.',
             mtError, [mbOK], 0);
  {$ENDIF}
end;

procedure TformBrowser.About1Click(Sender: TObject);
begin
  AboutBox := TAboutBox.CreateIt(Self, 'HTMLDemo', 'ThtmlViewer');
  try
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end;


procedure TformBrowser.SubmitEvent(Sender: TObject; const AnAction, Target,
  EncType, Method: String; Results: TStringList);
begin
  SubmitForm.ActionText.Text := AnAction;
  SubmitForm.MethodText.Text := Method;
  SubmitForm.ResultBox.Items := Results;
  Results.Free;
  SubmitForm.Show;
end;

procedure TformBrowser.Find1Click(Sender: TObject);
begin
  FindDialog.Execute;
end;

procedure TformBrowser.FindDialogFind(Sender: TObject);
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  with FindDialog do
  begin
  if not Viewer.FindEx(FindText, frMatchCase in Options, not (frDown in Options)) then
    MessageDlg('No further occurances of "'+FindText+'"', mtInformation, [mbOK], 0);
  end;
  {$endif}
end;

procedure TformBrowser.ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  if ProcessingOn then
  begin    {disable various buttons and menuitems during processing}
  buttonForward.Enabled := False;
  buttonBack.Enabled := False;
  buttonReload.Enabled := False;
  Print1.Enabled := False;
  PrintPreview.Enabled := False;
  Find1.Enabled := False;
  SelectAllItem.Enabled := False;
  Open.Enabled := False;
  CloseAll;    {in case hint window is open}
  end
  else
  begin
  buttonForward.Enabled := Viewer.HistoryIndex > 0;
  buttonBack.Enabled := Viewer.HistoryIndex < Viewer.History.Count-1;
  buttonReload.Enabled := Viewer.CurrentFile <> '';
  Print1.Enabled := Viewer.CurrentFile <> '';
  PrintPreview.Enabled := Viewer.CurrentFile <> '';
  Find1.Enabled := Viewer.CurrentFile <> '';
  SelectAllItem.Enabled := Viewer.CurrentFile <> '';
  Open.Enabled := True;
  end;
  {$endif}
end;

procedure TformBrowser.CopyItemClick(Sender: TObject);
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  Viewer.CopyToClipboard;
  {$endif}
end;

procedure TformBrowser.Edit2Click(Sender: TObject);
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  CopyItem.Enabled := Viewer.SelLength <> 0;
  {$endif}
end;

procedure TformBrowser.SelectAllItemClick(Sender: TObject);
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  Viewer.SelectAll;
  {$endif}
end;

procedure TformBrowser.OpenTextFileClick(Sender: TObject);
begin
{$ifdef FPBROWSER_THTMLCOMP}
  if Viewer.CurrentFile <> '' then
    OpenDialog.InitialDir := ExtractFilePath(Viewer.CurrentFile);
  OpenDialog.Filter := 'HTML Files (*.htm,*.html)|*.htm;*.html'+
    '|Text Files (*.txt)|*.txt'+
    '|All Files (*.*)|*.*';
  if OpenDialog.Execute then
  begin
    buttonReload.Enabled := False;
    Update;
    Viewer.LoadTextFile(OpenDialog.Filename);
    if Viewer.CurrentFile  <> '' then
    begin
      Caption := Viewer.DocumentTitle;
      buttonReload.Enabled := True;
    end;
  end;
{$endif}
end;

procedure TformBrowser.OpenImageFileClick(Sender: TObject);
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  if Viewer.CurrentFile <> '' then
    OpenDialog.InitialDir := ExtractFilePath(Viewer.CurrentFile);
  OpenDialog.Filter := 'Graphics Files (*.bmp,*.gif,*.jpg,*.jpeg,*.png)|'+
    '*.bmp;*.jpg;*.jpeg;*.gif;*.png|'+
    'All Files (*.*)|*.*';
  if OpenDialog.Execute then
  begin
    buttonReload.Enabled := False;
    Viewer.LoadImageFile(OpenDialog.Filename);
    if Viewer.CurrentFile  <> '' then
    begin
      Caption := Viewer.DocumentTitle;
      buttonReload.Enabled := True;
    end;
  end;
  {$endif}
end;

procedure TformBrowser.DropFiles(      Sender    : TObject;
                           const FileNames : array of string);
var
  S  : string;
  Ext: string;
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  S := FileNames[0];
  Ext := LowerCase(ExtractFileExt(S));
  if (Ext = '.htm') or (Ext = '.html') then
    Viewer.LoadFromFile(S)
  else if (Ext = '.txt') then
    Viewer.LoadTextFile(S)
  else if (Ext = '.bmp') or (Ext = '.gif') or (Ext = '.jpg')
        or (Ext = '.jpeg') or (Ext = '.png') then
    Viewer.LoadImageFile(S);
  {$endif}
end;

procedure TformBrowser.ViewimageClick(Sender: TObject);
var
  AForm: TImageForm;
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  AForm := TImageForm.Create(Self);
  AForm.ImageFormBitmap := FoundObject.Bitmap;
  AForm.Caption := '';
  AForm.Show;
  {$endif}
end;

procedure TformBrowser.CopyImageToClipboardClick(Sender: TObject);
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  Clipboard.Assign(FoundObject.Bitmap);
  {$endif}
end;

procedure TformBrowser.ObjectClick(Sender, Obj: TObject; const OnClick: String);
var
  S: string;
begin
{$ifdef FPBROWSER_THTMLCOMP}
  if OnClick = 'display' then
  begin
  if Obj is TFormControlObj then
    with TFormControlObj(Obj) do
      begin
      if TheControl is TCheckBox then
        with TCheckBox(TheControl) do
          begin
          S := Value + ' is ';
          if Checked then S := S + 'checked'
            else S := S + 'unchecked';
          MessageDlg(S, mtCustom, [mbOK], 0);
          end
      else if TheControl is TRadioButton then
        with TRadioButton(TheControl) do
          begin
          S := Value + ' is checked';
          MessageDlg(S, mtCustom, [mbOK], 0);
          end;
      end;
  end
  else if OnClick <> '' then
      MessageDlg(OnClick, mtCustom, [mbOK], 0);
{$endif}
end;

procedure TformBrowser.ViewerInclude(Sender: TObject; const Command: String;
  Params: TStrings; var S: string);
{OnInclude handler}  
var
  Filename: string;
  I: integer;
  MS: TMemoryStream;
begin
  if CompareText(Command, 'Date') = 0 then
    S := DateToStr(Date) { <!--#date --> }
  else if CompareText(Command, 'Time') = 0 then
    S := TimeToStr(Time)   { <!--#time -->  }
  else if CompareText(Command, 'Include') = 0 then
  begin   {an include file <!--#include FILE="filename" -->  }
    if (Params.count >= 1) then
    begin
    I := Pos('file=', Lowercase(Params[0]));
    if I > 0 then
      begin
      Filename := copy(Params[0],  6, Length(Params[0])-5);
      MS := TMemoryStream.Create;
      try
        try
          MS.LoadFromFile(Filename);
          SetString(S, PChar(MS.Memory), MS.Size);
        finally
          MS.Free;
          end;
      except
        end;
      end;
    end;
  end;
  Params.Free;
end;

procedure TformBrowser.FormDestroy(Sender: TObject);
begin
  HintWindow.Free;
  History.Free;
end;

procedure TformBrowser.OpenInNewWindowClick(Sender: TObject);
var
  PC: array[0..255] of char;
{$IFDEF LCL}
  PC2: array[0..255] of char;
{$ENDIF}
begin
{$IFNDEF LCL}
  WinExec(StrPCopy(PC, ParamStr(0)+' "'+NewWindowFile+'"'), sw_Show);
{$ELSE}
 {$IFDEF MSWINDOWS}
  ShellExecute(Handle, nil, StrPCopy(PC, ParamStr(0)), 
               StrPCopy(PC2, NewWindowFile), nil, SW_SHOWNORMAL); 
 {$ELSE}
  {$IFDEF DARWIN}
  Shell('open -n "' + 
        ExtractFileDir(ExtractFileDir(ExtractFileDir(ParamStr(0)))) + 
        '" --args "' + NewWindowFile + '"');
  {$ELSE}
  Shell('"' + ParamStr(0) + '" "' + NewWindowFile + '"');
  {$ENDIF}
 {$ENDIF}
{$ENDIF}
end;

procedure TformBrowser.MetaTimerTimer(Sender: TObject);
begin
{$ifdef FPBROWSER_THTMLCOMP}
  MetaTimer.Enabled := False;
  if Viewer.CurrentFile = PresentFile then  {don't load if current file has changed}
  begin
    Viewer.LoadFromFile(NextFile);
    Caption := Viewer.DocumentTitle;
  end;
  {$ENDIF}
end;

procedure TformBrowser.MetaRefreshEvent(Sender: TObject; Delay: Integer;
  const URL: String);
begin
{$ifdef FPBROWSER_THTMLCOMP}
  NextFile := Viewer.HTMLExpandFilename(URL);
  if FileExists(NextFile) then
  begin
    PresentFile := Viewer.CurrentFile;
    MetaTimer.Interval := Delay*1000;
    MetaTimer.Enabled := True;
  end;
  {$ENDIF}
end;

procedure TformBrowser.PrintpreviewClick(Sender: TObject);
{$IFNDEF LCL}
var
  pf: TPreviewForm;
  Abort: boolean;
begin
  pf := TPreviewForm.CreateIt(Self, Viewer, Abort);
  try
    if not Abort then
      pf.ShowModal;
  finally
    pf.Free;
  end;
{$ELSE}
begin
  MessageDlg('Not yet supported with LCL.',
             mtError, [mbOK], 0);
{$ENDIF}
end;

procedure TformBrowser.CloseAll;
begin
  Timer1.Enabled := False;
  HintWindow.ReleaseHandle;
  HintVisible := False;
end;

procedure TformBrowser.LoadURL(AURL: string);
begin
  GetCurrentBrowserViewer.LoadFromURL(AURL);
end;

procedure TformBrowser.AddBrowserTab(AURL: string; AGoToTab: Boolean);
var
  lTabSheet: TCDTabSheet;
begin
  lTabSheet := pageBrowser.AddPage(''); // This call requires Lazarus 0.9.31+
  lTabSheet.Caption := 'WebPage';

  AddBrowserControlsToTab(lTabSheet);

  if AGoToTab then
  begin
    CurrentTab := GetBrowerViewerCount() - 1;
    SetCurrentBrowserViewer(CurrentTab);
  end;
end;

procedure TformBrowser.AddBrowserControlsToTab(ATabSheet: TCDTabSheet);
var
  lViewer: TBrowserViewer;
  lTopPanel: TPanel;
  lbuttonReload, lbuttonBack, lbuttonForward: TBitBtn;
  leditURL: TEdit;
begin
  // Add the top panel of the tab with buttons and the edit box
  lTopPanel := TPanel.Create(ATabSheet);
  lTopPanel.Parent := ATabSheet;
  lTopPanel.Align := alTop;
  lTopPanel.Height := 33;

  lbuttonReload := TBitBtn.Create(lTopPanel);
  lbuttonReload.Top := 4;
  lbuttonReload.Left := 10;
  lbuttonReload.Width := 59;
  lbuttonReload.Height := 24;
  lbuttonReload.Parent := lTopPanel;
  lbuttonReload.Caption := 'Reload';
  lbuttonReload.OnClick := buttonReloadClick;

  lbuttonBack := TBitBtn.Create(lTopPanel);
  lbuttonBack.Top := 4;
  lbuttonBack.Left := 69;
  lbuttonBack.Width := 59;
  lbuttonBack.Height := 24;
  lbuttonBack.Parent := lTopPanel;
  lbuttonBack.Caption := 'Back';
  lbuttonBack.OnClick := ForwardClick;

  lbuttonForward := TBitBtn.Create(lTopPanel);
  lbuttonForward.Top := 4;
  lbuttonForward.Left := 128;
  lbuttonForward.Width := 59;
  lbuttonForward.Height := 24;
  lbuttonForward.Parent := lTopPanel;
  lbuttonForward.Caption := 'Forward';
  lbuttonForward.OnClick := BackClick;

  leditURL := TEdit.Create(lTopPanel);
  leditURL.Top := 4;
  leditURL.Left := 190;
  leditURL.Width := 337;
  leditURL.Height := 24;
  leditURL.Parent := lTopPanel;
  leditURL.OnKeyDown := editURLKeyDown;

  // Add the HTML Viewer
  lViewer := AddBrowserViewer();
  lViewer.CreateViewer(ATabSheet, Self);
end;

procedure TformBrowser.AddMemoTab(AText: TStringList; ACaption: string;
  AGoToTab: Boolean);
var
  lTabSheet: TCDTabSheet;
  lMemo: TMemo;
  lViewer: TBrowserViewer;
begin
  lTabSheet := pageBrowser.AddPage(''); // This call requires Lazarus 0.9.31+
  lTabSheet.Caption := ACaption;

  // Add the top panel of the tab with buttons and the edit box
  lMemo := TMemo.Create(lTabSheet);
  lMemo.Parent := lTabSheet;
  lMemo.Align := alClient;
  lMemo.Lines.Assign(AText);
  lMemo.ScrollBars := ssBoth;

  // Add a token viewer for the Memo
  lViewer := AddBrowserViewer();

  if AGoToTab then
  begin
    CurrentTab := GetBrowerViewerCount() - 1;
    SetCurrentBrowserViewer(CurrentTab);
  end;
end;

procedure TformBrowser.AddURLToHistory(AURL: string);
begin
{  History.Add(AURL);
  HistoryIndex := History.Count-1;
  buttonBack.Enabled := True;}
end;

procedure TformBrowser.HandlePageLoaderProgress(APercent: Integer);
begin
  labelProgress.Caption := 'Loading a Page';
  progressBar.Position := APercent;
end;

procedure TformBrowser.HandlePageLoaderTerminated(Sender: TObject);
begin
{  labelProgress.Caption := 'Finished Loading';
  progressBar.Position := 100;


  AddURLToHistory(MyPageLoader.LastPageURL);}
end;

procedure TformBrowser.HandleUserAddedPage(Sender: TObject; APage: TCDTabSheet);
begin
  AddBrowserControlsToTab(APage);
end;

procedure TformBrowser.Timer1Timer(Sender: TObject);
const
  StartCount = 2; {timer counts before hint window opens}
  EndCount = 20;  {after this many timer counts, hint window closes}
var
  Pt, Pt1: TPoint;
  ARect: TRect;
  TitleStr: string;

begin
{$ifdef FPBROWSER_THTMLCOMP}
  Inc(TimerCount);
  GetCursorPos(Pt);
  Pt1 := Viewer.ScreenToClient(Pt);
  TitleStr := Viewer.TitleAttr;
  if (TitleStr = '') or not PtInRect(Viewer.ClientRect, Pt1)then
  begin
    OldTitle := '';
    CloseAll;
    Exit;
  end;
  if TitleStr <> OldTitle then
  begin
    TimerCount := 0;
    OldTitle := TitleStr;
    HintWindow.ReleaseHandle;
    HintVisible := False;
    Exit;
  end;

  if TimerCount > EndCount then
    CloseAll
  else if (TimerCount >= StartCount) and not HintVisible then
  begin
    ARect := HintWindow.CalcHintRect(300, TitleStr, Nil);
    with ARect do
      HintWindow.ActivateHint(Rect(Pt.X, Pt.Y+18, Pt.X+Right, Pt.Y+18+Bottom), TitleStr);
    HintVisible := True;
  end;
{$endif}
end;

{HTML for print header and footer}
const
  HFText: string =  '<html><head><style>'+
            'body  {font: Arial 8pt;}'+
          '</style></head>'+
          '<body marginwidth="0">'+
          '<table border="0" cellspacing="2" cellpadding="1" width="100%">'+
            '<tr>'+
              '<td>#left</td><td align="right">#right</td>'+
            '</tr>'+
          '</table></body></html>';

function ReplaceStr(Const S, FromStr, ToStr: string): string;
{replace FromStr with ToStr in string S.
 for Delphi 6, 7, AnsiReplaceStr may be used instead.}
var
  I: integer;
begin
  I := Pos(FromStr, S);
  if I > 0 then
  begin
    Result := S;
    Delete(Result, I, Length(FromStr));
    Insert(ToStr, Result, I);
  end;
end;

initialization
{$IFDEF LCL}
{$I mainform.lrs}  {Include form's resource file}
{$ENDIF}

end.
