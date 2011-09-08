unit mainform;

{$define FPBROWSER_TURBOPOWERIPRO}
{.$define FPBROWSER_THTMLCOMP}

interface

uses
  LclIntf, LMessages, LclType, LResources, FPimage,
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Menus, StdCtrls, Clipbrd,
  PrintersDlgs,
  ComCtrls,
  {$IFDEF MSWINDOWS} ShellAPI, {$ELSE} Unix, {$ENDIF}
  {$ifdef FPBROWSER_THTMLCOMP}
  HtmlMisc, HTMLsubs, Htmlview, HTMLun2,
  {$endif}
  {$ifdef FPBROWSER_TURBOPOWERIPRO}
  IPHtml, Ipfilebroker, IpMsg,
  {$endif}
  HTMLabt,
  pageloader;

const
  MaxHistories = 6;  {size of History list}
type

  {$ifdef FPBROWSER_TURBOPOWERIPRO}
  { TMyIpHtmlDataProvider }

  TMyIpHtmlDataProvider = class(TIpHtmlDataProvider)
  protected
    function DoGetStream(const URL: string): TStream; override;
  end;
  {$endif}

  { TformBrowser }

  TformBrowser = class(TForm)
    memoSource: TMemo;
    memoDebug: TMemo;
    menuViewDebug: TMenuItem;
    N1: TMenuItem;
    OpenDialog: TOpenDialog;   
    MainMenu: TMainMenu;
    pageBrowser: TPageControl;
    panelBottom: TPanel;
    panelTop: TPanel;
    File1: TMenuItem;
    Open: TMenuItem;
    options1: TMenuItem;
    panelBrowser: TPanel;
    ShowImages: TMenuItem;
    Fonts: TMenuItem;
    editURL: TEdit;
    ReloadButton: TButton;
    BackButton: TButton;
    FwdButton: TButton;
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
    tabBrowser: TTabSheet;
    tabDebug: TTabSheet;
    tabSource: TTabSheet;
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
    procedure OpenFileClick(Sender: TObject);
    procedure ShowImagesClick(Sender: TObject);
    procedure ReloadButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FwdBackClick(Sender: TObject);
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
    procedure ViewerImageRequest(Sender: TObject; const SRC: string;
      var Stream: TMemoryStream);
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
  private
    { Private declarations }
{$IFDEF LCLCarbon}
    AppMenu : TMenuItem;
{$ENDIF}
    Histories: array[0..MaxHistories-1] of TMenuItem;
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
  {$ifdef FPBROWSER_THTMLCOMP}
  private
    Viewer: THTMLViewer;
    FoundObject: TImageObj;
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ViewerProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Integer);
    procedure ViewerPrintHTMLFooter(Sender: TObject; HFViewer: THTMLViewer;
      NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
      var StopPrinting: Boolean);
    procedure ViewerPrintHTMLHeader(Sender: TObject; HFViewer: THTMLViewer;
      NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
      var StopPrinting: Boolean);
    procedure HotSpotChange(Sender: TObject; const URL: string);
    procedure HotSpotClick(Sender: TObject; const URL: string;
              var Handled: boolean);
    procedure RightClick(Sender: TObject;
      Parameters: TRightClickParameters);
  {$endif}
  {$ifdef FPBROWSER_TURBOPOWERIPRO}
  private
    IpHtmlPanel1: TIpHtmlPanel;
    DataProvider1: TMyIpHtmlDataProvider;
    function DataProvider1CanHandle(Sender: TObject; const URL: string
      ): Boolean;
    procedure DataProvider1CheckURL(Sender: TObject; const URL: string;
      var Available: Boolean; var ContentType: string);
    procedure DataProvider1GetHtml(Sender: TObject; const URL: string;
      const PostData: TIpFormDataEntity; var Stream: TStream);
    procedure DataProvider1GetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure DataProvider1Leave(Sender: TIpHtml);
    procedure DataProvider1ReportReference(Sender: TObject; const URL: string);
    procedure ShowHTML(Src: string);
  {$endif}
  public
    { Public declarations }
    MyPageLoaderThread: TPageLoaderThread;
    MyPageLoader: TPageLoader;
    procedure LoadURL(AURL: string);
    procedure HandlePageLoaderProgress(APercent: Integer);
    procedure HandlePageLoaderTerminated(Sender: TObject);
  end;

var
  formBrowser: TformBrowser;

implementation

uses
  Submit, ImgForm;//, FontDlg;

{$ifdef FPBROWSER_TURBOPOWERIPRO}
function TMyIpHtmlDataProvider.DoGetStream(const URL: string): TStream;
var
  ms: TMemoryStream;
begin
  Result:=nil;
  WriteLn('TMyIpHtmlDataProvider.DoGetStream '+URL);

  if URL='fpdoc.css' then begin
    //debugln(['TMyIpHtmlDataProvider.DoGetStream ',FileExists(URL)]);
    ms:=TMemoryStream.Create;
    try
      ms.LoadFromFile(URL);
      ms.Position:=0;
    except
      ms.Free;
    end;
    Result:=ms;
  end;
end;

function TformBrowser.DataProvider1CanHandle(Sender: TObject; const URL: string
  ): Boolean;
begin
  WriteLn('TForm1.DataProvider1CanHandle ',URL);
  Result:=True;
end;

procedure TformBrowser.DataProvider1CheckURL(Sender: TObject; const URL: string;
  var Available: Boolean; var ContentType: string);
begin
  WriteLn('TForm1.DataProvider1CheckURL ',URL);
  Available:=True;
  ContentType:='text/html';
end;

procedure TformBrowser.DataProvider1GetHtml(Sender: TObject; const URL: string;
  const PostData: TIpFormDataEntity; var Stream: TStream);
var
  lStream: TMemoryStream;
begin
  WriteLn('TForm1.DataProvider1GetHtml ',URL);
  MyPageLoader.LoadBinaryResource(URL, lStream);
  Stream := lStream;
end;

procedure TformBrowser.DataProvider1GetImage(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
begin
  //debugln(['TForm1.DataProvider1GetImage ',URL]);
  Picture:=nil;
end;

procedure TformBrowser.DataProvider1Leave(Sender: TIpHtml);
begin

end;

procedure TformBrowser.DataProvider1ReportReference(Sender: TObject; const URL: string
  );
begin
  //debugln(['TForm1.DataProvider1ReportReference ',URL]);
end;

procedure TformBrowser.ShowHTML(Src: string);
var
  ss: TStringStream;
  NewHTML: TIpHtml;
begin
  ss := TStringStream.Create(Src);
  try
    NewHTML := TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
    //debugln(['TForm1.ShowHTML BEFORE SETHTML']);
    IpHtmlPanel1.SetHtml(NewHTML);
    //debugln(['TForm1.ShowHTML BEFORE LOADFROMSTREAM']);
    NewHTML.LoadFromStream(ss);
    //if Anchor <> '' then IpHtmlPanel1.MakeAnchorVisible(Anchor);
  finally
    ss.Free;
  end;
end;
{$endif}

procedure TformBrowser.FormCreate(Sender: TObject);
var
  I: integer;
begin
  MyPageLoader := TPageLoader.Create;

  {$ifdef FPBROWSER_TURBOPOWERIPRO}
  DataProvider1:=TMyIpHtmlDataProvider.Create(Self);
  DataProvider1.Name:='DataProvider1';
  DataProvider1.OnCanHandle:=DataProvider1CanHandle;
  DataProvider1.OnGetHtml:=DataProvider1GetHtml;
  DataProvider1.OnGetImage:=DataProvider1GetImage;
  DataProvider1.OnLeave:=DataProvider1Leave;
  DataProvider1.OnCheckURL:=DataProvider1CheckURL;
  DataProvider1.OnReportReference:=DataProvider1ReportReference;

  IpHtmlPanel1:=TIpHtmlPanel.Create(Self);
  IpHtmlPanel1.Name:='IpHtmlPanel1';
  IpHtmlPanel1.Parent:=panelBrowser;
  IpHtmlPanel1.Align:=alClient;
  IpHtmlPanel1.DefaultFontSize:=10;
  IpHtmlPanel1.DataProvider:=DataProvider1;
  {$endif}

  {$ifdef FPBROWSER_THTMLCOMP}
  Viewer := THTMLViewer.Create(Self);
  Viewer.Left := 1;
  Viewer.Height := 358;
  Viewer.Top := 1;
  Viewer.Width := 611;
  Viewer.OnHotSpotCovered := HotSpotChange;
  Viewer.OnHotSpotClick := HotSpotClick;
  Viewer.OnImageRequest := ViewerImageRequest;
  Viewer.OnFormSubmit := SubmitEvent;
  Viewer.OnHistoryChange := HistoryChange;
  Viewer.OnProgress := ViewerProgress;
  Viewer.TabStop := True;
  Viewer.TabOrder := 0;
  Viewer.Align := alClient;
  Viewer.DefBackground := clWindow;
  Viewer.BorderStyle := htFocused;
  Viewer.HistoryMaxCount := 6;
  Viewer.DefFontName := 'Times New Roman';
  Viewer.DefPreFontName := 'Courier New';
  Viewer.DefFontColor := clWindowText;
  Viewer.DefOverLinkColor := clFuchsia;
  Viewer.ImageCacheCount := 6;
  Viewer.NoSelect := False;
  Viewer.CharSet := DEFAULT_CHARSET;
  Viewer.PrintMarginLeft := 2;
  Viewer.PrintMarginRight := 2;
  Viewer.PrintMarginTop := 2;
  Viewer.PrintMarginBottom := 2;
  Viewer.PrintScale := 1;
  Viewer.OnMouseMove := ViewerMouseMove;
  Viewer.OnProcessing := ProcessingHandler;
  Viewer.OnPrintHTMLHeader := ViewerPrintHTMLHeader;
  Viewer.OnPrintHTMLFooter := ViewerPrintHTMLFooter;
  Viewer.OnInclude := ViewerInclude;
  Viewer.OnSoundRequest := SoundRequest;
  Viewer.OnMetaRefresh := MetaRefreshEvent;
  Viewer.OnObjectClick := ObjectClick;
  Viewer.OnRightClick := RightClick;
  Viewer.Parent := panelBrowser;

  ShowImages.Checked := Viewer.ViewImages;
  Viewer.HistoryMaxCount := MaxHistories;  {defines size of history list}
  {$endif}

  Position := poScreenCenter;

  {$IFDEF DARWIN} //Don't default to within app bundle.
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0)) + '../../../';
  {$ELSE}
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  {$ENDIF}

  Caption := 'HTML Demo, Version '+HTMLAbt.Version;

  for I := 0 to MaxHistories-1 do
  begin      {create the MenuItems for the history list}
    Histories[I] := TMenuItem.Create(HistoryMenuItem);
    HistoryMenuItem.Insert(I, Histories[I]);
    with Histories[I] do
    begin
    Visible := False;
    OnClick := HistoryClick;
    Tag := I;
    end;
  end;

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
    {$ifdef FPBROWSER_THTMLCOMP}
    Viewer.LoadFromFile(HtmlToDos(Trim(S)));
    {$endif}
  end;
end;

procedure TformBrowser.OpenFileClick(Sender: TObject);
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  if Viewer.CurrentFile <> '' then
    OpenDialog.InitialDir := ExtractFilePath(Viewer.CurrentFile);
  OpenDialog.Filter := 'HTML Files (*.htm,*.html)|*.htm;*.html';  //might have changed
  if OpenDialog.Execute then
  begin
    Update;
    Viewer.LoadFromFile(OpenDialog.Filename);
    Caption := Viewer.DocumentTitle;
  end;
  {$endif}
end;

procedure TformBrowser.editURLKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    LoadURL(editURL.Text);
  end;
end;

procedure TformBrowser.menuViewDebugClick(Sender: TObject);
begin
  pageBrowser.ShowTabs := True;
  pageBrowser.ActivePageIndex := 2;
end;

{$ifdef FPBROWSER_THTMLCOMP}
procedure TformBrowser.HotSpotChange(Sender: TObject; const URL: string);
{mouse moved over or away from a hot spot.  Change the status line}
var
  Caption: string;
begin
  Caption := '';
  if URL <> '' then
    Caption := Caption+'URL: '+URL+'     ';
  if Viewer.TitleAttr <> '' then
    Caption := Caption+'Title: '+Viewer.TitleAttr;
  panelBottom.Caption := Caption;
end;

{This routine handles what happens when a hot spot is clicked.  The assumption
 is made that DOS filenames are being used. .EXE, .WAV, .MID, and .AVI files are
 handled here, but other file types could be easily added.

 If the URL is handled here, set Handled to True.  If not handled here, set it
 to False and ThtmlViewer will handle it.}
procedure TformBrowser.HotSpotClick(Sender: TObject; const URL: string;
          var Handled: boolean);
const
  snd_Async = $0001;  { play asynchronously }
var
  PC: array[0..255] of char;
{$IFDEF LCL}
  PC2: array[0..255] of char;
{$ENDIF}  
  S, Params: string[255];
  Ext: string[5];
  ID: string;
  AbsURL: string;
  I, J, K: integer;
begin
  Handled := False;

  {The following looks for a link of the form, "IDExpand_XXX".  This is interpreted
   as meaning a block with an ID="XXXPlus" or ID="XXXMinus" attribute should
   have its Display property toggled.
  }
  I := Pos('IDEXPAND_', Uppercase(URL));
  if I=1 then
  begin
    ID := Copy(URL, 10, Length(URL)-9);
    Viewer.IDDisplay[ID+'Plus'] := not Viewer.IDDisplay[ID+'Plus'];
    Viewer.IDDisplay[ID+'Minus'] := not Viewer.IDDisplay[ID+'Minus'];
    Viewer.Reformat;
    Handled := True;
    Exit;
  end;

  AbsURL := MyPageLoader.URLToAbsoluteURL(URL);
  J := Pos('HTTP:', UpperCase(AbsURL));
  if (J > 0) then
  begin
    LoadURL(AbsURL);
    Handled := True;
    Exit;
  end;

  I := Pos(':', URL);
  J := Pos('FILE:', UpperCase(URL));
  if (I <= 2) or (J > 0) then
  begin                      {apparently the URL is a filename}
    S := URL;
    K := Pos(' ', S);     {look for parameters}
    if K = 0 then K := Pos('?', S);  {could be '?x,y' , etc}
    if K > 0 then
    begin
      Params := Copy(S, K+1, 255); {save any parameters}
      S[0] := chr(K-1);            {truncate S}
    end
    else Params := '';
    S := Viewer.HTMLExpandFileName(S);
    Ext := Uppercase(ExtractFileExt(S));
    if Ext = '.WAV' then
    begin
      Handled := True;
{$IFNDEF LCL}
      sndPlaySound(StrPCopy(PC, S), snd_ASync);
{$ENDIF}
    end
    else if Ext = '.EXE' then
    begin
      Handled := True;
{$IFNDEF LCL}
      WinExec(StrPCopy(PC, S+' '+Params), sw_Show);
{$ELSE}
 {$IFDEF MSWINDOWS}
      ShellExecute(Handle, nil, StrPCopy(PC, S), StrPCopy(PC2, Params),
                 nil, SW_SHOWNORMAL); 
 {$ELSE}  //Not sure if this makes any sense since executable won't have .exe.
  {$IFDEF DARWIN}
      Shell('open -n "' + S + '" --args "' + Params + '"');
  {$ELSE}
      Shell('"' + S + '" "' + Params + '"');
  {$ENDIF}
 {$ENDIF}
{$ENDIF}
    end
    else if (Ext = '.MID') or (Ext = '.AVI')  then
    begin
      Handled := True;
{$IFNDEF LCL}
      WinExec(StrPCopy(PC, 'MPlayer.exe /play /close '+S), sw_Show);
{$ELSE}
 {$IFDEF MSWINDOWS}
      ShellExecute(Handle, nil, 'MPlayer.exe', '/play /close',
                 nil, SW_SHOWNORMAL); 
 {$ELSE}  //No equivalent to MPlayer?
 {$ENDIF}
{$ENDIF}
    end;
  {else ignore other extensions}
    editURL.Text := URL;
    Exit;
  end;

  I := Pos('MAILTO:', UpperCase(URL));
  if (I > 0) then
  begin
{$IFDEF MSWINDOWS}
    ShellExecute(0, nil, pchar(URL), nil, nil, SW_SHOWNORMAL);
{$ELSE}
 {$IFDEF DARWIN}
    Shell('open "' + URL + '"');
 {$ELSE}
    Shell('"' + URL + '"');  //use LCL's OpenURL?
 {$ENDIF}
{$ENDIF}
    Handled := True;
    Exit;
  end;

  editURL.Text := URL;   {other protocall}
end;
{$endif}

{The Show Images menu item was clicked}
procedure TformBrowser.ShowImagesClick(Sender: TObject);
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  Viewer.ViewImages := not Viewer.ViewImages;
  (Sender as TMenuItem).Checked := Viewer.ViewImages;
  {$endif}
end;

procedure TformBrowser.ReloadButtonClick(Sender: TObject);
{the Reload button was clicked}
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  Viewer.ReLoadButton.Enabled := False;
  Viewer.ReLoad;
  Viewer.ReLoadButton.Enabled := CurrentFile <> '';
  Viewer.SetFocus;
  {$endif}
end;

procedure TformBrowser.FwdBackClick(Sender: TObject);
{Either the Forward or Back button was clicked}
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  with Viewer do
  begin
  if Sender = BackButton then
    HistoryIndex := HistoryIndex +1
  else
    HistoryIndex := HistoryIndex -1;
  Self.Caption := DocumentTitle;      
  end;
  {$endif}
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
  FwdButton.Enabled := HistoryIndex > 0;
  BackButton.Enabled := HistoryIndex < History.Count-1;

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


procedure TformBrowser.SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: String;
  Results: TStringList);
begin
with SubmitForm do
  begin
  ActionText.Text := AnAction;
  MethodText.Text := Method;
  ResultBox.Items := Results;
  Results.Free;
  Show;
  end;
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
  FwdButton.Enabled := False;
  BackButton.Enabled := False;
  ReLoadButton.Enabled := False;
  Print1.Enabled := False;
  PrintPreview.Enabled := False;
  Find1.Enabled := False;
  SelectAllItem.Enabled := False;
  Open.Enabled := False;
  CloseAll;    {in case hint window is open}
  end
else
  begin
  FwdButton.Enabled := Viewer.HistoryIndex > 0;
  BackButton.Enabled := Viewer.HistoryIndex < Viewer.History.Count-1;
  ReLoadButton.Enabled := Viewer.CurrentFile <> '';
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
  ReloadButton.Enabled := False;
  Update;
  Viewer.LoadTextFile(OpenDialog.Filename);
  if Viewer.CurrentFile  <> '' then
    begin
    Caption := Viewer.DocumentTitle;
    ReLoadButton.Enabled := True;
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
  ReloadButton.Enabled := False;
  Viewer.LoadImageFile(OpenDialog.Filename);
  if Viewer.CurrentFile  <> '' then
    begin
    Caption := Viewer.DocumentTitle;
    ReLoadButton.Enabled := True;
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

{ In this event we should provide images for the html component }
procedure TformBrowser.ViewerImageRequest(Sender: TObject; const SRC: string;
  var Stream: TMemoryStream);
var
  J: Integer;
  URL: string;
begin
  URL := MyPageLoader.URLToAbsoluteURL(SRC);

  J := Pos('http:', LowerCase(URL));
  if (J > 0) then
  begin
    MyPageLoader.LoadBinaryResource(URL, Stream);
    Exit;
  end;
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
  MyPageLoader.Free;
end;

{$ifdef FPBROWSER_THTMLCOMP}
procedure TformBrowser.RightClick(Sender: TObject; Parameters: TRightClickParameters);
var
  Pt: TPoint;
  S, Dest: string;
  I: integer;
  HintWindow: THintWindow;  
  ARect: TRect;
begin
  with Parameters do
  begin
  FoundObject := Image;
  ViewImage.Enabled := (FoundObject <> Nil) and (FoundObject.Bitmap <> Nil);
  CopyImageToClipboard.Enabled := (FoundObject <> Nil) and (FoundObject.Bitmap <> Nil);
  if URL <> '' then
    begin
    S := URL;
    I := Pos('#', S);
    if I >= 1 then
      begin
      Dest := System.Copy(S, I, 255);  {local destination}
      S := System.Copy(S, 1, I-1);     {the file name}
      end
    else
      Dest := '';    {no local destination}
    if S = '' then S := Viewer.CurrentFile
      else S := Viewer.HTMLExpandFileName(S);
    NewWindowFile := S+Dest;
    OpenInNewWindow.Enabled := FileExists(S);
    end
  else OpenInNewWindow.Enabled := False;

  GetCursorPos(Pt);
  if Length(CLickWord) > 0 then
    begin
    HintWindow := THintWindow.Create(Self);   
    try
      ARect := Rect(0,0,0,0);
      DrawTextW(HintWindow.Canvas.Handle, @ClickWord[1], Length(ClickWord), ARect, DT_CALCRECT);
      with ARect do
        HintWindow.ActivateHint(Rect(Pt.X+20, Pt.Y-(Bottom-Top)-15, Pt.x+30+Right, Pt.Y-15), ClickWord);
      PopupMenu.Popup(Pt.X, Pt.Y);
    finally
      HintWindow.Free;
      end;
    end
  else PopupMenu.Popup(Pt.X, Pt.Y);
  end;
end;
{$endif}

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

{$ifdef FPBROWSER_THTMLCOMP}
procedure TformBrowser.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TitleStr: string;
begin
  if not Timer1.Enabled and Assigned(ActiveControl) and ActiveControl.Focused then    {9.25}
  begin
    TitleStr := Viewer.TitleAttr;
    if TitleStr = '' then
      OldTitle := ''
    else if TitleStr <> OldTitle then
    begin
      TimerCount := 0;
      Timer1.Enabled := True;
      OldTitle := TitleStr;
    end;
  end;
end;
{$ENDIF}

procedure TformBrowser.CloseAll;
begin
  Timer1.Enabled := False;
  HintWindow.ReleaseHandle;
  HintVisible := False;
end;

procedure TformBrowser.LoadURL(AURL: string);
begin
  MyPageLoaderThread := TPageLoaderThread.Create(True);
  MyPageLoaderThread.URL := AURL;
  MyPageLoaderThread.PageLoader := MyPageLoader;
  MyPageLoaderThread.OnPageLoadProgress := HandlePageLoaderProgress;
  MyPageLoaderThread.OnTerminate := HandlePageLoaderTerminated;
  MyPageLoaderThread.FreeOnTerminate := True;
  MyPageLoaderThread.Resume;
end;

procedure TformBrowser.HandlePageLoaderProgress(APercent: Integer);
begin

end;

procedure TformBrowser.HandlePageLoaderTerminated(Sender: TObject);
begin
  {$ifdef FPBROWSER_THTMLCOMP}
  Viewer.LoadFromString(MyPageLoader.Contents);
  Caption := Viewer.DocumentTitle;
  {$endif}
  {$ifdef FPBROWSER_TURBOPOWERIPRO}
  ShowHTML(MyPageLoader.Contents);
  {$endif}

  // Load source and debug info
  memoSource.Lines.Clear();
  memoSource.Lines.AddStrings(MyPageLoader.ContentsList);
  memoDebug.Lines.Clear();
  memoDebug.Lines.AddStrings(MyPageLoader.DebugInfo);
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

{$ifdef FPBROWSER_THTMLCOMP}
procedure TformBrowser.ViewerProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Integer);
begin
ProgressBar.Position := PercentDone;
case Stage of
  psStarting:
    ProgressBar.Visible := True;
  psRunning:;
  psEnding:
    ProgressBar.Visible := False;
  end;
ProgressBar.Update;
end;
{$endif}

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

{$ifdef FPBROWSER_THTMLCOMP}
procedure TformBrowser.ViewerPrintHTMLHeader(Sender: TObject;
  HFViewer: THTMLViewer; NumPage: Integer; LastPage: boolean; var XL, XR: integer; var StopPrinting: Boolean);
var
  S: string;
begin
S := ReplaceStr(HFText, '#left', Viewer.DocumentTitle);
S := ReplaceStr(S, '#right', Viewer.CurrentFile);
HFViewer.LoadFromString(S);
end;

procedure TformBrowser.ViewerPrintHTMLFooter(Sender: TObject;
  HFViewer: THTMLViewer; NumPage: Integer; LastPage: boolean; var XL, XR: integer; var StopPrinting: Boolean);
var
  S: string;
begin
S := ReplaceStr(HFText, '#left', DateToStr(Date));
S := ReplaceStr(S, '#right', 'Page '+IntToStr(NumPage));
HFViewer.LoadFromString(S);
end;
{$endif}

initialization
{$IFDEF LCL}
{$I mainform.lrs}  {Include form's resource file}
{$ENDIF}

end.
