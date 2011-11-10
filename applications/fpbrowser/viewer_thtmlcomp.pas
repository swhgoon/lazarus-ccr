unit viewer_thtmlcomp;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  //
  browserviewer,
  //
  HtmlMisc, HTMLsubs, Htmlview, HTMLun2;

type

  { THtmlCompViewer }

  THtmlCompViewer = class(TBrowserViewer)
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
    procedure ViewerImageRequest(Sender: TObject; const SRC: string;
      var Stream: TMemoryStream);
  public
    procedure CreateViewer(AParent, AOwner: TWinControl); override;
    procedure LoadFromFile(AFilename: string); override;
//    procedure LoadFromURL(AURL: string); override;
    function GetDocumentTitle: string; override;
    procedure SetShowImages(AValue: Boolean); override;
    procedure HandlePageLoaderTerminated(Sender: TObject); override;
    procedure Reload; override;
  end;


implementation

{ THtmlCompViewer }

procedure THtmlCompViewer.ViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
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

procedure THtmlCompViewer.ViewerProgress(Sender: TObject;
  Stage: TProgressStage; PercentDone: Integer);
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

procedure THtmlCompViewer.ViewerPrintHTMLFooter(Sender: TObject;
  HFViewer: THTMLViewer; NumPage: Integer; LastPage: Boolean; var XL,
  XR: Integer; var StopPrinting: Boolean);
var
  S: string;
begin
  S := ReplaceStr(HFText, '#left', Viewer.DocumentTitle);
  S := ReplaceStr(S, '#right', Viewer.CurrentFile);
  HFViewer.LoadFromString(S);
end;

procedure THtmlCompViewer.ViewerPrintHTMLHeader(Sender: TObject;
  HFViewer: THTMLViewer; NumPage: Integer; LastPage: Boolean; var XL,
  XR: Integer; var StopPrinting: Boolean);
var
  S: string;
begin
  S := ReplaceStr(HFText, '#left', DateToStr(Date));
  S := ReplaceStr(S, '#right', 'Page '+IntToStr(NumPage));
  HFViewer.LoadFromString(S);
end;

procedure THtmlCompViewer.HotSpotChange(Sender: TObject; const URL: string);
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
procedure THtmlCompViewer.HotSpotClick(Sender: TObject; const URL: string;
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

procedure THtmlCompViewer.RightClick(Sender: TObject;
  Parameters: TRightClickParameters);
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

{ In this event we should provide images for the html component }
procedure THtmlCompViewer.ViewerImageRequest(Sender: TObject;
  const SRC: string; var Stream: TMemoryStream);
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

procedure THtmlCompViewer.CreateViewer(AParent, AOwner: TWinControl);
begin
  ViewerName := 'THTMLComp written in Pascal';

  Viewer := THTMLViewer.Create(AOwner);
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
  //Viewer.OnSoundRequest := SoundRequest;
  Viewer.OnMetaRefresh := MetaRefreshEvent;
  Viewer.OnObjectClick := ObjectClick;
  Viewer.OnRightClick := RightClick;
  Viewer.Parent := AParent;

//  ShowImages.Checked := Viewer.ViewImages;
  Viewer.HistoryMaxCount := MaxHistories;  {defines size of history list}
end;

procedure THtmlCompViewer.LoadFromFile(AFilename: string);
begin
  Viewer.LoadFromFile(HtmlToDos(Trim(AFilename)));
end;

function THtmlCompViewer.GetDocumentTitle: string;
begin
  Result := Viewer.DocumentTitle;
end;

procedure THtmlCompViewer.SetShowImages(AValue: Boolean);
begin
  Viewer.ViewImages := AValue;
end;

procedure THtmlCompViewer.HandlePageLoaderTerminated(Sender: TObject);
begin
  inherited HandlePageLoaderTerminated(Sender);

  Viewer.LoadFromString(MyPageLoader.Contents);
  Caption := Viewer.DocumentTitle;
end;

procedure THtmlCompViewer.Reload;
begin
  Viewer.ReLoad;
  Viewer.SetFocus;
end;

initialization
  SetBrowserViewerClass(THtmlCompViewer);
end.

