
{
Settings Dialog for Cactus Jukebox

written by Sebastian Kraft, <c> 2006-2008

Contact the author at: sebastian_kraft@gmx.de

This Software is published under the GPL






}


Unit settings;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
ExtCtrls, Buttons, ComCtrls, CheckLst, config, playerclass, mplayer;

resourcestring
rsAutoloadLast = 'Load last library at startup';
rsScanForNewFi = 'Scan for new files in background  on startup';
rsLanguage = 'Language';

rsGuessTagFrom = 'Guess tag from filename';
rsMoveToSectio = 'Move to section %sUnknown%s';
rsID3Type = 'ID3 type';
rsID3v1Priorit = 'ID3v1 Priority';
rsID3v2Priorit = 'ID3v2 Priority';
rsGeneral = 'General';
rsPathToMp3pla = 'Path to mp3player mountpoint';
rsCreateSubfol = 'Create subfolders on upload';
rsPathsToAddit = 'Paths to additional application needed for Cactus Jukebox';
rsCdda2wavTool = 'Cdda2wav (tool to rip CDs)';
rsSave = 'Save';
rsCancel = 'Cancel';
rsMobileDevice = 'Mobile Device';
rsPaths = 'Paths';
rsLameNeededTo = 'lame (needed to encode mp3 files)';
rsEnableKDESer = 'Enable KDE Service Menu';
rsAudioOutput = 'Audio Output';
rsDownloadAlbu = 'Download album cover image from internet';
rsClearCache = 'Clear Cache';
rsAutomaticlyS = 'Automaticly start playing first song in playlist';
rsLoadLastPlay = 'Load last playlist on startup';
rsAlwaysStartP = 'Always start playing first song in empty playlist';
rsStopPlayback = 'Stop playback when playlist is cleared';
rsSortAlbumsBy = 'Sort albums by track instead of title';
rsEnablePlugin = 'Enable plugins';



const TSETTINGS_SELECT_NONE = 0;
const TSETTINGS_SELECT_ID3 = 1;

Type

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  { TSettings }
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  TSettings = Class(TForm)
    AudioOut: TComboBox;
    autoload1: TCheckBox;
    Button1: TButton;
    backscan: TCheckBox;
    Button2: TButton;
    CDRomEdit: TEdit;
    AutoPlayBox: TCheckBox;
    AudioBackend: TComboBox;
    Image1: TImage;
    Label10: TLabel;
    Label11: TLabel;
    UseExternalCfgCheckBox: TCheckBox;
    Label9: TLabel;
    MarkGuessBox: TCheckBox;
    ClearCover: TButton;
    AlbumCoverSizeBox: TComboBox;
    CoverDownload: TCheckBox;
    EnablePluginsBox: TCheckBox;
    Label7: TLabel;
    Label8: TLabel;
    CmdlineMemo: TMemo;
    EditCmdlinePanel: TPanel;
    SortAlbumBox: TCheckBox;
    LoadPlaylistBox: TCheckBox;
    StopOnClearBox: TCheckBox;
    Label4: TLabel;
    Label6: TLabel;
    TabSheet6: TTabSheet;
    txtFormatString: TEdit;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    LAudioBackend: TLabel;
    LAudioOut: TLabel;
    PluginList: TCheckListBox;
    guesstag1: TRadioButton;
    GuessTagBox: TGroupBox;
    ID3typebox: TGroupBox;
    kdeservicebox: TCheckBox;
    Label1: TLabel;
    LanguageBox: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    PluginInfo: TMemo;
    PathBox: TGroupBox;
    LLanguage: TLabel;
    Lcdda2wav: TLabel;
    Llame: TLabel;
    Label5: TLabel;
    RadioGroup1: TRadioGroup;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    subfolders: TCheckBox;
    LMountpoint: TLabel;
    PageControl1: TPageControl;
    playerpathedit1: TEdit;
    savebut: TButton;
    cancelbut: TButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet5: TTabSheet;
    AudioTab: TTabSheet;
    unknown1: TRadioButton;
    v1_prio: TRadioButton;
    v1_prio1: TRadioButton;
    v2_prio: TRadioButton;
    v2_prio1: TRadioButton;
    procedure AudioBackendChange(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure ClearCoverClick(Sender: TObject);
    procedure StaticText1Click(Sender: TObject);
    procedure UseExternalCfgCheckBoxChange(Sender: TObject);
    procedure EnablePluginsBoxClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    procedure guesstag1Change(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    Procedure LanguageBoxChange(Sender: TObject);
    Procedure PluginListMouseDown(Sender: TObject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
    Procedure cancelbutClick(Sender: TObject);
    Procedure kdeserviceboxChange(Sender: TObject);
    Procedure savebutClick(Sender: TObject);
    function ShowModal(intActivePage: integer = TSETTINGS_SELECT_NONE): Integer;
    Private 
    { private declarations }
    servicemenu_changed: boolean;
    Public
    { public declarations }
  End;

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Var 
  setupwin: TSettings;


  Implementation

  Uses mainform, translations, functions{, plugin};


  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{ TSettings }
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TSettings.savebutClick(Sender: TObject);

Var
  i: integer;
Begin
  //   Main.mpg123:=mpg123pathedit1.text;
  //   Main.lame:=lamepathedit1.text;
  CactusConfig.DAPPath := IncludeTrailingPathDelimiter(playerpathedit1.Text);
  CactusConfig.CDRomDevice := CDRomEdit.Text;
{$ifdef linux}
  If servicemenu_changed Then
    If kdeservicebox.checked Then
      Begin
        If FileExists(CactusConfig.HomeDir+'/.kde/share/apps/konqueror/servicemenus/') Then
          Begin
            If FileCopy(CactusConfig.DataPrefix+'/tools/cactus_servicemenu.desktop', CactusConfig.
               HomeDir+'/.kde/share/apps/konqueror/servicemenus/cactus_servicemenu.desktop')
              Then CactusConfig.KDEServiceMenu := true
            Else
              Begin
                CactusConfig.KDEServiceMenu := false;
                ShowMessage('ERROR: Couldn''t create service menu...');
              End;
          End;
      End
  Else
    Begin
      DeleteFile(CactusConfig.HomeDir+
                 '/.kde/share/apps/konqueror/servicemenus/cactus_servicemenu.desktop');
      CactusConfig.KDEServiceMenu := true;
    End;
  If AudioOut.ItemIndex=0 Then CactusConfig.AudioSystem := ALSAOUT
       Else CactusConfig.AudioSystem := OSSOUT;
  
  PlayerObj.OutputMode:=CactusConfig.AudioSystem;
  
  if AudioBackend.ItemIndex=0 then CactusConfig.AudioBackend:=MPLAYERBACK
      else CactusConfig.AudioBackend:=FMODBACK;
{$endif}
  CactusConfig.MPlayerUseExternalConfig:=UseExternalCfgCheckBox.Checked;
  if (PlayerObj is TMPlayerClass) then begin
         (PlayerObj as TMPlayerClass).UseExternalConfig:=CactusConfig.MPlayerUseExternalConfig;
         if (PlayerObj as TMPlayerClass).UseExternalConfig then
            CmdlineMemo.Lines.SaveToFile((PlayerObj as TMPlayerClass).ExternalConfigFile);
  end;

  CactusConfig.GuessTag := guesstag1.checked;
  CactusConfig.MarkGuessedTags:= MarkGuessBox.Checked;
  CactusConfig.background_scan := backscan.Checked;

  CactusConfig.id3v2_prio := v2_prio.Checked;
  CactusConfig.mobile_subfolders := subfolders.checked;

  CactusConfig.CoverDownload := CoverDownload.Checked;
  if AlbumCoverSizeBox.ItemIndex=0 then CactusConfig.CoverSize:='medium'
       else CactusConfig.CoverSize:='large';

  CactusConfig.AutostartPlay:=AutoPlayBox.Checked;
  CactusConfig.StopOnClear:=StopOnClearBox.Checked;
  CactusConfig.LoadLastPlaylist:=LoadPlaylistBox.Checked;
  CactusConfig.strTagToNameFormatString := txtFormatString.Text;
  CactusConfig.language := LanguageBox.Items[LanguageBox.ItemIndex];
  CactusConfig.FlushConfig;

  CactusConfig.SortAlbumByTrack:=SortAlbumBox.Checked;

  CactusConfig.PluginsEnabled:=EnablePluginsBox.Checked;

  (*
  For i:=0 To PluginList.Count-1 Do
    If PluginList.Checked[i] Then
      CactusPlugins.Items[i].enabled := true
    Else CactusPlugins.Items[i].enabled := false;
  *)
  TranslateUnitResourceStrings('settings', CactusConfig.DataPrefix+'languages/cactus.%s.po',
                               CactusConfig.language, '');
  TranslateUnitResourceStrings('mp3', CactusConfig.DataPrefix+'languages/cactus.%s.po', CactusConfig
                               .language, '');
  close;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TSettings.ShowModal(intActivePage: integer = TSETTINGS_SELECT_NONE): Integer;
begin

  case intActivePage of
    TSETTINGS_SELECT_NONE: PageControl1.ActivePage := TabSheet1;
    TSETTINGS_SELECT_ID3: PageControl1.ActivePage := TabSheet4;
  end;

  inherited ShowModal;
end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TSettings.cancelbutClick(Sender: TObject);
Begin
  close;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TSettings.kdeserviceboxChange(Sender: TObject);
Begin
  servicemenu_changed := true;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TSettings.Button1Click(Sender: TObject);
Begin
  If FileExists(CactusConfig.DAPPath) Then Main.Selectdirectorydialog1.initialdir := CactusConfig.
                                                                                     DAPPath
  Else Main.Selectdirectorydialog1.initialdir := '/';
  Main.Selectdirectorydialog1.title := 'Choose mp3 player directory...';
  If Main.Selectdirectorydialog1.execute=true Then
    Begin
      playerpathedit1.text := IncludeTrailingPathDelimiter(Main.Selectdirectorydialog1.FileName);
    End;
End;

procedure TSettings.AudioBackendChange(Sender: TObject);
begin
   ShowMessage('Please restart Cactus for audio backend change to take effect');
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TSettings.ClearCoverClick(Sender: TObject);
Begin
  If DirectoryExists(CactusConfig.ConfigPrefix+DirectorySeparator+'covercache') Then
    Begin
      If EraseDirectory(CactusConfig.ConfigPrefix+DirectorySeparator+'covercache') Then
        writeln('Covercache has been cleared...')
      Else writeln('ERROR while clearing covercache...');
    End;
End;

procedure TSettings.StaticText1Click(Sender: TObject);
begin

end;

procedure TSettings.UseExternalCfgCheckBoxChange(Sender: TObject);
begin
  CmdlineMemo.Enabled:=UseExternalCfgCheckBox.Checked;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TSettings.EnablePluginsBoxClick(Sender: TObject);
begin
  If EnablePluginsBox.Checked then begin
       PluginList.Enabled:=true;
       PluginInfo.Enabled:=true;
       Label1.Enabled:=true;
  end else begin
       PluginList.Enabled:=false;
       PluginInfo.Enabled:=false;
       Label1.Enabled:=false;
  end;
 // ShowMessage('You have to restart Cactus Jukebox for changes to take effect!!');
 //TDOD: Add warning that plugins are enabled/disabled only after restart
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TSettings.FormCreate(Sender: TObject);

Var srec: TSearchRec;
  i: integer;
Begin
  //Look for available translations and add them into the combobox
  If FindFirst(IncludeTrailingPathDelimiter(CactusConfig.DataPrefix)+'languages'+DirectorySeparator+
     '*.mo', faAnyFile,srec)=0 Then
    Begin
      Repeat
        Begin
          i := LanguageBox.Items.Add(Copy(srec.Name, 8, length(srec.Name)-10));
          If (CactusConfig.language=LanguageBox.Items[i]) Or (copy(CactusConfig.language, 0, 2)=
             LanguageBox.Items[i]) Then LanguageBox.ItemIndex := i;
        End;
      Until FindNext(srec)<>0;
    End;
   {$ifdef win32}
  label2.Hide;
  CDRomEdit.Hide;;
   {$endif}
  TranslateUnitResourceStrings('settings', CactusConfig.DataPrefix+'languages/cactus.%s.po',
                               CactusConfig.language, copy(CactusConfig.language, 0, 2));
  autoload1.Caption := rsAutoloadLast;
  LoadPlaylistBox.Caption:=rsLoadLastPlay;
  backscan.Caption := rsScanForNewFi;
  LLanguage.Caption := rsLanguage;
  guesstag1.Caption := rsGuessTagFrom;
  unknown1.Caption := Format(rsMoveToSectio, ['"', '"']);
  ID3typebox.Caption := rsID3Type;
  v1_prio.Caption := rsID3v1Priorit;
  v2_prio.Caption := rsID3v2Priorit;
  TabSheet1.Caption := rsGeneral;
  TabSheet2.Caption := rsMobileDevice;
  TabSheet3.Caption := rsPaths;
  LMountpoint.Caption := rsPathToMp3pla;
  subfolders.Caption := rsCreateSubfol;
  PathBox.Caption := rsPathsToAddit;
  Lcdda2wav.Caption := rsCdda2wavTool;
  llame.Caption := rsLameNeededTo;
  savebut.Caption := rsSave;
  cancelbut.Caption := rsCancel;
  kdeservicebox.Caption := rsEnableKDESer;
  LAudioOut.Caption := rsAudioOutput;
  CoverDownload.Caption := rsDownloadAlbu;
  ClearCover.Caption := rsClearCache;
  LLanguage.Caption := rsLanguage;
  AutoPlayBox.Caption:= rsAlwaysStartP;
  StopOnClearBox.Caption:= rsStopPlayback;
  SortAlbumBox.Caption:= rsSortAlbumsBy;
  EnablePluginsBox.Caption:= rsEnablePlugin;




  //   AutostartBox.Caption:=rsAutomaticlyS;

 {$ifdef linux}
  kdeservicebox.Checked := CactusConfig.KDEServiceMenu;
  If CactusConfig.AudioSystem=ALSAOUT then AudioOut.ItemIndex := 0
          Else AudioOut.ItemIndex := 1;
  If CactusConfig.AudioBackend=MPLAYERBACK then begin
     AudioBackend.ItemIndex := 0;
     EditCmdlinePanel.Visible:=true;
     UseExternalCfgCheckBox.Checked:=CactusConfig.MPlayerUseExternalConfig;
     CmdlineMemo.Enabled:=CactusConfig.MPlayerUseExternalConfig;
     CmdlineMemo.Lines.LoadFromFile((PlayerObj as TMPlayerClass).ExternalConfigFile);
  end Else begin
      AudioBackend.ItemIndex := 1;
      EditCmdlinePanel.Visible:=False;
  end;
  servicemenu_changed := false;
  kdeservicebox.Visible := true;
 {$else}
  AudioOut.Visible := false;
  LAudioOut.Visible := false;
  kdeservicebox.Visible := false;
 {$endif}
  CoverDownload.Checked := CactusConfig.CoverDownload;
  if CactusConfig.CoverSize='large' then AlbumCoverSizeBox.ItemIndex:=1
        else AlbumCoverSizeBox.ItemIndex:=0;

  playerpathedit1.text := CactusConfig.DAPPath;
  CDRomEdit.Text := CactusConfig.CDRomDevice;
  If CactusConfig.GuessTag Then begin
        guesstag1.checked := true;
        MarkGuessBox.Enabled:=true;
     end
     Else begin
        unknown1.checked := true;
        MarkGuessBox.Enabled:=false;
     end;
  MarkGuessBox.Checked:=CactusConfig.MarkGuessedTags;
  If CactusConfig.background_scan Then backscan.checked := true
     Else backscan.checked := false;
  If CactusConfig.mobile_subfolders Then subfolders.checked := true
     Else subfolders.checked := false;
  If CactusConfig.id3v2_prio Then v2_prio.Checked := true
     Else v1_prio.checked := true;
  AutoPlayBox.Checked := CactusConfig.AutostartPlay;
  LoadPlaylistBox.Checked:=CactusConfig.LoadLastPlaylist;

  StopOnClearBox.Checked:=CactusConfig.StopOnClear;
  txtFormatString.Text := CactusConfig.strTagToNameFormatString;

  SortAlbumBox.Checked:=CactusConfig.SortAlbumByTrack;

  EnablePluginsBox.Checked:=CactusConfig.PluginsEnabled;
  If EnablePluginsBox.Checked then begin
       PluginList.Enabled:=true;
       PluginInfo.Enabled:=true;
       Label1.Enabled:=true;
  end else begin
       PluginList.Enabled:=false;
       PluginInfo.Enabled:=false;
       Label1.Enabled:=false;
  end;

  (*
  For i:=0 To CactusPlugins.Count-1 Do
    Begin
      writeln(i);
      PluginList.Items.Add(CactusPlugins.Items[i].Name);
      PluginList.Checked[i] := CactusPlugins.Items[i].enabled;
    End;
    *)
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TSettings.guesstag1Change(Sender: TObject);
begin
     if guesstag1.Checked then MarkGuessBox.Enabled:=true
            else MarkGuessBox.Enabled:=false;
end;

procedure TSettings.Label9Click(Sender: TObject);
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TSettings.LanguageBoxChange(Sender: TObject);
Begin
  ShowMessage('To show user interface with new selected language'+LineEnding+
              ' you need to restart cactus Jukebox');
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TSettings.PluginListMouseDown(Sender: TObject; Button: TMouseButton;
                                        Shift: TShiftState; X, Y: Integer);
Var index: integer;
Begin
  index := PluginList.GetIndexAtY(Y);
  PluginInfo.Clear;
  //  if index >= 0 then PluginInfo.Lines.Add(CactusPlugins.Items[index].Comment);
End;




initialization
  {$I settings.lrs}

End.
