
{
Config Object for Cactus Jukebox

written by Sebastian Kraft, <c> 2006-2008

Contact the author at: sebastian_kraft@gmx.de

This Software is published under the GPL






}

Unit config;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, xmlcfg, gettext, playerclass;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    { TConfigObject }
//Object to read and list config data
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Type 
  TConfigObject = Class
    Public 
    GuessTag, MarkGuessedTags, id3v2_prio: boolean;
    Mobile_Subfolders, background_scan, CoverDownload: boolean;
    KDEServiceMenu: boolean;
    AudioSystem: TOutputMode;
    AudioBackend: TAudioBackend;
    AutostartPlay, StopOnClear, LoadLastPlaylist: Boolean;
    language: string;    // country code, e.g. de -> germany
    DAPPath, CDRomDevice: string;
    CurrentSkin, LastLib, StreamColPath, LoadOnStart: string;
    Lame, CDDA2wav: string;
    strTagToNameFormatString: string;

    strCleanLibNotToRemove: string;
    
    DataPrefix, ConfigPrefix, LibraryPrefix, HomeDir: string;
    WWidth, WHeight, WSplitterWidth: Integer;

    AlbumCoverFirsttime, bDisplayPlayTimeNegated: boolean;
    CoverSize:string;

    MPlayerPath: string;
    MPlayerConfigFile: string;
    MPlayerUseExternalConfig: boolean;

    //Columns to show in titelview
    TLShowArtist, TLShowTitle, TLShowAlbum, TLShowGenre, TLShowTrack, TLShowFilename: boolean;
    SortAlbumByTrack: boolean;

    PluginsEnabled: boolean;

    constructor create(ConfigFile:String);
    destructor destroy;

    Function ReadConfig: boolean;
    Function FlushConfig: boolean;
    Procedure Clear;
    Private 
    FConfigPath: string;
    FConfigFile: TXMLConfig;
  End;

Var   CactusConfig: TConfigObject;

Const CONFIGNAME = 'cactus.cfg';


  Implementation

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{ TConfigObject }
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  constructor TConfigObject.create(ConfigFile: String);
Begin
  FConfigPath := ConfigFile;

  FConfigFile := TXMLConfig.Create(Nil);
  FConfigFile.Filename := FConfigPath;
  ReadConfig;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

destructor TConfigObject.destroy;
Begin
  FlushConfig;
  FConfigFile.Free;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TConfigObject.ReadConfig: boolean;

Var tmps1, tmps2: string;
Begin
  result := true;

  Try
    GuessTag := FConfigFile.GetValue('Library/GuessTags', false);
    MarkGuessedTags := FConfigFile.GetValue('Library/MarkGuessedTags', true);
    Mobile_Subfolders := FConfigFile.GetValue('Mobile_Player/Subfolders', true);
    id3v2_prio := FConfigFile.GetValue('Library/id3v2_prio', true);
    //    background_scan:=FConfigFile.GetValue('Library/background_scan', false);
    background_scan := false;
    DAPPath := IncludeTrailingPathDelimiter(FConfigFile.getValue('Mobile_Player/Mountpoint', ''));
    If FConfigFile.GetValue('Networking/Album_Cover_Download/Enabled','')='' Then
      AlbumCoverFirsttime := true;
    CoverDownload := FConfigFile.GetValue('Networking/Album_Cover_Download/Enabled', false);
    CoverSize := FConfigFile.GetValue('Networking/Album_Cover_Download/ImageSize', 'small');

    CurrentSkin := FConfigFile.getValue('Skin/File', 'green.xml');
    KDEServiceMenu := FConfigFile.GetValue('KDE/servicemenu', false);

    {$ifdef linux}
    If FConfigFile.GetValue('Audio/Output', 'Alsa')='Alsa' Then AudioSystem:=ALSAOUT
         Else AudioSystem:=OSSOUT;
    {$endif}{$ifdef win32}
    If FConfigFile.GetValue('Audio/Output', 'Win32')='Win32' Then AudioSystem:=WIN32
         Else AudioSystem:=DIRECTX;
    {$endif}

    If FConfigFile.GetValue('Audio/Backend', 'mplayer')='mplayer' Then AudioBackend:=MPLAYERBACK
         Else AudioBackend:=FMODBACK;

    MPlayerPath:=FConfigFile.GetValue('Audio/Backend/MPlayer/Path','');
    MPlayerUseExternalConfig:=FConfigFile.GetValue('Audio/Backend/MPlayer/UseExternalConfig',false);
    LastLib := FConfigFile.GetValue('Library/autoload','');
    StreamColPath := FConfigFile.GetValue('Library/StreamCollection','');
    AutostartPlay := FConfigFile.GetValue('Playlist/Autoplay', true);
    StopOnClear:= FConfigFile.GetValue('Playlist/StopOnClear', false);
    LoadLastPlaylist:=FConfigFile.getValue('Playlist/LoadPrevious', true);
    SortAlbumByTrack:=FConfigFile.getValue('Library/SortAlbumByTrack', false);

    Lame := FConfigFile.GetValue('Lame/Path', '/usr/bin/lame');
    GetLanguageIDs(tmps1, tmps2);

    {$ifdef linux}
    strTagToNameFormatString := FConfigFile.GetValue('Library/TagToNameFormatString',
                             '%a/%a - %b - %n - %t');
    {$endif}{$ifdef win32}
    strTagToNameFormatString := FConfigFile.GetValue('Library/TagToNameFormatString',
                             '%a\%a - %b - %n - %t');
    {$endif}

    WWidth := FConfigFile.GetValue('Userinterface/Window/Width', 854);
    WHeight := FConfigFile.GetValue('Userinterface/Window/Height', 680);
    WSplitterWidth := FConfigFile.GetValue('Userinterface/Window/SplitterWidth', 270);
    language := FConfigFile.GetValue('Userinterface/Language/Code', tmps1);
    bDisplayPlayTimeNegated := FConfigFile.GetValue('Userinterface/DisplayPlayTimeNegated', false);
    strCleanLibNotToRemove := FConfigFile.GetValue('Userinterface/CleanLibrary/NotToRemove', 'wav mp3 ogg wma fla flac');

    TLShowArtist:= FConfigFile.getValue('Userinterface/Titlelistcolumns/Artist', true);
    TLShowAlbum:= FConfigFile.getValue('Userinterface/Titlelistcolumns/Album', true);
    TLShowTitle:= FConfigFile.getValue('Userinterface/Titlelistcolumns/Title', true);
    TLShowTrack:= FConfigFile.getValue('Userinterface/Titlelistcolumns/Track', true);
    TLShowGenre:= FConfigFile.getValue('Userinterface/Titlelistcolumns/Genre', false);
    TLShowFilename:= FConfigFile.getValue('Userinterface/Titlelistcolumns/Filename', false);

    SortAlbumByTrack:=FConfigFile.getValue('Userinterface/SortAlbumByTrack', false);

    CDRomDevice := FConfigFile.GetValue('Devices/CDROM/Name', '/dev/cdrom');

    PluginsEnabled:= FConfigFile.GetValue('Plugins/Enabled', false);
  Except
    result := false;
  End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TConfigObject.FlushConfig: boolean;
Begin
  result := true;
  Try
    FConfigFile.SetValue('Library/id3v2_prio',id3v2_prio);
    FConfigFile.SetValue('Mobile_Player/Mountpoint', DAPPath);

    case AudioSystem of
         ALSAOUT: FConfigFile.SetValue('Audio/Output', 'Alsa');
         OSSOUT:FConfigFile.SetValue('Audio/Output', 'OSS');
         DIRECTX:FConfigFile.SetValue('Audio/Output', 'DirectX');
         WIN32:FConfigFile.SetValue('Audio/Output', 'Win32');
    end;

    If AudioBackend=MPLAYERBACK Then
      Begin
        FConfigFile.SetValue('Audio/Backend', 'mplayer');
      End
    Else
      Begin
        FConfigFile.SetValue('Audio/Backend', 'fmod');
      End;
    FConfigFile.SetValue('Audio/Backend/MPlayer/Path',MPlayerPath);
    FConfigFile.SetValue('Audio/Backend/MPlayer/UseExternalConfig',MPlayerUseExternalConfig);
   // FConfigFile.SetValue('Audio/Backend/MPlayer/ConfigFile',MPlayerConfigFile);
    FConfigFile.SetValue('Mobile_Player/Subfolders',mobile_subfolders);
    FConfigFile.SetValue('Networking/Album_Cover_Download/Enabled', CoverDownload);
    FConfigFile.SetValue('Networking/Album_Cover_Download/ImageSize', CoverSize);
    FConfigFile.SetValue('Lame/Path', lame);
    FConfigFile.SetValue('Library/GuessTags', guesstag);
    FConfigFile.SetValue('Library/MarkGuessedTags', MarkGuessedTags);
    FConfigFile.SetValue('Library/background_scan', background_scan);
    FConfigFile.SetValue('Library/autoload', LastLib);
    FConfigFile.SetValue('Library/StreamCollection', StreamColPath);
    FConfigFile.SetValue('Library/TagToNameFormatString', strTagToNameFormatString);
    FConfigFile.SetValue('Skin/File', CurrentSkin);
    FConfigFile.SetValue('Userinterface/Language/Code', language);
    FConfigFile.SetValue('Userinterface/DisplayPlayTimeNegated', bDisplayPlayTimeNegated);
    FConfigFile.SetValue('Playlist/Autoplay', AutostartPlay);
    FConfigFile.SetValue('Playlist/StopOnClear', StopOnClear);
    FConfigFile.SetValue('Playlist/LoadPrevious', LoadLastPlaylist);
    FConfigFile.SetValue('Userinterface/Window/Width', WWidth);
    FConfigFile.SetValue('Userinterface/Window/Height', WHeight);
    FConfigFile.SetValue('Userinterface/Window/SplitterWidth', WSplitterWidth);
    FConfigFile.SetValue('Userinterface/CleanLibrary/NotToRemove', strCleanLibNotToRemove);
    FConfigFile.SetValue('Userinterface/Titlelistcolumns/Artist', TLShowArtist);
    FConfigFile.SetValue('Userinterface/Titlelistcolumns/Album', TLShowAlbum);
    FConfigFile.SetValue('Userinterface/Titlelistcolumns/Title', TLShowTitle);
    FConfigFile.SetValue('Userinterface/Titlelistcolumns/Track', TLShowTrack);
    FConfigFile.SetValue('Userinterface/Titlelistcolumns/Genre', TLShowGenre);
    FConfigFile.SetValue('Userinterface/Titlelistcolumns/Filename', TLShowFilename);
    FConfigFile.SetValue('Userinterface/SortAlbumByTrack', SortAlbumByTrack);

    FConfigFile.SetValue('Devices/CDROM/Name', CDRomDevice);

    FConfigFile.SetValue('Plugins/Enabled', PluginsEnabled);

    FConfigFile.Flush;
  Except
    result := false;
  End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TConfigObject.Clear;
Begin
  FConfigFile.Free;
  DeleteFile(IncludeTrailingPathDelimiter(ConfigPrefix)+CONFIGNAME);
  FConfigFile := TXMLConfig.Create(Nil);
  FConfigFile.Filename := FConfigPath;
  ReadConfig;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

End.
