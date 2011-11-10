{
Main unit for Cactus Jukebox

written by Sebastian Kraft, <c> 2006-2008

Contact the author at: sebastian_kraft@gmx.de

This Software is published under the GPL

}

//TODO: Check if position icon in playlist works after loading playlist from file

unit mainform;

//{$mode delphi}{$H+}
{$mode objfpc}{$H+}
{$ifdef CPU86}//compile with fmod support enabled by default on i386
   {$define fmod}
{$endif}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, ComCtrls, StdCtrls, Menus,{$ifdef fmod} fmodplayer,{$endif}
  ActnList, FileUtil, mediacol, dos, SimpleIPC, functions, EditBtn, last_fm, debug, config,
  playlist, playerclass, MPlayer, mp3file, Messages, LMessages, cj_interfaces;

resourcestring
  rsQuit = 'Quit';
  rsFile = 'File';
  rsOpenFile = 'Open File...';
  rsOpenDirector = 'Open Directory...';
  rsPlayerOnly = 'Player only';
  rsChooseSkin = 'Choose Skin...';
  rsSettings = 'Settings...';
  rsLibrary = 'Library';
  rsNewLibrary = 'New library';
  rsLoadLibrary = 'Load library';
  rsSaveLibrary = 'Save library';
  rsLibraryInfo = 'Library info';
  rsManageLibrar = 'Manage library...';
  // unused ? rsRescanDirect = 'Rescan directories';
  rsPlaylist = 'Playlist';
  rsPlay = 'Play';
  rsNext = 'Next';
  rsPrevious = 'Previous';
  rsMute = 'Mute';
  rsLoadPlaylist = 'Load playlist';
  rsSavePlaylist = 'Save playlist';
  rsMobilePlayer = 'Mobile player';
  rsClearPlaylist = 'Clear Playlist';
  rsRandomPlaylist = 'Random Playlist';
  rsDevices = 'Devices';
  rsDeviceInfo = 'Device info';
  rsScanPlayer = 'Scan player';
  rsSync = 'Sync';
  rsClearPlayer = 'Clear player';
  rsUndoSelectio = 'Undo selection';
  rsAudioCD = 'Audio CD';
  rsRipEncode = 'Rip / Encode...';
  rsHelp = 'Help';
  rsAbout = 'About...';
  rsManual = 'Manual...';
  rsClear = 'Clear';
  rsSearch = 'Search';
  rsAlbum = 'Album';
  rsFilename = 'Filename';
  rsArtist = 'Artist';
  rsTitle = 'Title';
  rsRandom = 'Random';
  rsNotConnected = 'Device not Connected';
  rsOK = 'OK';
  rsLenght = 'Length';
  rsTrack = 'Track';

type
  TCactusViewMode = (cvmLibrary, cvmArtist, cvmDevice, cvmNetwork);

  TCactusFlags = (cfTrayIconPressed, cfProgHide);

  { TMain }

  TMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    artistsearch: TEdit;
    ArtistTree: TTreeView;
    Button1: TButton;
    Button2: TButton;
    clear_list: TBitBtn;
    CoverImage: TImage;
    current_title_edit: TEdit;
    current_title_edit1: TEdit;
    FileItem: TMenuItem;
    filetypebox: TComboBox;
    ImageListNormal: TImageList;
    ImageListHot: TImageList;
    ImageListDis: TImageList;
    itemPlugins: TMenuItem;
    lblPath: TLabel;
    LibraryModeBtn: TToolButton;
    Mainmenu1: TMainMenu;
    MenuItem12: TMenuItem;
    Menuitem21: TMenuItem;
    Menuitem24: TMenuItem;
    MenuItem25: TMenuItem;
    itemTrayExit: TMenuItem;
    itemTrayPlay: TMenuItem;
    itemTrayStop: TMenuItem;
    itemTrayNext: TMenuItem;
    itemTrayPrev: TMenuItem;
    MenuItem27: TMenuItem;
    menuShowCactus: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MIabout: TMenuItem;
    MIClearPlayer: TMenuItem;
    MIclear_playlist: TMenuItem;
    MIDeviceInfo: TMenuItem;
    MIDevices: TMenuItem;
    MIhelp: TMenuItem;
    MIlibinfo: TMenuItem;
    MIlibrary: TMenuItem;
    MIloadlib: TMenuItem;
    MIload_list: TMenuItem;
    MIManagLib: TMenuItem;
    MImanual: TMenuItem;
    MIMobilePlayer: TMenuItem;
    MImute: TMenuItem;
    MInewlib: TMenuItem;
    MInext: TMenuItem;
    MIplay: TMenuItem;
    MIPlaylist: TMenuItem;
    MIprevious: TMenuItem;
    MIrandom_playlist: TMenuItem;
    MIRipAudio: TMenuItem;
    MIsavelib: TMenuItem;
    MIsave_list: TMenuItem;
    MIScanPlayer: TMenuItem;
    MISyncPlayer: TMenuItem;
    MIUndoPlayer: TMenuItem;
    MIViewArtist: TMenuItem;
    MIViewTitle: TMenuItem;
    MIViewAlbum: TMenuItem;
    MIViewTrack: TMenuItem;
    MIViewGenre: TMenuItem;
    MIViewFilename: TMenuItem;
    mnuCleanLib: TMenuItem;
    MTitleView: TMenuItem;
    opendir: TMenuItem;
    openfile: TMenuItem;
    player_lib: TMenuItem;
    PopupMenuTray: TPopupMenu;
    MIremoveRadio: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem32: TMenuItem;
    mute: TSpeedButton;
    NetworktreePopup: TPopupMenu;
    NextButtonImg: TImage;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlPlaytime: TPanel;
    PauseButtonImg: TImage;
    PlayButtonImg: TImage;
    PlayerControlsPanel: TPanel;
    Playlist: TListView;
    PreviousButtonImg: TImage;
    QuitItem: TMenuItem;
    randomcheck: TCheckBox;
    RemoveItem: TMenuItem;
    removselectItem: TMenuItem;
    searchstr: TEdit;
    SettingsItem: TMenuItem;
    skinmenu: TMenuItem;
    space3: TMenuItem;
    space4: TMenuItem;
    spacer15: TMenuItem;
    spacer29: TMenuItem;
    spacer41: TMenuItem;
    Splitter2: TSplitter;
    SrchFileItem: TMenuItem;
    SrchArtItem: TMenuItem;
    SrchTitleItem: TMenuItem;
    SrchAlbumItem: TMenuItem;
    SearchMenu: TPopupMenu;
    SidebarImgList: TImageList;
    MenuItem13: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem6: TMenuItem;
    srch_button: TButton;
    StopButtonImg: TImage;
    ToolBar1: TToolBar;
    ArtistsModeBtn: TToolButton;
    NetModeBtn: TToolButton;
    DeviceModeBtn: TToolButton;
    Label2: TLabel;
    MenuItem2: TMenuItem;
    Panel4: TPanel;
    ArtistSrchField: TPanel;
    SimpleIPCServer1: TSimpleIPCServer;
    SpeedButton1: TSpeedButton;
    TitleTree: TListView;
    MenuItem11: TMenuItem;
    Panel1: TPanel;
    Menuitem26: TMenuItem;
    MenuItem3: TMenuItem;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    toplaylistitem: TMenuItem;
    ImageList1: TImageList;
    MenuItem14: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem30: TMenuItem;
    rm_artist_playeritem: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem33: TMenuItem;
    SaveDialog1: TSaveDialog;
    checkmobile: TTimer;
    space11: TMenuItem;
    Menuitem27a: TMenuItem;
    space1: TMenuItem;
    artisttreemenu: TPopupMenu;
    TEditID3item: TMenuItem;
    Menuitem1: TMENUITEM;
    Menuitem4: TMENUITEM;
    Menuitem5: TMENUITEM;
    Menuitem8: TMENUITEM;
    playlistmenu: TPOPUPMENU;
    titlelistmenu: TPOPUPMENU;
    Selectdirectorydialog1: TSELECTDIRECTORYDIALOG;
    playtimer: TTimer;
    seldirdialog: TSelectDirectoryDialog;
    trackbar: TTrackBar;
    Trackinfo: TSpeedButton;
    TrayIcon: TTrayIcon;
    Volumebar: TProgressBar;
    procedure ApplicationProperties1Minimize(Sender: TObject);
    procedure ArtistTreeClick(Sender: TObject);
    procedure ArtistTreeDblClick(Sender: TObject);
    procedure ArtistTreeEndDrag(Sender, Target: TObject; X, Y: integer);
    procedure ArtistTreeKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ArtistTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ArtistTreeSelectionChanged(Sender: TObject);
    procedure ArtistTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CoverImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DeviceModeBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure itemTrayExitClick(Sender: TObject);
    procedure itemTrayPlayClick(Sender: TObject);
    procedure ArtistsModeBtnClick(Sender: TObject);
    procedure LibraryModeBtnClick(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem25Click(Sender: TObject);
    procedure MIrandom_playlistClick(Sender: TObject);
    procedure MIViewAlbumClick(Sender: TObject);
    procedure MIViewArtistClick(Sender: TObject);
    procedure MenuItem32Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure MIDeviceInfoClick(Sender: TObject);
    procedure MIremoveRadioClick(Sender: TObject);
    procedure MIRipAudioClick(Sender: TObject);
    procedure MIViewFilenameClick(Sender: TObject);
    procedure MIViewGenreClick(Sender: TObject);
    procedure MIViewTitleClick(Sender: TObject);
    procedure MIViewTrackClick(Sender: TObject);
    procedure mnuCleanLibClick(Sender: TObject);
    procedure NetModeBtnClick(Sender: TObject);
    procedure NextButtonImgClick(Sender: TObject);
    procedure NextButtonImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure NextButtonImgMouseEnter(Sender: TObject);
    procedure NextButtonImgMouseLeave(Sender: TObject);
    procedure NextButtonImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PlaylistCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: boolean);
    procedure pnlPlaytimeClick(Sender: TObject);
    procedure PopupMenuTrayPopup(Sender: TObject);
    procedure randomcheckChange(Sender: TObject);
    procedure SearchPanelClick(Sender: TObject);
    procedure PlayerControlsPanelClick(Sender: TObject);
    procedure PauseButtonImgClick(Sender: TObject);
    procedure PauseButtonImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PauseButtonImgMouseEnter(Sender: TObject);
    procedure PauseButtonImgMouseLeave(Sender: TObject);
    procedure PauseButtonImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PlayButtonImgClick(Sender: TObject);
    procedure PlayButtonImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PlayButtonImgMouseEnter(Sender: TObject);
    procedure PlayButtonImgMouseLeave(Sender: TObject);
    procedure PlayButtonImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MainClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MainCreate(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem37Click(Sender: TObject);
    procedure MenuItem43Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure Panel4Click(Sender: TObject);
    procedure ArtistSrchFieldClick(Sender: TObject);
    procedure PreviousButtonImgClick(Sender: TObject);
    procedure PreviousButtonImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PreviousButtonImgMouseEnter(Sender: TObject);
    procedure PreviousButtonImgMouseLeave(Sender: TObject);
    procedure PreviousButtonImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SettingsItemClick(Sender: TObject);
    procedure SimpleIPCServer1Message(Sender: TObject);
    procedure skinmenuClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);

    procedure Splitter1Moved(Sender: TObject);
    procedure SrchAlbumItemClick(Sender: TObject);
    procedure SrchArtItemClick(Sender: TObject);
    procedure SrchFileItemClick(Sender: TObject);
    procedure SrchTitleItemClick(Sender: TObject);
    procedure StopButtonImgClick(Sender: TObject);
    procedure StopButtonImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure StopButtonImgMouseEnter(Sender: TObject);
    procedure StopButtonImgMouseLeave(Sender: TObject);
    procedure StopButtonImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure TestPlugin1(Sender: TObject);

    procedure TitleTreeClick(Sender: TObject);
    procedure TitleTreeColumnClick(Sender: TObject; Column: TListColumn);
    procedure TitleTreeDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure TitleTreeEndDrag(Sender, Target: TObject; X, Y: integer);
    procedure TitleTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure TitleTreeSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure TitleTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure TrackInfoClick(Sender: TObject);
    procedure artisttreemenuPopup(Sender: TObject);
    procedure checkmobileTimer(Sender: TObject);
    procedure clearPlayerItemClick(Sender: TObject);
    procedure clear_listClick(Sender: TObject);
    procedure filetypeboxChange(Sender: TObject);
    procedure libinfoClick(Sender: TObject);
    procedure muteClick(Sender: TObject);
    procedure opendirClick(Sender: TObject);
    procedure openfileClick(Sender: TObject);
    procedure pauseClick(Sender: TObject);
    procedure player_libClick(Sender: TObject);
    procedure playlistClick(Sender: TObject);
    procedure playlistDblClick(Sender: TObject);
    procedure playlistDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure playlistDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure playlistEndDrag(Sender, Target: TObject; X, Y: integer);
    procedure playlistKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure playlistKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure playlistMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure playlistSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure playlistStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure playtimerStartTimer(Sender: TObject);
    procedure prevClick(Sender: TObject);
    procedure EditID3itemClick(Sender: TObject);
    procedure MenuItem30Click(Sender: TObject);
    procedure MenuItem33Click(Sender: TObject);
    procedure rm_artist_playeritemClick(Sender: TObject);
    procedure searchstrClick(Sender: TObject);
    procedure syncplayeritem(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem22aClick(Sender: TObject);
    procedure Menuitem10Click(Sender: TObject);
    procedure RemoveClick(Sender: TObject);
    procedure QuitItemClick(Sender: TObject);
    procedure TitleTreeDblClick(Sender: TObject);
    procedure loadlibClick(Sender: TObject);
    procedure newlibClick(Sender: TObject);
    procedure nextClick(Sender: TObject);
    procedure playClick(Sender: TObject);
    procedure playtimerTimer(Sender: TObject);
    procedure removeselectClick(Sender: TObject);
    procedure save_listClick(Sender: TObject);
    procedure savelibClick(Sender: TObject);
    procedure scanplayeritemClick(Sender: TObject);
    procedure searchstrKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure srch_buttonClick(Sender: TObject);
    procedure stopClick(Sender: TObject);
    procedure titlelistmenuPopup(Sender: TObject);
    procedure toggle_playpause(Sender: TObject);
    procedure trackbarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure trackbarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure TrayIconClick(Sender: TObject);
    procedure undoSyncItemClick(Sender: TObject);

    procedure loadskin(Sender: TObject);
    procedure update_player_hdd_relations;
    procedure VolumebarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    { private declarations }
    ctrl_pressed, SplitterResize: boolean;

    oldSplitterWidth, LoopCount: integer;
    sourceitem: TListItem;
    CoverFound, title_drag, playlist_drag, artist_drag: boolean;
    ViewMode: TCactusViewMode;
    LastFMAPI: TLastfmAPIObject;
    ScanSyncCount: integer;
    FileOpneDialogPath: string;
    bPnlPlaytimeNegated: boolean;
    oldWindowState: TWindowState;
    fromTrayDBLClick: boolean;
    FFlags: set of TCactusFlags;
    procedure MoveNode(TargetNode, SourceNode: TTreeNode);
    procedure ApplicationIdle(Sender: TObject; var Done: boolean);
    procedure update_player_display;
    function LoadFile(path: string): boolean;
    procedure MinimizeMe(Data: Ptrint);

  public
    player_connected, playermode: boolean;
    playpos: integer;
    playnode: TTreeNode;
    playitem: TListitem;
    curlib: string;

    tempbitmap, timetmpbmp: TBitmap;
    player_freespace, player_totalspace: longint;

    skinmenuitems: array[1..16] of TMenuItem;

    TestPluginI: TCJ_MenuItem;

    procedure update_playlist;
    procedure disconnectDAP;
    function connectDAP: byte;
    procedure ScanSyncronize(dir: string);
    procedure update_artist_view;
    procedure update_library_view;

    { public declarations }

    //Test Plugins....cut in future
    procedure SayHello(Sender: TCJ_MenuItem);
    function SayMsgHello(var Message: TMessage): boolean;
    function SayMsgHello2(var Message: TMessage): boolean;

  end;


type

  { TScanThread }

  TScanThread = class(TThread)
  private
    procedure ShowStatus;
  protected
    procedure Execute;
      override;
  public
    fStatus: byte;
    tmpcollection: TMediaCollectionClass;
    PTargetCollection: TMediaCollectionClass;
    constructor Create(Suspd: boolean);
  end;

{ TScanThread }

type

  { TSyncThread }

  TSyncAction = (SCopy, SDelete);

  TSyncThread = class(TThread)
  private
    procedure SyncStatus;
  protected
    CopyList, TargetList, DeleteList: TStringList;
    DeletedCnt, DeleteTotal, CopyTotal, CopiedCnt: integer;
    OpSuccess, finished: boolean;
    SAction: TSyncAction;
    TargetCollection: TMediaCollectionClass;
    procedure Execute;
      override;
  public
    Target: string;
    constructor Create(Suspd: boolean);
    destructor Destroy;
      override;
    procedure CopyFile(fromFile, toFile: string);
    procedure DeleteFile(path: string);
  end;

{ TSyncThread }


var
  Main: TMain;
  SyncThread: TSyncThread;
  ScanThread: TscanThread;

//procedure update_title_view_album;
procedure update_title_view;
procedure artist_to_playlist;
procedure artist_to_playlist_at(index: integer);
procedure title_to_playlist_at(index: integer);
procedure title_to_playlist;


implementation

uses editid3, status, settings, player, directories, skin, cdrip,
  translations, bigcoverimg,
  streamcol, addradio, CleanLibrary, global_vars, cj_pluginslist,
  cj_interfaces_impl, LCLType;

{$R *.lfm}

{$i cactus_const.inc}

var
  sizediff: int64;


{ TSyncThread }

procedure TSyncThread.SyncStatus;
begin
  if finished = False then
  begin
    if SAction = SCopy then
      Main.StatusBar1.Panels[1].Text :=
        IntToStr(CopiedCnt) + ' of ' + IntToStr(
        CopyTotal) +
        ' copied. Don''t Disconnect...';
    if SAction = SDelete then
      Main.StatusBar1.Panels[1].Text :=
        IntToStr(DeletedCnt) + ' of ' + IntToStr
        (DeleteTotal) +
        ' deleted. Don''t Disconnect...';
  end
  else
  begin
    DebugOutLn('finished', 0);
    TargetCollection.SaveToFile;
    TargetCollection.Free;
    main.connectDAP;
    Main.StatusBar1.Panels[1].Text := 'Synchronizing finished. Device Ready...';
  end;
end;

procedure TSyncThread.Execute;
begin
  finished := False;
  DeleteTotal := DeleteList.Count;
  CopyTotal := CopyList.Count;
  DeletedCnt := 0;
  CopiedCnt := 0;
  TargetCollection := TMediaCollectionClass.Create;
  TargetCollection.PathFmt := FRelative;
  TargetCollection.LoadFromFile(Target);
  while DeleteList.Count > 0 do
  begin
    OpSuccess := False;
    try

      SysUtils.DeleteFile(self.DeleteList[0]);
      if FileExists(self.DeleteList[0]) = False then
      begin
        TargetCollection.remove(TargetCollection.getIndexByPath(self.DeleteList[0]));
      end;
      if DirectoryIsEmpty(ExtractFileDir(DeleteList[0])) then
        RemoveDir(ExtractFileDir(DeleteList[0]))
    except
    end;
    Inc(DeletedCnt);
    SAction := SDelete;
    self.DeleteList.Delete(0);
    Synchronize(@SyncStatus);
  end;
  DebugOutLn('copying files...', 6);
  while CopyList.Count > 0 do
  begin
    OpSuccess := False;
    OpSuccess := FileCopy(CopyList[0], TargetList[0]);
    Inc(CopiedCnt);
    SAction := SCopy;
    TargetCollection.add(TargetList[0]);
    CopyList.Delete(0);
    TargetList.Delete(0);
    Synchronize(@SyncStatus);
  end;

  Finished := True;
  Synchronize(@SyncStatus);
end;

constructor TSyncThread.Create(Suspd: boolean);
begin
  inherited Create(suspd);
  FreeOnTerminate := True;
  CopyList := TStringList.Create;
  TargetList := TStringList.Create;
  DeleteList := TStringList.Create;
  DeletedCnt := 0;
  CopiedCnt := 0;
end;

destructor TSyncThread.Destroy;
begin
  inherited Destroy;
  CopyList.Free;
  TargetList.Free;
  DeleteList.Free;
end;

procedure TSyncThread.CopyFile(fromFile, toFile: string);
begin
  CopyList.Add(fromFile);
  TargetList.Add(toFile);

end;


procedure TSyncThread.DeleteFile(path: string);
begin
  DeleteList.Add(path);
end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{       TScanThread : Thread to scan for new media files in background }

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TScanThread.ShowStatus;
begin
  if fStatus = 1 then
    Main.StatusBar1.Panels[0].Text := 'Scanning folders in background...';
  if fStatus = 0 then
  begin
    main.Enabled := False;
    if MessageDlg('Some files on your harddisk seem to have changed.' + LineEnding +
      'Adopt changes in Cactus library?', mtWarning, mbOKCancel, 0) = mrOk then
    begin
      fstatus := 255;
      DebugOutLn('[TScanThread.ShowStatus] assigning', 0);

      //             PTargetCollection^.Assign(tmpcollection);
      DebugOutLn('saving', 0);
      //             PTargetCollection^.save_lib(PTargetCollection^.savepath);
      Main.clear_listClick(nil);

      DebugOutLn('WARNING: if excEption occurs, playlist has to be cleared here!', 0);
      //   Main.update_player_hdd_relations;
      main.update_artist_view;
      update_title_view;

      Main.StatusBar1.Panels[0].Text := ('Succesfully updated library...');
      tmpcollection.Free;
    end;
    main.Enabled := True;
  end;
  if (fstatus = 0) or (fstatus = 128) then
  begin
    Main.StatusBar1.Panels[0].Text := 'Ready';
    DebugOutLn('fstatus 0, 126', 0);
    Main.StatusBar1.Panels[1].Alignment := taRightJustify;
    tmpcollection.Free;
  end;
  DebugOutLn('showStatus', 0);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TScanThread.Execute;
begin
  fStatus := 1;
  Synchronize(@ShowStatus);

  //   fstatus:=tmpcollection.ScanForNew;
  Synchronize(@ShowStatus);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

constructor TScanThread.Create(Suspd: boolean);
begin
  inherited Create(suspd);
  FreeOnTerminate := True;
  tmpcollection := TMediaCollectionClass.Create;
  fStatus := 255;
end;

{   // End TScanThread                   }



//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{ TMain }

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


procedure TMain.loadskin(Sender: TObject);
begin
  DebugOutLn('loading skin', 2);
  with (Sender as TMenuitem) do
  begin
    SkinData.load_skin(Caption);
    CactusConfig.CurrentSkin := Caption;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.update_player_hdd_relations;

var
  i, z: integer;
begin
  for i := 0 to PlayerCol.itemcount - 1 do
  begin
    z := 0;
    PlayerCol.items[i].action := AONPLAYER;
    while z < MediaCollection.ItemCount - 1 do
    begin
      if MediaCollection.items[z].id = PlayerCol.items[i].id then
      begin
        MediaCollection.items[z].action := 1;
        z := MediaCollection.ItemCount - 1;
      end;
      Inc(z);
    end;
  end;
  Playercol.SaveToFile(CactusConfig.DAPPath + 'cactuslib');
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.VolumebarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);

var
  newVolume: byte;
begin
  if y > Volumebar.Height then
    y := Volumebar.Height;
  if y < 0 then
    y := 0;
  DebugOutLn(IntToStr(y), 0);

  newVolume := 100 - ((y * 100) div (Volumebar.Height));
  PlayerObj.set_volume(newVolume);
  Volumebar.Position := newVolume;
  DebugOutLn('volume set ' + IntToStr(PlayerObj.volume), 3);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.loadlibClick(Sender: TObject);

var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(self);
  OpenDialog.Filter := 'Mp3lib Library|*.mlb';
  OpenDialog.InitialDir := CactusConfig.HomeDir;
  OpenDialog.FilterIndex := 1;
  if Opendialog.Execute = True then
    MediaCollection.LoadFromFile(Opendialog.Filename);
  OpenDialog.Free;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.newlibClick(Sender: TObject);
begin
  Enabled := False;
  Selectdirectorydialog1.initialdir := CactusConfig.HomeDir;
  Selectdirectorydialog1.title := 'Add Directory...';
  if Selectdirectorydialog1.Execute = True then
  begin
    DebugOutLn('clear old collection', 7);
    MediaCollection.Clear;
    DebugOutLn('lll', 7);
    update_artist_view;
    update_title_view;
    Application.ProcessMessages;
    MediaCollection.add_directory(Selectdirectorydialog1.Filename);
    DebugOutLn('finished scan of ' + Selectdirectorydialog1.Filename, 2);
    if MediaCollection.ItemCount > 0 then
    begin
      ArtistTree.Selected := nil;
      update_artist_view;
      update_title_view;
    end;
  end;
  Enabled := True;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.nextClick(Sender: TObject);

var
  oldindex, err, i: integer;
begin
  playtimer.Enabled := False;
  oldindex := PlayerObj.CurrentTrack;
  if randomcheck.Checked = False then
    err := PlayerObj.next_track
  else
    err := PlayerObj.play(PlayerObj.Playlist.RandomIndex);
  if err = 0 then
  begin
    i := PlayerObj.CurrentTrack;
    if i >= 0 then
    begin
      if oldindex >= 0 then
        playlist.Items[oldindex].ImageIndex := -1;
      DebugOutLn(IntToStr(oldindex), 0);
      playlist.Items[i].ImageIndex := 0;
      playlist.Items[i].MakeVisible(False);
      playtimer.Enabled := True;
      //CactusPlugins.SendEvent(evnStartPlay, PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].artist+' - '+PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].title);
    end;
  end
  else
    stopClick(nil);
  update_player_display;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.prevClick(Sender: TObject);

var
  err: byte;
  i, OldTrack: integer;
begin
  playtimer.Enabled := False;
  OldTrack := PlayerObj.CurrentTrack;
  err := PlayerObj.prev_track;
  if (err = 0) then
  begin
    i := PlayerObj.CurrentTrack;
    if playlist.Items.Count > 1 then
    begin
      if OldTrack >= 0 then
        playlist.Items[OldTrack].ImageIndex := -1;
      playlist.Items[i].ImageIndex := 0;
      playlist.Items[i].MakeVisible(False);
      //CactusPlugins.SendEvent(evnStartPlay, PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].artist+' - '+PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].title);
    end;
    playtimer.Enabled := True;
  end
  else
    stopClick(nil);
  update_player_display;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.playClick(Sender: TObject);

var
  err: integer;
begin
  DebugOutLn('[TMain.playClick] START', 1);
  if (not PlayerObj.paused) then
  begin
    playtimer.Enabled := False;
    if (Playlist.items.Count > 0) and (Playlist.Selected = nil) then
      playitem := Playlist.Items[0]
    else
      playitem := playlist.selected;
    if (PlayerObj.playing) and (PlayerObj.Playlist.Count > 0) and
      (PlayerObj.CurrentTrack < PlayerObj.Playlist.Count) and (PlayerObj.CurrentTrack >= 0)
    then
      playlist.Items[PlayerObj.CurrentTrack].ImageIndex := -1;
    ;
    if playitem <> nil then
    begin
      err := PlayerObj.play(playitem.Index);
      if (err = 0) then
      begin
        Playlist.BeginUpdate;
        playitem.ImageIndex := 0;
        Playlist.EndUpdate;
        DebugOutLn(Format('ImageIndex=%d', [playitem.ImageIndex]), 0);
        DebugOutLn(Format('index=%d', [playitem.index]), 0);
        playitem.MakeVisible(False);
        update_player_display;
        //CactusPlugins.SendEvent(evnStartPlay, PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].artist+' - '+PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].title);
        playtimer.Enabled := True;
      end
      else
      begin
        if (err = 1) then
          ShowMessage(
            'File not Found! Goto Library/Rescan Directories for updating file links'
            );
        if (err = 2) then
          ShowMessage('Init of sound device failed.' + #10 + #13 +
            'Perhaps sound ressource is blocked by another application...'
            );
      end;
    end;
  end
  else
  begin
    //if player paused
    pauseClick(nil);
  end;
  DebugOutLn('[TMain.playClick] END', 1);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.StopClick(Sender: TObject);
begin
  playtimer.Enabled := False;
  if (PlayerObj.CurrentTrack >= 0) and
    (PlayerObj.CurrentTrack < PlayerObj.Playlist.ItemCount) then
    playlist.Items[PlayerObj.CurrentTrack].ImageIndex := -1;
  PlayerObj.stop;
  PlayerObj.playlist.reset_random;
  update_player_display;
  //CactusPlugins.SendEvent(evnStopPlay, '');
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.playtimerTimer(Sender: TObject);

var
  spos, slength: real;
  r: real;
  x2: integer;
  tmppos: integer;
  fileobj: TMediaFileClass;
begin
  DebugOutLn('[TMain.playtimerTimer] START', 3);
  try
    // if PlayerObj.playing=false then stopClick(nil);
    if PlayerObj.PlaybackMode = STREAMING_MODE then
    begin
      if PlayerObj.Get_Stream_Status = STREAM_READY then
        StatusBar1.Panels[0].Text := 'Stream Ready'
      else
        StatusBar1.Panels[0].Text := 'Buffering Stream...';
    end;
    DebugOutLn('[TMain.playtimerTimer] A', 9);
    if (PlayerObj.playing) and (PlayerObj.PlaybackMode = FILE_MODE) and
      (PlayerObj.paused = False) then
    begin
      DebugOutLn('[TMain.playtimerTimer] player playing', 9);

      if not bPnlPlaytimeNegated then
        pnlPlaytime.Caption := PlayerObj.get_timestr
      else
        pnlPlaytime.Caption := PlayerObj.Get_TimeRemainingStr;
      playwin.TimeImg.Picture.LoadFromFile(SkinData.Time.Img);
      playwin.TimeImg.Canvas.Font.Color := ClNavy;
      playwin.TimeImg.Canvas.TextOut(5, 3, pnlPlaytime.Caption);

      DebugOutLn('[TMain.playtimerTimer] B', 9);

      DebugOutLn(Format('[TMain.playtimerTimer] tmppos=%d', [tmppos]), 9);
      tmppos := PlayerObj.Get_FilePosition;
      trackbar.position := tmppos;
      x2 := (trackbar.position * 2) - 3;
      if x2 < 3 then
        x2 := 3;
      DebugOutLn('[TMain.playtimerTimer] D', 9);
      if (tmppos = 100) then
      begin
        DebugOutLn('[TMain.playtimerTimer] E', 9);
        // writeln('nexttrack');
        // WriteLn(PlayerObj.CurrentTrack);
        if (PlayerObj.CurrentTrack < PlayerObj.Playlist.ItemCount) then
          nextclick(nil)
        else
          stopClick(nil);
      end;
      if CactusConfig.CoverDownload and (CoverFound = False) and (LoopCount < 20) then
      begin
        DebugOutLn('[TMain.playtimerTimer] F', 9);
        Inc(LoopCount);
        if (assigned(LastFMAPI)) and (LastFMAPI.data_ready) then
        begin
          fileobj := TMediaFileClass(playlist.Items[PlayerObj.CurrentTrack].Data);
          if FileExists(fileobj.CoverPath) then
          begin
            try
              CoverImage.Picture.LoadFromFile(fileobj.CoverPath);
              playwin.AlbumCoverImg.Picture.LoadFromFile(fileobj.CoverPath);
            except
              DebugOutLn('[TMain.playtimerTimer] EXCEPTION', 1);
            end;
          end;
          CoverFound := True;
          FreeAndNil(LastFMAPI);
        end;
      end
      else if (LoopCount >= 20) and (CoverFound = False) then
        CoverImage.Picture.Clear;
      DebugOutLn('[TMain.playtimerTimer] G', 9);
    end
    else
    begin
      DebugOutLn('[TMain.playtimerTimer] H', 9);
      {playtimer.Enabled:=false};
    end;
  except
    DebugOutLn('[TMain.playtimerTimer] CAUGHT EXCEPTION IN PLAYTIMER!!!!', 1);
  end;
  DebugOutLn('[TMain.playtimerTimer] END', 3);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.removeselectClick(Sender: TObject);

var
  curartist, curalbum: string;
  album_mode: boolean;
  MediaFileObj: TMediaFileClass;
  MediaColObj: TMediaCollectionClass;
  z: integer;
  tsnode: TTreeNode;
begin
  tsnode := Main.ArtistTree.Selected;
  if (tsnode <> nil) and (tsnode.Level > 0) then
  begin
    if MessageDlg('The selected file(s) will permanently be' + #10 +
      #13 + 'removed from harddisk!' + #10 + #13 + ' Proceed?', mtWarning,
      mbOKCancel, 0) = mrOk then
    begin
      if tsnode.level < 2 then
        album_mode := False
      else
        album_mode := True;
      MediaFileObj := TMediaFileClass(tsnode.Data);
      MediaColObj := MediaFileObj.Collection;
      curartist := lowercase(MediaFileObj.Artist);
      curalbum := lowercase(MediaFileObj.album);

      z := MediaColObj.getTracks(curartist, MediaFileObj.index);
      repeat
        begin
          if (album_mode = False) or ((album_mode = True) and
            (lowercase(MediaColObj.items[z].album) = curalbum)) then
          begin
            if DeleteFile(MediaColObj.items[z].path) then
            begin
              DebugOutLn('deleted file from disk: ' + MediaColObj.items[z].path, 2);
              MediaColObj.remove(z);
            end
            else
              DebugOutLn('ERROR deleting file: ' + MediaColObj.items[z].path, 2);
          end;
          z := MediaColObj.getNext;
        end;
      until (z = -1);

      update_artist_view;
      update_title_view;
      MediaColObj.SaveToFile;
    end;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.save_listClick(Sender: TObject);
begin
  SaveDialog1.Title := 'Save Playlist...';
  saveDialog1.Filter := 'M3U Playlist|*.m3u';
  saveDialog1.DefaultExt := 'm3u';
  saveDialog1.FilterIndex := 1;
  SaveDialog1.InitialDir := CactusConfig.HomeDir;
  if Savedialog1.Execute = True then
  begin
    if FileExists(SaveDialog1.FileName) then
      if MessageDlg('File ' + SaveDialog1.FileName + ' alreday exists' +
        sLineBreak + sLineBreak + 'Overwrite?', mtWarning, mbOKCancel, 0) = mrCancel then
        exit;
    PlayerObj.playlist.SaveToFile(Savedialog1.Filename);
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.savelibClick(Sender: TObject);
begin
  SaveDialog1.Title := 'Save Library...';
  saveDialog1.Filter := 'Cactus Media Library|*.cml';
  saveDialog1.DefaultExt := 'cml';
  saveDialog1.FilterIndex := 1;
  SaveDialog1.InitialDir := CactusConfig.HomeDir;
  if Savedialog1.Execute = True then
    MediaCollection.SaveToFile(Savedialog1.Filename);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.scanplayeritemClick(Sender: TObject);

var
  tmps: string;
  ScanCol: TMediaCollectionClass;
begin
  if FileExists(CactusConfig.DAPPath) = False then
  begin
    ShowMessage(rsNotConnected);
    exit;
  end;

  if FileExists(CactusConfig.DAPPath) then
  begin
    checkmobile.Enabled := False;
    disconnectDAP;
    ScanCol := TMediaCollectionClass.Create;
    ScanCol.syncronize := @ScanSyncronize;
    Enabled := False;
    ScanCol.PathFmt := FRelative;
    ScanCol.savepath := CactusConfig.DAPPath + 'cactuslib';
    ScanCol.add_directory(CactusConfig.DAPPath);
    ScanCol.SaveToFile;
    ScanCol.Free;
    Enabled := True;
    connectDAP;
    checkmobile.Enabled := True;
    tmps := ByteToFmtString(FreeSpaceOnDAP, 3, 2);
    StatusBar1.Panels[1].Text := 'Device connected     ' + tmps + ' Free';
  end
  else
    DebugOutLn(CactusConfig.DAPPath + ' does not exist', 2);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.searchstrKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if length(searchstr.Text) > 1 then
    srch_buttonClick(nil)
  else
    TitleTree.Clear;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.srch_buttonClick(Sender: TObject);

var
  searchstring, ft: string;
  found: boolean;
  Listitem: TListitem;
  i: integer;
begin
  TitleTree.Items.Clear;
  TitleTree.BeginUpdate;
  ArtistTree.selected := nil;
  searchstring := lowercase(searchstr.Text);
  found := False;
  for i := 0 to MediaCollection.ItemCount - 1 do
  begin
    if SrchTitleItem.Checked then
      if pos(searchstring, lowercase(MediaCollection.items[i].title)) <> 0 then
        found := True;
    if SrchArtItem.Checked then
      if pos(searchstring, lowercase(MediaCollection.items[i].Artist)) <> 0 then
        found := True;
    if SrchAlbumItem.Checked then
      if pos(searchstring, lowercase(MediaCollection.items[i].album)) <> 0 then
        found := True;
    if SrchFileItem.Checked then
      if pos(searchstring, lowercase(extractfilename(MediaCollection.items[i].path))) <> 0 then
        found := True;
    if found then
    begin
      found := False;
      ft := '';
      case filetypebox.ItemIndex of
        0: ft := 'all';
        1: ft := '.flac';
        2: ft := '.mp3';
        3: ft := '.ogg';
        4: ft := '.wav';
      end;
      if (ft = 'all') or (ft = MediaCollection.items[i].filetype) then
      begin

        ListItem := Main.Titletree.Items.Add;

        listitem.Data := MediaCollection.items[i];
        Listitem.ImageIndex := MediaCollection.items[i].action;
        Listitem.Caption := '';

        if MediaCollection.items[i].title <> '' then
          ListItem.SubItems.Add(MediaCollection.items[i].Artist)
        else
          ListItem.SubItems.Add(SysToUTF8(extractfilename(MediaCollection.items[i].path)));
        ListItem.SubItems.Add(MediaCollection.items[i].title);
        ListItem.SubItems.Add(MediaCollection.items[i].album);
        ListItem.SubItems.Add(MediaCollection.items[i].Track);
        ListItem.SubItems.Add(MediaCollection.items[i].playtime);
      end;
    end;
  end;
  TitleTree.EndUpdate;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.EditID3itemClick(Sender: TObject);
var
  tsitem: TListitem;
begin
  tsitem := TitleTree.Selected;
  if tsitem <> nil then
  begin
    editid3win.display_window(TMediaFileClass(tsitem.Data));
    EditID3win.ShowModal;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.ArtistTreeSelectionChanged(Sender: TObject);
begin
  if ViewMode <> cvmNetwork then
    update_title_view;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.ArtistTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  artist_drag := True;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.Button1Click(Sender: TObject);
begin
  TitleTree.Clear;
end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.CoverImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  medFileObj: TMediaFileClass;
begin
  //TODO: check why large cover image is linux only?
  if PlayerObj.playing and (PlayerObj.PlaybackMode = FILE_MODE) and
    (PlayerObj.CurrentTrack >= 0) then
  begin

    MedFileObj := TMediaFileClass(playlist.Items[PlayerObj.CurrentTrack].Data);
    if (MedFileObj.CoverPath <> '') and FileExists(MedFileObj.CoverPath) then
    begin

      BigCoverImgForm := TBigCoverImg.Create(self);
      BigCoverImgForm.Caption :=
        PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].Album;

      BigCoverImgForm.Image1.Picture.LoadFromFile(MedFileObj.CoverPath);
      BigCoverImgForm.Width := BigCoverImgForm.Image1.Picture.Width + 32;
      BigCoverImgForm.Height := BigCoverImgForm.Image1.Picture.Height + 32;

      BigCoverImgForm.Image1.AutoSize := True;
      BigCoverImgForm.BackImg.Width := BigCoverImgForm.Image1.Picture.Width + 32;
      BigCoverImgForm.BackImg.Height := BigCoverImgForm.Image1.Picture.Height + 32;

      BigCoverImgForm.BackImg.Canvas.FillRect(0, 0, BigCoverImgForm.Width,
        BigCoverImgForm.Height);

      BigCoverImgForm.BackImg.Canvas.Rectangle(8, 8, BigCoverImgForm.Width - 8,
        BigCoverImgForm.Height - 8);

      BigCoverImgForm.Image1.Top := 16;
      BigCoverImgForm.Image1.Left := 16;




      BigCoverImgForm.Left := x + Panel1.Left + self.Left;
      BigCoverImgForm.Top := y + Panel1.Height + self.top - 220;


            {$ifdef win32 or win64}
      BigCoverImgForm.Position := poScreenCenter;
            {$endif}
      BigCoverImgForm.BorderStyle := bsDialog;
      BigCoverImgForm.ShowModal;

    end;
  end;

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.DeviceModeBtnClick(Sender: TObject);
begin
  DeviceModeBtn.Down := True;
  if ViewMode <> cvmDevice then
  begin
    ArtistTree.Selected := nil;
    ArtistsModeBtn.Down := False;
    NetModeBtn.Down := False;
    ViewMode := cvmDevice;
    Playlist.Enabled := True;
    TitleTree.Enabled := True;
    trackbar.Enabled := True;
    update_artist_view;
  end;
end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ArtistSrchField.hide;
  //unguenstig, wird bei jedem klick aufgerufen... :(
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.itemTrayExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.itemTrayPlayClick(Sender: TObject);
begin
  if (PlayerObj.playing) then
    pauseClick(nil)
  else
    playClick(nil);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.ArtistsModeBtnClick(Sender: TObject);
begin
  ArtistsModeBtn.Down := True;
  if ViewMode <> cvmArtist then
  begin
    ArtistTree.Selected := nil;
    DeviceModeBtn.Down := False;
    NetModeBtn.Down := False;
    LibraryModeBtn.Down := False;
    ViewMode := cvmArtist;
    Playlist.Enabled := True;
    TitleTree.Enabled := True;
    trackbar.Enabled := True;
    update_artist_view;
  end;
end;

procedure TMain.LibraryModeBtnClick(Sender: TObject);
begin
  LibraryModeBtn.Down := True;
  if ViewMode <> cvmLibrary then
  begin
    ArtistTree.Selected := nil;
    DeviceModeBtn.Down := False;
    NetModeBtn.Down := False;
    ArtistsModeBtn.Down := False;
    ViewMode := cvmLibrary;
    Playlist.Enabled := False;
    TitleTree.Enabled := True;
    trackbar.Enabled := True;
    update_library_view;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem15Click(Sender: TObject);

var
  i: integer;
begin
  i := playlist.Items.Count;
  artist_to_playlist;
  Playlist.Selected := nil;
  if playlist.Items.Count > 0 then
    playlist.Items[i].Selected := True;
  playClick(nil);

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem25Click(Sender: TObject);
begin
  addRadioForm := TaddRadioForm.Create(self);
  addRadioForm.ShowModal;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIViewAlbumClick(Sender: TObject);
begin
  MIViewAlbum.Checked := not MIViewAlbum.Checked;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIViewArtistClick(Sender: TObject);
begin
  MIViewArtist.Checked := not MIViewArtist.Checked;
  CactusConfig.TLShowArtist := MIViewArtist.Checked;
  TitleTree.Column[1].Visible := CactusConfig.TLShowArtist;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem32Click(Sender: TObject);
begin
  if (ArtistTree.Selected <> nil) and (ArtistTree.Selected.Level > 0) then
  begin
    editid3win.display_window(TStreamInfoItemClass(ArtistTree.Selected.Data));
    EditID3win.ShowModal;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem6Click(Sender: TObject);

var
  MedFileObj: TMediaFileClass;
  MedColObj: TMediaCollectionClass;
  i: integer;
begin
  if TitleTree.Selected <> nil then
    if MessageDlg('The selected file(s) will permanently be' + #10 + #13 +
      'removed from harddisk!' + #10 + #13 + ' Proceed?', mtWarning, mbOKCancel, 0) = mrOk then
    begin
      MedFileObj := TMediaFileClass(TitleTree.Selected.Data);
      MedColObj := MedFileObj.collection;
      i := MedFileObj.index;

      if DeleteFile(MedFileObj.path) then
      begin
        DebugOutLn('deleted file from disk: ' + MedFileObj.path, 2);
        MedColObj.remove(i);
      end
      else
      if FileGetAttr(MedFileObj.path) = faReadOnly then
        ShowMessage('File is read only!');

      update_artist_view;
      update_title_view;
      MedColObj.SaveToFile;
    end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem7Click(Sender: TObject);
begin
  title_to_playlist;
  Playlist.Items[Playlist.Items.Count - 1].Focused := True;
    {$ifdef win32}
  Playlist.Items[Playlist.Items.Count - 1].Selected := True;
{$endif}
  playClick(nil);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem9Click(Sender: TObject);
begin
  title_to_playlist_at(PlayerObj.CurrentTrack + 1);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIDeviceInfoClick(Sender: TObject);

var
  z: int64;
  s, tmps, used: string;
  i: integer;
begin
  if player_connected then
  begin
    z := 0;
    for i := 0 to PlayerCol.ItemCount - 1 do
      z := z + PlayerCol.items[i].size;

    used := ByteToFmtString(z, 4, 2);

    tmps := ByteToFmtString(FreeSpaceOnDAP, 4, 2);
    str(PlayerCol.ItemCount - 1, s);

    ShowMessage(s + ' Files on mobile player    ' + #10 + used + ' of music' + #10 +
      'Free Disk Space: ' + tmps);
  end
  else
    ShowMessage(rsNotConnected);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIremoveRadioClick(Sender: TObject);

var
  index: integer;
begin
  if (ArtistTree.Selected <> nil) and (ArtistTree.Selected.Level > 0) then
  begin
    index := StreamCollection.IndexOfObject(TStreamInfoItemClass(
      ArtistTree.Selected.Data));
    StreamCollection.Delete(index);
    ArtistTree.Selected := nil;
  end;
  update_artist_view;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIRipAudioClick(Sender: TObject);
begin
  cdripwin := Tcdrip.Create(Application);
  Enabled := False;
  cdripwin.ShowModal;
  cdripwin.Free;
  Enabled := True;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIViewFilenameClick(Sender: TObject);
begin
  MIViewFilename.Checked := not MIViewFilename.Checked;
  CactusConfig.TLShowFilename := MIViewFilename.Checked;
  TitleTree.Column[6].Visible := CactusConfig.TLShowFilename;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIViewGenreClick(Sender: TObject);
begin
  MIViewGenre.Checked := not MIViewGenre.Checked;
  CactusConfig.TLShowGenre := MIViewGenre.Checked;
  TitleTree.Column[5].Visible := CactusConfig.TLShowGenre;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIViewTitleClick(Sender: TObject);
begin
  MIViewTitle.Checked := not MIViewTitle.Checked;
  CactusConfig.TLShowTitle := MIViewTitle.Checked;
  TitleTree.Column[2].Visible := CactusConfig.TLShowTitle;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIViewTrackClick(Sender: TObject);
begin
  MIViewTrack.Checked := not MIViewTrack.Checked;
  CactusConfig.TLShowTrack := MIViewTrack.Checked;
  TitleTree.Column[4].Visible := CactusConfig.TLShowTrack;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.mnuCleanLibClick(Sender: TObject);
begin
  FrmCleanLibrary := TFrmCleanLibrary.Create(Application);
  Enabled := False;
  FrmCleanLibrary.ShowModal;
  FrmCleanLibrary.Free;
  update_artist_view;
  update_title_view;
  Enabled := True;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.NetModeBtnClick(Sender: TObject);
begin
  NetModeBtn.Down := True;
  if ViewMode <> cvmNetwork then
  begin
    ArtistTree.Selected := nil;
    DeviceModeBtn.down := False;
    ArtistsModeBtn.down := False;
    ViewMode := cvmNetwork;
    Playlist.Enabled := False;
    TitleTree.Enabled := False;
    trackbar.Enabled := False;
    update_artist_view;
    update_title_view;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.NextButtonImgClick(Sender: TObject);
begin
  nextClick(nil);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.NextButtonImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  NextButtonImg.Picture.LoadFromFile(SkinData.Next.Clicked);

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.NextButtonImgMouseEnter(Sender: TObject);
begin
  NextButtonImg.Picture.LoadFromFile(SkinData.Next.MouseOver);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.NextButtonImgMouseLeave(Sender: TObject);
begin
  NextButtonImg.Picture.LoadFromFile(SkinData.Next.Img);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.NextButtonImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  NextButtonImg.Picture.LoadFromFile(SkinData.Next.MouseOver);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PlaylistCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: boolean);
begin
  // not working because font colors not implemented in Lazarus 0.9.23

  if (PlayerObj.Playlist.Items[Item.Index].Played) and
    (PlayerObj.CurrentTrack <> Item.Index) then
    Sender.Canvas.Font.Color := clGrayText
  else
    Sender.Canvas.Font.Color := clWindowText;

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.pnlPlaytimeClick(Sender: TObject);
begin
  bPnlPlaytimeNegated := not bPnlPlaytimeNegated;
end;

procedure TMain.PopupMenuTrayPopup(Sender: TObject);
begin
  if (PlayerObj.playing) and (not (PlayerObj.paused)) then
  begin
    itemTrayPlay.Caption := 'Pause';
    itemTrayPlay.ImageIndex := 2;
  end
  else
  begin
    itemTrayPlay.Caption := 'Play';
    itemTrayPlay.ImageIndex := 1;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.randomcheckChange(Sender: TObject);
begin
  PlayerObj.Playlist.reset_random;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.SearchPanelClick(Sender: TObject);
begin
  ArtistSrchField.Hide;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PlayerControlsPanelClick(Sender: TObject);
begin
  ArtistSrchField.hide;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PauseButtonImgClick(Sender: TObject);
begin
  pauseClick(nil);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PauseButtonImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  pauseButtonImg.Picture.LoadFromFile(SkinData.pause.Clicked);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PauseButtonImgMouseEnter(Sender: TObject);
begin
  pauseButtonImg.Picture.LoadFromFile(SkinData.pause.MouseOver);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PauseButtonImgMouseLeave(Sender: TObject);
begin
  pauseButtonImg.Picture.LoadFromFile(SkinData.pause.Img);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PauseButtonImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  pauseButtonImg.Picture.LoadFromFile(SkinData.pause.MouseOver);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PlayButtonImgClick(Sender: TObject);
begin
  playclick(nil);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PlayButtonImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  PlayButtonImg.Picture.LoadFromFile(SkinData.play.Clicked);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PlayButtonImgMouseEnter(Sender: TObject);
begin
  PlayButtonImg.Picture.LoadFromFile(SkinData.play.MouseOver);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PlayButtonImgMouseLeave(Sender: TObject);
begin
  PlayButtonImg.Picture.LoadFromFile(SkinData.play.Img);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PlayButtonImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  PlayButtonImg.Picture.LoadFromFile(SkinData.play.MouseOver);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


procedure TMain.MainClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  PlayerObj.stop;
  //  if CactusConfig.PluginsEnabled then CactusPlugins.SendEvent(evnStopPlay, 'ps');
  CactusConfig.WHeight := Height;
  CactusConfig.WWidth := Width;
  CactusConfig.WSplitterWidth := Splitter1.Left;
  CactusConfig.bDisplayPlayTimeNegated := bPnlPlaytimeNegated;

  if CactusConfig.LoadLastPlaylist and (PlayerObj.Playlist.Count > 0) then
  begin
    if PlayerObj.Playlist.SaveToFile(CactusConfig.ConfigPrefix +
      'lib' + DirectorySeparator + 'last.m3u') <> 0 then
      DebugOutLn('ERROR saving playlist', 2);
  end
  else if PlayerObj.Playlist.Count = 0 then
    DeleteFile(CactusConfig.ConfigPrefix + 'lib' + DirectorySeparator + 'last.m3u');

  if (MediaCollection.ItemCount > 0) then
  begin
    MediaCollection.SaveToFile(CactusConfig.ConfigPrefix + 'lib' +
      DirectorySeparator + 'last.mlb');
    CactusConfig.LastLib := MediaCollection.savepath;
  end;
  if StreamCollection.Count > 0 then
  begin
    StreamCollection.SaveToFile(CactusConfig.ConfigPrefix + 'lib' +
      DirectorySeparator + 'streams.col');
    CactusConfig.StreamColPath :=
      CactusConfig.ConfigPrefix + 'lib' + DirectorySeparator + 'streams.col';
  end
  else
  begin
    if not DeleteFile(CactusConfig.ConfigPrefix + 'lib' + DirectorySeparator +
      'streams.col') then
      DebugOutLn('Cannot delete streamcolelction savefile: ' +
        CactusConfig.ConfigPrefix + 'lib' + DirectorySeparator + 'streams.col', 1);
  end;
  MediaCollection.Free;
  PlayerCol.Free;
  checkmobile.Enabled := False;
  playtimer.Enabled := False;


  PlayerObj.Free;
  CoverImage.Free;


  if playermode = False then
  begin
    playwin.Close;
    // playwin.Free;
  end;
  try
    SimpleIPCServer1.StopServer;
    SimpleIPCServer1.Free;
  except
    DebugOutLn('ERROR: Exception while shutting down IPC server', 2);
  end;
  DebugOutLn('end.', 0);
  //CactusPlugins.FlushPluginConfig;
  //CactusPlugins.Free;
  if CactusConfig.FlushConfig then
    DebugOutLn('Config succesfully written to disk', 3)
  else
    DebugOutLn('ERROR: writing config to disk', 3);
  CactusConfig.Free;
  Application.Terminate;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := PluginsList.CanClose;
  if CanClose then
  begin
    PluginsList.Free;
    CJ_Interface.Free;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MainCreate(Sender: TObject);

var
  tmps1: string;
  MPlayerExeDialog: TSelectDirectoryDialog;
  id: longint;
  listitem: TListItem;
begin
  DebugOutLn('## Main.onCreate ##', 3);
  Caption := 'Cactus Jukebox ' + CACTUS_VERSION;

  ViewMode := cvmLibrary;
  ArtistsModeBtn.Down := True;
  DeviceModeBtn.Down := False;
  NetModeBtn.Down := False;

  MediaCollection.syncronize := @ScanSyncronize;

  Width := CactusConfig.WWidth;
  Height := CactusConfig.WHeight;
  DebugOutLn('loading main form translations...', 5);
  TranslateUnitResourceStrings('mainform', CactusConfig.DataPrefix +
    'languages' + DirectorySeparator + 'cactus.%s.po',
    CactusConfig.language, copy(CactusConfig.language, 0, 2));
  if SystemCharSetIsUTF8 then
    DebugOutLn('##System charset is UTF8', 3);


  // Load resourcestrings to Captions
  QuitItem.Caption := rsQuit;
  FileItem.Caption := rsFile;
  openfile.Caption := rsOpenFile;
  opendir.Caption := rsOpenDirector;
  player_lib.Caption := rsPlayerOnly;
  skinmenu.Caption := rsChooseSkin;
  SettingsItem.Caption := rsSettings;

  MIlibrary.Caption := rsLibrary;
  MInewlib.Caption := rsNewLibrary;
  MIloadlib.Caption := rsLoadLibrary;
  MIsavelib.Caption := rsSaveLibrary;
  MIlibinfo.Caption := rsLibraryInfo;
  MIManagLib.Caption := rsManageLibrar;

  MIPlaylist.Caption := rsPlaylist;
  MIplay.Caption := rsPlay;
  MInext.Caption := rsNext;
  MIprevious.Caption := rsPrevious;
  MImute.Caption := rsMute;
  MIload_list.Caption := rsLoadPlaylist;
  MIsave_list.Caption := rsSavePlaylist;
  MIclear_playlist.Caption := rsClearPlaylist;
  MIrandom_playlist.Caption := rsRandomPlaylist;

  MIDevices.Caption := rsDevices;

  MIMobilePlayer.Caption := rsMobilePlayer;
  MIDeviceInfo.Caption := rsDeviceInfo;
  MIScanPlayer.Caption := rsScanPlayer;
  MISyncPlayer.Caption := rsSync;
  MIClearPlayer.Caption := rsClearPlayer;
  MIUndoPlayer.Caption := rsUndoSelectio;

  MIRipAudio.Caption := rsRipEncode;

  MIhelp.Caption := rsHelp;
  MIabout.Caption := rsAbout;
  MImanual.Caption := rsManual;

  Playlist.Column[0].Caption := rsPlaylist;

  clear_list.Caption := rsClear;
  srch_button.Caption := rsSearch;
  SrchAlbumItem.Caption := rsAlbum;
  SrchArtItem.Caption := rsArtist;
  SrchFileItem.Caption := rsFilename;
  SrchTitleItem.Caption := rsTitle;
  randomcheck.Caption := rsRandom;

  TitleTree.Column[1].Caption := rsArtist;
  TitleTree.Column[2].Caption := rsTitle;
  TitleTree.Column[3].Caption := rsAlbum;
  TitleTree.Column[4].Caption := rsTrack;

  TitleTree.Column[7].Caption := rsLenght;

  TitleTree.Column[1].Visible := CactusConfig.TLShowArtist;
  TitleTree.Column[2].Visible := CactusConfig.TLShowTitle;
  TitleTree.Column[3].Visible := CactusConfig.TLShowAlbum;
  TitleTree.Column[4].Visible := CactusConfig.TLShowTrack;
  TitleTree.Column[5].Visible := CactusConfig.TLShowGenre;
  TitleTree.Column[6].Visible := CactusConfig.TLShowFilename;
  TitleTree.Column[7].Visible := True;

  MIViewFilename.Checked := CactusConfig.TLShowFilename;
  MIViewGenre.Checked := CactusConfig.TLShowGenre;
  MIViewTrack.Checked := CactusConfig.TLShowTrack;
  MIViewAlbum.Checked := CactusConfig.TLShowAlbum;
  MIViewTitle.Checked := CactusConfig.TLShowTitle;
  MIViewArtist.Checked := CactusConfig.TLShowArtist;


  oldSplitterWidth := CactusConfig.WSplitterWidth;
  bPnlPlaytimeNegated := CactusConfig.bDisplayPlayTimeNegated;
  SplitterResize := True;
  SrchTitleItem.Checked := True;
  SrchArtItem.Checked := True;
{$ifdef CPU86}
  if CactusConfig.AudioBackend = MPLAYERBACK then
  begin
    PlayerObj := TMPlayerClass.Create;
    DebugOutLn('MPlayer audio backend loaded', 2);
  end
  else
  begin
      {$ifndef fmod}
    DebugOutLn(
      'WARNING: Cactus Jukebox has been compiled without fmod support. Trying to load mplayer backend instead',
      0);
    PlayerObj := TMPlayerClass.Create;
    DebugOutLn('MPlayer audio backend loaded', 2);
      {$endif}
      {$ifdef fmod}
    PlayerObj := TFModPlayerClass.Create;
    DebugOutLn('FMOD audio backend loaded', 2);
      {$endif}
  end;
{$endif}

{$ifdef CPUX86_64}// Fmod library is only available on 32bit systems. Always try to load mplayer instead
  if CactusConfig.AudioBackend = MPLAYERBACK then
  begin
    PlayerObj := TMPlayerClass.Create;
    DebugOutLn('MPlayer audio backend loaded', 2);
  end
  else
  begin
    PlayerObj := TMPlayerClass.Create;
    DebugOutLn(
      'WARNING: Fmod backend not available on 64bit systems. Trying to load mplayer backend instead',
      0);
  end;
{$endif}
  if (PlayerObj is TMPlayerClass) then
  begin
    (PlayerObj as TMPlayerClass).UseExternalConfig :=
      CactusConfig.MPlayerUseExternalConfig;
    if FileExists(IncludeTrailingPathDelimiter(CactusConfig.DataPrefix) +
      'mplayer.cfg') then
      (PlayerObj as TMPlayerClass).ExternalConfigFile :=
        IncludeTrailingPathDelimiter(CactusConfig.DataPrefix) + 'mplayer.cfg';
    if FileExists(IncludeTrailingPathDelimiter(CactusConfig.ConfigPrefix) +
      'mplayer.cfg') then
      (PlayerObj as TMPlayerClass).ExternalConfigFile :=
        IncludeTrailingPathDelimiter(CactusConfig.ConfigPrefix) + 'mplayer.cfg';
  end;

  if (PlayerObj is TMPlayerClass) and ((PlayerObj as TMPlayerClass).MPlayerPath = '') then
  begin
    if CactusConfig.MPlayerPath = '' then
    begin
      ShowMessage('MPlayer executable not found! Please select MPlayer directory...');
      MPlayerExeDialog := TSelectDirectoryDialog.Create(self);
      MPlayerExeDialog.Title := 'Locate mplayer executable...';
      if MPlayerExeDialog.Execute then
      begin
        CactusConfig.MPlayerPath := MPlayerExeDialog.FileName;
      end;
    end;
    if (PlayerObj as TMPlayerClass).setMplayerBinaryDir(
      CactusConfig.MPlayerPath) = False then
    begin
      ShowMessage('MPlayer executable not found in ' + LineEnding +
        MPlayerExeDialog.FileName);
      halt;
    end;
  end;

  PlayerObj.OutputMode := CactusConfig.AudioSystem;

  player_connected := False;
  {$ifdef linux}
  try
    DebugOut('loading program icon...  ', 2);
    Icon.LoadFromFile(CactusConfig.DataPrefix + 'icon' + DirectorySeparator +
      'cactus-icon.ico');

    //  CoverImage.Picture.LoadFromFile(DataPrefix+'tools'+DirectorySeparator+'cactus-logo-small.png');
    DebugOutLn('... loaded', 2);
  except
    DebugOutLn('ERROR loading bitmaps, files not found', 2);
  end;
  {$endif}


{$ifdef win32}
  pnlPlaytime.Canvas.Font.Height := 10;
  pnlPlaytime.Canvas.Font.Size := 10;
{$endif win32}

{$ifdef LCLGtk}
  pnlplaytime.Font.Height := 13;
  pnlplaytime.Font.Size := 13;
{$endif}

{$ifdef LCLGtk2}
  Main.Titletree.Columns[0].Width := 20;
{$endif LCLGtk2}


  SimpleIPCServer1.ServerID := 'cactusjukeboxipc';

{$ifdef unix}
  Application.OnIdle := @ApplicationIdle;
{$endif}
  SimpleIPCServer1.Global := True;

  SimpleIPCServer1.StartServer;

  checkmobile.Enabled := True;

  // unused ??
  main.tempbitmap := TBitmap.Create;
  main.timetmpbmp := TBitmap.Create;
  main.tempbitmap.Width := 300;
  main.tempbitmap.Height := 150;
  // ------

  if FileExists(CactusConfig.LastLib) then
  begin
    //main.StatusBar1.Panels[0].Text:='Loading last library...';
    if Mediacollection.LoadFromFile(CactusConfig.LastLib) = False then
    begin
      //MediaCollection.clear;
      ShowMessage('ERROR while reading last library. You need to create a new one.' +
        LineEnding + 'Please choose a directory to scan for mediafiles...');
      newlibClick(nil);
      TitleTree.Clear;
    end;
  end;
  if FileExists(CactusConfig.StreamColPath) then
  begin
    if StreamCollection.LoadFromFile(CactusConfig.StreamColPath) = False then
    begin
      DebugOutLn('Error loading stream collection', 0);
    end;
  end;

  if CactusConfig.LoadLastPlaylist then
  begin
    if FileExists(CactusConfig.ConfigPrefix + 'lib' + DirectorySeparator + 'last.m3u') then
    begin
      if PlayerObj.Playlist.LoadFromFile(
        CactusConfig.ConfigPrefix + 'lib' + DirectorySeparator + 'last.m3u') <> 0 then
        DebugOutLn('ERROR loading last playlist', 2);
      for id := 0 to PlayerObj.Playlist.Count - 1 do
      begin
        ListItem := Playlist.Items.Add;
        listitem.Data :=
          TMediaFileClass.Create(PlayerObj.Playlist.Items[id].path, nil);
        if (PlayerObj.Playlist.items[id].Artist <> '') or
          (PlayerObj.Playlist.items[id].Title <> '') then
          ListItem.Caption :=
            PlayerObj.Playlist.items[id].Artist + ' - ' + PlayerObj.Playlist.Items[id].Title
        else
          ListItem.Caption :=
            UTF8Encode(ExtractFileName(PlayerObj.Playlist.items[id].Path));
      end;
      if Playlist.Items.Count > 0 then
        Playlist.Selected := Playlist.Items[0];
    end;
  end;

  if CactusConfig.AlbumCoverFirsttime then
  begin
    tmps1 :=
      'Cactus Jukebox can download album cover art from internet and show it on playback'
      + LineEnding + '(No private data is submitted. Only album title and artist)'
      + LineEnding + LineEnding + 'Do you want to enable this feature?'
      + LineEnding + LineEnding +
      'You can change this behaviour later in File->Settings';

    if MessageDlg(tmps1, mtConfirmation, mbYesNo, 0) = mrYes then
      CactusConfig.CoverDownload :=
        True
    ;
  end;


  // Load file specified on commandline
  if CactusConfig.LoadOnStart <> '' then
  begin
    LoadFile(CactusConfig.LoadOnStart);
  end;
  DebugOutLn('main.create end', 5);
  update_artist_view;
  //update_title_view;
  DebugOutLn('main.create end', 5);

  Self.fromTrayDBLClick := False;
  oldWindowState := Self.WindowState;
  //Build of Interfaces and Plugins List (may be moved....)
  MenuOwner := Self;
  CJ_Interface := TCJ_Interface_Impl.Create;
  global_vars.AppMainMenu := Self.Mainmenu1;
  global_vars.AppTrayIcon := Self.TrayIcon;
  global_vars.ImageListNormal := Self.ImageListNormal;
  PluginsList := TCJ_PluginsList.Create;
  PluginsList.LoadFromINI;
  Self.TrayIcon.Hint := 'Cactus Juke Box...';
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.ApplicationIdle(Sender: TObject; var Done: boolean);

begin
{$ifdef linux}
  //linux doesn't recognize onIPCMessage Event. so we call it manually
  if SimpleIPCServer1.PeekMessage(1, True) then
  begin
    //PeekMessage automaticly calls OnMessage event
    DebugOutLn('IPC Messge received', 2);
  end;
{$endif}
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.update_player_display;

var
  i: integer;
begin
  if PlayerObj.playing then
  begin
    i := PlayerObj.CurrentTrack;
    //MedFileObj:=playlist.Items[player.CurrentTrack].Data;

    if PlayerObj.Playlist.items[i].Artist <> '' then
      current_title_edit.Text := PlayerObj.Playlist.items[i].Artist
    else
      current_title_edit.Text := ExtractFileName(PlayerObj.Playlist.items[i].path);
    current_title_edit1.Text := PlayerObj.Playlist.items[i].title;

    playwin.TitleImg.Picture.LoadFromFile(SkinData.Title.Img);
    playwin.TitleImg.canvas.Font.Color := Clnavy;

    if PlayerObj.Playlist.items[i].Artist <> '' then
      playwin.TitleImg.canvas.textout(5, 5, PlayerObj.Playlist.items[i].Artist)
    else
      playwin.TitleImg.canvas.textout(5, 5, ExtractFileName(PlayerObj.Playlist.items[i].path));
    playwin.TitleImg.canvas.textout(5, 25, PlayerObj.Playlist.items[i].title);
  end
  else
  begin
    //clear everything
    playwin.TitleImg.canvas.Clear;
    CoverImage.Picture.Clear;
    playwin.TimeImg.Canvas.Clear;
    current_title_edit.Text := '';
    current_title_edit1.Text := '';
    pnlPlaytime.Caption := '00:00';
    trackbar.Position := 0;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TMain.LoadFile(path: string): boolean;

var
  z: integer;
  listitem: TListItem;
begin
  DebugOutLn('** Loadfile **', 2);
  Application.ProcessMessages;
  if FileExistsUTF8(Path) then
  begin
    z := MediaCollection.GetIndexByPath(path);
    DebugOutLn(z, 3);
    if z < 0 then
    begin
      z := MediaCollection.add(path);
    end;
    PlayerObj.playlist.add(MediaCollection.items[z]);
    ListItem := Playlist.Items.Add;
    listitem.Data := MediaCollection.items[z];

    if MediaCollection.items[z].title <> '' then
      ListItem.Caption := MediaCollection.items[z].Artist
        +
        ' - ' + MediaCollection.items[z]
        .title
    else
      ListItem.Caption := extractfilename(MediaCollection.items[z].path);
    playlist.Column[0].Caption :=
      'Playlist                       (' + IntToStr(
      PlayerObj.playlist.ItemCount) +
      ' Files/ ' + PlayerObj.Playlist.TotalPlayTimeStr + ')';
    Result := True;
    update_artist_view;
    update_title_view;
  end
  else
    Result := False;
  Application.ProcessMessages;
end;

procedure TMain.MinimizeMe(Data: Ptrint);
begin
  Include(FFlags, cfProgHide);
  Hide;
end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.ScanSyncronize(dir: string);
begin
  Inc(ScanSyncCount);

  if ScanSyncCount >= 500 then
  begin
    update_artist_view;
    ScanSyncCount := 0;
  end;

  if (ScanSyncCount mod 50) = 0 then
  begin
    StatusBar1.Panels[0].Text := 'scanning trough:  ' + dir;
    Application.ProcessMessages;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.update_artist_view;
// TODO: rewrite method to reselect artist
var
  curartist, curalbum: string;
  tsnode, artnode, TopNode: Ttreenode;
  AlbumList: TStringList;
  MedFileObj: TMediaFileClass;
  i, z: integer;
  restoreEnabled, album_selected: boolean;
begin

  if Enabled then
    restoreEnabled := True
  else
    restoreEnabled := False;
  Enabled := False;
  StatusBar1.Panels[0].Text := 'Please wait... updating...';
  ArtistTree.OnSelectionChanged := nil;
  //Disable event while working on selection in ArtistTree!!
  DebugOutLn('', 2);
  DebugOut('## update artist view... ', 2);
  tsnode := ArtistTree.Selected;
  if (tsnode <> nil) then
  begin
    if tsnode.Level = 1 then
    begin
      curartist := lowercase(tsnode.Text);
      curalbum := '';
      album_selected := False;
    end;
    if tsnode.Level = 2 then
    begin
      curartist := lowercase(tsnode.Parent.Text);
      curalbum := lowercase(tsnode.Text);
      // writeln(curalbum);
      album_selected := True;
    end;
  end
  else
    curartist := '';
  ArtistTree.beginupdate;
  DebugOut(' clear tree...', 2);
  ArtistTree.Items.Clear;

  if ViewMode = cvmNetwork then
  begin

    TopNode := ArtistTree.Items.Add(nil, 'Webradio stations');
    TopNode.Data := pointer(1);
    for i := 0 to StreamCollection.Count - 1 do
    begin
      artnode := ArtistTree.Items.AddChild(TopNode, StreamCollection.Strings[i]);
      with artnode do
      begin
        MakeVisible;
        ImageIndex := -1;
        SelectedIndex := -1;
        Data := StreamCollection.Objects[i];
      end;
    end;
  end;
  // If library mode add Mediacollection
  if (ViewMode = cvmLibrary) and (MediaCollection.Count > 0) then
  begin
    TopNode := Main.ArtistTree.Items.Add(nil, rsLibrary);
    TopNode.ImageIndex := 4;
    TopNode.SelectedIndex := 4;
    TopNode.Data := pointer(1);

    i := MediaCollection.getArtists;
    repeat
      begin
        if MediaCollection.Items[i].Artist <> '' then
          artnode := Main.ArtistTree.Items.AddChild(
            TopNode,
            MediaCollection.Items[i].
            Artist)
        else
          artnode := Main.ArtistTree.Items.AddChild(TopNode, 'Unknown');
        with artnode do
        begin
          MakeVisible;
          ImageIndex := 6; //MediaCollection.Items[i].Action;
          SelectedIndex := 6; //MediaCollection.Items[i].Action;
          Data := MediaCollection.items[i];
          Expanded := False;
        end;
        AlbumList := MediaCollection.getAlbums(MediaCollection.Items[i].Artist, i);
        for z := 0 to AlbumList.Count - 1 do
        begin
          // add albums to node of current SrchArtItem
          with Main.ArtistTree.Items.Addchild(artnode, AlbumList[z]) do
          begin
            MakeVisible;
            MedFileObj := TMediaFileClass(AlbumList.Objects[z]);
            ImageIndex := 7;//MedFileObj.Action;
            SelectedIndex := 7;//MedFileObj.Action;
            Data := AlbumList.Objects[z];
          end;
        end;
        artnode.Expanded := False;
        i := MediaCollection.getNextArtist;
      end;
    until i < 0;
    // finally free AlbumList
    AlbumList.Free;
  end;

  // If Device mode add playercollection and other devices
  if (ViewMode = cvmDevice) and player_connected then
  begin

    TopNode := Main.ArtistTree.Items.Add(nil, rsMobileDevice);
    TopNode.SelectedIndex := 1;
    TopNode.ImageIndex := 1;
    TopNode.Data := pointer(0);

    i := PlayerCol.getArtists;
    repeat
      begin
        if PlayerCol.Items[i].Artist <> '' then
          artnode := Main.ArtistTree.Items.AddChild(TopNode,
            PlayerCol.Items[i].Artist)
        else
          artnode := Main.ArtistTree.Items.AddChild(TopNode, 'Unknown');
        with artnode do
        begin
          MakeVisible;
          ImageIndex := PlayerCol.Items[i].Action;
          SelectedIndex := PlayerCol.Items[i].Action;
          Data := PlayerCol.items[i];
        end;
        AlbumList := PlayerCol.getAlbums(PlayerCol.Items[i].Artist, i);
        for z := 0 to AlbumList.Count - 1 do
        begin
          // add albums to node of current SrchArtItem
          with Main.ArtistTree.Items.Addchild(artnode, AlbumList[z]) do
          begin
            MakeVisible;
            MedFileObj := TMediaFileClass(AlbumList.Objects[z]);
            ImageIndex := MedFileObj.Action;
            SelectedIndex := MedFileObj.Action;
            Data := AlbumList.Objects[z];
          end;
        end;
        artnode.Expanded := False;
        i := PlayerCol.getNextArtist;
      end;
    until i < 0;
    // finally free AlbumList
    AlbumList.Free;
  end;
  ArtistTree.EndUpdate;
  DebugOut(' reselecting last item ', 2);
  // Reselect last selected item if possible
  i := 0;
  if ArtistTree.Items.Count > 0 then
    ArtistTree.Selected := ArtistTree.Items[0];
  if ((curalbum <> '') or (curartist <> '')) and (ArtistTree.Items.Count > 0) then
  begin
    repeat  //try to keep old album
      begin
        MedFileObj := TMediaFileClass(ArtistTree.items[i].Data);
        Inc(i);
      end;
    until ((lowercase(ArtistTree.items[i].Text) = curalbum) and
        (ArtistTree.Items[i].Level = 2)) or (i >= ArtistTree.items.Count - 1);

    if lowercase(ArtistTree.items[i].Text) = curalbum then
    begin
      ArtistTree.selected := main.artisttree.items[i];
    end

    else if (curartist <> '') and (ArtistTree.Items.Count > 0) then
    begin //Select artist if album not possible
      i := 0;
      repeat
        begin
          MedFileObj := TMediaFileClass(ArtistTree.items[i].Data);
          Inc(i);
        end;
      until ((lowercase(ArtistTree.items[i].Text) = curartist) and
          (ArtistTree.Items[i].Level = 1)) or (i >= ArtistTree.items.Count - 1);
      DebugOutLn(curartist, 0);
      DebugOutLn(ArtistTree.items[i].Text, 0);
      if lowercase(ArtistTree.items[i].Text) = curartist then
      begin
        ArtistTree.selected := main.artisttree.items[i];
      end;
    end;

    if ArtistTree.Selected.AbsoluteIndex < ArtistTree.Items.Count - 10 then
    begin
      ArtistTree.Items[ArtistTree.Selected.AbsoluteIndex + 9].MakeVisible;
      if ArtistTree.Items[ArtistTree.Selected.AbsoluteIndex + 9].Level > 1 then
        ArtistTree.Items[ArtistTree.Selected.AbsoluteIndex +
          9].Parent.Expanded := False;
    end
    else
    begin
      ArtistTree.Items[ArtistTree.Items.Count - 1].MakeVisible;
      if ArtistTree.Items[ArtistTree.Items.Count - 1].Level > 1 then
        ArtistTree.Items[ArtistTree.Items.Count - 1].Parent.Expanded := False;
    end;
  end;
  ArtistTree.OnSelectionChanged := @ArtistTreeSelectionChanged;  //Reenable event!!

  DebugOutLn(' finished artistview ##', 2);
  StatusBar1.Panels[0].Text := 'Ready.';
  Enabled := restoreEnabled;

end;

procedure TMain.update_library_view;
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem11Click(Sender: TObject);
begin
  dirwin := Tdirwin.Create(Application);
  Enabled := False;
  dirwin.ShowModal;
  dirwin.Free;
  Enabled := True;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem14Click(Sender: TObject);

var
  tsitem: TListItem;
  tmps: string;
  MedFileObj: TMediaFileClass;
  i: integer;
begin
  tsitem := TitleTree.Selected;
  if (tsitem <> nil) and player_connected then
  begin
    MedFileObj := TMediaFileClass(tsitem.Data);
    for i := 1 to MediaCollection.ItemCount - 1 do
      if MedFileObj.id = MediaCollection.items[i].id then
        MediaCollection.items[i].action := AREMOVE;

    for i := 1 to PlayerCol.ItemCount - 1 do
      if MedFileObj.id = PlayerCol.items[i].id then
        PlayerCol.items[i].action := AREMOVE;
    update_artist_view;
    update_title_view;

    tmps := ByteToFmtString(FreeSpaceOnDAP + sizediff, 3, 2);

    StatusBar1.Panels[1].Text := 'Device connected     ' + tmps + ' Free';
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem16Click(Sender: TObject);

var
  tsitem: TListItem;
  tmps: string;
  MedFileObj: TMediaFileClass;
begin
  tsitem := TitleTree.Selected;
  if (tsitem <> nil) and player_connected then
  begin
    MedFileObj := TMediaFileClass(tsitem.Data);
    MedFileObj.action := AUPLOAD;

    update_artist_view;
    update_title_view;

    tmps := ByteToFmtString(FreeSpaceOnDAP + sizediff, 3, 2);
    StatusBar1.Panels[1].Text := 'Device connected     ' + tmps + ' Free';
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem20Click(Sender: TObject);

var
  tsitem: TListItem;
  MedFileObj: TMediaFileClass;
  tmps: string;
  i: integer;
begin
  tsitem := TitleTree.Selected;
  if tsitem <> nil then
  begin
    MedFileObj := TMediaFileClass(tsitem.Data);
    if MedFileObj.action = AREMOVE then
    begin
      //PFobj^.action:=1;
      sizediff := sizediff - MedFileObj.size;
      for i := 1 to MediaCollection.ItemCount - 1 do
        if MedFileObj.id = MediaCollection.items[i].id then
          MediaCollection.items[i].action := 1;

      for i := 1 to PlayerCol.ItemCount - 1 do
        if MedFileObj.id = PlayerCol.items[i].id then
          PlayerCol.items[i].action := -1;
    end
    else
    begin
      MedFileObj.action := -1;
      sizediff := sizediff + MedFileObj.size;
    end;

    update_artist_view;
    update_title_view;

    tmps := ByteToFmtString(FreeSpaceOnDAP + sizediff, 3, 2);
    StatusBar1.Panels[1].Text := 'Device connected     ' + tmps + ' Free';
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//Random Paylist with 50 tracks by monta

procedure TMain.MIrandom_playlistClick(Sender: TObject);
var
  listitem: TListitem;
  MedFileObj: TMediaFileClass;
  i: integer;
begin
  if MediaCollection.Count < 2 then
    exit;
  Randomize;
  i := 1;
  repeat
    MedFileObj := MediaCollection.Items[Random(MediaCollection.Count)];
    PlayerObj.playlist.add(MedFileObj);
    ListItem := Main.Playlist.Items.Add;
    listitem.Data := MedFileObj;
    listitem.MakeVisible(False);
    if MedFileObj.title <> '' then
      ListItem.Caption := MedFileObj.Artist + ' - ' + MedFileObj.title
    else
      ListItem.Caption := extractfilename(MedFileObj.path);
    if not PlayerObj.playing and CactusConfig.AutostartPlay and
      (main.Playlist.Items.Count = 1) then
    begin
      main.Playlist.Selected := Main.Playlist.Items[0];
      DebugOutLn(Main.Playlist.Selected.Caption, 3);
      Main.playClick(main);
    end;
    Inc(i);
  until (i > 50) or (i >= MediaCollection.Count);
  main.playlist.Column[0].Caption :=
    rsplaylist + '            (' + IntToStr(
    PlayerObj.playlist.ItemCount) +
    ' Files/ ' + PlayerObj.Playlist.TotalPlayTimeStr + ' )';
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem26Click(Sender: TObject);
begin

  ShowMessage('Cactus Jukebox' + LineEnding + 'version' + CACTUS_VERSION + LineEnding +
    'written by Sebastian Kraft ' + LineEnding + LineEnding +
    '(c) 2005-2009' + LineEnding + 'http://cactus.hey-you-freaks.de     ');

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem27Click(Sender: TObject);

var
  id: longint;
  listitem: TListitem;
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(self);
  OpenDialog.Filter := 'M3U Playlist|*.m3u|All Files|*.*';
  OpenDialog.InitialDir := CactusConfig.HomeDir;
  OpenDialog.FilterIndex := 1;
  if Opendialog.Execute = True then
  begin
    playlist.Clear;
    PlayerObj.Playlist.Clear;
    DebugOut('Loading playlist from -> ' + Opendialog.Filename + ' ... ', 2);
    if PlayerObj.Playlist.LoadFromFile(Opendialog.Filename) = 0 then
    begin
      DebugOutLn('done', 2);
      DebugOut('Adding items... ', 4);
      for id := 0 to PlayerObj.Playlist.Count - 1 do
      begin
        ListItem := Playlist.Items.Add;
        listitem.ImageIndex := -1;
        listitem.Data :=
          TMediaFileClass.Create(PlayerObj.Playlist.Items[id].path, nil);
        if (PlayerObj.Playlist.items[id].Artist <> '') or
          (PlayerObj.Playlist.items[id].Title <> '') then
          ListItem.Caption :=
            PlayerObj.Playlist.items[id].Artist + ' - ' + PlayerObj.Playlist.Items[id].Title
        else
          ListItem.Caption :=
            UTF8Encode(ExtractFileName(PlayerObj.Playlist.items[id].Path));
      end;
      DebugOutLn('done', 4);
      if Playlist.Items.Count > 0 then
        Playlist.Selected := Playlist.Items[0];
    end
    else
      ShowMessage('ERROR loading Playlist');
  end;
  OpenDialog.Free;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem2Click(Sender: TObject);

var
  i: integer;
  MedFileObj: TMediaFileClass;
  Listitem: TListItem;

begin

  if (TitleTree.Items.Count > 0) then
  begin
    for i := 0 to TitleTree.Items.Count - 1 do
    begin
      MedFileObj := TMediaFileClass(TitleTree.Items[i].Data);
      PlayerObj.playlist.add(MedFileObj);

      ListItem := Playlist.Items.Add;
      listitem.Data := MedFileObj;

      if MedFileObj.title <> '' then
        ListItem.Caption := MedFileObj.Artist + ' - ' + MedFileObj.title
      else
        ListItem.Caption := extractfilename(MedFileObj.path);
    end;
  end;
  playlist.Column[0].Caption :=
    'Playlist                       (' + IntToStr(
    PlayerObj.playlist.ItemCount) + ' Files/ ' +
    PlayerObj.Playlist.TotalPlayTimeStr + ')';
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem37Click(Sender: TObject);

var
  MedColObj: TMediaCollectionClass;
  curartist, curalbum, tmps: string;
  MedFileObj: TMediaFileClass;
  i: integer;
  tsnode: TTreeNode;
begin
  tsnode := ArtistTree.Selected;

  if (tsnode <> nil) and (tsnode.level > 0) then
  begin
    MedFileObj := TMediaFileClass(tsnode.Data);
    MedColObj := MedFileObj.collection;
    i := MedColObj.getTracks(MedFileObj.Artist, MedFileObj.index);
    if tsnode.level = 2 then
    begin
      curartist := lowercase(MedFileObj.Artist);
      curalbum := lowercase(MedFileObj.album);
      repeat
        begin
          if (lowercase(MedColObj.items[i].album) = curalbum) and
            (MedColObj.items[i].action = AREMOVE) then
          begin
            MedColObj.items[i].action := AONPLAYER;
            sizediff := sizediff - MedColObj.items[i].size;
          end;
          if (lowercase(MedColObj.items[i].album) = curalbum) and
            (MedColObj.items[i].action <> AONPLAYER) then
          begin
            MedColObj.items[i].action := ANOTHING;
            sizediff := sizediff + MedColObj.items[i].size;
          end;
          i := MedColObj.GetNext;
        end;
      until i < 0;

    end;
    if tsnode.level = 1 then
    begin
      curartist := lowercase(MedFileObj.Artist);
      repeat
        begin
          if (MedColObj.items[i].action = AREMOVE) then
          begin
            MedColObj.items[i].action := AONPLAYER;
            sizediff := sizediff - MedColObj.items[i].size;
          end;
          if (MedColObj.items[i].action <> AONPLAYER) then
          begin
            MedColObj.items[i].action := ANOTHING;
            sizediff := sizediff + MedColObj.items[i].size;
          end;
          i := MedColObj.GetNext;
        end;
      until i < 0;
    end;
    update_artist_view;
    update_title_view;

    tmps := ByteToFmtString(FreeSpaceOnDAP + sizediff, 3, 2);
    StatusBar1.Panels[1].Text := 'Device connected     ' + tmps + ' Free';
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//Open manual/homepage in browser
procedure TMain.MenuItem43Click(Sender: TObject);
begin
  {$ifdef linux}
  exec('/usr/bin/firefox', 'http://cactus.hey-you-freaks.de');
  if Dosexitcode <> 0 then
    exec('/usr/bin/mozilla-firefox',
      'http://cactus.hey-you-freaks.de/index.php?page=manual');
  if Dosexitcode <> 0 then
    exec('/usr/bin/konqueror',
      'http://cactus.hey-you-freaks.de/index.php?page=manual');
  if Dosexitcode <> 0 then
    ShowMessage('The manual can be found at http://cactus.hey-you-freaks.de');
  {$endif}
  {$ifdef win32}//TODO: Open manual in Browser on win32
  ShowMessage('The manual can be found at http://cactus.hey-you-freaks.de');
  {$endif}
end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.Panel1Click(Sender: TObject);
begin
  ArtistSrchField.hide;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.Panel1Resize(Sender: TObject);
var
  i: integer;
begin
  // Splitter1.Left:=oldSplitterWidth;
{$ifdef win32}//TODO: check column autosize on win32

  Playlist.Columns[0].Width := Playlist.Width;
  Titletree.Columns[5].Width := 45;
  Titletree.Columns[4].Width := 45;
  Titletree.Columns[3].Width := 110;
  i := TitleTree.Width - 45 - 45 - 110 - 140 - 16 - 15;
  if i > 0 then
    Titletree.Columns[2].Width := i;
  Titletree.Columns[1].Width := 140;
  Titletree.Columns[0].Width := 16;
{$endif}
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.Panel4Click(Sender: TObject);
begin
  ArtistSrchField.hide;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.ArtistSrchFieldClick(Sender: TObject);
begin
  ArtistSrchField.hide;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PreviousButtonImgClick(Sender: TObject);
begin
  prevClick(nil);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PreviousButtonImgMouseDown(Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  PreviousButtonImg.Picture.LoadFromFile(SkinData.previous.Clicked);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PreviousButtonImgMouseEnter(Sender: TObject);
begin
  PreviousButtonImg.Picture.LoadFromFile(SkinData.previous.MouseOver);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PreviousButtonImgMouseLeave(Sender: TObject);
begin
  PreviousButtonImg.Picture.LoadFromFile(SkinData.previous.Img);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.PreviousButtonImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  PreviousButtonImg.Picture.LoadFromFile(SkinData.previous.MouseOver);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.SettingsItemClick(Sender: TObject);
begin
  Enabled := False;
  setupwin := Tsettings.Create(Application);
  setupwin.ShowModal;
  setupwin.Free;
  Enabled := True;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.SimpleIPCServer1Message(Sender: TObject);

var
  fpath: string;
  CommandCode: integer;
begin
  DebugOutLn(' IPC Message received', 2);
  if length(SimpleIPCServer1.StringMessage) > 2 then
  begin
    fpath := copy(SimpleIPCServer1.StringMessage,
      Pos(':', SimpleIPCServer1.StringMessage) + 2, length(
      SimpleIPCServer1.StringMessage));
    CommandCode := StrToInt(Copy(SimpleIPCServer1.StringMessage, 0,
      Pos(':', SimpleIPCServer1.StringMessage) - 1));
  end
  else
  begin
    fpath := '';
    CommandCode := StrToInt(SimpleIPCServer1.StringMessage);
  end;
  case CommandCode of
    VOLUME_UP: if volumebar.Position > 4 then
        volumebar.Position := volumebar.Position - 5;
    VOLUME_DOWN: if volumebar.Position < 46 then
        volumebar.Position := volumebar.Position + 5;
    NEXT_TRACK: nextClick(self);
    STOP_PLAYING: stopClick(self);
    START_PLAYING: playClick(self);
    PREV_TRACK: prevClick(self);
    PAUSE_PLAYING: pauseClick(self);
    OPEN_FILE: if FileExists(fpath) then
      begin
        LoadFile(fpath);
        Playlist.Selected := Playlist.Items[Playlist.Items.Count - 1];
        playClick(self);
      end
      else
        DebugOutLn('--> Filename received from IPC does not exist', 2);
    ENQUEU_FILE: if FileExists(fpath) then
      begin
        LoadFile(fpath);
      end
      else
        DebugOutLn('--> Filename received from IPC does not exist', 2);
    else
      DebugOutLn(' --> Invalid message/filename received via IPC', 2);
  end;
  DebugOutLn('IPC end', 0);
end;

procedure TMain.skinmenuClick(Sender: TObject);
begin
  // JRA
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.SpeedButton1Click(Sender: TObject);
begin
  ArtistSrchField.hide;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.Splitter1Moved(Sender: TObject);
begin
  oldSplitterWidth := Panel4.Width;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.SrchAlbumItemClick(Sender: TObject);
begin
  SrchAlbumItem.Checked := not SrchAlbumItem.Checked;
end;

procedure TMain.SrchArtItemClick(Sender: TObject);
begin
  SrchArtItem.Checked := not SrchArtItem.Checked;
end;

procedure TMain.SrchFileItemClick(Sender: TObject);
begin
  SrchFileItem.Checked := not SrchFileItem.Checked;
end;

procedure TMain.SrchTitleItemClick(Sender: TObject);
begin
  SrchTitleItem.Checked := not SrchTitleItem.Checked;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.StopButtonImgClick(Sender: TObject);
begin
  stopClick(nil);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.StopButtonImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  StopButtonImg.Picture.LoadFromFile(SkinData.stop.Clicked);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.StopButtonImgMouseEnter(Sender: TObject);
begin
  StopButtonImg.Picture.LoadFromFile(SkinData.stop.MouseOver);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.StopButtonImgMouseLeave(Sender: TObject);
begin
  StopButtonImg.Picture.LoadFromFile(SkinData.stop.Img);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.StopButtonImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  StopButtonImg.Picture.LoadFromFile(SkinData.stop.MouseOver);
end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.TitleTreeClick(Sender: TObject);
begin
  ArtistSrchField.Hide;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function NumericCompare(List: TStringList; Index1, Index2: integer): integer;

var
  i1, i2: integer;
begin
  try
    i1 := StrToInt(List[Index2]);
    i2 := StrToInt(List[Index1]);
    Result := i2 - i1;
  except
    Result := 0;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.TitleTreeColumnClick(Sender: TObject; Column: TListColumn);

var
  sl: TStringList;
  //used for sorting
  counter, ListitemCount, SubItemsColumnCount, IndexOfCurrentColumn, i: integer;
begin
  sl := TStringList.Create;
  try
    IndexOfCurrentColumn := column.index;
    DebugOutLn(Format('IndexOfCurrentColumn=%d', [IndexOfCurrentColumn]), 0);

    if IndexOfCurrentColumn <> 0 then
    begin
      ListitemCount := TitleTree.Items.Count;
      for counter := 0 to ListitemCount - 1 do
      begin
        SubItemsColumnCount := titletree.items[counter].subitems.Count;
        sl.AddObject(titletree.items[counter].SubItems[IndexOfCurrentColumn - 1],
          titletree.items[counter]);
      end;
      //   for i:= 0 to sl.Count-1 do writeln(sl[i]);
      if IndexOfCurrentColumn <> 4 then
        sl.sort
      else
        sl.CustomSort(@NumericCompare);
      //    TitleTree.BeginUpdate;
      //        TitleTree.Clear;
      //        TitleTree.Items.
      for counter := 0 to ListitemCount - 1 do
      begin
        titletree.items[counter] := (TListItem(sl.Objects[counter]));
        //          writeln(sl[counter]);
      end;
      //  TitleTree.EndUpdate;
    end;
  finally
    sl.Free;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.TitleTreeDragOver(Sender, Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
begin
{$ifdef LCLGtk}
  //Workaround for GTK1.x to reset selected Item while dragging
  if title_drag then
  begin
    TitleTree.Selected := nil;
    TitleTree.Items[sourceitem.Index].Selected := True;
  end;
{$endif}
  Accept := False;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.TitleTreeEndDrag(Sender, Target: TObject; X, Y: integer);
begin
  title_drag := False;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.TitleTreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // ensure that the popup menu is only opened when an item is selected
  // the menu is reanabled in TMain.TitleTreeSelectItem

  {$ifdef  LCLQT}//TODO: QT interface doesn't set selected item
  TitleTree.Selected := TitleTree.GetItemAt(x, y);
  {$endif}

  {$ifdef  LCLGtk2}//TODO: GTK2 interface doe snot selcte item on right click
  if (Button = mbRight) then
    TitleTree.Selected := TitleTree.GetItemAt(x, y);
  {$endif}

  //TODO check titlelist popupmenu on win32
  {$ifdef win32}
  if (Button = mbRight) and (TitleTree.Selected <> nil) then
    TitleTree.PopupMenu.PopUp(self.Left + Panel1.Left + TitleTree.left + X,
      self.top + Panel1.Top + TitleTree.top + Y + 50);
  {$else}
  if (Button = mbRight) and (TitleTree.Selected = nil) then
    TitleTree.PopupMenu.AutoPopup := False;
  {$endif}
  //Enable Dragging
  if Button = mbLeft then
  begin { only drag if left button pressed }
    sourceitem := nil;
    sourceitem := TitleTree.GetItemAt(x, y);
    if sourceitem <> nil then
      TitleTree.BeginDrag(False, 10);
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.TitleTreeSelectItem(Sender: TObject; Item: TListItem;
  Selected: boolean);
begin
  // reanable the popupmenu in case ist was disabled in TMain.TitleTreeMouseDown
  TitleTree.PopupMenu.AutoPopup := True;
  lblPath.Caption := UTF8Encode(TMediaFileClass(Item.Data).Path);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.TitleTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  title_drag := True;
  //sourceitem:=TitleTree.Selected;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.TrackInfoClick(Sender: TObject);
begin
  if (PlayerObj.CurrentTrack) >= 0 then
  begin
    playlist.selected := playlist.Items[PlayerObj.CurrentTrack];
    MenuItem10Click(nil);
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.artisttreemenuPopup(Sender: TObject);

var
  MedFileObj: TMediaFileClass;
begin
  if ArtistTree.Selected.Level > 0 then
  begin
    MedFileObj := TMediaFileClass(ArtistTree.Selected.Data);
    if MedFileObj.collection = PlayerCol then
      Menuitem30.Enabled := False
    else
      Menuitem30.Enabled := True;
    if ArtistTree.Selected.Level = 1 then
    begin
      if ArtistTree.Selected.ImageIndex = 1 then
      begin
        MenuItem37.Enabled := False;
        rm_artist_playeritem.Enabled := True;
        MenuItem30.Enabled := True;
      end;
      if ArtistTree.Selected.ImageIndex = -1 then
      begin
        MenuItem37.Enabled := False;
        rm_artist_playeritem.Enabled := False;
        MenuItem30.Enabled := True;
      end;
      if ArtistTree.Selected.ImageIndex = 2 then
      begin
        MenuItem37.Enabled := True;
        rm_artist_playeritem.Enabled := False;
        MenuItem30.Enabled := False;
      end;
      if ArtistTree.Selected.ImageIndex = 3 then
      begin
        MenuItem37.Enabled := True;
        rm_artist_playeritem.Enabled := False;
        MenuItem30.Enabled := False;
      end;
    end
    else
    begin
      MenuItem37.Enabled := True;
      rm_artist_playeritem.Enabled := True;
      MenuItem30.Enabled := True;
    end;
    if player_connected = False then
    begin
      MenuItem30.Visible := False;
      rm_artist_playeritem.Visible := False;
      menuitem37.Visible := False;
      space1.Visible := False;
    end;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.checkmobileTimer(Sender: TObject);
var
  PlayerScanThread: TScanThread;
  tmps: string;
begin
  DebugOutLn('[TMain.checkmobileTimer] START', 1);
  if (player_connected = False) and FileExists(CactusConfig.DAPPath + 'cactuslib') then
  begin
    DebugOut('DAP detected...', 2);
    if connectDAP = 0 then
    begin
      tmps := GetCurrentDir;
      // get free memory on player, format string
      DebugOut('loaded', 2);
      tmps := ByteToFmtString(FreeSpaceOnDAP, 3, 2);
      DebugOutLn(FreeSpaceOnDAP, 0);
      StatusBar1.Panels[1].Text := 'Device connected     ' + tmps + ' free';
      if CactusConfig.background_scan then
      begin
        PlayerScanThread := TScanThread.Create(True);
        PlayerScanThread.tmpcollection.Assign(PlayerCol);
        PlayerScanThread.PTargetCollection := PlayerCol;
        PlayerScanThread.Resume;
      end;
    end
    else
    begin
      checkmobile.Enabled := False;
      ShowMessage('Error while opening player device. ' + #10 + #13 +
        'Try to scan player again...');
      player_connected := True;
    end;
  end;

  Application.ProcessMessages;
  if (player_connected = True) and
    (FileExists(CactusConfig.DAPPath + 'cactuslib') = False) then
  begin
    disconnectDAP;
    StatusBar1.Panels[1].Text := 'Device disconnected';
  end;
  DebugOutLn('[TMain.checkmobileTimer] END', 1);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.clearPlayerItemClick(Sender: TObject);
begin

(*if player_connected then begin
    res:=MessageDlg('All music files on the player will definitely be removed!!'+#10+#13+' Continue?', mtWarning, mbOKCancel, 0);
    if res=mrOK then begin
      err:=true
      i:= PlayerCol.max_index-1;
      repeat begin
             err:=DeleteFile(PlayerCol.items[i].path);
             if err=true then dec(PlayerCol.max_index); {array length is not shorten here !!}
  dec(i);
End;
Until i=0;

If err=false Then ShowMessage('Error while deleting one or more files.'+#10+#13+
                              ' Perhaps no write permission or file doesn''t exist')
End;

End
Else ShowMessage(rsNotConnected);*)
  ShowMessage('Not implemented yet!');
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.Menuitem10Click(Sender: TObject);

var
  MedFileObj: TMediaFileClass;
begin

  if playlist.Selected <> nil then
  begin
    MedFileObj := TMediaFileClass(Playlist.Selected.Data);
    editid3win.display_window(MedFileObj);
    Enabled := False;
    EditID3win.ShowModal;
    Enabled := True;
  end;

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.clear_listClick(Sender: TObject);
begin
  if CactusConfig.StopOnClear then
    StopButtonImgClick(Sender);
  Playlist.BeginUpdate;
  DebugOutLn('clear', 0);
  Playlist.Items.Clear;
  playlist.Column[0].Caption := rsPlaylist;
  PlayerObj.playlist.Clear;
  Playlist.EndUpdate;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.filetypeboxChange(Sender: TObject);
begin
  srch_buttonClick(nil);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.libinfoClick(Sender: TObject);

var
  z: int64;
  s, used: string;
  i: integer;
begin
  z := 0;
  for i := 1 to MediaCollection.ItemCount - 1 do
    z := z + MediaCollection.items[i].size;

  used := ByteToFmtString(z, 3, 2);
  s := IntToStr(MediaCollection.ItemCount);
  ShowMessage(s + ' Files in library ' + #10 + ' ' + used + ' of music files');
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.muteClick(Sender: TObject);
var
  png: TPortableNetworkGraphic;
begin
  if PlayerObj.playing then
  begin

    PlayerObj.mute;

    png := TPortableNetworkGraphic.Create;
    if PlayerObj.muted then
      png.LoadFromFile(SkinData.DefaultPath + DirectorySeparator +
        'icon' +
        DirectorySeparator + 'mute2.png')
    else
      png.LoadFromFile(SkinData.DefaultPath + DirectorySeparator +
        'icon' +
        DirectorySeparator + 'mute1.png');

    mute.Glyph.Assign(png);
    png.Free;
  end;
end;

procedure TMain.opendirClick(Sender: TObject);

var
  i: integer;
begin
  SelectDirectoryDialog1.InitialDir := CactusConfig.HomeDir;
  Selectdirectorydialog1.title := 'Add Directory...';
  if SelectDirectoryDialog1.Execute = True then
  begin
    for i := 0 to MediaCollection.dirlist.Count - 1 do
    begin
      if pos(MediaCollection.dirlist[i], SelectDirectoryDialog1.FileName) = 1 then
      begin
        ShowMessage('Directory ' + SelectDirectoryDialog1.FileName +
          ' is still part of directorylist');
        exit;
      end;
    end;
    Enabled := False;
    Application.ProcessMessages;
    MediaCollection.add_directory(SelectDirectoryDialog1.FileName);
    DebugOutLn('finished scan of ' + Selectdirectorydialog1.Filename, 3);
    if MediaCollection.ItemCount > 1 then
    begin
      Main.ArtistTree.Selected := nil;
      update_artist_view;
      update_title_view;
    end;
    Enabled := True;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.openfileClick(Sender: TObject);

var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(self);
  OpenDialog.Filter :=
    'All supported audio|*.wav;*.mp3;*.ogg;*.wma;*.flac;*.fla|MP3|*.mp3|OGG|*.ogg|WAV|*.wav|WMA|*.wma|FLAC|*.flac;*.fla';
  if FileOpneDialogPath <> '' then
    OpenDialog.InitialDir := FileOpneDialogPath
  else
    OpenDialog.InitialDir := CactusConfig.HomeDir;
  OpenDialog.FilterIndex := 1;
  if Opendialog.Execute = True then
  begin
    LoadFile(Opendialog.Filename);
    FileOpneDialogPath := ExtractFilePath(OpenDialog.FileName);
  end;
  OpenDialog.Free;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.pauseClick(Sender: TObject);
begin
  PlayerObj.pause;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.player_libClick(Sender: TObject);
begin
  if playwin.Active then
  begin
    playwin.hide;
    main.Show;
    playermode := False;
  end
  else
  begin
    playwin.Show;
    main.hide;
    playermode := True;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.playlistClick(Sender: TObject);
begin
  ArtistSrchField.hide;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.playlistDblClick(Sender: TObject);
begin
  playclick(nil);
end;

procedure TMain.playlistDragDrop(Sender, Source: TObject; X, Y: integer);

var
  Targetitem, tmpitem: TListItem;
  sourceNode: TTreeNode;
  ind: integer;
  MedFileObj: TMediaFileClass;
begin
  if ViewMode <> cvmNetwork then
  begin
    DebugOutLn('ondragdrop', 3);
    Targetitem := nil;

    try
      if Playlist.Items.Count > 0 then
        Targetitem := Playlist.GetItemAt(x, y);
    except
      Targetitem := nil;
    end;

    if Playlist.Items.Count = 0 then
      Targetitem := nil;
    if Targetitem <> nil then
      DebugOutLn(Targetitem.Index, 3)
    else
      DebugOutLn('TARGET NIL', 3);

    if title_drag then
    begin
      title_drag := False;
      MedFileObj := TMediaFileClass(sourceitem.Data);
      tmpitem := TListItem.Create(Playlist.Items);
      if (MedFileObj.Artist <> '') or (MedFileObj.Title <> '') then
      begin
        tmpitem.Caption := MedFileObj.Artist + ' - ' + MedFileObj.Title;
      end
      else
        tmpitem.Caption := ExtractFileName(MedFileObj.Path);
      tmpitem.Data := MedFileObj;

      if (Targetitem <> nil) and (Targetitem.Index < Playlist.Items.Count - 1) and
        (Playlist.Items.Count > 0) then
      begin
        ind := Targetitem.Index;
        Playlist.Items.InsertItem(tmpitem, ind + 1);
        PlayerObj.Playlist.insert(tmpitem.Index + 1, MedFileObj);
      end
      else
      begin
        Playlist.Items.AddItem(tmpitem);
        PlayerObj.playlist.add(MedFileObj);
      end;
      sourceitem := nil;
      if not PlayerObj.playing and (CactusConfig.AutostartPlay) and
        (Playlist.Items.Count = 1) then
        playClick(self);
    end;

    if artist_drag then
    begin
      artist_drag := False;
      sourceNode := ArtistTree.Selected;
      if (Targetitem <> nil) and (Targetitem.Index < Playlist.Items.Count - 1) and
        (Playlist.Items.Count > 0) then
        artist_to_playlist_at(Targetitem.Index + 1)
      else
        artist_to_playlist;
    end;

    if playlist_drag then
    begin
      playlist_drag := False;
      sourceitem := Playlist.Selected;
      DebugOutLn('playlist_Drag', 3);
      if (sourceitem <> nil) and (Targetitem <> sourceitem) then
      begin
        ind := sourceitem.Index;
        DebugOutLn('OK', 3);
        tmpitem := TListItem.Create(Playlist.Items);
        tmpitem.Assign(sourceitem);
        if (Targetitem <> nil) and (Targetitem.Index < Playlist.Items.Count - 1) then
        begin
          Playlist.Items.InsertItem(tmpitem, Targetitem.Index + 1);
          sourceitem.Delete;
          if ind > tmpitem.Index - 1 then
            PlayerObj.Playlist.move(ind, tmpitem.Index)
          else
            PlayerObj.Playlist.move(ind, tmpitem.Index);
          DebugOutLn('MOVE', 3);
        end
        else
        begin
          Playlist.Items.AddItem(tmpitem);
          sourceitem.Delete;
          PlayerObj.Playlist.move(ind, tmpitem.Index);
          DebugOutLn('ADD', 3);
        end;
      end;
    end;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.playlistDragOver(Sender, Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
begin
{$ifdef LCLGtk}
  //Workaround for GTK1.x to reset selected Item while dragging
  DebugOutLn('gtk1', 5);
  if playlist_drag then
  begin
    playlist.Selected := nil;
    playlist.Items[sourceitem.Index].Selected := True;
  end;
{$endif}
  accept := True;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.playlistEndDrag(Sender, Target: TObject; X, Y: integer);

var
  tmplitem: TListItem;
begin
  playlist_drag := False;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.playlistKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);

var
  tempitem: TListItem;
  i: integer;
begin
  DebugOutLn(Format('Playlist keypress event: Keycode = %x', [Key]), 2);
  case key of

    // Key Ctrl
    17: ctrl_pressed := True;

    // Key UP
    38: if playlist.Selected.Index > 0 then
      begin
        i := playlist.Selected.Index;
        DebugOutLn(i, 2);
        if ctrl_pressed then
        begin
          tempitem := playlist.selected;
          PlayerObj.playlist.move(i, i - 1);
          playlist.items[i] := playlist.items[i - 1];
          playlist.items[i - 1] := tempitem;
          Playlist.SetFocus;
          playlist.items[i].Selected := False;
          playlist.items[i - 1].Selected := True;
          //tempitem.MakeVisible(true);
        end;
      end;

    // Key DOWN
    40: if playlist.Selected.Index < playlist.items.Count - 1 then
      begin
        i := playlist.Selected.Index;
        DebugOutLn(i, 2);
        if ctrl_pressed then
        begin
          tempitem := playlist.selected;
          PlayerObj.playlist.move(i, i + 1);
          playlist.items[i] := playlist.items[i + 1];
          playlist.items[i + 1] := tempitem;
          Playlist.SetFocus;
          playlist.items[i].Selected := False;
          playlist.items[i + 1].Selected := True;
          //tempitem.MakeVisible(true);
        end;
        DebugOutLn(playlist.Selected.Index, 2);
      end;
    // Key Del
    46: MenuItem3Click(nil);
  end;
end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.playlistKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 17 then
    ctrl_pressed := False;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.playlistMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);

var
  tempitem: TListItem;
begin
  // ensure that the popup menu is only opened when an item is selected
  // the menu is reanabled in TMain.playlistSelectItem

  {$ifdef  LCLQT}//TODO: QT interface doesn't set selected item
  Playlist.Selected := Playlist.GetItemAt(x, y);
  {$endif}

  {$ifdef  LCLGtk2}//TODO: GTK2 interface doe snot selcte item on right click
  if (Button = mbRight) then
    Playlist.Selected := Playlist.GetItemAt(x, y);
  {$endif}

  {$ifdef win32}
  if (Button = mbRight) and (Playlist.Selected <> nil) then
    Playlist.PopupMenu.PopUp(self.Left + panel4.Width + Panel3.Left + Playlist.left + X + 10,
      self.top + Panel3.Top + Playlist.top + Y + 50);
  {$else}
  if (Button = mbRight) and (playlist.Selected = nil) then
    playlist.PopupMenu.AutoPopup := False;
  {$endif}
  //Enable Dragging
  if Button = mbLeft then
  begin { only drag if left button pressed }
    sourceitem := nil;
    sourceitem := Playlist.GetItemAt(x, y);
    if sourceitem <> nil then
      Playlist.BeginDrag(False, 10);
  end;

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.playlistSelectItem(Sender: TObject; Item: TListItem;
  Selected: boolean);
begin
  // reanable the popupmenu in case ist was disabled in TMain.playlistMouseDown
  playlist.PopupMenu.AutoPopup := True;
  if (Item.Data <> nil) then
  begin
    lblPath.Caption := UTF8Encode(TMediaFileClass(Item.Data).Path);
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.playlistStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  playlist_drag := True;
  DebugOutLn('playlist drag', 3);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.playtimerStartTimer(Sender: TObject);

var
  MedFileObj: TMediaFileClass;
  i: integer;
begin
  if PlayerObj.PlaybackMode = FILE_MODE then
  begin
    CoverFound := False;
    LoopCount := 0;
    i := PlayerObj.CurrentTrack;
    MedFileObj := TMediaFileClass(playlist.Items[PlayerObj.CurrentTrack].Data);
    if (MedFileObj.album <> '') then
    begin
      MedFileObj.CoverPath := CactusConfig.GetCoverPath(MedFileObj.GetCoverFile);
      if (FileExists(MedFileObj.CoverPath) = False) then
      begin
        CoverImage.Picture.Clear;
        if (CactusConfig.CoverDownload) then
        begin
          LastFMAPI := TLastfmAPIObject.Create;
          if CactusConfig.CoverSize = 'large' then
            LastFMAPI.CoverSize := ExtralargeImage
          else
            LastFMAPI.CoverSize := LargeImage;
          LastFMAPI.album_downloadCover(MedFileObj.Artist,
            MedFileObj.Album, MedFileObj.CoverPath);
        end;
      end
      else
      begin
        try
          CoverImage.Picture.LoadFromFile(MedFileObj.CoverPath);
          playwin.AlbumCoverImg.Picture.LoadFromFile(MedFileObj.CoverPath);
        except
          DebugOutLn('EXCEPTION', 3);
        end;
        CoverFound := True;
      end;
    end
    else
      CoverImage.Picture.Clear;

    //CoverImage.Picture.LoadFromFile(DataPrefix+'tools'+DirectorySeparator+'cactus-logo-small.png');
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.ArtistTreeDblClick(Sender: TObject);

var
  StreamInfoObj: TStreamInfoItemClass;
begin
  if ViewMode in [cvmLibrary, cvmDevice] then
  begin
    if (ArtistTree.Selected <> nil) and (ArtistTree.Selected.Level > 0) then
      artist_to_playlist;
  end;

  if ViewMode = cvmNetwork then
  begin
    if (ArtistTree.Selected <> nil) and (ArtistTree.Selected.Level > 0) then
    begin
      StatusBar1.Panels[0].Text := 'Buffering stream...';
      StreamInfoObj := TStreamInfoItemClass(ArtistTree.Selected.Data);
      DebugOutLn(PlayerObj.play(StreamInfoObj.URL), 0);
      current_title_edit.Text := 'Playing radio stream...';
      current_title_edit1.Text := StreamInfoObj.Name;
      playtimer.Enabled := True;
    end;
  end;

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.ArtistTreeEndDrag(Sender, Target: TObject; X, Y: integer);
begin
  artist_drag := False;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.ArtistTreeClick(Sender: TObject);
begin
  ArtistSrchField.Hide;
  // if ArtistTree.Selected<>nil then update_title_view;
end;

procedure TMain.ApplicationProperties1Minimize(Sender: TObject);
begin
  Application.QueueAsyncCall(@MinimizeMe, 0);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.ArtistTreeKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);

var
  b: byte;
  c: char;
  i: integer;
begin
  b := key;
  c := char(b);
  case key of
    45: for i := 0 to ArtistTree.Items.Count - 1 do
        if ArtistTree.Items[i].Level = 0 then
          ArtistTree.Items[i].Expanded := False;
    43: for i := 0 to ArtistTree.Items.Count - 1 do
        if ArtistTree.Items[i].Level = 0 then
          ArtistTree.Items[i].Expanded := True;
    27: ArtistSrchField.Hide;
    13: ArtistSrchField.Hide;

    65..255:
    begin
      if not ArtistSrchField.Visible then
      begin
        ArtistSrchField.Top := main.Height - 120;
        ArtistSrchField.Left := Panel4.Width - 155;
        ArtistSrchField.Show;
        artistsearch.Text := c;
        artistsearch.SetFocus;
        artistsearch.SelStart := 1;
        artistsearch.SelLength := 0;

      end;
      i := 0;
      repeat
        Inc(i)
      until ((pos(lowercase(artistsearch.Text),
          lowercase(ArtistTree.Items[i].Text)) = 1) and
          (ArtistTree.Items[i].Level = 1)) or (i >= ArtistTree.Items.Count - 1);
      if ArtistTree.Items[i].Level > 0 then
      begin
        ArtistTree.Selected := ArtistTree.Items[i];
        if ArtistTree.Selected.AbsoluteIndex < ArtistTree.Items.Count - 10 then
        begin
          ArtistTree.Items[ArtistTree.Selected.AbsoluteIndex +
            9].MakeVisible;
          if ArtistTree.Items[ArtistTree.Selected.AbsoluteIndex +
            9].Level > 1 then
            ArtistTree.Items[ArtistTree.Selected.AbsoluteIndex +
              9].Parent.Expanded := False;
        end
        else
        begin
          ArtistTree.Items[ArtistTree.Items.Count - 1].MakeVisible;
          if ArtistTree.Items[ArtistTree.Items.Count - 1].Level > 1 then
            ArtistTree.Items[ArtistTree.Items.Count -
              1].Parent.Expanded := False;
        end;
      end;
    end;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.ArtistTreeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ArtistTree.SetFocus;
  // ensure that the popup menu is only opened when an item is selected
  if Button = mbRight then
  begin
    if ViewMode = cvmNetwork then
      ArtistTree.PopupMenu := NetworktreePopup
    else
      ArtistTree.PopupMenu := artisttreemenu;
    if (ArtistTree.GetNodeAt(X, Y) = nil) then
      ArtistTree.PopupMenu.AutoPopup := False
    else
      ArtistTree.PopupMenu.AutoPopup := True;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem30Click(Sender: TObject);

var
  MedColObj: TMediaCollectionClass;
  curartist, curalbum, tmps: string;
  tmpsize: int64;
  MedFileObj: TMediaFileClass;
  i: integer;
  tsnode: TTreeNode;
begin
  tsnode := ArtistTree.Selected;
  tmpsize := 0;

  if (tsnode <> nil) and (tsnode.level > 0) and player_connected then
  begin
    MedFileObj := TMediaFileClass(tsnode.Data);
    MedColObj := MedFileObj.collection;
    curartist := lowercase(MedFileObj.Artist);
    i := MedColObj.getTracks(MedFileObj.Artist, MedFileObj.index);
    if tsnode.level = 2 then     //album
    begin
      curalbum := lowercase(MedFileObj.album);
      repeat
        begin
          if (lowercase(MedColObj.items[i].album) = curalbum) and
            (MedColObj.items[i].action = AREMOVE) then
          begin
            MedColObj.items[i].action := AONPLAYER;
            sizediff := sizediff - MedColObj.items[i].size;
          end;
          if (lowercase(MedColObj.items[i].album) = curalbum) and
            (MedColObj.items[i].action <> AONPLAYER) then
          begin
            MedColObj.items[i].action := AUPLOAD;
            sizediff := sizediff - MedColObj.items[i].size;
          end;
          i := MedColObj.GetNext;
        end;
      until i < 0;

    end;
    if tsnode.level = 1 then     //SrchArtItem
    begin
      repeat
        begin
          if (MedColObj.items[i].action = AREMOVE) then
          begin
            MedColObj.items[i].action := AONPLAYER;
            sizediff := sizediff - MedColObj.items[i].size;
          end;
          if (MedColObj.items[i].action <> AONPLAYER) then
          begin
            MedColObj.items[i].action := AUPLOAD;
            sizediff := sizediff - MedColObj.items[i].size;
          end;
          i := MedColObj.GetNext;
        end;
      until i < 0;
    end;
    update_artist_view;
    update_title_view;

    tmps := ByteToFmtString(FreeSpaceOnDAP + sizediff, 3, 2);
    StatusBar1.Panels[1].Text := 'Device connected     ' + tmps + ' Free';
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem33Click(Sender: TObject);

var
  MedFileObj: TMediaFileClass;
  tsnode: TTreeNode;
begin
  if ArtistTree.Selected <> nil then
  begin
    tsnode := ArtistTree.Selected;
    MedFileObj := TMediaFileClass(tsnode.Data);
    if tsnode.level = 1 then
    begin
      editid3win.display_window(MedFileObj, ARTIST_MODE);
    end;
    if tsnode.level = 2 then
    begin
      editid3win.display_window(MedFileObj, ALBUM_MODE);
    end;
    EditID3win.ShowModal;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.rm_artist_playeritemClick(Sender: TObject);

var
  MedColObj: TMediaCollectionClass;
  tmps: string;
  MedFileObj: TMediaFileClass;
  i, z: integer;
  tsnode: TTreeNode;
begin
  tsnode := ArtistTree.Selected;
  if (tsnode <> nil) and (tsnode.level > 0) and player_connected then
  begin
    MedFileObj := TMediaFileClass(tsnode.Data);
    MedColObj := MedFileObj.collection;
    if tsnode.level = 2 then   //remove one album
    begin
      i := PlayerCol.getTracks(MedFileObj.Artist, MedFileObj.Album);
      repeat
        begin
          if PlayerCol.Items[i].Action = AONPLAYER then
          begin
            PlayerCol.items[i].action := AREMOVE;
            for z := 0 to MediaCollection.ItemCount - 1 do
              if PlayerCol.items[i].id = MediaCollection.items[z].id then
                MediaCollection.items[z].action := AREMOVE;
            sizediff := sizediff + PlayerCol.items[i].size;
          end;
          i := PlayerCol.getNext;
        end;
      until (i < 0);
    end;
    if tsnode.level = 1 then      //remove the SrchArtItem
    begin
      i := PlayerCol.getTracks(MedFileObj.Artist);
      repeat
        begin
          if PlayerCol.items[i].action = AONPLAYER then
          begin
            PlayerCol.items[i].action := AREMOVE;
            for z := 0 to MediaCollection.ItemCount - 1 do
              if PlayerCol.items[i].id = MediaCollection.items[z].id then
                MediaCollection.items[
                  z].action := AREMOVE;
            sizediff := sizediff + PlayerCol.items[i].size;
          end;
          i := PlayerCol.getNext;
        end;
      until (i < 0);
    end;
    update_artist_view;
    update_title_view;

    tmps := ByteToFmtString(FreeSpaceOnDAP + sizediff, 3, 2);
    StatusBar1.Panels[1].Text := 'Device connected     ' + tmps + ' Free';
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.searchstrClick(Sender: TObject);
begin
  ArtistSrchField.Hide;
end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.syncplayeritem(Sender: TObject);

var
  newfile: string;
  n: integer;
  ucount, rcount: integer;
  bytesneeded: int64;
begin
  if player_connected = False then
  begin
    ShowMessage(rsNotConnected);
    exit;
  end;
  StatusBar1.Panels[1].Text := 'Calculating...';
  rcount := 1;
  ucount := 1;
  bytesneeded := 0;
  //Calculation the disk space that has to be available on player
  SyncThread := TSyncThread.Create(True);
  SyncThread.Target := PlayerCol.savepath;
  Enabled := False;
  for n := 0 to MediaCollection.ItemCount - 1 do
  begin
    //search for uploads in mediacollection
    if MediaCollection.items[n].action = AUPLOAD then
    begin
      Inc(ucount);
      bytesneeded := bytesneeded + MediaCollection.items[n].size;
      if CactusConfig.mobile_subfolders then
      begin
        if not DirectoryExists(CactusConfig.DAPPath + lowercase(
          MediaCollection.items[n].Artist)) then
          mkdir(CactusConfig.DAPPath + lowercase(MediaCollection.items[n].Artist));
        newfile := CactusConfig.DAPPath + lowercase(
          MediaCollection.items[n].Artist) + '/' + ExtractFileName(
          MediaCollection.items[n].path);
      end
      else
        newfile := CactusConfig.DAPPath + ExtractFileName(
          MediaCollection.items[n].path);
      DoDirSeparators(newfile);
      SyncThread.copyFile(MediaCollection.items[n].path, newfile);
    end;
  end;
  for n := 0 to PlayerCol.ItemCount - 1 do
  begin
    //find files to be deleted in playercollection
    if PlayerCol.items[n].action = AREMOVE then
    begin
      Inc(rcount);
      bytesneeded := bytesneeded - PlayerCol.items[n].size;
      SyncThread.deleteFile(PlayerCol.items[n].path);
      DebugOutLn(PlayerCol.items[n].path + ' to be deleted', 2);
    end;
  end;
  Enabled := True;

  if FreeSpaceOnDAP < bytesneeded then
  begin
    //Check if there is enough free disk space on player
    ShowMessage('ERROR: Not enough free disk space on mobile device!');
    StatusBar1.Panels[1].Text := 'Ready';
    SyncThread.Free;
    exit;
    //Free thread and exit
  end;

  checkmobile.Enabled := False;
  disconnectDAP;

  StatusBar1.Panels[1].Text := 'Please Wait...';
  SyncThread.Resume;

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem3Click(Sender: TObject);

var
  s1, s2: string;
  i: integer;
begin
  if playlist.selected <> nil then
  begin
    i := playlist.selected.index;
    PlayerObj.playlist.remove(i);
    Playlist.Selected.Delete;
    s1 := IntToStr((PlayerObj.Playlist.TotalPlayTime div 60) mod 60);
    s2 := IntToStr((PlayerObj.Playlist.TotalPlayTime div 60) div 60);
    playlist.Column[0].Caption :=
      rsPlaylist + '            (' + IntToStr(
      PlayerObj.Playlist.ItemCount) +
      ' Files/ ' + s2 + 'h ' + s1 + 'min )';
    if (i >= 1) and (i = playlist.items.Count) then
      Dec(i);
    playlist.selected := playlist.items[i];
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MenuItem22aClick(Sender: TObject);
begin
  if PlayerObj.playing then
    artist_to_playlist_at(PlayerObj.CurrentTrack + 1)
  else
    artist_to_playlist;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.RemoveClick(Sender: TObject);
begin
  MediaCollection.Clear;
  TitleTree.Items.Clear;
  ArtistTree.Items.Clear;
  Playlist.Items.Clear;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.QuitItemClick(Sender: TObject);
begin
  Main.Close;
  Application.ProcessMessages;

  // halt;
  Application.Terminate;
  //  Application.Free;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MoveNode(TargetNode, SourceNode: TTreeNode);

var
  nodeTmp: TTreeNode;
begin

{  with Selecttree do
  begin
    nodeTmp := Items.AddChild(TargetNode,SourceNode.Text);
    for i := 0 to SourceNode.Count -1 do
    begin
      MoveNode(nodeTmp,SourceNode.items[i]);
    end;
  end;}
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.titlelistmenuPopup(Sender: TObject);

var
  MedFileObj: TMediaFileClass;
begin
  DebugOutLn('[TMain.titlelistmenuPopup]', 0);
  if TitleTree.Selected <> nil then
  begin
    MedFileObj := TMediaFileClass(TitleTree.Selected.Data);
    // Menuitem16.ImageIndex:=1;
    if MedFileObj.collection = PlayerCol then
    begin
      Menuitem16.Enabled := False;
      Menuitem14.Enabled := True;
    end
    else
    begin
      Menuitem16.Enabled := False;
      //upload
      Menuitem14.Enabled := False;
      //remove

      if MedFileObj.action = -1 then
      begin
        Menuitem16.Enabled := True;
      end;

      if MedFileObj.action = 1 then
      begin
        Menuitem14.Enabled := True;
      end;
    end;
    if player_connected = False then
    begin
      MenuItem16.Visible := False;
      menuitem14.Visible := False;
      menuitem20.Visible := False;
      menuitem11.Visible := False;
    end;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.toggle_playpause(Sender: TObject);
begin
  if PlayerObj.playing then
    pauseClick(nil)
  else
    playClick(nil);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.trackbarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  playtimer.Enabled := False;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.trackbarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);

var
  k: real;
  i: integer;
begin
  k := (x * 100) / trackbar.Width;
  i := round(k);
  PlayerObj.set_fileposition(i);
  trackbar.Position := i;
  if PlayerObj.playing then
    playtimer.Enabled := True;
end;

procedure TMain.TrayIconClick(Sender: TObject);
begin
  if not Visible then
  begin
    Show;
    BringToFront;
  end
  else
  begin
    Hide;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.undoSyncItemClick(Sender: TObject);

var
  tmps: string;
  i: integer;
begin
  begin
    for i := 1 to MediaCollection.ItemCount - 1 do
    begin
      if MediaCollection.items[i].action = AUPLOAD then
        MediaCollection.items[i].action :=
          ANOTHING
      ;
      if MediaCollection.items[i].action = AREMOVE then
        MediaCollection.items[i].action :=
          AONPLAYER
      ;
    end;

    for i := 1 to PlayerCol.ItemCount - 1 do
    begin
      if PlayerCol.items[i].action = AUPLOAD then
        PlayerCol.items[i].action := ANOTHING;
      if PlayerCol.items[i].action = AREMOVE then
        PlayerCol.items[i].action := AONPLAYER;
    end;
    update_artist_view;
    update_title_view;
    sizediff := 0;

    tmps := ByteToFmtString(FreeSpaceOnDAP, 3, 2);
    StatusBar1.Panels[1].Text := 'Device connected     ' + tmps + ' Free';
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.TitleTreeDblClick(Sender: TObject);
begin
  Application.ProcessMessages;
  title_to_playlist;
  Application.ProcessMessages;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure title_to_playlist_at(index: integer);

var
  tsnode: TListitem;
  listitem: TListitem;
  MedFileObj: TMediaFileClass;
begin
  tsnode := main.TitleTree.Selected;
  if (tsnode <> nil) and (tsnode.ImageIndex <> 4) then
  begin
    if main.Playlist.Items.Count = 0 then
    begin
      title_to_playlist;
      exit;
    end;
    MedFileObj := TMediaFileClass(tsnode.Data);

    PlayerObj.playlist.Insert(index, MedFileObj);

    ListItem := Main.Playlist.Items.Insert(index);
    listitem.Data := MedFileObj;
    listitem.MakeVisible(False);
    if MedFileObj.title <> '' then
      ListItem.Caption := MedFileObj.Artist + ' - ' + MedFileObj.title
    else
      ListItem.Caption := extractfilename(MedFileObj.path);
  end;
  main.playlist.Column[0].Caption :=
    rsPlaylist + '                       (' + IntToStr(
    PlayerObj.playlist.ItemCount) +
    ' Files/ ' + PlayerObj.Playlist.TotalPlayTimeStr + ')';
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure title_to_playlist;

var
  tsnode: TListitem;
  listitem: TListitem;
  MedFileObj: TMediaFileClass;
begin
  tsnode := main.TitleTree.Selected;
  DebugOutLn('title to playlist', 5);
  if (tsnode <> nil) and (tsnode.ImageIndex <> 4) then
  begin
    DebugOutLn('title to playlist2', 5);
    MedFileObj := TMediaFileClass(tsnode.Data);

    PlayerObj.playlist.add(MedFileObj);

    ListItem := Main.Playlist.Items.Add;
    listitem.Data := MedFileObj;
    listitem.MakeVisible(False);
    //     listitem.Focused:=true;
    if MedFileObj.title <> '' then
      ListItem.Caption := MedFileObj.Artist + ' - ' + MedFileObj.title
    else
      ListItem.Caption := extractfilename(MedFileObj.path);
    if not PlayerObj.playing and CactusConfig.AutostartPlay and
      (main.Playlist.Items.Count = 1) then
    begin
      DebugOutLn('title to playlist3', 5);
      main.Playlist.Selected := Main.Playlist.Items[0];
      DebugOutLn(Main.Playlist.Selected.Caption, 3);
      Main.playClick(main);
    end;
  end;
  main.playlist.Column[0].Caption :=
    rsPlaylist + '                       (' + IntToStr(
    PlayerObj.playlist.ItemCount) +
    ' Files/ ' + PlayerObj.Playlist.TotalPlayTimeStr + ')';
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure artist_to_playlist_at(index: integer);

var
  tsnode: TTreeNode;
  curartist, curalbum: string;
  Listitem: TListitem;
  album_mode: boolean;
  MedColObj: TMediaCollectionClass;
  MedFileObj: TMediaFileClass;
  z: integer;
begin
  tsnode := Main.ArtistTree.Selected;
  if main.Playlist.Items.Count = 0 then
  begin
    artist_to_playlist;
    exit;
  end;

  if (tsnode <> nil) and (tsnode.Level > 0) then
  begin
    if tsnode.level < 2 then
      album_mode := False
    else
      album_mode := True;
    MedFileObj := TMediaFileClass(tsnode.Data);
    MedColObj := MedFileObj.Collection;
    curartist := lowercase(MedFileObj.Artist);
    curalbum := lowercase(MedFileObj.Album);
    z := MedColObj.getTracks(MedFileObj.Artist, MedFileObj.index);
    repeat
      begin
        DebugOutLn(MedColObj.items[z].title, 3);
        if (album_mode = False) or ((album_mode = True) and
          (lowercase(MedColObj.items[z].album) = curalbum)) then
        begin
          PlayerObj.playlist.insert(index, MedColObj.items[z]);
          ListItem := Main.Playlist.Items.Insert(index);
          Inc(index);
          listitem.Data := MedColObj.items[z];
          // Listitem.Focused:=true;
          if MedColObj.items[z].title <> '' then
            ListItem.Caption := MedColObj.items[z].Artist +
              ' - ' +
              MedColObj.items[z].
              title
          else
            ListItem.Caption := extractfilename(MedColObj.items[z].path);
        end;
        z := MedColObj.GetNext;
      end;
    until z < 0;
    Listitem.MakeVisible(False);
  end;
  main.playlist.Column[0].Caption :=
    rsPlaylist + '            (' + IntToStr(
    PlayerObj.playlist.ItemCount) +
    ' Files/ ' + PlayerObj.Playlist.TotalPlayTimeStr + ' )';
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure artist_to_playlist;

var
  tsnode: TTreeNode;
  curartist, curalbum: string;
  Listitem: TListitem;
  album_mode: boolean;
  MedColObj: TMediaCollectionClass;
  MedFileObj: TMediaFileClass;
  z, oldcount: integer;
begin
  //TODO: add album to playlist sorted by TRACK
  tsnode := Main.ArtistTree.Selected;
  if (tsnode <> nil) and (tsnode.Level > 0) then
  begin
    oldcount := main.Playlist.Items.Count;
    if tsnode.level < 2 then
      album_mode := False
    else
      album_mode := True;
    MedFileObj := TMediaFileClass(tsnode.Data);
    MedColObj := MedFileObj.Collection;
    curartist := lowercase(MedFileObj.Artist);
    curalbum := lowercase(MedFileObj.Album);
    z := MedColObj.getTracks(MedFileObj.Artist, MedFileObj.index);
    repeat
      begin
        DebugOutLn(MedColObj.items[z].title, 3);
        if (album_mode = False) or ((album_mode = True) and
          (lowercase(MedColObj.items[z].album) = curalbum)) then
        begin
          PlayerObj.playlist.add(MedColObj.items[z]);
          ListItem := Main.Playlist.Items.Add;
          listitem.Data := MedColObj.items[z];
          // Listitem.Focused:=true;
          if MedColObj.items[z].title <> '' then
            ListItem.Caption := MedColObj.items[z].Artist +
              ' - ' +
              MedColObj.items[z].
              title
          else
            ListItem.Caption := extractfilename(MedColObj.items[z].path);
        end;
        z := MedColObj.GetNext;
      end;
    until z < 0;
    Listitem.MakeVisible(False);
    if CactusConfig.AutostartPlay and (oldcount = 0) and
      (main.Playlist.Items.Count > 0) then
    begin
      main.Playlist.Selected := Main.Playlist.Items[0];
      Main.playClick(main);
    end;
  end;
  main.playlist.Column[0].Caption :=
    rsplaylist + '            (' + IntToStr(
    PlayerObj.playlist.ItemCount) +
    ' Files/ ' + PlayerObj.Playlist.TotalPlayTimeStr + ' )';
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure update_title_view;

var
  tsnode: TTreeNode;
  curartist, curalbum: string;
  Listitem: TListItem;
  album_mode: boolean;
  MedColObj: TMediaCollectionClass;
  MedFileObj: TMediaFileClass;
  i: integer;
begin
  tsnode := Main.ArtistTree.Selected;
  main.StatusBar1.Panels[0].Text := 'Please wait... updating...';

  DebugOutLn('', 2);
  DebugOut('## update title view...', 2);
  main.TitleTree.Selected := nil;

  Main.TitleTree.Clear;
{$ifndef LCLGtk2}
  Main.TitleTree.BeginUpdate;
{$endif}

{$ifdef LCLGtk2}
  DebugOut(' <TODO: BeginUpdate/EndUpdate disabled in GTK2 due to some bugs in LCL> ',
    2);
  //TODO: BeginUpdate/EndUpdate disabled in GTK2 due to some bugs in LCL
{$endif}

  DebugOut(' cleared items... ', 2);

  if (tsnode <> nil) and (tsnode.level > 0) then
  begin
    if tsnode.level = 2 then
      album_mode := True
    else
      album_mode := False;

    MedFileObj := TMediaFileClass(tsnode.Data);
    MedColObj := MedFileObj.Collection;
    curartist := lowercase(MedFileObj.Artist);
    curalbum := lowercase(MedFileObj.album);
    DebugOut(curartist, 2);

    i := MedColObj.getTracks(MedFileObj.Artist, 0{MedFileObj.index});

    repeat
      begin
        if (album_mode = False) or ((album_mode) and
          (curalbum = lowercase(MedColObj.items[i].album))) then
        begin
          ListItem := Main.Titletree.Items.Add;
          MedColObj.items[i].index := i;
          listitem.Data := MedColObj.items[i];
          Listitem.ImageIndex := MedColObj.items[i].action;
          Listitem.Caption := '';

          if MedColObj.items[i].title <> '' then
            ListItem.SubItems.Add(MedColObj.items[i].Artist)
          else
            ListItem.SubItems.Add(SysToUTF8(ExtractFileName(MedColObj.items[i].path)));

          ListItem.SubItems.Add((MedColObj.items[i].title));
          ListItem.SubItems.Add((MedColObj.items[i].album));
          ListItem.SubItems.Add(MedColObj.items[i].track);
          ListItem.SubItems.Add(ID3Genre[MedColObj.items[i].GenreID]);
          ListItem.SubItems.Add(SysToUTF8(ExtractFileName(MedColObj.items[i].Path)));
          ListItem.SubItems.Add(MedColObj.items[i].playtime);
        end;
        i := MedColObj.GetNext;
      end;
    until (i < 0);
  end;
  if CactusConfig.SortAlbumByTrack and album_mode then
    main.TitleTreeColumnClick(main, main.TitleTree.Column[4]);
  DebugOutLn(' finished title view ##', 2);
{$ifndef LCLGtk2}
  Main.TitleTree.EndUpdate;
{$endif}
  main.StatusBar1.Panels[0].Text := 'Ready.';

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.update_playlist;

var
  MedfileObj: TMediaFileClass;
  i: integer;
begin

  for i := 0 to PlayerObj.Playlist.ItemCount - 1 do
  begin
    MedfileObj := TMediaFileClass(playlist.Items[i].Data);
    PlayerObj.Playlist.Items[i].update(MedfileObj);

    if MedfileObj.title <> '' then
      playlist.Items[i].Caption := MedfileObj.Artist + ' - ' + MedfileObj.title
    else
      playlist.Items[i].Caption := extractfilename(MedfileObj.path);
  end;
  update_player_display;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.disconnectDAP;

var
  i: integer;
begin
  if player_connected then
  begin
    DebugOutLn('### Disconnect DAP ###', 2);
    Enabled := False;
    i := 0;
    while i < playlist.Items.Count do
    begin
      if TMediaFileClass(playlist.Items[i].Data).collection = PlayerCol then
      begin
        PlayerObj.playlist.remove(i);
        if PlayerObj.Playlist.ItemCount <> 0 then
          Playlist.Items[i].Delete;
        Dec(i);
      end;
      Inc(i);
    end;
    FreeAndNil(PlayerCol);
    player_connected := False;
    for i := 1 to MediaCollection.ItemCount - 1 do
      MediaCollection.items[i].action := -1;
    ArtistTree.Selected := nil;
    Enabled := True;
    update_artist_view;
    update_title_view;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function TMain.connectDAP: byte;

var
  i, z: integer;
begin
  Result := 255;
  PlayerCol := TMediaCollectionClass.Create;
  PlayerCol.PathFmt := FRelative;
  DebugOutLn('### ConnectDAP  ###', 2);
  if PlayerCol.LoadFromFile(CactusConfig.DAPPath + 'cactuslib') = True then
  begin

    sizediff := 0;
    for i := 0 to PlayerCol.ItemCount - 1 do
    begin
      z := 0;
      PlayerCol.items[i].action := AONPLAYER;
      while z < MediaCollection.ItemCount - 1 do
      begin
        if MediaCollection.items[z].id = PlayerCol.items[i].id then
        begin
          MediaCollection.items[z].action := AONPLAYER;
          z := MediaCollection.ItemCount - 1;
        end;
        Inc(z);
      end;

    end;
    player_connected := True;
    update_artist_view;
    update_title_view;
    checkmobile.Enabled := True;
    Result := 0;
  end
  else
    FreeAndNil(PlayerCol);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


procedure TMain.TestPlugin1(Sender: TObject);
var
  tt: TCJ_Signals;
begin
  //TestPluginI :=CJ_Interface.GetMenu.Add(-CJ_MAINMENU_PLUGINS, 'Hello World...', @SayHello);
  //TestPluginI :=CJ_Interface.GetMenu.Add(-CJ_MAINMENU_ROOT, 'Hello World...', @SayHello);
  TestPluginI := CJ_Interface.GetMenu.Add(-CJ_TRAYMENU_ROOT,
    'Hello World...', @SayHello);
  tt := CJ_Interface.GetSignals;
  if tt <> nil then
  begin
    tt.Connect(@SayMsgHello, 1);
    tt.Connect(@SayMsgHello2, 1);
    tt.Connect(@SayMsgHello, 1); //Test for no insertion of this....
  end;
end;


procedure TMain.SayHello(Sender: TCJ_MenuItem);
begin
  Dialogs.MessageDlg('Plugins', 'Hello World Click', mtInformation, [mbOK], 0);
  CJ_Interface.GetMenu.Remove(TestPluginI);
end;

function TMain.SayMsgHello(var Message: TMessage): boolean;
begin
  Dialogs.MessageDlg('Plugins', 'Hello World From Messages...' +
    #13#10 + IntToStr(Message.WParam) + ' ' + IntToStr(Message.LParam), mtInformation, [mbOK], 0);
  Result := True;
end;

function TMain.SayMsgHello2(var Message: TMessage): boolean;
begin
  Dialogs.MessageDlg('Plugins', 'Hello World 2 From Messages...' +
    #13#10 + IntToStr(Message.WParam) + ' ' + IntToStr(Message.LParam), mtInformation, [mbOK], 0);
  Result := True;
end;

procedure TMain.Button2Click(Sender: TObject);
var
  msgHandled: boolean;

begin
  CJ_Interface.GetSignals.Signal(1, 24, 50, msgHandled);
end;

end.

