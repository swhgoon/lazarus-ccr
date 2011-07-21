

{
Main unit for Cactus Jukebox

written by Sebastian Kraft, <c> 2006-2008

Contact the author at: sebastian_kraft@gmx.de

This Software is published under the GPL






}

//TODO: Check if position icon in playlist works after loading playlist from file

Unit mainform;


//{$mode delphi}{$H+}
{$mode objfpc}{$H+}
{$ifdef CPU86}          //compile with fmod support enabled by default on i386
   {$define fmod}
{$endif}

Interface


Uses 

Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
ExtCtrls, ComCtrls, StdCtrls, Menus,{$ifdef fmod} fmodplayer,{$endif}
ActnList, mediacol, dos, SimpleIPC, functions, EditBtn, last_fm, debug, config,
playlist, playerclass, mplayer, mp3file, Messages, LMessages, cj_interfaces;

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

Type 

  { TMain }

  TMain = Class(TForm)
    ArtistTree: TTreeView;
    artistsearch: TEdit;
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
    LibModeBtn: TToolButton;
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
    Procedure ArtistTreeClick(Sender: TObject);
    Procedure ArtistTreeDblClick(Sender: TObject);
    Procedure ArtistTreeEndDrag(Sender, Target: TObject; X, Y: Integer);
    Procedure ArtistTreeKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState
    );
    Procedure ArtistTreeMouseDown(Sender: TOBject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
    Procedure ArtistTreeSelectionChanged(Sender: TObject);
    Procedure ArtistTreeStartDrag(Sender: TObject; Var DragObject: TDragObject);
    Procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure checkmobileStartTimer(Sender: TObject);
    Procedure CoverImageMouseUp(Sender: TObject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);
    Procedure DeviceModeBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    Procedure FormMouseDown(Sender: TOBject; Button: TMouseButton;
                            Shift: TShiftState; X, Y: Integer);
    Procedure FormResize(Sender: TObject);
    procedure itemTrayExitClick(Sender: TObject);
    procedure itemTrayPlayClick(Sender: TObject);
    Procedure LibModeBtnClick(Sender: TObject);
    Procedure MenuItem15Click(Sender: TObject);
    Procedure MenuItem25Click(Sender: TObject);
    procedure MIrandom_playlistClick(Sender: TObject);
    procedure MIViewAlbumClick(Sender: TObject);
    procedure MIViewArtistClick(Sender: TObject);
    Procedure MenuItem32Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem7Click(Sender: TObject);
    Procedure MenuItem9Click(Sender: TObject);
    procedure MIDeviceInfoClick(Sender: TObject);
    Procedure MIremoveRadioClick(Sender: TObject);
    procedure MIRipAudioClick(Sender: TObject);
    procedure MIViewFilenameClick(Sender: TObject);
    procedure MIViewGenreClick(Sender: TObject);
    procedure MIViewTitleClick(Sender: TObject);
    procedure MIViewTrackClick(Sender: TObject);
    procedure mnuCleanLibClick(Sender: TObject);
    Procedure NetModeBtnClick(Sender: TObject);
    Procedure NextButtonImgClick(Sender: TObject);
    Procedure NextButtonImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                     Shift: TShiftState; X, Y: Integer);
    Procedure NextButtonImgMouseEnter(Sender: TObject);
    Procedure NextButtonImgMouseLeave(Sender: TObject);
    Procedure NextButtonImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                   Shift: TShiftState; X, Y: Integer);
    Procedure PlaylistCustomDrawItem(Sender: TCustomListView; Item: TListItem;
                                     State: TCustomDrawState; Var DefaultDraw: Boolean);
    procedure pnlPlaytimeClick(Sender: TObject);
    procedure PopupMenuTrayPopup(Sender: TObject);
    procedure randomcheckChange(Sender: TObject);
    procedure scan(Sender: TObject);
    Procedure SearchPanelClick(Sender: TObject);
    Procedure PlayerControlsPanelClick(Sender: TObject);
    Procedure PauseButtonImgClick(Sender: TObject);
    Procedure PauseButtonImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                      Shift: TShiftState; X, Y: Integer);
    Procedure PauseButtonImgMouseEnter(Sender: TObject);
    Procedure PauseButtonImgMouseLeave(Sender: TObject);
    Procedure PauseButtonImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
    Procedure PlayButtonImgClick(Sender: TObject);
    Procedure PlayButtonImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                     Shift: TShiftState; X, Y: Integer);
    Procedure PlayButtonImgMouseEnter(Sender: TObject);
    Procedure PlayButtonImgMouseLeave(Sender: TObject);
    Procedure PlayButtonImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                   Shift: TShiftState; X, Y: Integer);
    Procedure MainClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure MainCreate(Sender: TObject);
    Procedure MenuItem11Click(Sender: TObject);
    Procedure MenuItem14Click(Sender: TObject);
    Procedure MenuItem16Click(Sender: TObject);
    Procedure MenuItem19Click(Sender: TObject);
    Procedure MenuItem20Click(Sender: TObject);
    Procedure MenuItem26Click(Sender: TObject);
    Procedure MenuItem27Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem37Click(Sender: TObject);
    Procedure MenuItem43Click(Sender: TObject);
    Procedure Panel1Click(Sender: TObject);
    Procedure Panel1Resize(Sender: TObject);
    Procedure Panel4Click(Sender: TObject);
    Procedure ArtistSrchFieldClick(Sender: TObject);
    Procedure PreviousButtonImgClick(Sender: TObject);
    Procedure PreviousButtonImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                         Shift: TShiftState; X, Y: Integer);
    Procedure PreviousButtonImgMouseEnter(Sender: TObject);
    Procedure PreviousButtonImgMouseLeave(Sender: TObject);
    Procedure PreviousButtonImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                       Shift: TShiftState; X, Y: Integer);
    Procedure SettingsItemClick(Sender: TObject);
    Procedure SimpleIPCServer1Message(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);

    Procedure Splitter1Moved(Sender: TObject);
    Procedure SrchAlbumItemClick(Sender: TObject);
    Procedure SrchArtItemClick(Sender: TObject);
    Procedure SrchFileItemClick(Sender: TObject);
    Procedure SrchTitleItemClick(Sender: TObject);
    Procedure srch_buttonKeyUp(Sender: TObject; Var Key: Word;
                               Shift: TShiftState);
    Procedure StopButtonImgClick(Sender: TObject);
    Procedure StopButtonImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                     Shift: TShiftState; X, Y: Integer);
    Procedure StopButtonImgMouseEnter(Sender: TObject);
    Procedure StopButtonImgMouseLeave(Sender: TObject);
    Procedure StopButtonImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                   Shift: TShiftState; X, Y: Integer);
    procedure TestPlugin1(Sender: TObject);

    Procedure TitleTreeClick(Sender: TObject);
    Procedure TitleTreeColumnClick(Sender: TObject; Column: TListColumn);
    Procedure TitleTreeDragOver(Sender, Source: TObject; X, Y: Integer;
                                State: TDragState; Var Accept: Boolean);
    Procedure TitleTreeEndDrag(Sender, Target: TObject; X, Y: Integer);
    Procedure TitleTreeMouseDown(Sender: TOBject; Button: TMouseButton;
                                 Shift: TShiftState; X, Y: Integer);
    Procedure TitleTreeSelectItem(Sender: TObject; Item: TListItem;
                                  Selected: Boolean);
    Procedure TitleTreeStartDrag(Sender: TObject; Var DragObject: TDragObject);
    Procedure TrackInfoClick(Sender: TObject);
    Procedure artisttreemenuPopup(Sender: TObject);
    Procedure checkmobileTimer(Sender: TObject);
    Procedure clearPlayerItemClick(Sender: TObject);
    Procedure clear_listClick(Sender: TObject);
    Procedure filetypeboxChange(Sender: TObject);
    Procedure libinfoClick(Sender: TObject);
    Procedure muteClick(Sender: TObject);
    Procedure opendirClick(Sender: TObject);
    Procedure openfileClick(Sender: TObject);
    Procedure pauseClick(Sender: TObject);
    Procedure player_libClick(Sender: TObject);
    Procedure playlistClick(Sender: TObject);
    Procedure playlistDblClick(Sender: TObject);
    Procedure playlistDragDrop(Sender, Source: TObject; X, Y: Integer);
    Procedure playlistDragOver(Sender, Source: TObject; X, Y: Integer;
                               State: TDragState; Var Accept: Boolean);
    Procedure playlistEndDrag(Sender, Target: TObject; X, Y: Integer);
    Procedure playlistKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
    );
    Procedure playlistKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure playlistMouseDown(Sender: TOBject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);
    Procedure playlistSelectItem(Sender: TObject; Item: TListItem;
                                 Selected: Boolean);
    Procedure playlistStartDrag(Sender: TObject; Var DragObject: TDragObject);
    Procedure playtimerStartTimer(Sender: TObject);
    Procedure prevClick(Sender: TObject);
    Procedure EditID3itemClick(Sender: TObject);
    Procedure MenuItem30Click(Sender: TObject);
    Procedure MenuItem33Click(Sender: TObject);
    Procedure rm_artist_playeritemClick(Sender: TObject);
    Procedure searchstrClick(Sender: TObject);
    Procedure skinmenuClick(Sender: TObject);
    Procedure syncplayeritem(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem22aClick(Sender: TObject);
    Procedure Menuitem10Click(Sender: TObject);
    Procedure RemoveClick(Sender: TObject);
    Procedure QuitItemClick(Sender: TObject);
    Procedure TitleTreeDblClick(Sender: TObject);
    Procedure loadlibClick(Sender: TObject);
    Procedure newlibClick(Sender: TObject);
    Procedure nextClick(Sender: TObject);
    Procedure playClick(Sender: TObject);
    Procedure playtimerTimer(Sender: TObject);
    Procedure removeselectClick(Sender: TObject);
    Procedure save_listClick(Sender: TObject);
    Procedure savelibClick(Sender: TObject);
    Procedure scanplayeritemClick(Sender: TObject);
    Procedure searchstrKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState
    );
    Procedure srch_buttonClick(Sender: TObject);
    Procedure stopClick(Sender: TObject);
    Procedure titlelistmenuPopup(Sender: TObject);
    Procedure toggle_playpause(Sender: TObject);
    Procedure trackbarMouseDown(Sender: TOBject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);
    Procedure trackbarMouseUp(Sender: TOBject; Button: TMouseButton;
                              Shift: TShiftState; X, Y: Integer);
    procedure TrayIconDblClick(Sender: TObject);
    Procedure undoSyncItemClick(Sender: TObject);

    Procedure loadskin(Sender: TObject);
    Procedure update_player_hdd_relations;
    Procedure VolumebarMouseUp(Sender: TObject; Button: TMouseButton;
                               Shift: TShiftState; X, Y: Integer);
  Private
  { private declarations }
    ctrl_pressed, SplitterResize: boolean;

    oldSplitterWidth, LoopCount: integer;
    sourceitem: TListItem;
    CoverFound, title_drag, playlist_drag, artist_drag: Boolean;
    DeviceMode, NetworkMode, LibraryMode: boolean;
    LastFMAPI: TLastfmAPIObject;
    ScanSyncCount: Integer;
    FileOpneDialogPath: string;
    bPnlPlaytimeNegated: boolean;
    oldWindowState :TWindowState;
    fromTrayDBLClick :Boolean;
    Procedure MoveNode(TargetNode, SourceNode : TTreeNode);
    Procedure ApplicationIdle(Sender: TObject; Var Done: Boolean);
    Procedure update_player_display;
    Function LoadFile(path: String): boolean;

  Public
    player_connected, playermode: boolean;
    playpos: integer;
    playnode: TTreeNode;
    playitem: TListitem;
    curlib: string;

    tempbitmap, timetmpbmp: TBitmap;
    player_freespace, player_totalspace: longint;

    skinmenuitems: array[1..16] Of TMenuItem;

    TestPluginI :TCJ_MenuItem;

    Procedure update_playlist;
    Procedure disconnectDAP;
    Function connectDAP: byte;
    Procedure ScanSyncronize(dir:String);
    Procedure update_artist_view;

    { public declarations }
    procedure WMSize(var Message: TLMSize); message LM_Size;

    //Test Plugins....cut in future
    procedure SayHello(Sender :TCJ_MenuItem);
    function SayMsgHello(var Message: TMessage):Boolean;
    function SayMsgHello2(var Message: TMessage):Boolean;

  End;


Type 

   { TScanThread }

  TScanThread = Class(TThread)
   Private
    Procedure ShowStatus;
    Protected 
    Procedure Execute;
    override;
   Public
    fStatus : byte;
    tmpcollection: TMediaCollectionClass;
    PTargetCollection: TMediaCollectionClass;
    Constructor Create(Suspd : boolean);
  End;

 { TScanThread }

Type 

   { TSyncThread }

  TSyncAction = (SCopy, SDelete);

  TSyncThread = Class(TThread)
    Private 
    Procedure SyncStatus;
    Protected
    CopyList, TargetList, DeleteList: TStringList;
    DeletedCnt, DeleteTotal, CopyTotal, CopiedCnt: Integer;
    OpSuccess, finished: boolean;
    SAction: TSyncAction;
    TargetCollection: TMediaCollectionClass;
    Procedure Execute;
    override;
    Public 
    Target: String;
    Constructor Create(Suspd : boolean);
    destructor Destroy;
    override;
    Procedure CopyFile( fromFile, toFile: String);
    Procedure DeleteFile( path: String);
  End;

 { TSyncThread }


Var 
  Main: TMain;
  SyncThread: TSyncThread;
  ScanThread: TscanThread;

  //procedure update_title_view_album;
Procedure update_title_view;
Procedure artist_to_playlist;
Procedure artist_to_playlist_at(index: integer);
Procedure title_to_playlist_at(index: integer);
Procedure title_to_playlist;


Implementation

Uses editid3, status, settings, player, directories, skin, cdrip, translations, bigcoverimg,
streamcol, addradio, CleanLibrary, global_vars, cj_pluginslist, cj_interfaces_impl, LCLType;

{$i cactus_const.inc}

Var     sizediff: int64;


{ TSyncThread }

Procedure TSyncThread.SyncStatus;
Begin
  If finished=false Then
    Begin
      If SAction=SCopy Then Main.StatusBar1.Panels[1].Text := IntToStr(CopiedCnt)+' of '+IntToStr(
                                                              CopyTotal)+
                                                              ' copied. Don''t Disconnect...';
      If SAction=SDelete Then Main.StatusBar1.Panels[1].Text := IntToStr(DeletedCnt)+' of '+IntToStr
                                                                (DeleteTotal)+
                                                                ' deleted. Don''t Disconnect...';
    End
  Else
    Begin
      writeln('finished');
      TargetCollection.SaveToFile;
      TargetCollection.Free;
      main.connectDAP;
      Main.StatusBar1.Panels[1].Text := 'Synchronizing finished. Device Ready...';
    End;
End;

Procedure TSyncThread.Execute;
Begin
  finished := false;
  DeleteTotal := DeleteList.Count;
  CopyTotal := CopyList.Count;
  DeletedCnt := 0;
  CopiedCnt := 0;
  TargetCollection := TMediaCollectionClass.create;
  TargetCollection.PathFmt := FRelative;
  TargetCollection.LoadFromFile(Target);
  While DeleteList.Count>0 Do
    Begin
      OpSuccess := false;
      Try

        sysutils.DeleteFile(self.DeleteList[0]);
        If FileExists(self.DeleteList[0])=false Then
          Begin
            TargetCollection.remove(TargetCollection.getIndexByPath(self.DeleteList[0]));
          End;
        If DirectoryIsEmpty(ExtractFileDir(DeleteList[0])) Then
          RemoveDir(ExtractFileDir(DeleteList[0]))
        Except
        End;
        inc(DeletedCnt);
        SAction := SDelete;
        self.DeleteList.Delete(0);
        Synchronize(@SyncStatus);
    End;
  DebugOutLn('copying files...', 6);
  While CopyList.Count>0 Do
    Begin
      OpSuccess := false;
      OpSuccess := FileCopy(CopyList[0],TargetList[0]);
      inc(CopiedCnt);
      SAction := SCopy;
      TargetCollection.add(TargetList[0]);
      CopyList.Delete(0);
      TargetList.Delete(0);
      Synchronize(@SyncStatus);
    End;

  Finished := true;
  Synchronize(@SyncStatus);
End;

constructor TSyncThread.Create(Suspd: boolean);
Begin
  inherited Create(suspd);
  FreeOnTerminate := True;
  CopyList := TStringList.Create;
  TargetList := TStringList.Create;
  DeleteList := TStringList.Create;
  DeletedCnt := 0;
  CopiedCnt := 0;
End;

destructor TSyncThread.Destroy;
Begin
  inherited Destroy;
  CopyList.Free;
  TargetList.Free;
  DeleteList.free;
End;

Procedure TSyncThread.CopyFile(fromFile, toFile: String);
Begin
  CopyList.Add(fromFile);
  TargetList.Add(toFile);

End;


Procedure TSyncThread.DeleteFile(path: String);
Begin
  DeleteList.Add(path);
End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{       TScanThread : Thread to scan for new media files in background }

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TScanThread.ShowStatus;
Begin
  If fStatus=1 Then Main.StatusBar1.Panels[0].Text := 'Scanning folders in background...';
  If fStatus=0 Then
    Begin
      main.Enabled := false;
      If  MessageDlg('Some files on your harddisk seem to have changed.'+LineEnding+
         'Adopt changes in Cactus library?', mtWarning, mbOKCancel, 0)= mrOK Then
        Begin
          writeln(1);
          fstatus := 255;
          writeln('assigning');

          //             PTargetCollection^.Assign(tmpcollection);
          writeln('saving');
          //             PTargetCollection^.save_lib(PTargetCollection^.savepath);
          Main.clear_listClick(Nil);

          writeln('WARNING: if excption occurs, playlist has to be cleared here!');
          //   Main.update_player_hdd_relations;
          main.update_artist_view;
          update_title_view;

          Main.StatusBar1.Panels[0].Text := ('Succesfully updated library...');
          tmpcollection.Free;
        End;
      main.Enabled := true;
    End;
  If (fstatus=0) Or (fstatus=128) Then
    Begin
      Main.StatusBar1.Panels[0].Text := 'Ready';
      writeln('fstatus 0, 126');
      Main.StatusBar1.Panels[1].Alignment := taRightJustify;
      tmpcollection.Free;
    End;
  writeln('showStatus');
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TScanThread.Execute;
Begin
  fStatus := 1;
  Synchronize(@ShowStatus);

  //   fstatus:=tmpcollection.ScanForNew;
  Synchronize(@ShowStatus);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

constructor TScanThread.Create(Suspd: boolean);
Begin
  inherited Create(suspd);
  FreeOnTerminate := True;
  tmpcollection := TMediaCollectionClass.create;
  fStatus := 255;
End;

{   // End TScanThread                   }



//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{ TMain }

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


Procedure TMain.loadskin(Sender: TObject);
Begin
  DebugOutLn('loading skin', 2);
  With (sender as TMenuitem) Do
    Begin
      SkinData.load_skin(caption);
      CactusConfig.CurrentSkin := caption;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.update_player_hdd_relations;

Var i, z: integer;
Begin
  For i:= 0 To PlayerCol.itemcount-1 Do
    Begin
      z := 0;
      PlayerCol.items[i].action := AONPLAYER;
      While z < MediaCollection.ItemCount-1 Do
        Begin
          If MediaCollection.items[z].id=PlayerCol.items[i].id Then
            Begin
              MediaCollection.items[z].action := 1;
              z := MediaCollection.ItemCount-1;
            End;
          inc(z);
        End;
    End;
  Playercol.SaveToFile(CactusConfig.DAPPath+'cactuslib');
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.VolumebarMouseUp(Sender: TObject; Button: TMouseButton;
                                 Shift: TShiftState; X, Y: Integer);

Var newVolume: byte;
Begin
  If y>Volumebar.Height Then y := Volumebar.Height;
  If y<0 Then y := 0;
  writeln(y);

  newVolume := 100-((y*100) Div (Volumebar.Height));
  PlayerObj.set_volume(newVolume);
  Volumebar.Position := newVolume;
  DebugOutLn('volume set '+ IntToStr(PlayerObj.volume), 3);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.loadlibClick(Sender: TObject);

Var OpenDialog: TOpenDialog;
Begin
  OpenDialog := TOpenDialog.Create(self);
  OpenDialog.Filter := 'Mp3lib Library|*.mlb';
  OpenDialog.InitialDir := CactusConfig.HomeDir;
  OpenDialog.FilterIndex := 1;
  If Opendialog.execute=true Then MediaCollection.LoadFromFile(Opendialog.Filename);
  OpenDialog.free;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.newlibClick(Sender: TObject);
Begin
  Enabled := false;
  Selectdirectorydialog1.initialdir := CactusConfig.HomeDir;
  Selectdirectorydialog1.title := 'Add Directory...';
  If Selectdirectorydialog1.execute=true Then
    Begin
      DebugOutLn('clear old collection', 7);
      MediaCollection.clear;
      DebugOutLn('lll', 7);
      update_artist_view;
      update_title_view;
      Application.ProcessMessages;
      MediaCollection.add_directory(Selectdirectorydialog1.Filename);
      DebugOutLn('finished scan of '+Selectdirectorydialog1.Filename, 2);
      If MediaCollection.ItemCount>0 Then
        Begin
          ArtistTree.Selected := Nil;
          update_artist_view;
          update_title_view;
        End;
    End;
  Enabled := true;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.nextClick(Sender: TObject);

Var oldindex, err, i: integer;
Begin
  playtimer.Enabled := false;
  oldindex := PlayerObj.CurrentTrack;
  If randomcheck.Checked=false Then err := PlayerObj.next_track
      Else err := PlayerObj.play(PlayerObj.Playlist.RandomIndex);
  If err=0 Then
    Begin
      i := PlayerObj.CurrentTrack;
      If i >= 0 Then
        Begin
          If oldindex>=0 Then playlist.Items[oldindex].ImageIndex := -1;
          writeln(oldindex);
          playlist.Items[i].ImageIndex := 0;
          playlist.Items[i].MakeVisible(false);
          playtimer.Enabled := true;
          //CactusPlugins.SendEvent(evnStartPlay, PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].artist+' - '+PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].title);
        End;
    End
  Else stopClick(Nil);
  update_player_display;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.prevClick(Sender: TObject);

Var err: byte;
  i, OldTrack: integer;
Begin
  playtimer.Enabled := false;
  OldTrack := PlayerObj.CurrentTrack;
  err := PlayerObj.prev_track;
  If (err=0) Then
    Begin
      i := PlayerObj.CurrentTrack;
      If playlist.Items.Count>1 Then
        Begin
          If OldTrack>=0 Then playlist.Items[OldTrack].ImageIndex := -1;
          playlist.Items[i].ImageIndex := 0;
          playlist.Items[i].MakeVisible(false);
          //CactusPlugins.SendEvent(evnStartPlay, PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].artist+' - '+PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].title);
        End;
      playtimer.Enabled := true;
    End
  Else stopClick(Nil);
  update_player_display;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.playClick(Sender: TObject);

Var err: integer;
Begin
  If (Not PlayerObj.paused) Then
    Begin
      playtimer.Enabled := false;
      If (Playlist.items.count>0) And (Playlist.Selected=Nil)Then playitem := Playlist.Items[0]
            Else playitem := playlist.selected;
      If (PlayerObj.playing) And (PlayerObj.Playlist.Count>0) And (PlayerObj.CurrentTrack<PlayerObj.Playlist.Count) And (PlayerObj.CurrentTrack>=0)
            Then playlist.Items[PlayerObj.CurrentTrack].ImageIndex := -1;;
      If playitem<>Nil Then
        Begin
          err := PlayerObj.play(playitem.Index);
          If (err=0) Then
            Begin
              Playlist.BeginUpdate;
              playitem.ImageIndex := 0;
              Playlist.EndUpdate;
              writeln(playitem.ImageIndex);
              writeln(playitem.index);
              playitem.MakeVisible(false);
              update_player_display;
              //CactusPlugins.SendEvent(evnStartPlay, PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].artist+' - '+PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].title);
              playtimer.enabled := true;
            End
          Else
            Begin
              If (err=1) Then Showmessage(
                           'File not Found! Goto Library/Rescan Directories for updating file links'
                );
              If (err=2) Then Showmessage('Init of sound device failed.'+#10+#13+
                                      'Perhaps sound ressource is blocked by another application...'
                );
            End;
        End;
    End
  Else
    Begin
      //if player paused
      pauseClick(Nil);
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.stopClick(Sender: TObject);
Begin
  playtimer.Enabled:=false;
  If (PlayerObj.CurrentTrack>=0) And (PlayerObj.CurrentTrack<PlayerObj.
     Playlist.ItemCount) Then playlist.Items[PlayerObj.CurrentTrack].ImageIndex := -1;
  PlayerObj.stop;
  PlayerObj.playlist.reset_random;
  update_player_display;
  //CactusPlugins.SendEvent(evnStopPlay, '');
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.playtimerTimer(Sender: TObject);

Var spos, slength: real;
  r: real;
  x2: integer;
  tmppos: integer;
  fileobj: TMediaFileClass;
Begin
  Try
   // if PlayerObj.playing=false then stopClick(nil);
    If PlayerObj.PlaybackMode=STREAMING_MODE Then
      Begin
        If PlayerObj.Get_Stream_Status=STREAM_READY Then
          StatusBar1.Panels[0].Text := 'Stream Ready'
        Else
          StatusBar1.Panels[0].Text := 'Buffering Stream...';
      End;
//    writeln('ontimer');
    If (PlayerObj.playing) And (PlayerObj.PlaybackMode=FILE_MODE) and (PlayerObj.paused=false) Then
      Begin
       // writeln('player playing');

         if not bPnlPlaytimeNegated then
           pnlPlaytime.Caption:= PlayerObj.get_timestr
         else
           pnlPlaytime.Caption:= PlayerObj.Get_TimeRemainingStr;
        playwin.TimeImg.Picture.LoadFromFile(SkinData.Time.Img);
        playwin.TimeImg.Canvas.Font.Color := ClNavy;
        playwin.TimeImg.Canvas.TextOut(5,3, pnlPlaytime.Caption);

        tmppos:= PlayerObj.Get_FilePosition;
        trackbar.position:= tmppos;
       // writeln(tmppos);
        x2 := (trackbar.position*2)-3;
        If x2<3 Then x2 := 3;
        If (tmppos=100) then begin
            // writeln('nexttrack');
            // WriteLn(PlayerObj.CurrentTrack);
             if (PlayerObj.CurrentTrack<PlayerObj.Playlist.ItemCount) Then nextclick(Nil)
                    else stopClick(nil);
           end;
        If CactusConfig.CoverDownload and (CoverFound=false) And (LoopCount<20) Then
          Begin
            inc(LoopCount);
            If (assigned(LastFMAPI)) And (LastFMAPI.data_ready) Then
              Begin
                fileobj := TMediaFileClass(playlist.Items[PlayerObj.CurrentTrack].Data);
                If FileExists(fileobj.CoverPath) Then
                  Begin
                    Try
                      CoverImage.Picture.LoadFromFile(fileobj.CoverPath);
                      playwin.AlbumCoverImg.Picture.LoadFromFile(fileobj.CoverPath);
                    Except
                      DebugOutLn('EXCEPTION', 3);
                    End;
                  End;
                CoverFound := true;
                FreeAndNil(LastFMAPI);
              End;
          End
        Else If (LoopCount>=20) And (CoverFound=false) Then CoverImage.Picture.Clear;
      End
    Else {playtimer.Enabled:=false};
  Except
    DebugOutLn('CAUGHT EXCEPTION IN PLAYTIMER!!!!', 3);
  End;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.removeselectClick(Sender: TObject);

Var curartist, curalbum: string;
  album_mode: boolean;
  MediaFileObj: TMediaFileClass;
  MediaColObj: TMediaCollectionClass;
  z: integer;
  tsnode: TTreeNode;
Begin
  tsnode := Main.ArtistTree.Selected;
  If (tsnode<>Nil) And (tsnode.Level>0) Then
    Begin
      If MessageDlg('The selected file(s) will permanently be'+#10+#13+'removed from harddisk!'+#10+
         #13+' Proceed?', mtWarning, mbOKCancel, 0)=mrOK Then
        Begin
          If tsnode.level<2 Then album_mode := false
          Else album_mode := true;
          MediaFileObj := TMediaFileClass(tsnode.data);
          MediaColObj := MediaFileObj.Collection;
          curartist := lowercase(MediaFileObj.Artist);
          curalbum := lowercase(MediaFileObj.album);

          z := MediaColObj.getTracks(curartist, MediaFileObj.index);
          Repeat
            Begin
              If (album_mode=false) Or
                 ((album_mode=true) And (lowercase(MediaColObj.items[z].album)=curalbum)) Then
                Begin
                  If DeleteFile(MediaColObj.items[z].path) Then
                  begin
                    DebugOutLn('deleted file from disk: '+MediaColObj.items[z].path, 2);
                    MediaColObj.remove(z);
                  end
                  else
                    DebugOutLn('ERROR deleting file: '+MediaColObj.items[z].path, 2);
                End;
              z := MediaColObj.getNext;
            End;
          Until (z=-1);
          
          update_artist_view;
          update_title_view;
          MediaColObj.SaveToFile;
        End;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.save_listClick(Sender: TObject);
Begin
  SaveDialog1.Title := 'Save Playlist...';
  saveDialog1.Filter := 'M3U Playlist|*.m3u';
  saveDialog1.DefaultExt := 'm3u';
  saveDialog1.FilterIndex := 1;
  SaveDialog1.InitialDir := CactusConfig.HomeDir;
  If Savedialog1.execute=true Then
    Begin
      If FileExists(SaveDialog1.FileName) Then
        If MessageDlg('File '+SaveDialog1.FileName+' alreday exists'+sLineBreak+sLineBreak+
           'Overwrite?', mtWarning, mbOKCancel, 0)=mrCancel Then exit;
      PlayerObj.playlist.SaveToFile(Savedialog1.Filename);
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.savelibClick(Sender: TObject);
Begin
  SaveDialog1.Title := 'Save Library...';
  saveDialog1.Filter := 'Cactus Media Library|*.cml';
  saveDialog1.DefaultExt := 'cml';
  saveDialog1.FilterIndex := 1;
  SaveDialog1.InitialDir := CactusConfig.HomeDir;
  If Savedialog1.execute=true Then MediaCollection.SaveToFile(Savedialog1.Filename);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.scanplayeritemClick(Sender: TObject);

Var tmps: string;
  ScanCol: TMediaCollectionClass;
Begin
  If FileExists(CactusConfig.DAPPath)=false Then
    Begin
      ShowMessage(rsNotConnected);
      exit;
    End;

  If FileExists(CactusConfig.DAPPath) Then
    Begin
      checkmobile.Enabled := false;
      writeln('ooo');
      disconnectDAP;
      writeln('aa');
      ScanCol := TMediaCollectionClass.create;
      ScanCol.syncronize := @ScanSyncronize;
      Enabled := false;
      writeln('ll');
      ScanCol.PathFmt := FRelative;
      ScanCol.savepath := CactusConfig.DAPPath+'cactuslib';
      writeln('dd');
      ScanCol.add_directory(CactusConfig.DAPPath);
      ScanCol.SaveToFile;
      ScanCol.Free;
      Enabled := true;
      connectDAP;
      checkmobile.Enabled := true;
      tmps := ByteToFmtString(FreeSpaceOnDAP, 3, 2);
      StatusBar1.Panels[1].Text := 'Device connected     '+tmps+' Free';
    End
  Else DebugOutLn(CactusConfig.DAPPath+' does not exist', 2);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.searchstrKeyUp(Sender: TObject; Var Key: Word;
                               Shift: TShiftState);
Begin
  If length(searchstr.Text)>1 Then srch_buttonClick(Nil)
  Else TitleTree.Clear;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.srch_buttonClick(Sender: TObject);

Var searchstring, ft: string;
  found: boolean;
  Listitem: TListitem;
  i: integer;
Begin
  TitleTree.Items.Clear;
  TitleTree.BeginUpdate;
  artisttree.selected := Nil;
  searchstring := lowercase(searchstr.text);
  found := false;
  For i:= 0 To MediaCollection.ItemCount-1 Do
    Begin
      If SrchTitleItem.checked Then If  pos(searchstring,lowercase(MediaCollection.items[i].title))<>0 Then found := true;
      If SrchArtItem.checked Then If  pos(searchstring,lowercase(MediaCollection.items[i].Artist))<>0 Then found := true;
      If SrchAlbumItem.checked Then If  pos(searchstring,lowercase(MediaCollection.items[i].album))<>0 Then found := true;
      If SrchFileItem.checked Then If  pos(searchstring,lowercase(extractfilename(MediaCollection.items[i].path)))<>0 Then found := true;
      If found Then
        Begin
          found := false;
          ft := '';
          Case filetypebox.ItemIndex Of 
            0: ft := 'all';
            1: ft := '.flac';
            2: ft := '.mp3';
            3: ft := '.ogg';
            4: ft := '.wav';
          End;
          If (ft='all') Or (ft=MediaCollection.items[i].filetype) Then
            Begin

              ListItem := Main.Titletree.Items.Add;

              listitem.data := MediaCollection.items[i];
              Listitem.ImageIndex := MediaCollection.items[i].action;
              Listitem.caption := '';

              If MediaCollection.items[i].title<>'' Then ListItem.SubItems.Add(MediaCollection.items
                                                                               [i].Artist)
              Else ListItem.SubItems.Add(extractfilename(MediaCollection.items[i].path));
              ListItem.SubItems.Add (MediaCollection.items[i].title);
              ListItem.SubItems.Add (MediaCollection.items[i].album);
              ListItem.SubItems.Add (MediaCollection.items[i].Track);
              ListItem.SubItems.Add (MediaCollection.items[i].playtime);
            End;
        End;
    End;
  TitleTree.EndUpdate;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.EditID3itemClick(Sender: TObject);
Var tsitem: TListitem;
Begin
  tsitem := TitleTree.Selected;
  if tsitem<>nil then begin
     editid3win.display_window(TMediaFileClass(tsitem.data));
     EditID3win.ShowModal;
   end;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.ArtistTreeSelectionChanged(Sender: TObject);
Begin
  If Not NetworkMode Then update_title_view;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.ArtistTreeStartDrag(Sender: TObject; Var DragObject: TDragObject);
Begin
  artist_drag := true;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.Button1Click(Sender: TObject);
Begin
  TitleTree.Clear;
End;


procedure TMain.checkmobileStartTimer(Sender: TObject);
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.CoverImageMouseUp(Sender: TObject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
var medFileObj: TMediaFileClass;
Begin
  //TODO: check why large cover image is linux only?
  If PlayerObj.playing and (PlayerObj.PlaybackMode=FILE_MODE) and (PlayerObj.CurrentTrack>=0) Then
    Begin

      MedFileObj := TMediaFileClass(playlist.Items[PlayerObj.CurrentTrack].Data);
      If (MedFileObj.CoverPath<>'') and FileExists(MedFileObj.CoverPath) then begin

            BigCoverImgForm := TBigCoverImg.Create(self);
            BigCoverImgForm.Caption := PlayerObj.Playlist.Items[PlayerObj.CurrentTrack].Album;

            BigCoverImgForm.Image1.Picture.LoadFromFile(MedFileObj.CoverPath);
            BigCoverImgForm.Width := BigCoverImgForm.Image1.Picture.Width+32;
            BigCoverImgForm.Height := BigCoverImgForm.Image1.Picture.Height+32;

            BigCoverImgForm.Image1.AutoSize := true;
            BigCoverImgForm.BackImg.Width := BigCoverImgForm.Image1.Picture.Width+32;
            BigCoverImgForm.BackImg.Height := BigCoverImgForm.Image1.Picture.Height+32;

            BigCoverImgForm.BackImg.Canvas.FillRect(0,0, BigCoverImgForm.Width, BigCoverImgForm.Height);

            BigCoverImgForm.BackImg.Canvas.Rectangle(8, 8, BigCoverImgForm.Width-8, BigCoverImgForm.Height-8);

            BigCoverImgForm.Image1.Top := 16;
            BigCoverImgForm.Image1.Left := 16;




            BigCoverImgForm.Left := x+Panel1.Left+self.Left;
            BigCoverImgForm.Top := y+Panel1.height+self.top- 220;


            {$ifdef win32 or win64}
                        BigCoverImgForm.Position:= poScreenCenter;
            {$endif}
            BigCoverImgForm.BorderStyle := bsDialog;
            BigCoverImgForm.ShowModal;

        end;
    End;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.DeviceModeBtnClick(Sender: TObject);
Begin
  DeviceModeBtn.Down := true;
  If Not DeviceMode Then
    Begin
      ArtistTree.Selected := Nil;
      LibModeBtn.Down := false;
      NetModeBtn.Down := false;
      LibraryMode := false;
      DeviceMode := true;
      NetworkMode := false;
      Playlist.Enabled := true;
      TitleTree.Enabled := true;
      trackbar.Enabled := true;
      update_artist_view;
    End;
End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.FormMouseDown(Sender: TOBject; Button: TMouseButton;
                              Shift: TShiftState; X, Y: Integer);
Begin
  ArtistSrchField.hide;
  //unguenstig, wird bei jedem klick aufgerufen... :(
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.FormResize(Sender: TObject);
Begin
  Panel4.Width := oldSplitterWidth;
  Panel1.Width := Width-oldSplitterWidth-8;
End;


procedure TMain.WMSize(var Message: TLMSize);
begin
  if not (csDesigning in ComponentState) then
  begin
       case (Message.SizeType and not SIZE_SourceIsInterface) of
       SIZEICONIC:begin
                       if not(fromTrayDBLClick) then
                       begin
                            Visible :=False;
                            oldWindowState :=Self.WindowState;
                            exit;
                       end;
                       fromTrayDBLClick :=False;
                  end;
       end;
  end;
  inherited WMSize(Message);
end;

procedure TMain.TrayIconDblClick(Sender: TObject);
begin
     if not(Self.Visible) then
     begin
          //Avoid handling on OnWindowStateChange
          Self.fromTrayDBLClick :=True;
//          Self.WindowState:=oldWindowState;
          Self.Visible:=True;
          Self.BringToFront;
     end;
end;

procedure TMain.itemTrayExitClick(Sender: TObject);
begin
     Close;
end;

procedure TMain.itemTrayPlayClick(Sender: TObject);
begin
     if (PlayerObj.playing)
     then pauseClick(nil)
     else playClick(nil);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.LibModeBtnClick(Sender: TObject);
Begin
  LibModeBtn.Down := true;
  If Not LibraryMode Then
    Begin
      ArtistTree.Selected := Nil;
      DeviceModeBtn.Down := false;
      NetModeBtn.Down := false;
      LibraryMode := true;
      DeviceMode := false;
      NetworkMode := false;
      Playlist.Enabled := true;
      TitleTree.Enabled := true;
      trackbar.Enabled := true;
      update_artist_view;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem15Click(Sender: TObject);

Var i: integer;
Begin
  i := playlist.Items.Count;
  artist_to_playlist;
  Playlist.Selected := Nil;
  If playlist.Items.Count>0 Then playlist.Items[i].Selected := true;
  playClick(Nil);

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem25Click(Sender: TObject);
Begin
  addRadioForm := TaddRadioForm.Create(self);
  addRadioForm.ShowModal;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIViewAlbumClick(Sender: TObject);
begin
  MIViewAlbum.Checked := not MIViewAlbum.Checked;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIViewArtistClick(Sender: TObject);
begin
  MIViewArtist.Checked := not MIViewArtist.Checked;
  CactusConfig.TLShowArtist:= MIViewArtist.Checked;
  TitleTree.Column[1].Visible := CactusConfig.TLShowArtist;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem32Click(Sender: TObject);
Begin
  If (ArtistTree.Selected<>Nil) And (ArtistTree.Selected.Level>0) Then
    Begin
      editid3win.display_window(TStreamInfoItemClass(ArtistTree.Selected.Data));
      EditID3win.ShowModal;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem6Click(Sender: TObject);

Var MedFileObj: TMediaFileClass;
  MedColObj: TMediaCollectionClass;
  i: integer;
Begin
  If TitleTree.Selected<>Nil Then
    If MessageDlg('The selected file(s) will permanently be'+#10+#13+'removed from harddisk!'+#10+#13+' Proceed?', mtWarning, mbOKCancel, 0)=mrOK Then
      Begin
        MedFileObj := TMediaFileClass(TitleTree.Selected.Data);
        MedColObj := MedFileObj.collection;
        i := MedFileObj.index;

        If DeleteFile(MedFileObj.path) Then
          Begin
            DebugOutLn('deleted file from disk: '+MedFileObj.path, 2);
            MedColObj.remove(i);
          End
        Else
            If FileGetAttr(MedFileObj.path)=faReadOnly Then
              ShowMessage('File is read only!');

        update_artist_view;
        update_title_view;
        MedColObj.SaveToFile;
      End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem7Click(Sender: TObject);
Begin
  title_to_playlist;
  Playlist.Items[Playlist.Items.Count-1].Focused := true;
    {$ifdef win32}
  Playlist.Items[Playlist.Items.Count-1].Selected := true;{$endif}
  playClick(Nil);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem9Click(Sender: TObject);
Begin
  title_to_playlist_at(PlayerObj.CurrentTrack+1);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIDeviceInfoClick(Sender: TObject);

Var z: int64;
  s, tmps, used: string;
  i: integer;
Begin
  If player_connected Then
    Begin
      z := 0;
      For i:= 0 To PlayerCol.ItemCount-1 Do
        z := z+PlayerCol.items[i].size;

      used := ByteToFmtString(z, 4, 2);

      tmps := ByteToFmtString(FreeSpaceOnDAP, 4 , 2);
      str(PlayerCol.ItemCount-1, s);

      ShowMessage(s+' Files on mobile player    '+#10+used+' of music'+#10+'Free Disk Space: '+tmps)
      ;
    End
  Else ShowMessage(rsNotConnected);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MIremoveRadioClick(Sender: TObject);

Var index: integer;
Begin
  If (ArtistTree.Selected<>Nil) And (ArtistTree.Selected.Level>0) Then
    Begin
      index := StreamCollection.IndexOfObject(TStreamInfoItemClass(ArtistTree.Selected.Data));
      StreamCollection.Delete(index);
      ArtistTree.Selected := Nil;
    End;
  update_artist_view;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIRipAudioClick(Sender: TObject);
begin
  cdripwin := Tcdrip.Create(Application);
  Enabled := false;
  cdripwin.ShowModal;
  cdripwin.Free;
  Enabled := true;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIViewFilenameClick(Sender: TObject);
begin
  MIViewFilename.Checked := not MIViewFilename.Checked;
  CactusConfig.TLShowFilename:= MIViewFilename.Checked;
  TitleTree.Column[6].Visible := CactusConfig.TLShowFilename;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIViewGenreClick(Sender: TObject);
begin
  MIViewGenre.Checked := not MIViewGenre.Checked;
  CactusConfig.TLShowGenre:=MIViewGenre.Checked;
  TitleTree.Column[5].Visible := CactusConfig.TLShowGenre;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIViewTitleClick(Sender: TObject);
begin
  MIViewTitle.Checked := not MIViewTitle.Checked;
  CactusConfig.TLShowTitle:= MIViewTitle.Checked;
  TitleTree.Column[2].Visible := CactusConfig.TLShowTitle;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.MIViewTrackClick(Sender: TObject);
begin
  MIViewTrack.Checked := not MIViewTrack.Checked;
  CactusConfig.TLShowTrack:= MIViewTrack.Checked;
  TitleTree.Column[4].Visible := CactusConfig.TLShowTrack;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.mnuCleanLibClick(Sender: TObject);
begin
  FrmCleanLibrary := TFrmCleanLibrary.Create(Application);
  Enabled := false;
  FrmCleanLibrary.ShowModal;
  FrmCleanLibrary.Free;
  update_artist_view;
  update_title_view;
  Enabled := true;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.NetModeBtnClick(Sender: TObject);
Begin
  NetModeBtn.Down := true;
  If Not NetworkMode Then
    Begin
      ArtistTree.Selected := Nil;
      DeviceModeBtn.down := false;
      LibModeBtn.down := false;
      LibraryMode := false;
      DeviceMode := false;
      NetworkMode := true;
      Playlist.Enabled := false;
      TitleTree.Enabled := false;
      trackbar.Enabled := false;
      update_artist_view;
      update_title_view;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.NextButtonImgClick(Sender: TObject);
Begin
  nextClick(Nil);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.NextButtonImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                       Shift: TShiftState; X, Y: Integer);
Begin
  NextButtonImg.Picture.LoadFromFile(SkinData.next.Clicked);

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.NextButtonImgMouseEnter(Sender: TObject);
Begin
  NextButtonImg.Picture.LoadFromFile(SkinData.next.MouseOver);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.NextButtonImgMouseLeave(Sender: TObject);
Begin
  NextButtonImg.Picture.LoadFromFile(SkinData.next.Img);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.NextButtonImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                     Shift: TShiftState; X, Y: Integer);
Begin
  NextButtonImg.Picture.LoadFromFile(SkinData.next.MouseOver);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PlaylistCustomDrawItem(Sender: TCustomListView;
                                       Item: TListItem; State: TCustomDrawState; Var DefaultDraw:
                                       Boolean);
Begin
  // not working because font colors not implemented in Lazarus 0.9.23

  If (PlayerObj.Playlist.Items[Item.Index].Played) And (PlayerObj.CurrentTrack<>Item
     .Index) Then
    Sender.Canvas.Font.Color := clGrayText
  Else
    Sender.Canvas.Font.Color := clWindowText;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.pnlPlaytimeClick(Sender: TObject);
begin
  bPnlPlaytimeNegated := not bPnlPlaytimeNegated;
end;

procedure TMain.PopupMenuTrayPopup(Sender: TObject);
begin
     if (PlayerObj.playing) and (not(PlayerObj.paused))
     then begin
               itemTrayPlay.Caption:='Pause';
               itemTrayPlay.ImageIndex:=2;
          end
     else begin
               itemTrayPlay.Caption:='Play';
               itemTrayPlay.ImageIndex:=1;
          end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.randomcheckChange(Sender: TObject);
begin
  PlayerObj.Playlist.reset_random;
end;

procedure TMain.scan(Sender: TObject);
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.SearchPanelClick(Sender: TObject);
Begin
  ArtistSrchField.Hide;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PlayerControlsPanelClick(Sender: TObject);
Begin
  ArtistSrchField.hide;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PauseButtonImgClick(Sender: TObject);
Begin
  pauseClick(Nil);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PauseButtonImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                        Shift: TShiftState; X, Y: Integer);
Begin
  pauseButtonImg.Picture.LoadFromFile(SkinData.pause.Clicked);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PauseButtonImgMouseEnter(Sender: TObject);
Begin
  pauseButtonImg.Picture.LoadFromFile(SkinData.pause.MouseOver);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PauseButtonImgMouseLeave(Sender: TObject);
Begin
  pauseButtonImg.Picture.LoadFromFile(SkinData.pause.Img);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PauseButtonImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                      Shift: TShiftState; X, Y: Integer);
Begin
  pauseButtonImg.Picture.LoadFromFile(SkinData.pause.MouseOver);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PlayButtonImgClick(Sender: TObject);
Begin
  playclick(Nil);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PlayButtonImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                       Shift: TShiftState; X, Y: Integer);
Begin
  PlayButtonImg.Picture.LoadFromFile(SkinData.play.Clicked);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PlayButtonImgMouseEnter(Sender: TObject);
Begin
  PlayButtonImg.Picture.LoadFromFile(SkinData.play.MouseOver);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PlayButtonImgMouseLeave(Sender: TObject);
Begin
  PlayButtonImg.Picture.LoadFromFile(SkinData.play.Img);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PlayButtonImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                     Shift: TShiftState; X, Y: Integer);
Begin
  PlayButtonImg.Picture.LoadFromFile(SkinData.play.MouseOver);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


Procedure TMain.MainClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  PlayerObj.stop;
//  if CactusConfig.PluginsEnabled then CactusPlugins.SendEvent(evnStopPlay, 'ps');
  CactusConfig.WHeight := Height;
  CactusConfig.WWidth := Width;
  CactusConfig.WSplitterWidth := Splitter1.Left;
  CactusConfig.bDisplayPlayTimeNegated:= bPnlPlaytimeNegated;

  if CactusConfig.LoadLastPlaylist and (PlayerObj.Playlist.Count>0) then begin
       if PlayerObj.Playlist.SaveToFile(CactusConfig.ConfigPrefix+'lib'+DirectorySeparator+'last.m3u')<>0 then
             DebugOutLn('ERROR saving playlist', 2);
     end else if PlayerObj.Playlist.Count=0 then DeleteFile(CactusConfig.ConfigPrefix+'lib'+DirectorySeparator+'last.m3u');

  If (MediaCollection.ItemCount>1) Then
    Begin
      MediaCollection.SaveToFile(CactusConfig.ConfigPrefix+'lib'+DirectorySeparator+'last.mlb');
      CactusConfig.LastLib := MediaCollection.savepath;
    End;
  If StreamCollection.Count>0 Then
    Begin
      StreamCollection.SaveToFile(CactusConfig.ConfigPrefix+'lib'+DirectorySeparator+'streams.col');
      CactusConfig.StreamColPath := CactusConfig.ConfigPrefix+'lib'+DirectorySeparator+'streams.col'
      ;
    End
  Else
    Begin
      If Not DeleteFile(CactusConfig.ConfigPrefix+'lib'+DirectorySeparator+'streams.col') Then
        DebugOutLn('Cannot delete streamcolelction savefile: '+CactusConfig.ConfigPrefix+'lib'+
                   DirectorySeparator+'streams.col', 1);
    End;
  MediaCollection.Free;
  PlayerCol.free;
  checkmobile.Enabled := false;
  playtimer.Enabled := false;


  PlayerObj.free;
  CoverImage.Free;


  If playermode=false Then
    Begin
      playwin.close;
      // playwin.Free;
    End;
  Try
    SimpleIPCServer1.StopServer;
    SimpleIPCServer1.free;
  Except
    DebugOutLn('ERROR: Exception while shutting down IPC server', 2);
  End;
  writeln('end.');
  //CactusPlugins.FlushPluginConfig;
  //CactusPlugins.Free;
  If CactusConfig.FlushConfig Then DebugOutLn('Config succesfully written to disk', 3)
  Else DebugOutLn('ERROR: writing config to disk', 3);
  CactusConfig.Free;
  Application.Terminate;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
     CanClose :=PluginsList.CanClose;
     if CanClose then
     begin
          PluginsList.Free;
          CJ_Interface.Free;
     end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MainCreate(Sender: TObject);

Var tmps1: string;
    MPlayerExeDialog: TSelectDirectoryDialog;
    id: longint;
    listitem: TListItem;
Begin
  DebugOutLn('## Main.onCreate ##', 3);
  Caption := 'Cactus Jukebox '+CACTUS_VERSION;

  LibraryMode := true;
  DeviceMode := false;
  NetworkMode := false;
  LibModeBtn.Down := true;
  DeviceModeBtn.Down := false;
  NetModeBtn.Down := false;

  MediaCollection.syncronize := @ScanSyncronize;

  Width := CactusConfig.WWidth;
  Height := CactusConfig.WHeight;
  DebugOutLn('loading main form translations...', 5);
  TranslateUnitResourceStrings('mainform', CactusConfig.DataPrefix+'languages'+DirectorySeparator+
                               'cactus.%s.po', CactusConfig.language, copy(CactusConfig.language, 0,
                               2));
  If SystemCharSetIsUTF8 Then DebugOutLn('##System charset is UTF8', 3);


  // Load resourcestrings to Captions
  QuitItem.Caption := rsQuit;
  FileItem.Caption := rsFile;
  openfile.Caption :=  rsOpenFile;
  opendir.Caption := rsOpenDirector;
  player_lib.Caption := rsPlayerOnly;
  skinmenu.Caption := rsChooseSkin;
  SettingsItem.Caption := rsSettings;

  MIlibrary.Caption := rsLibrary;
  MInewlib.Caption := rsNewLibrary;
  MIloadlib.Caption :=  rsLoadLibrary;
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

  MIDevices.Caption:=rsDevices;

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
  TitleTree.Column[7].Visible := true;

  MIViewFilename.Checked:=CactusConfig.TLShowFilename;
  MIViewGenre.Checked:=CactusConfig.TLShowGenre;
  MIViewTrack.Checked:=CactusConfig.TLShowTrack;
  MIViewAlbum.Checked:=CactusConfig.TLShowAlbum;
  MIViewTitle.Checked:=CactusConfig.TLShowTitle;
  MIViewArtist.Checked:=CactusConfig.TLShowArtist;


  oldSplitterWidth := CactusConfig.WSplitterWidth;
  bPnlPlaytimeNegated := CactusConfig.bDisplayPlayTimeNegated;
  SplitterResize := true;
  SrchTitleItem.checked := true;
  SrchArtItem.checked := true;
{$ifdef CPU86}
  if CactusConfig.AudioBackend=MPLAYERBACK then begin
       PlayerObj:=TMPlayerClass.create;
       DebugOutLn('MPlayer audio backend loaded', 2);
      end
      else begin
      {$ifndef fmod}
        DebugOutLn('WARNING: Cactus Jukebox has been compiled without fmod support. Trying to load mplayer backend instead', 0);
        PlayerObj:=TMPlayerClass.create;
        DebugOutLn('MPlayer audio backend loaded', 2);
      {$endif}
      {$ifdef fmod}
        PlayerObj:=TFModPlayerClass.create;
        DebugOutLn('FMOD audio backend loaded', 2);
      {$endif}
    end;
{$endif}

{$ifdef CPUX86_64}   // Fmod library is only available on 32bit systems. Always try to load mplayer instead
  if CactusConfig.AudioBackend=MPLAYERBACK then begin
       PlayerObj:=TMPlayerClass.create;
       DebugOutLn('MPlayer audio backend loaded', 2);
      end
      else begin
        PlayerObj:=TMPlayerClass.create;
        DebugOutLn('WARNING: Fmod backend not available on 64bit systems. Trying to load mplayer backend instead', 0);
    end;
{$endif}
  if (PlayerObj is TMPlayerClass) then begin
         (PlayerObj as TMPlayerClass).UseExternalConfig:=CactusConfig.MPlayerUseExternalConfig;
         if FileExists(IncludeTrailingPathDelimiter(CactusConfig.DataPrefix)+'mplayer.cfg') then
            (PlayerObj as TMPlayerClass).ExternalConfigFile:=IncludeTrailingPathDelimiter(CactusConfig.DataPrefix)+'mplayer.cfg';
         if FileExists(IncludeTrailingPathDelimiter(CactusConfig.ConfigPrefix)+'mplayer.cfg') then
            (PlayerObj as TMPlayerClass).ExternalConfigFile:=IncludeTrailingPathDelimiter(CactusConfig.ConfigPrefix)+'mplayer.cfg';
  end;

  if (PlayerObj is TMPlayerClass) and ((PlayerObj as TMPlayerClass).MPlayerPath='') then begin
       if CactusConfig.MPlayerPath='' then begin
           ShowMessage('MPlayer executable not found! Please select MPlayer directory...');
           MPlayerExeDialog:=TSelectDirectoryDialog.Create(self);
           MPlayerExeDialog.Title:='Locate mplayer executable...';
           if MPlayerExeDialog.Execute then begin
                CactusConfig.MPlayerPath:=MPlayerExeDialog.FileName;
             end;
          end;
       if (PlayerObj as TMPlayerClass).setMplayerBinaryDir(CactusConfig.MPlayerPath)=false then begin
           ShowMessage('MPlayer executable not found in '+LineEnding+MPlayerExeDialog.FileName);
           halt;
          end;
    end;

  PlayerObj.OutputMode:=CactusConfig.AudioSystem;

  player_connected := false;
  {$ifdef linux}
  Try
    DebugOut('loading program icon...  ', 2);
    Icon.LoadFromFile(CactusConfig.DataPrefix+'icon'+DirectorySeparator+'cactus-icon.ico');

 //  CoverImage.Picture.LoadFromFile(DataPrefix+'tools'+DirectorySeparator+'cactus-logo-small.png');
    DebugOutLn('... loaded', 2);
  Except
    DebugOutLn('ERROR loading bitmaps, files not found', 2);
  End;
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
  Main.Titletree.Columns[0].width := 20;
{$endif LCLGtk2}


  SimpleIPCServer1.ServerID := 'cactusjukeboxipc';

{$ifdef unix}
  Application.OnIdle := @ApplicationIdle;
{$endif}
  SimpleIPCServer1.Global := true;

  SimpleIPCServer1.StartServer;

  checkmobile.Enabled := true;

  // unused ??
  main.tempbitmap := TBitmap.Create;
  main.timetmpbmp := TBitmap.Create;
  main.tempbitmap.width := 300;
  main.tempbitmap.Height := 150;
  // ------

  If FileExists(CactusConfig.LastLib) Then
    Begin
      //main.StatusBar1.Panels[0].Text:='Loading last library...';
      If Mediacollection.LoadFromFile(CactusConfig.LastLib)=false Then
        Begin
          //MediaCollection.clear;
          ShowMessage('ERROR while reading last library. You need to create a new one.'+LineEnding+
                      'Please choose a directory to scan for mediafiles...');
          newlibClick(Nil);
          TitleTree.Clear;
        End;
    End;
  If FileExists(CactusConfig.StreamColPath) Then
    Begin
      If StreamCollection.LoadFromFile(CactusConfig.StreamColPath)=false Then
        Begin
          writeln('Error loading stream collection');
        End;
    End;

  If CactusConfig.LoadLastPlaylist then begin
       if FileExists(CactusConfig.ConfigPrefix+'lib'+DirectorySeparator+'last.m3u') then
          Begin
            if PlayerObj.Playlist.LoadFromFile(CactusConfig.ConfigPrefix+'lib'+DirectorySeparator+'last.m3u')<>0 then
                DebugOutLn('ERROR loading last playlist',2);
            For id:= 0 To PlayerObj.Playlist.Count-1 Do
               Begin
                  ListItem := Playlist.Items.Add;
                  listitem.Data := TMediaFileClass.create(PlayerObj.Playlist.Items[id].path, Nil);
                  if (PlayerObj.Playlist.items[id].Artist<>'') or (PlayerObj.Playlist.items[id].Title<>'') then
                       ListItem.Caption := PlayerObj.Playlist.items[id].Artist+' - '+PlayerObj.Playlist.Items[id].Title
                     else
                       ListItem.Caption := ExtractFileName(PlayerObj.Playlist.items[id].Path);
               End;
            if Playlist.Items.Count>0 then Playlist.Selected:=Playlist.Items[0];
          end;
      end;

  If CactusConfig.AlbumCoverFirsttime Then
    Begin
      tmps1 := 'Cactus Jukebox can download album cover art from internet and show it on playback'
               +LineEnding+'(No private data is submitted. Only album title and artist)'
               +LineEnding+LineEnding+'Do you want to enable this feature?'
               +LineEnding+LineEnding+'You can change this behaviour later in File->Settings';

      If MessageDlg(tmps1, mtConfirmation , mbYesNo, 0)=mrYes Then CactusConfig.CoverDownload := 
                                                                                                true
      ;
    End;


  // Load file specified on commandline
  If CactusConfig.LoadOnStart<>'' Then
    Begin
      LoadFile(CactusConfig.LoadOnStart);
    End;
  DebugOutLn('main.create end', 5);
  update_artist_view;
  //update_title_view;
  DebugOutLn('main.create end', 5);

  Self.fromTrayDBLClick :=False;
  oldWindowState :=Self.WindowState;
  //Build of Interfaces and Plugins List (may be moved....)
  MenuOwner :=Self;
  CJ_Interface :=TCJ_Interface_Impl.Create;
  global_vars.AppMainMenu :=Self.Mainmenu1;
  global_vars.AppTrayIcon :=Self.TrayIcon;
  global_vars.ImageListNormal :=Self.ImageListNormal;
  PluginsList :=TCJ_PluginsList.Create;
  PluginsList.LoadFromINI;
  Self.TrayIcon.Hint:='Cactus Juke Box...';
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.ApplicationIdle(Sender: TObject; Var Done: Boolean);

Begin
{$ifdef linux}
  //linux doesn't recognize onIPCMessage Event. so we call it manually
  If SimpleIPCServer1.PeekMessage(1,true) Then
    Begin
      //PeekMessage automaticly calls OnMessage event
      DebugOutLn('IPC Messge received', 2);
    End;
{$endif}
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.update_player_display;

Var
  i: integer;
Begin
  If PlayerObj.playing Then
    Begin
      i := PlayerObj.CurrentTrack;
      //MedFileObj:=playlist.Items[player.CurrentTrack].Data;

      If PlayerObj.Playlist.items[i].Artist<>'' Then
               current_title_edit.text := PlayerObj.Playlist.items[i].Artist
            Else current_title_edit.text := ExtractFileName(PlayerObj.Playlist.items[i].path);
      current_title_edit1.text := PlayerObj.Playlist.items[i].title;

      playwin.TitleImg.Picture.LoadFromFile(SkinData.Title.Img);
      playwin.TitleImg.canvas.Font.Color := Clnavy;

      If PlayerObj.Playlist.items[i].Artist<>'' Then
               playwin.TitleImg.canvas.textout(5,5,PlayerObj.Playlist.items[i].Artist)
            Else playwin.TitleImg.canvas.textout(5,5,ExtractFileName(PlayerObj.Playlist.items[i].path));
      playwin.TitleImg.canvas.textout(5,25,PlayerObj.Playlist.items[i].title);
    End
  Else
    Begin
      //clear everything
      playwin.TitleImg.canvas.Clear;
      CoverImage.Picture.Clear;
      playwin.TimeImg.Canvas.Clear;
      current_title_edit.Text := '';
      current_title_edit1.Text := '';
      pnlPlaytime.Caption:= '00:00';
      trackbar.Position := 0;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMain.LoadFile(path: String): boolean;

Var z: integer;
  listitem: TListItem;
Begin
  DebugOutLn('** Loadfile **', 2);
  Application.ProcessMessages;
  If FileExists(path) Then
    Begin
      z := MediaCollection.GetIndexByPath(path);
      DebugOutLn(z, 3);
      If z<0 Then
        Begin
          z := MediaCollection.add(path);
        End;
      PlayerObj.playlist.add(MediaCollection.items[z]);
      ListItem := Playlist.Items.Add;
      listitem.data := MediaCollection.items[z];

      If MediaCollection.items[z].title<>'' Then ListItem.Caption := MediaCollection.items[z].Artist
                                                                     +' - '+MediaCollection.items[z]
                                                                     .title
      Else ListItem.Caption := extractfilename(MediaCollection.items[z].path);
      playlist.Column[0].Caption := 'Playlist                       ('+IntToStr(PlayerObj.
                                    playlist.ItemCount)+' Files/ '+PlayerObj.Playlist.
                                    TotalPlayTimeStr +')';
      result := true;
      update_artist_view;
      update_title_view;
    End
  Else result := false;
  Application.ProcessMessages;
End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.ScanSyncronize(dir: String);
Begin
  inc(ScanSyncCount);

  If ScanSyncCount>=500 Then
    Begin
      update_artist_view;
      ScanSyncCount := 0;
    End;

  if (ScanSyncCount mod 50)= 0 then begin
      StatusBar1.Panels[0].Text := 'scanning trough:  '+dir;
      Application.ProcessMessages;
   end;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.update_artist_view;
// TODO: rewrite method to reselect artist
Var curartist, curalbum: string;
  tsnode, artnode, TopNode: Ttreenode;
  AlbumList: TStringList;
  MedFileObj: TMediaFileClass;
  i, z: integer;
  restoreEnabled, album_selected: boolean;
Begin

  If Enabled Then restoreEnabled := true Else restoreEnabled := false;
  Enabled := false;
  StatusBar1.Panels[0].Text := 'Please wait... updating...';
  ArtistTree.OnSelectionChanged:=nil;  //Disable event while working on selection in artisttree!!
  DebugOutLn('', 2);
  DebugOut('## update artist view... ', 2);
  tsnode := ArtistTree.Selected;
  If (tsnode<>Nil) Then begin
    if tsnode.Level=1 then begin
         curartist := lowercase(tsnode.Text);
         curalbum := '';
         album_selected:=false;
       end;
    if tsnode.Level=2 then begin
         curartist := lowercase(tsnode.Parent.Text);
         curalbum := lowercase(tsnode.Text);
        // writeln(curalbum);
         album_selected:=true;
       end;
   end
  Else
    curartist := '';
  artisttree.beginupdate;
  DebugOut(' clear tree...', 2);
  ArtistTree.Items.Clear;

  If NetworkMode Then
    Begin

      TopNode := ArtistTree.Items.Add(Nil, 'Webradio stations');
      TopNode.Data := pointer(1);
      For i:=0 To StreamCollection.Count-1 Do
        Begin
          artnode := ArtistTree.Items.AddChild(TopNode, StreamCollection.Strings[i]);
          With artnode Do
            Begin
              MakeVisible;
              ImageIndex := -1;
              SelectedIndex := -1;
              Data := StreamCollection.Objects[i];
            End;
        End;
    End;
  // If library mode add Mediacollection
  If LibraryMode And (MediaCollection.Count>0) Then
    Begin
      TopNode := Main.ArtistTree.Items.Add(Nil, rsLibrary);
      TopNode.ImageIndex := 4;
      TopNode.SelectedIndex := 4;
      TopNode.data := pointer(1);

      i := MediaCollection.getArtists;
      Repeat
        Begin
          If MediaCollection.Items[i].Artist<>'' Then artnode := Main.ArtistTree.Items.AddChild(
                                                                 TopNode, MediaCollection.Items[i].
                                                                 Artist)
          Else artnode := Main.ArtistTree.Items.AddChild(TopNode, 'Unknown');
          With artnode Do
            Begin
              MakeVisible;
              ImageIndex := MediaCollection.Items[i].Action;
              SelectedIndex := MediaCollection.Items[i].Action;
              Data := MediaCollection.items[i];
              Expanded:=false;
            End;
          AlbumList := MediaCollection.getAlbums(MediaCollection.Items[i].Artist, i);
          For z:=0 To AlbumList.Count-1 Do
            Begin
              // add albums to node of current SrchArtItem
              With Main.ArtistTree.Items.Addchild(artnode, AlbumList[z]) Do
                Begin
                  MakeVisible;
                  MedFileObj:=TMediaFileClass(AlbumList.Objects[z]);
                  ImageIndex := MedFileObj.Action;
                  SelectedIndex := MedFileObj.Action;
                  Data := AlbumList.Objects[z];
                End;
            End;
          artnode.Expanded := false;
          i := MediaCollection.getNextArtist;
        End;
      Until i<0;
      // finally free AlbumList
      AlbumList.Free;
    End;

  // If Device mode add playercollection and other devices
  If DeviceMode And player_connected Then
    Begin

      TopNode := Main.ArtistTree.Items.Add(Nil, rsMobileDevice);
      TopNode.SelectedIndex := 1;
      TopNode.ImageIndex := 1;
      TopNode.data := pointer(0);

      i := PlayerCol.getArtists;
      Repeat
        Begin
          If PlayerCol.Items[i].Artist<>'' Then artnode := Main.ArtistTree.Items.AddChild(TopNode,
                                                           PlayerCol.Items[i].Artist)
          Else artnode := Main.ArtistTree.Items.AddChild(TopNode, 'Unknown');
          With artnode Do
            Begin
              MakeVisible;
              ImageIndex := PlayerCol.Items[i].Action;
              SelectedIndex := PlayerCol.Items[i].Action;
              Data := PlayerCol.items[i];
            End;
          AlbumList := PlayerCol.getAlbums(PlayerCol.Items[i].Artist, i);
          For z:=0 To AlbumList.Count-1 Do
            Begin
              // add albums to node of current SrchArtItem
              With Main.ArtistTree.Items.Addchild(artnode, AlbumList[z]) Do
                Begin
                  MakeVisible;
                  MedFileObj:=TMediaFileClass(AlbumList.Objects[z]);
                  ImageIndex := MedFileObj.Action;
                  SelectedIndex := MedFileObj.Action;
                  Data := AlbumList.Objects[z];
                End;
            End;
          artnode.Expanded := false;
          i := PlayerCol.getNextArtist;
        End;
      Until i<0;
      // finally free AlbumList
      AlbumList.Free;
    End;
  ArtistTree.EndUpdate;
  DebugOut(' reselecting last item ', 2);
  // Reselect last selected item if possible
  i := 0;
  If ArtistTree.Items.Count>0 Then ArtistTree.Selected := ArtistTree.Items[0];
  If ((curalbum<>'') or (curartist<>'')) And (ArtistTree.Items.Count>0) Then
    Begin
      Repeat  //try to keep old album
        Begin
          MedFileObj := TMediaFileClass(ArtistTree.items[i].data);
          inc(i);
        End;
      Until ((lowercase(artisttree.items[i].text)=curalbum) And (ArtistTree.Items[i].Level=2))
            Or (i>=artisttree.items.count-1);

      If lowercase(artisttree.items[i].text)=curalbum Then
        Begin
          artisttree.selected := main.artisttree.items[i];
        End

      Else if (curartist<>'') And (ArtistTree.Items.Count>0) then
      begin //Select artist if album not possible
      i:=0;
      Repeat
        Begin
          MedFileObj := TMediaFileClass(ArtistTree.items[i].data);
          inc(i);
        End;
      Until ((lowercase(artisttree.items[i].text)=curartist) And (ArtistTree.Items[i].Level=1))
            Or (i>=artisttree.items.count-1);
       writeln(curartist);
       writeln(artisttree.items[i].text);
      If lowercase(artisttree.items[i].text)=curartist Then
        Begin
          artisttree.selected := main.artisttree.items[i];
        End;
      End;

     if ArtistTree.Selected.AbsoluteIndex<ArtistTree.Items.Count-10 then
        begin
          ArtistTree.Items[ArtistTree.Selected.AbsoluteIndex+9].MakeVisible;
          if ArtistTree.Items[ArtistTree.Selected.AbsoluteIndex+9].Level>1 then
                ArtistTree.Items[ArtistTree.Selected.AbsoluteIndex+9].Parent.Expanded:=false;
        end
        else begin
          ArtistTree.Items[ArtistTree.Items.Count-1].MakeVisible;
          if ArtistTree.Items[ArtistTree.Items.Count-1].Level>1 then
                ArtistTree.Items[ArtistTree.Items.Count-1].Parent.Expanded:=false;
        end;
   End;
  ArtistTree.OnSelectionChanged:=@ArtistTreeSelectionChanged;  //Reenable event!!

  DebugOutLn(' finished artistview ##', 2);
  StatusBar1.Panels[0].Text := 'Ready.';
  Enabled := restoreEnabled;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem11Click(Sender: TObject);
Begin
  dirwin := Tdirwin.Create(Application);
  Enabled := false;
  dirwin.ShowModal;
  dirwin.Free;
  Enabled := true;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem14Click(Sender: TObject);

Var tsitem: TListItem;
  tmps : string;
  MedFileObj: TMediaFileClass;
  i: integer;
Begin
  tsitem := TitleTree.Selected;
  If (tsitem<>Nil) And player_connected Then
    Begin
      MedFileObj := TMediaFileClass(tsitem.data);
      For i:= 1 To MediaCollection.ItemCount-1 Do
        If MedFileObj.id=MediaCollection.items[i].id Then MediaCollection.items[i].action := AREMOVE
      ;

      For i:= 1 To PlayerCol.ItemCount-1 Do
        If MedFileObj.id=PlayerCol.items[i].id Then PlayerCol.items[i].action := AREMOVE;
      update_artist_view;
      update_title_view;

      tmps := ByteToFmtString(FreeSpaceOnDAP + sizediff, 3, 2);

      StatusBar1.Panels[1].Text := 'Device connected     '+tmps+' Free';
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem16Click(Sender: TObject);

Var tsitem: TListItem;
  tmps: string;
  MedFileObj: TMediaFileClass;
Begin
  tsitem := TitleTree.Selected;
  If (tsitem<>Nil) And player_connected Then
    Begin
      MedFileObj := TMediaFileClass(tsitem.data);
      MedFileObj.action := AUPLOAD;

      update_artist_view;
      update_title_view;

      tmps := ByteToFmtString(FreeSpaceOnDAP + sizediff, 3, 2);
      StatusBar1.Panels[1].Text := 'Device connected     '+tmps+' Free';
    End;
End;

procedure TMain.MenuItem19Click(Sender: TObject);
begin

end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem20Click(Sender: TObject);

Var tsitem: TListItem;
  MedFileObj: TMediaFileClass;
  tmps: string;
  i: integer;
Begin
  tsitem := TitleTree.Selected;
  If tsitem<>Nil Then
    Begin
      MedFileObj := TMediaFileClass(tsitem.data);
      If MedFileObj.action=AREMOVE Then
        Begin
          //PFobj^.action:=1;
          sizediff := sizediff-MedFileObj.size;
          For i:= 1 To MediaCollection.ItemCount-1 Do
            If MedFileObj.id=MediaCollection.items[i].id Then MediaCollection.items[i].action := 1;

          For i:= 1 To PlayerCol.ItemCount-1 Do
            If MedFileObj.id=PlayerCol.items[i].id Then PlayerCol.items[i].action := -1;
        End
      Else
        Begin
          MedFileObj.action := -1;
          sizediff := sizediff+MedFileObj.size;
        End;

      update_artist_view;
      update_title_view;

      tmps := ByteToFmtString(FreeSpaceOnDAP + sizediff, 3, 2);
      StatusBar1.Panels[1].Text := 'Device connected     '+tmps+' Free';
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//Random Paylist with 50 tracks by monta

procedure TMain.MIrandom_playlistClick(Sender: TObject);
  Var listitem: TListitem;
  MedFileObj: TMediaFileClass;
  i: integer;
Begin
  if MediaCollection.Count < 2 then exit;
  Randomize;
  i := 1;
  repeat
    MedFileObj := MediaCollection.Items[Random(MediaCollection.Count)];
    PlayerObj.playlist.add(MedFileObj);
    ListItem := Main.Playlist.Items.Add;
    listitem.data := MedFileObj;
    listitem.MakeVisible(false);
    If MedFileObj.title<>'' Then ListItem.Caption := MedFileObj.Artist+' - '+MedFileObj.title
    Else ListItem.Caption := extractfilename(MedFileObj.path);
    If Not PlayerObj.playing And CactusConfig.AutostartPlay And (main.Playlist.Items.Count
       =1) Then
      Begin
        main.Playlist.Selected := Main.Playlist.Items[0];
        DebugOutLn(Main.Playlist.Selected.Caption, 3);
        Main.playClick(main);
      End;
      inc(i);
  until (i > 50) or (i >= MediaCollection.count);
  main.playlist.Column[0].Caption := rsplaylist+'            ('+IntToStr(PlayerObj.playlist.
                                     ItemCount)+' Files/ '+PlayerObj.Playlist.
                                     TotalPlayTimeStr+' )';
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem26Click(Sender: TObject);
Begin

  ShowMessage('Cactus Jukebox'+LineEnding+'version'+CACTUS_VERSION+LineEnding+
              'written by Sebastian Kraft '+LineEnding+LineEnding+'(c) 2005-2009'+LineEnding+
              'http://cactus.hey-you-freaks.de     ');

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem27Click(Sender: TObject);

Var id: longint;
  listitem: TListitem;
  OpenDialog: TOpenDialog;
Begin
  OpenDialog := TOpenDialog.Create(self);
  OpenDialog.Filter := 'M3U Playlist|*.m3u|All Files|*.*';
  OpenDialog.InitialDir := CactusConfig.HomeDir;
  OpenDialog.FilterIndex := 1;
  If Opendialog.execute=true Then
    Begin
      playlist.Clear;
      PlayerObj.Playlist.clear;
      DebugOut('Loading playlist from -> '+Opendialog.Filename+' ... ', 2);
      if PlayerObj.Playlist.LoadFromFile(Opendialog.Filename)=0 then begin
         DebugOutLn('done', 2);
         DebugOut('Adding items... ', 4);
         For id:= 0 To PlayerObj.Playlist.Count-1 Do
             Begin
                  ListItem := Playlist.Items.Add;
                  listitem.ImageIndex:=-1;
                  listitem.Data := TMediaFileClass.create(PlayerObj.Playlist.Items[id].path, Nil);
                  if (PlayerObj.Playlist.items[id].Artist<>'') or (PlayerObj.Playlist.items[id].Title<>'') then
                     ListItem.Caption := PlayerObj.Playlist.items[id].Artist+' - '+PlayerObj.Playlist.Items[id].Title
                   else
                      ListItem.Caption := ExtractFileName(PlayerObj.Playlist.items[id].Path);
            End;
         DebugOutLn('done', 4);
         if Playlist.Items.Count>0 then Playlist.Selected:=Playlist.Items[0];
       end else ShowMessage('ERROR loading Playlist');
    End;
  OpenDialog.Free;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem2Click(Sender: TObject);

Var i: integer;
  MedFileObj: TMediaFileClass;
  Listitem: TListItem;

Begin

  If (TitleTree.Items.Count>0) Then
    Begin
      For i:= 0 To TitleTree.Items.Count-1 Do
        Begin
          MedFileObj := TMediaFileClass(TitleTree.Items[i].Data);
          PlayerObj.playlist.add(MedFileObj);

          ListItem := Playlist.Items.Add;
          listitem.data := MedFileObj;

          If MedFileObj.title<>'' Then
            ListItem.Caption := MedFileObj.Artist+' - '+MedFileObj.title
          Else
            ListItem.Caption := extractfilename(MedFileObj.path);
        End;
    End;
  playlist.Column[0].Caption := 'Playlist                       ('+IntToStr(PlayerObj.
                                playlist.ItemCount)+' Files/ '+PlayerObj.Playlist.
                                TotalPlayTimeStr +')';
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem37Click(Sender: TObject);

Var MedColObj: TMediaCollectionClass;
  curartist, curalbum, tmps: string;
  MedFileObj: TMediaFileClass;
  i: integer;
  tsnode: TTreeNode;
Begin
  tsnode := ArtistTree.Selected;

  If (tsnode<>Nil) And (tsnode.level>0) Then
    Begin
      MedFileObj := TMediaFileClass(tsnode.data);
      MedColObj := MedFileObj.collection;
      i := MedColObj.getTracks(MedFileObj.Artist, MedFileObj.index);
      If tsnode.level=2 Then
        Begin
          curartist := lowercase(MedFileObj.Artist);
          curalbum := lowercase(MedFileObj.album);
          Repeat
            Begin
              If (lowercase(MedColObj.items[i].album)=curalbum) And (MedColObj.items[i].action=
                 AREMOVE) Then
                Begin
                  MedColObj.items[i].action := AONPLAYER;
                  sizediff := sizediff-MedColObj.items[i].size;
                End;
              If (lowercase(MedColObj.items[i].album)=curalbum) And (MedColObj.items[i].action<>
                 AONPLAYER) Then
                Begin
                  MedColObj.items[i].action := ANOTHING;
                  sizediff := sizediff+MedColObj.items[i].size;
                End;
              i := MedColObj.GetNext;
            End;
          Until i<0;

        End;
      If tsnode.level=1 Then
        Begin
          curartist := lowercase(MedFileObj.Artist);
          Repeat
            Begin
              If (MedColObj.items[i].action=AREMOVE) Then
                Begin
                  MedColObj.items[i].action := AONPLAYER;
                  sizediff := sizediff-MedColObj.items[i].size;
                End;
              If (MedColObj.items[i].action<>AONPLAYER) Then
                Begin
                  MedColObj.items[i].action := ANOTHING;
                  sizediff := sizediff+MedColObj.items[i].size;
                End;
              i := MedColObj.GetNext;
            End;
          Until i<0;
        End;
      update_artist_view;
      update_title_view;

      tmps := ByteToFmtString(FreeSpaceOnDAP + sizediff, 3, 2);
      StatusBar1.Panels[1].Text := 'Device connected     '+tmps+' Free';
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//Open manual/homepage in browser
Procedure TMain.MenuItem43Click(Sender: TObject);
Begin
  {$ifdef linux}
  exec('/usr/bin/firefox', 'http://cactus.hey-you-freaks.de');
  If Dosexitcode<>0 Then exec('/usr/bin/mozilla-firefox',
                              'http://cactus.hey-you-freaks.de/index.php?page=manual');
  If Dosexitcode<>0 Then exec('/usr/bin/konqueror',
                              'http://cactus.hey-you-freaks.de/index.php?page=manual');
  If Dosexitcode<>0 Then Showmessage('The manual can be found at http://cactus.hey-you-freaks.de');
  {$endif}
  {$ifdef win32} //TODO: Open manual in Browser on win32
  Showmessage('The manual can be found at http://cactus.hey-you-freaks.de');
  {$endif}
End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.Panel1Click(Sender: TObject);
Begin
  ArtistSrchField.hide;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.Panel1Resize(Sender: TObject);
var i: integer;
Begin
  // Splitter1.Left:=oldSplitterWidth;
{$ifdef win32}  //TODO: check column autosize on win32

  Playlist.Columns[0].width := Playlist.Width;
  Titletree.Columns[5].width := 45;
  Titletree.Columns[4].width := 45;
  Titletree.Columns[3].width := 110;
  i:= TitleTree.Width-45-45-110-140-16-15;
  if i>0 then Titletree.Columns[2].width := i;
  Titletree.Columns[1].width := 140;
  Titletree.Columns[0].width := 16;
{$endif}
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.Panel4Click(Sender: TObject);
Begin
  ArtistSrchField.hide;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.ArtistSrchFieldClick(Sender: TObject);
Begin
  ArtistSrchField.hide;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PreviousButtonImgClick(Sender: TObject);
Begin
  prevClick(Nil);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PreviousButtonImgMouseDown(Sender: TOBject;
                                           Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  PreviousButtonImg.Picture.LoadFromFile(SkinData.previous.Clicked);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PreviousButtonImgMouseEnter(Sender: TObject);
Begin
  PreviousButtonImg.Picture.LoadFromFile(SkinData.previous.MouseOver);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PreviousButtonImgMouseLeave(Sender: TObject);
Begin
  PreviousButtonImg.Picture.LoadFromFile(SkinData.previous.Img);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.PreviousButtonImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                         Shift: TShiftState; X, Y: Integer);
Begin
  PreviousButtonImg.Picture.LoadFromFile(SkinData.previous.MouseOver);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.SettingsItemClick(Sender: TObject);
Begin
  Enabled := false;
  setupwin := Tsettings.create(Application);
  setupwin.ShowModal;
  setupwin.Free;
  Enabled := true;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.SimpleIPCServer1Message(Sender: TObject);

Var fpath: string;
  CommandCode: integer;
Begin
  DebugOutLn(' IPC Message received', 2);
  If length(SimpleIPCServer1.StringMessage)>2 Then
    Begin
      fpath := copy(SimpleIPCServer1.StringMessage, Pos(':', SimpleIPCServer1.StringMessage)+2,
               length(SimpleIPCServer1.StringMessage));
      CommandCode := StrToInt(Copy(SimpleIPCServer1.StringMessage,0 , Pos(':', SimpleIPCServer1.
                     StringMessage)-1 ));
    End
  Else
    Begin
      fpath := '';
      CommandCode := StrToInt(SimpleIPCServer1.StringMessage);
    End;
  Case CommandCode Of 
    VOLUME_UP: If volumebar.Position>4 Then  volumebar.Position := volumebar.Position-5;
    VOLUME_DOWN: If volumebar.Position<46 Then volumebar.Position := volumebar.Position+5;
    NEXT_TRACK: nextClick(self);
    STOP_PLAYING: stopClick(self);
    START_PLAYING: playClick(self);
    PREV_TRACK: prevClick(self);
    PAUSE_PLAYING: pauseClick(self);
    OPEN_FILE: If FileExists(fpath) Then
                 Begin
                   LoadFile(fpath);
                   Playlist.Selected := Playlist.Items[Playlist.Items.Count-1];
                   playClick(self);
                 End
               Else  DebugOutLn('--> Filename received from IPC does not exist', 2);
    ENQUEU_FILE: If FileExists(fpath) Then
                   Begin
                     LoadFile(fpath);
                   End
                 Else  DebugOutLn('--> Filename received from IPC does not exist', 2);
    Else DebugOutLn(' --> Invalid message/filename received via IPC', 2);
  End;
  Writeln('IPC end');
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.SpeedButton1Click(Sender: TObject);
Begin
  ArtistSrchField.hide;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.Splitter1Moved(Sender: TObject);
Begin
  oldSplitterWidth := Panel4.width;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.SrchAlbumItemClick(Sender: TObject);
Begin
  SrchAlbumItem.Checked := Not SrchAlbumItem.Checked;
End;

Procedure TMain.SrchArtItemClick(Sender: TObject);
Begin
  SrchArtItem.Checked := Not SrchArtItem.Checked;
End;

Procedure TMain.SrchFileItemClick(Sender: TObject);
Begin
  SrchFileItem.Checked := Not SrchFileItem.Checked;
End;

Procedure TMain.SrchTitleItemClick(Sender: TObject);
Begin
  SrchTitleItem.Checked := Not SrchTitleItem.Checked;
End;

Procedure TMain.srch_buttonKeyUp(Sender: TObject; Var Key: Word;
                                 Shift: TShiftState);
Begin

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.StopButtonImgClick(Sender: TObject);
Begin
  stopClick(Nil);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.StopButtonImgMouseDown(Sender: TOBject; Button: TMouseButton;
                                       Shift: TShiftState; X, Y: Integer);
Begin
  StopButtonImg.Picture.LoadFromFile(SkinData.stop.Clicked);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.StopButtonImgMouseEnter(Sender: TObject);
Begin
  StopButtonImg.Picture.LoadFromFile(SkinData.stop.MouseOver);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.StopButtonImgMouseLeave(Sender: TObject);
Begin
  StopButtonImg.Picture.LoadFromFile(SkinData.stop.Img);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.StopButtonImgMouseUp(Sender: TOBject; Button: TMouseButton;
                                     Shift: TShiftState; X, Y: Integer);
Begin
  StopButtonImg.Picture.LoadFromFile(SkinData.stop.MouseOver);
End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.TitleTreeClick(Sender: TObject);
Begin
  ArtistSrchField.Hide;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function NumericCompare(List: TStringList; Index1, Index2: Integer): Integer;

Var i1, i2: integer;
Begin
  Try
    i1 := StrToInt(List[Index2]);
    i2 := StrToInt(List[Index1]);
    Result := i2 - i1;
  Except
    result := 0;
  End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.TitleTreeColumnClick(Sender: TObject; Column: TListColumn);

Var sl: TstringList;
  //used for sorting
  counter, ListitemCount, SubItemsColumnCount, IndexOfCurrentColumn, i: integer;
Begin
  sl := TStringList.Create;
  Try
    IndexOfCurrentColumn := column.index;
    writeln(IndexOfCurrentColumn);

  If IndexOfCurrentColumn <> 0 Then
     Begin
        ListitemCount:=TitleTree.Items.Count;
        For counter := 0 To ListitemCount-1 Do
          Begin
            SubItemsColumnCount := titletree.items[counter].subitems.Count;
            sl.AddObject(titletree.items[counter].SubItems[IndexOfCurrentColumn-1], titletree.items[counter])
          End;
     //   for i:= 0 to sl.Count-1 do writeln(sl[i]);
        If IndexOfCurrentColumn<>4 Then sl.sort
            Else sl.CustomSort(@NumericCompare);
    //    TitleTree.BeginUpdate;
//        TitleTree.Clear;
//        TitleTree.Items.
        For counter := 0 To ListitemCount-1 Do
            Begin
               titletree.items[counter]:=(TListItem(sl.Objects[counter]));
     //          writeln(sl[counter]);
            End;
      //  TitleTree.EndUpdate;
     End;
  Finally
    sl.free;
  End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.TitleTreeDragOver(Sender, Source: TObject; X, Y: Integer;
                                  State: TDragState; Var Accept: Boolean);
Begin
{$ifdef LCLGtk}
  //Workaround for GTK1.x to reset selected Item while dragging
  If title_drag Then
    Begin
      TitleTree.Selected := Nil;
      TitleTree.Items[sourceitem.Index].Selected := true;
    End;
{$endif}
  Accept := false;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.TitleTreeEndDrag(Sender, Target: TObject; X, Y: Integer);
Begin
  title_drag := false;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.TitleTreeMouseDown(Sender: TOBject; Button: TMouseButton;
                                   Shift: TShiftState; X, Y: Integer);
Begin
  // ensure that the popup menu is only opened when an item is selected
  // the menu is reanabled in TMain.TitleTreeSelectItem

  {$ifdef  LCLQT} //TODO: QT interface doesn't set selected item
       TitleTree.Selected:= TitleTree.GetItemAt(x, y);
  {$endif}

  {$ifdef  LCLGtk2} //TODO: GTK2 interface doe snot selcte item on right click
        If (Button = mbRight) then TitleTree.Selected := TitleTree.GetItemAt(x, y);
  {$endif}

  //TODO check titlelist popupmenu on win32
  {$ifdef win32}
  If (Button = mbRight) And (TitleTree.Selected <> Nil) Then
    TitleTree.PopupMenu.PopUp(self.Left+Panel1.Left+TitleTree.left+X, self.top+Panel1.Top+TitleTree.
                              top+Y+50);
  {$else}
  If (Button = mbRight) And (TitleTree.Selected = Nil) Then
    TitleTree.PopupMenu.AutoPopup := false;
  {$endif}
  //Enable Dragging
  If Button = mbLeft Then
    Begin { only drag if left button pressed }
      sourceitem := Nil;
      sourceitem := TitleTree.GetItemAt(x, y);
      If sourceitem<>Nil Then TitleTree.BeginDrag(false, 10);
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.TitleTreeSelectItem(Sender: TObject; Item: TListItem;
                                    Selected: Boolean);
Begin
  // reanable the popupmenu in case ist was disabled in TMain.TitleTreeMouseDown
  TitleTree.PopupMenu.AutoPopup := true;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.TitleTreeStartDrag(Sender: TObject; Var DragObject: TDragObject
);
Begin
  title_drag := true;
  //sourceitem:=TitleTree.Selected;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.TrackInfoClick(Sender: TObject);
Begin
  If (PlayerObj.CurrentTrack)>=0 Then
    Begin
      playlist.selected := playlist.Items[PlayerObj.CurrentTrack];
      MenuItem10Click(Nil);
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.artisttreemenuPopup(Sender: TObject);

Var    MedFileObj: TMediaFileClass;
Begin
  If ArtistTree.Selected.Level>0 Then
    Begin
      MedFileObj := TMediaFileClass(ArtistTree.Selected.Data);
      If MedFileObj.collection=PlayerCol Then Menuitem30.enabled := false
      Else Menuitem30.enabled := true;
      If ArtistTree.Selected.Level=1 Then
        Begin
          If ArtistTree.Selected.ImageIndex=1 Then
            Begin
              MenuItem37.Enabled := false;
              rm_artist_playeritem.Enabled := true;
              MenuItem30.Enabled := true;
            End;
          If ArtistTree.Selected.ImageIndex=-1 Then
            Begin
              MenuItem37.Enabled := false;
              rm_artist_playeritem.Enabled := false;
              MenuItem30.Enabled := true;
            End;
          If ArtistTree.Selected.ImageIndex=2 Then
            Begin
              MenuItem37.Enabled := true;
              rm_artist_playeritem.Enabled := false;
              MenuItem30.Enabled := false;
            End;
          If ArtistTree.Selected.ImageIndex=3 Then
            Begin
              MenuItem37.Enabled := true;
              rm_artist_playeritem.Enabled := false;
              MenuItem30.Enabled := false;
            End;
        End
      Else
        Begin
          MenuItem37.Enabled := true;
          rm_artist_playeritem.Enabled := true;
          MenuItem30.Enabled := true;
        End;
      If player_connected=false Then
        Begin
          MenuItem30.Visible := false;
          rm_artist_playeritem.Visible := false;
          menuitem37.Visible := false;
          space1.Visible := false;
        End;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.checkmobileTimer(Sender: TObject);

Var
  PlayerScanThread: TScanThread;
  tmps: string;

Begin
  If (player_connected=false) And FileExists(CactusConfig.DAPPath+'cactuslib') Then
    Begin
      DebugOut('DAP detected...', 2);
      If connectDAP=0 Then
        Begin
          tmps := GetCurrentDir;
          // get free memory on player, format string
          DebugOut('loaded', 2);
          tmps := ByteToFmtString(FreeSpaceOnDAP, 3, 2);
          writeln(FreeSpaceOnDAP);
          StatusBar1.Panels[1].Text := 'Device connected     '+tmps+' free';
          If CactusConfig.background_scan Then
            Begin
              PlayerScanThread := TScanThread.Create(true);
              PlayerScanThread.tmpcollection.Assign(PlayerCol);
              PlayerScanThread.PTargetCollection := PlayerCol;
              PlayerScanThread.Resume;
            End;
        End
      Else
        Begin
          checkmobile.Enabled := false;
          ShowMessage('Error while opening player device. '+#10+#13+'Try to scan player again...');
          player_connected := true;
        End;
    End;

  Application.ProcessMessages;
  If (player_connected=true) And (FileExists(CactusConfig.DAPPath+'cactuslib')=false) Then
    Begin
      disconnectDAP;
      StatusBar1.Panels[1].Text := 'Device disconnected';
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.clearPlayerItemClick(Sender: TObject);
Begin

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
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem10Click(Sender: TObject);

Var MedFileObj: TMediaFileClass;
Begin


  If playlist.Selected<>Nil Then
    Begin
      MedFileObj := TMediaFileClass(Playlist.Selected.Data);
      editid3win.display_window(MedFileObj);
      enabled := false;
      EditID3win.ShowModal;
      Enabled:=true;
    End;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.clear_listClick(Sender: TObject);
Begin
  if CactusConfig.StopOnClear then StopButtonImgClick(Sender);
  Playlist.BeginUpdate;
  writeln(Playlist.Items.Count);
  Playlist.Items.Clear;
  writeln('clear');
  playlist.Column[0].Caption := rsPlaylist;
  PlayerObj.playlist.clear;
  Playlist.EndUpdate;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.filetypeboxChange(Sender: TObject);
Begin
  srch_buttonClick(Nil);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.libinfoClick(Sender: TObject);

Var z: int64;
  s, used: string;
  i: integer;
Begin
  z := 0;
  For i:= 1 To MediaCollection.ItemCount-1 Do
    z := z+MediaCollection.items[i].size;

  used := ByteToFmtString(z, 3, 2);
  s := IntToStr(MediaCollection.ItemCount);
  ShowMessage(s+' Files in library '+#10+' '+used+' of music files');
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.muteClick(Sender: TObject);
var png: TPortableNetworkGraphic;
Begin
if PlayerObj.playing then begin

  PlayerObj.mute;

  png:=TPortableNetworkGraphic.Create;
  If PlayerObj.muted Then png.LoadFromFile(SkinData.DefaultPath+DirectorySeparator+
                                                          'icon'+DirectorySeparator+'mute2.png')
  Else png.LoadFromFile(SkinData.DefaultPath+DirectorySeparator+
                                                          'icon'+DirectorySeparator+'mute1.png');

  mute.Glyph.assign(png);
  png.Free;
end;
End;

Procedure TMain.opendirClick(Sender: TObject);

Var i: integer;
Begin
  SelectDirectoryDialog1.InitialDir := CactusConfig.HomeDir;
  Selectdirectorydialog1.title := 'Add Directory...';
  If SelectDirectoryDialog1.Execute=true Then
    Begin
      For i:= 0 To MediaCollection.dirlist.Count-1 Do
        Begin
          If pos(MediaCollection.dirlist[i], SelectDirectoryDialog1.FileName)=1 Then
            Begin
              ShowMessage('Directory '+SelectDirectoryDialog1.FileName+
                          ' is still part of directorylist');
              exit;
            End;
        End;
      Enabled := false;
      Application.ProcessMessages;
      MediaCollection.add_directory(SelectDirectoryDialog1.FileName);
      DebugOutLn('finished scan of '+Selectdirectorydialog1.Filename, 3);
      If MediaCollection.ItemCount>1 Then
        Begin
          Main.ArtistTree.Selected := Nil;
          update_artist_view;
          update_title_view;
        End;
      Enabled := true;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.openfileClick(Sender: TObject);

Var OpenDialog: TOpenDialog;
Begin
  OpenDialog := TOpenDialog.Create(self);
  OpenDialog.Filter := 'All supported audio|*.wav;*.mp3;*.ogg;*.wma;*.flac;*.fla|MP3|*.mp3|OGG|*.ogg|WAV|*.wav|WMA|*.wma|FLAC|*.flac;*.fla';
  if FileOpneDialogPath<>''then
    OpenDialog.InitialDir := FileOpneDialogPath
   else
    OpenDialog.InitialDir := CactusConfig.HomeDir;
  OpenDialog.FilterIndex := 1;
  If Opendialog.execute=true Then
    Begin
      LoadFile(Opendialog.Filename);
      FileOpneDialogPath:=ExtractFilePath(OpenDialog.FileName);
    End;
  OpenDialog.Free;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.pauseClick(Sender: TObject);
Begin
  PlayerObj.pause;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.player_libClick(Sender: TObject);
Begin
  If playwin.Active Then
    Begin
      playwin.hide;
      main.show;
      playermode := false;
    End
  Else
    Begin
      playwin.show;
      main.hide;
      playermode := true;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.playlistClick(Sender: TObject);
Begin
  ArtistSrchField.hide;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.playlistDblClick(Sender: TObject);
Begin
  playclick(Nil);
End;

Procedure TMain.playlistDragDrop(Sender, Source: TObject; X, Y: Integer);

Var   Targetitem, tmpitem : TListItem;
  sourceNode: TTreeNode;
  ind: integer;
  MedFileObj: TMediaFileClass;
Begin
  If Not NetworkMode Then
    Begin
      DebugOutLn('ondragdrop', 3);
      Targetitem := Nil;

      Try
        If Playlist.Items.Count>0 Then Targetitem := Playlist.GetItemAt(x, y);
      Except
        Targetitem := Nil;
      End;

      If Playlist.Items.Count=0 Then Targetitem := Nil;
      If Targetitem<>Nil Then DebugOutLn(Targetitem.Index, 3)
      Else DebugOutLn('TARGET NIL', 3);

      If title_drag  Then
        Begin
          title_drag := false;
          MedFileObj := TMediaFileClass(sourceitem.Data);
          tmpitem := TListItem.Create(Playlist.Items);
          If (MedFileObj.Artist<>'') Or (MedFileObj.Title<>'') Then
            Begin
              tmpitem.Caption := MedFileObj.Artist+' - '+MedFileObj.Title;
            End
          Else
            tmpitem.Caption := ExtractFileName(MedFileObj.Path);
          tmpitem.Data := MedFileObj;

          If (Targetitem<>Nil) And (Targetitem.Index<Playlist.Items.Count-1) And (Playlist.Items.
             Count>0) Then
            Begin
              ind := Targetitem.Index;
              Playlist.Items.InsertItem(tmpitem, ind+1);
              PlayerObj.Playlist.insert(tmpitem.Index+1, MedFileObj);
            End
          Else
            Begin
              Playlist.Items.AddItem(tmpitem);
              PlayerObj.playlist.add(MedFileObj);
            End;
          sourceitem := Nil;
          If Not PlayerObj.playing And (CactusConfig.AutostartPlay) And (Playlist.Items.
             Count=1) Then playClick(self);
        End;

      If artist_drag Then
        Begin
          artist_drag := false;
          sourceNode := ArtistTree.Selected;
          If (Targetitem<>Nil) And (Targetitem.Index<Playlist.Items.Count-1) And (Playlist.Items.
             Count>0) Then
            artist_to_playlist_at(Targetitem.Index+1)
          Else artist_to_playlist;
        End;

      If playlist_drag Then
        Begin
          playlist_drag := false;
          sourceitem := Playlist.Selected;
          DebugOutLn('playlist_Drag', 3);
          If (sourceitem<>Nil) And (Targetitem<>sourceitem) Then
            Begin
              ind := sourceitem.Index;
              DebugOutLn('OK', 3);
              tmpitem := TListItem.Create(Playlist.Items);
              tmpitem.Assign(sourceitem);
              If (Targetitem<>Nil) And (Targetitem.Index<Playlist.Items.Count-1) Then
                Begin
                  Playlist.Items.InsertItem(tmpitem, Targetitem.Index+1);
                  sourceitem.Delete;
                  If ind>tmpitem.Index-1 Then PlayerObj.Playlist.move(ind, tmpitem.Index)
                  Else PlayerObj.Playlist.move(ind, tmpitem.Index);
                  DebugOutLn('MOVE', 3)
                End
              Else
                Begin
                  Playlist.Items.AddItem(tmpitem);
                  sourceitem.Delete;
                  PlayerObj.Playlist.move(ind, tmpitem.Index);
                  DebugOutLn('ADD', 3);
                End;
            End;
        End;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.playlistDragOver(Sender, Source: TObject; X, Y: Integer;
                                 State: TDragState; Var Accept: Boolean);
Begin
{$ifdef LCLGtk}
  //Workaround for GTK1.x to reset selected Item while dragging
  DebugOutLn('gtk1', 5);
  If playlist_drag Then
    Begin
      playlist.Selected := Nil;
      playlist.Items[sourceitem.Index].Selected := true;
    End;
{$endif}
  accept := true;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.playlistEndDrag(Sender, Target: TObject; X, Y: Integer);

Var tmplitem: TListItem;
Begin
  playlist_drag := false;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.playlistKeyDown(Sender: TObject; Var Key: Word;
                                Shift: TShiftState);

Var tempitem: TListItem;
  i: integer;
Begin
  DebugOutLn('Playlist keypress event: Keycode ', 2);
  writeln(key);
  Case key Of 

    // Key Ctrl
    17: ctrl_pressed := true;

    // Key UP
    38: If playlist.Selected.Index>0 Then
          Begin
            i := playlist.Selected.Index;
            DebugOutLn(i, 2);
            If ctrl_pressed Then
              Begin
                tempitem := playlist.selected;
                PlayerObj.playlist.move(i, i-1);
                playlist.items[i] := playlist.items[i-1];
                playlist.items[i-1] := tempitem;
                Playlist.SetFocus;
                playlist.items[i].Selected := false;
                playlist.items[i-1].Selected := true;
                //tempitem.MakeVisible(true);
              End;
            writeln(playlist.Selected.Index);
          End;

    // Key DOWN
    40:  If playlist.Selected.Index<playlist.items.Count-1 Then
           Begin
             i := playlist.Selected.Index;
             DebugOutLn(i, 2);
             If ctrl_pressed Then
               Begin
                 tempitem := playlist.selected;
                 PlayerObj.playlist.move(i,i+1);
                 playlist.items[i] := playlist.items[i+1];
                 playlist.items[i+1] := tempitem;
                 Playlist.SetFocus;
                 playlist.items[i].Selected := false;
                 playlist.items[i+1].Selected := true;
                 //tempitem.MakeVisible(true);
               End;
             DebugOutLn(playlist.Selected.Index, 2);
           End;
    // Key Del
    46: MenuItem3Click(Nil);
  End;
End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.playlistKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState
);
Begin
  If key=17 Then ctrl_pressed := false;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.playlistMouseDown(Sender: TOBject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);

Var tempitem: TListItem;
Begin
  // ensure that the popup menu is only opened when an item is selected
  // the menu is reanabled in TMain.playlistSelectItem

  {$ifdef  LCLQT} //TODO: QT interface doesn't set selected item
       Playlist.Selected:= Playlist.GetItemAt(x, y);
  {$endif}

  {$ifdef  LCLGtk2} //TODO: GTK2 interface doe snot selcte item on right click
        If (Button = mbRight) then Playlist.Selected := Playlist.GetItemAt(x, y);
  {$endif}

  {$ifdef win32}
  If (Button = mbRight) And (Playlist.Selected <> Nil) Then
    Playlist.PopupMenu.PopUp(self.Left+panel4.Width+Panel3.Left+Playlist.left+X+10, self.top+Panel3.
                             Top+Playlist.top+Y+50);
  {$else}
  If (Button = mbRight) And (playlist.Selected = Nil) Then
    playlist.PopupMenu.AutoPopup := false;
  {$endif}
  //Enable Dragging
  If Button = mbLeft Then
    Begin { only drag if left button pressed }
      sourceitem := Nil;
      sourceitem := Playlist.GetItemAt(x, y);
      If sourceitem<>Nil Then Playlist.BeginDrag(false, 10);
    End;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.playlistSelectItem(Sender: TObject; Item: TListItem;
                                   Selected: Boolean);
Begin
  // reanable the popupmenu in case ist was disabled in TMain.playlistMouseDown
  playlist.PopupMenu.AutoPopup := true;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.playlistStartDrag(Sender: TObject; Var DragObject: TDragObject);
Begin
  playlist_drag := true;
  DebugOutLn('playlist drag', 3);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.playtimerStartTimer(Sender: TObject);

Var MedFileObj: TMediaFileClass;
  i: integer;
Begin
  If PlayerObj.PlaybackMode=FILE_MODE Then
    Begin
      CoverFound := false;
      LoopCount := 0;
      i := PlayerObj.CurrentTrack;
      MedFileObj := TMediaFileClass(playlist.Items[PlayerObj.CurrentTrack].Data);
      If (MedFileObj.album<>'') Then
        Begin
          MedFileObj.CoverPath := CactusConfig.ConfigPrefix+DirectorySeparator+'covercache'+
                                  DirectorySeparator+MedFileObj.Artist+'_'+MedFileObj.album+'.jpeg';
          If (FileExists(MedFileObj.CoverPath)=false) Then
            Begin
              CoverImage.Picture.Clear;
              If  (CactusConfig.CoverDownload) Then
                Begin
                  LastFMAPI := TLastfmAPIObject.Create;
                  if CactusConfig.CoverSize='large' then LastFMAPI.CoverSize:=ExtralargeImage
                            else LastFMAPI.CoverSize:=LargeImage;
                  LastFMAPI.album_downloadCover(MedFileObj.Artist, MedFileObj.Album, MedFileObj.CoverPath);
                End;
            End
          Else
            Begin
              Try
                CoverImage.Picture.LoadFromFile(MedFileObj.CoverPath);
                playwin.AlbumCoverImg.Picture.LoadFromFile(MedFileObj.CoverPath);
              Except
                DebugOutLn('EXCEPTION', 3);
              End;
              CoverFound := true;
            End;
        End
      Else CoverImage.Picture.Clear;

   //CoverImage.Picture.LoadFromFile(DataPrefix+'tools'+DirectorySeparator+'cactus-logo-small.png');
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.ArtistTreeDblClick(Sender: TObject);

Var StreamInfoObj: TStreamInfoItemClass;
Begin
  If LibraryMode Or DeviceMode Then
    Begin
      If (ArtistTree.Selected<>Nil) And (ArtistTree.Selected.Level>0) Then artist_to_playlist;
    End;

  If NetworkMode Then
    Begin
      If (ArtistTree.Selected<>Nil) And (ArtistTree.Selected.Level>0) Then
        Begin
          StatusBar1.Panels[0].Text := 'Buffering stream...';
          StreamInfoObj := TStreamInfoItemClass(ArtistTree.Selected.Data);
          writeln(PlayerObj.play(StreamInfoObj.URL));
          current_title_edit.Text := 'Playing radio stream...';
          current_title_edit1.Text := StreamInfoObj.Name;
          playtimer.Enabled := true;
        End;
    End;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.ArtistTreeEndDrag(Sender, Target: TObject; X, Y: Integer);
Begin
  artist_drag := false;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.ArtistTreeClick(Sender: TObject);
Begin
  ArtistSrchField.Hide;
  // if ArtistTree.Selected<>nil then update_title_view;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.ArtistTreeKeyUp(Sender: TObject; Var Key: Word;
                                Shift: TShiftState);

Var b: byte;
  c: char;
  i: integer;
Begin
  writeln(key);
  b := key;
  c := char(b);
  Case key Of 
    45: For i:=0 To ArtistTree.Items.Count-1 Do
          If ArtistTree.Items[i].Level=0 Then ArtistTree.Items[i].Expanded := false;
    43: For i:=0 To ArtistTree.Items.Count-1 Do
          If ArtistTree.Items[i].Level=0 Then ArtistTree.Items[i].Expanded := true;
    27: ArtistSrchField.Hide;
    13: ArtistSrchField.Hide;

    65..255:
             Begin
               If Not ArtistSrchField.visible Then
                 Begin
                   ArtistSrchField.Top := main.Height-120;
                   ArtistSrchField.Left := Panel4.Width-155;
                   ArtistSrchField.Show;
                   artistsearch.Text := c;
                   artistsearch.SetFocus;
                   artistsearch.SelStart := 1;
                   artistsearch.SelLength:=0;

                 End;
               i := 0;
               Repeat
                 inc(i)
               Until ((pos(lowercase(artistsearch.Text), lowercase(ArtistTree.Items[i].Text))=1) And
                     (ArtistTree.Items[i].Level=1)) Or (i>=ArtistTree.Items.Count-1);
               if ArtistTree.Items[i].Level>0 then begin
                  ArtistTree.Selected := ArtistTree.Items[i];
                  if ArtistTree.Selected.AbsoluteIndex<ArtistTree.Items.Count-10 then
                     begin
                          ArtistTree.Items[ArtistTree.Selected.AbsoluteIndex+9].MakeVisible;
                          if ArtistTree.Items[ArtistTree.Selected.AbsoluteIndex+9].Level>1 then
                                ArtistTree.Items[ArtistTree.Selected.AbsoluteIndex+9].Parent.Expanded:=false;
                     end
                     else begin
                          ArtistTree.Items[ArtistTree.Items.Count-1].MakeVisible;
                          if ArtistTree.Items[ArtistTree.Items.Count-1].Level>1 then
                                ArtistTree.Items[ArtistTree.Items.Count-1].Parent.Expanded:=false;
                     end;
                 End;
            End;
  End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.ArtistTreeMouseDown(Sender: TOBject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
Begin
  ArtistTree.SetFocus;
  // ensure that the popup menu is only opened when an item is selected
  If Button = mbRight Then
    Begin
      If NetworkMode Then ArtistTree.PopupMenu := NetworktreePopup
      Else ArtistTree.PopupMenu := artisttreemenu;
      If (ArtistTree.GetNodeAt(X, Y) = Nil) Then
        ArtistTree.PopupMenu.AutoPopup := false
      Else
        ArtistTree.PopupMenu.AutoPopup := true;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem30Click(Sender: TObject);

Var MedColObj: TMediaCollectionClass;
  curartist, curalbum, tmps: string;
  tmpsize: int64;
  MedFileObj: TMediaFileClass;
  i: integer;
  tsnode: TTreeNode;
Begin
  tsnode := ArtistTree.Selected;
  tmpsize := 0;

  If (tsnode<>Nil) And (tsnode.level>0) And player_connected Then
    Begin
      MedFileObj := TMediaFileClass(tsnode.data);
      MedColObj := MedFileObj.collection;
      curartist := lowercase(MedFileObj.Artist);
      i := MedColObj.getTracks(MedFileObj.Artist, MedFileObj.index);
      If tsnode.level=2 Then     //album
        Begin
          curalbum := lowercase(MedFileObj.album);
          Repeat
            Begin
              If (lowercase(MedColObj.items[i].album)=curalbum) And (MedColObj.items[i].action=
                 AREMOVE) Then
                Begin
                  MedColObj.items[i].action := AONPLAYER;
                  sizediff := sizediff - MedColObj.items[i].size;
                End;
              If (lowercase(MedColObj.items[i].album)=curalbum) And (MedColObj.items[i].action<>
                 AONPLAYER) Then
                Begin
                  MedColObj.items[i].action := AUPLOAD;
                  sizediff := sizediff - MedColObj.items[i].size;
                End;
              i := MedColObj.GetNext;
            End;
          Until i<0;

        End;
      If tsnode.level=1 Then     //SrchArtItem
        Begin
          Repeat
            Begin
              If (MedColObj.items[i].action=AREMOVE) Then
                Begin
                  MedColObj.items[i].action := AONPLAYER;
                  sizediff := sizediff - MedColObj.items[i].size;
                End;
              If (MedColObj.items[i].action<>AONPLAYER) Then
                Begin
                  MedColObj.items[i].action := AUPLOAD;
                  sizediff := sizediff - MedColObj.items[i].size;
                End;
              i := MedColObj.GetNext;
            End;
          Until i<0;
        End;
      update_artist_view;
      update_title_view;

      tmps := ByteToFmtString(FreeSpaceOnDAP + sizediff, 3, 2);
      StatusBar1.Panels[1].Text := 'Device connected     '+tmps+' Free';
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem33Click(Sender: TObject);

Var MedFileObj: TMediaFileClass;
  tsnode: TTreeNode;
Begin
  If ArtistTree.Selected<>Nil Then
    Begin
      tsnode := ArtistTree.Selected;
      MedFileObj := TMediaFileClass(tsnode.data);
      If tsnode.level= 1 Then
        Begin
          editid3win.display_window(MedFileObj, ARTIST_MODE);
        End;
      If tsnode.level= 2 Then
        Begin
          editid3win.display_window(MedFileObj, ALBUM_MODE);
        End;
      EditID3win.ShowModal;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.rm_artist_playeritemClick(Sender: TObject);

Var MedColObj: TMediaCollectionClass;
  tmps: string;
  MedFileObj: TMediaFileClass;
  i, z: integer;
  tsnode: TTreeNode;
Begin
  tsnode := ArtistTree.Selected;
  If (tsnode<>Nil) And (tsnode.level>0) And player_connected Then
    Begin
      MedFileObj := TMediaFileClass(tsnode.data);
      MedColObj := MedFileObj.collection;
      If tsnode.level=2 Then   //remove one album
        Begin
          i := PlayerCol.getTracks(MedFileObj.Artist, MedFileObj.Album);
          Repeat
            Begin
              If PlayerCol.Items[i].Action=AONPLAYER Then
                Begin
                  PlayerCol.items[i].action := AREMOVE;
                  For z:= 0 To MediaCollection.ItemCount-1 Do
                    If PlayerCol.items[i].id=MediaCollection.items[z].id Then MediaCollection.items[z].action := AREMOVE;
                  sizediff := sizediff + PlayerCol.items[i].size;
                End;
              i := PlayerCol.getNext;
            End;
          Until  (i<0);
        End;
      If tsnode.level=1 Then      //remove the SrchArtItem
        Begin
          i := PlayerCol.getTracks(MedFileObj.Artist);
          Repeat
            Begin
              If PlayerCol.items[i].action=AONPLAYER Then
                Begin
                  PlayerCol.items[i].action := AREMOVE;
                  For z:= 0 To MediaCollection.ItemCount-1 Do
                    If PlayerCol.items[i].id=MediaCollection.items[z].id Then MediaCollection.items[
                      z].action := AREMOVE;
                  sizediff := sizediff + PlayerCol.items[i].size;
                End;
              i := PlayerCol.getNext;
            End;
          Until (i<0);
        End;
      update_artist_view;
      update_title_view;
      
      tmps := ByteToFmtString(FreeSpaceOnDAP + sizediff, 3, 2);
      StatusBar1.Panels[1].Text := 'Device connected     '+tmps+' Free';
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.searchstrClick(Sender: TObject);
Begin
  ArtistSrchField.Hide;
End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.skinmenuClick(Sender: TObject);
Begin

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.syncplayeritem(Sender: TObject);

Var
  newfile: string;
  n: integer;
  ucount, rcount: integer;
  bytesneeded: int64;
Begin
  If player_connected=false Then
    Begin
      ShowMessage(rsNotConnected);
      exit;
    End;
  StatusBar1.Panels[1].Text := 'Calculating...';
  rcount := 1;
  ucount := 1;
  bytesneeded := 0;
  //Calculation the disk space that has to be available on player
  SyncThread := TSyncThread.Create(true);
  SyncThread.Target := PlayerCol.savepath;
  Enabled := false;
  For n:= 0 To MediaCollection.ItemCount-1 Do
    Begin
      //search for uploads in mediacollection
      If MediaCollection.items[n].action=AUPLOAD Then
        Begin
          inc(ucount);
          bytesneeded := bytesneeded + MediaCollection.items[n].size;
          If CactusConfig.mobile_subfolders Then
            Begin
              If Not DirectoryExists(CactusConfig.DAPPath+lowercase(MediaCollection.items[n].Artist)
                 ) Then mkdir(CactusConfig.DAPPath+lowercase(MediaCollection.items[n].Artist));
              newfile := CactusConfig.DAPPath+lowercase(MediaCollection.items[n].Artist)+'/'+
                         ExtractFileName(MediaCollection.items[n].path);
            End
          Else
            newfile := CactusConfig.DAPPath+ExtractFileName(MediaCollection.items[n].path);
          DoDirSeparators(newfile);
          SyncThread.copyFile(MediaCollection.items[n].path, newfile);
        End;
    End;
  For n:= 0 To PlayerCol.ItemCount-1 Do
    Begin
      //find files to be deleted in playercollection
      If PlayerCol.items[n].action=AREMOVE Then
        Begin
          inc(rcount);
          bytesneeded := bytesneeded - PlayerCol.items[n].size;
          SyncThread.deleteFile(PlayerCol.items[n].path);
          DebugOutLn(PlayerCol.items[n].path+' to be deleted', 2);
        End;
    End;
  Enabled := true;

  If FreeSpaceOnDAP < bytesneeded Then
    Begin
      //Check if there is enough free disk space on player
      ShowMessage('ERROR: Not enough free disk space on mobile device!');
      StatusBar1.Panels[1].Text := 'Ready';
      SyncThread.Free;
      exit;
      //Free thread and exit
    End;

  checkmobile.Enabled := false;
  disconnectDAP;

  StatusBar1.Panels[1].Text := 'Please Wait...';
  SyncThread.Resume;

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem3Click(Sender: TObject);

Var s1, s2: string;
  i: integer;
Begin
  If playlist.selected<>Nil Then
    Begin
      i := playlist.selected.index;
      PlayerObj.playlist.remove(i);
      Playlist.Selected.delete;
      s1 := IntToStr((PlayerObj.Playlist.TotalPlayTime Div 60) Mod 60 );
      s2 := IntToStr((PlayerObj.Playlist.TotalPlayTime Div 60) Div 60 );
      playlist.Column[0].Caption := rsPlaylist+'            ('+IntToStr(PlayerObj.Playlist.
                                    ItemCount)+' Files/ '+s2+'h '+s1+'min )';
      If (i>=1) And (i=playlist.items.count) Then dec(i);
      playlist.selected := playlist.items[i];
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MenuItem22aClick(Sender: TObject);
Begin
  If PlayerObj.playing Then
    artist_to_playlist_at(PlayerObj.CurrentTrack+1)
  Else artist_to_playlist;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.RemoveClick(Sender: TObject);
Begin
  MediaCollection.clear;
  TitleTree.Items.Clear;
  ArtistTree.Items.Clear;
  Playlist.Items.Clear;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.QuitItemClick(Sender: TObject);
Begin
  Main.close;
  Application.ProcessMessages;

  // halt;
  Application.Terminate;
  //  Application.Free;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.MoveNode(TargetNode, SourceNode : TTreeNode);

Var 
  nodeTmp : TTreeNode;
Begin

{  with Selecttree do
  begin
    nodeTmp := Items.AddChild(TargetNode,SourceNode.Text);
    for i := 0 to SourceNode.Count -1 do
    begin
      MoveNode(nodeTmp,SourceNode.items[i]);
    end;
  end;}
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.titlelistmenuPopup(Sender: TObject);

Var MedFileObj: TMediaFileClass;
Begin
  writeln('titletree popupmenu');
  If TitleTree.Selected<>Nil Then
    Begin
      MedFileObj := TMediaFileClass(TitleTree.Selected.Data);
      // Menuitem16.ImageIndex:=1;
      If MedFileObj.collection=PlayerCol Then
        Begin
          Menuitem16.enabled := false;
          Menuitem14.enabled := true;
        End
      Else
        Begin
          Menuitem16.enabled := false;
          //upload
          Menuitem14.Enabled := false;
          //remove

          If MedFileObj.action=-1 Then
            Begin
              Menuitem16.enabled := true;
            End;

          If MedFileObj.action=1 Then
            Begin
              Menuitem14.enabled := true;
            End;
        End;
      If player_connected=false Then
        Begin
          MenuItem16.Visible := false;
          menuitem14.Visible := false;
          menuitem20.Visible := false;
          menuitem11.Visible := false;
        End;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.toggle_playpause(Sender: TObject);
Begin
  If PlayerObj.playing Then pauseClick(Nil)
  Else playClick(Nil);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.trackbarMouseDown(Sender: TOBject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
Begin
  playtimer.enabled := false;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.trackbarMouseUp(Sender: TOBject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);

Var k: real;
  i: integer;
Begin
  k := (x*100) / trackbar.Width;
  i:=round(k);
  PlayerObj.set_fileposition(i);
  trackbar.Position := i;
  If PlayerObj.playing Then playtimer.enabled := true;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.undoSyncItemClick(Sender: TObject);

Var tmps: string;
  i: integer;
Begin
    Begin
      For i:= 1 To MediaCollection.ItemCount-1 Do
        Begin
          If MediaCollection.items[i].action=AUPLOAD Then MediaCollection.items[i].action := 
                                                                                            ANOTHING
          ;
          If MediaCollection.items[i].action=AREMOVE Then MediaCollection.items[i].action := 
                                                                                           AONPLAYER
          ;
        End;

      For i:= 1 To PlayerCol.ItemCount-1 Do
        Begin
          If PlayerCol.items[i].action=AUPLOAD Then PlayerCol.items[i].action := ANOTHING;
          If PlayerCol.items[i].action=AREMOVE Then PlayerCol.items[i].action := AONPLAYER;
        End;
      update_artist_view;
      update_title_view;
      sizediff := 0;

      tmps := ByteToFmtString(FreeSpaceOnDAP, 3, 2);
      StatusBar1.Panels[1].Text := 'Device connected     '+tmps+' Free';
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.TitleTreeDblClick(Sender: TObject);
Begin
  Application.ProcessMessages;
  title_to_playlist;
  Application.ProcessMessages;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure title_to_playlist_at(index: integer);

Var tsnode: TListitem;
  listitem: TListitem;
  MedFileObj: TMediaFileClass;
Begin
  tsnode := main.TitleTree.Selected;
  If (tsnode<>Nil) And (tsnode.ImageIndex<>4) Then
    Begin
      If main.Playlist.Items.Count=0 Then
        Begin
          title_to_playlist;
          exit;
        End;
      MedFileObj := TMediaFileClass(tsnode.data);

      PlayerObj.playlist.Insert(index, MedFileObj);

      ListItem := Main.Playlist.Items.Insert(index);
      listitem.data := MedFileObj;
      listitem.MakeVisible(false);
      If MedFileObj.title<>'' Then ListItem.Caption := MedFileObj.Artist+' - '+MedFileObj.title
      Else ListItem.Caption := extractfilename(MedFileObj.path);
    End;
  main.playlist.Column[0].Caption := rsPlaylist+'                       ('+IntToStr(
                                     PlayerObj.playlist.ItemCount)+' Files/ '+PlayerObj.
                                     Playlist.TotalPlayTimeStr +')';
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure title_to_playlist;

Var tsnode: TListitem;
  listitem: TListitem;
  MedFileObj: TMediaFileClass;
Begin
  tsnode := main.TitleTree.Selected;
  DebugOutLn('title to playlist', 5);
  If (tsnode<>Nil) And (tsnode.ImageIndex<>4) Then
    Begin
      DebugOutLn('title to playlist2', 5);
      MedFileObj := TMediaFileClass(tsnode.data);

      PlayerObj.playlist.add(MedFileObj);

      ListItem := Main.Playlist.Items.Add;
      listitem.data := MedFileObj;
      listitem.MakeVisible(false);
      //     listitem.Focused:=true;
      If MedFileObj.title<>'' Then ListItem.Caption := MedFileObj.Artist+' - '+MedFileObj.title
      Else ListItem.Caption := extractfilename(MedFileObj.path);
      If Not PlayerObj.playing And CactusConfig.AutostartPlay And (main.Playlist.Items.Count
         =1) Then
        Begin
          DebugOutLn('title to playlist3', 5);
          main.Playlist.Selected := Main.Playlist.Items[0];
          DebugOutLn(Main.Playlist.Selected.Caption, 3);
          Main.playClick(main);
        End;
    End;
  main.playlist.Column[0].Caption := rsPlaylist+'                       ('+IntToStr(
                                     PlayerObj.playlist.ItemCount)+' Files/ '+PlayerObj.
                                     Playlist.TotalPlayTimeStr +')';
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure artist_to_playlist_at(index: integer);

Var tsnode: TTreeNode;
  curartist, curalbum: string;
  Listitem: TListitem;
  album_mode: boolean;
  MedColObj: TMediaCollectionClass;
  MedFileObj: TMediaFileClass;
  z: integer;
Begin
  tsnode := Main.ArtistTree.Selected;
  If main.Playlist.Items.Count=0 Then
    Begin
      artist_to_playlist;
      exit;
    End;

  If (tsnode<>Nil) And (tsnode.Level>0) Then
    Begin
      If tsnode.level<2 Then album_mode := false
      Else album_mode := true;
      MedFileObj := TMediaFileClass(tsnode.data);
      MedColObj := MedFileObj.Collection;
      curartist := lowercase(MedFileObj.Artist);
      curalbum := lowercase(MedFileObj.Album);
      z := MedColObj.getTracks(MedFileObj.Artist, MedFileObj.index);
      Repeat
        Begin
          DebugOutLn(MedColObj.items[z].title, 3);
          If (album_mode=false) Or ((album_mode=true) And (lowercase(MedColObj.items[z].album)=
             curalbum)) Then
            Begin
              PlayerObj.playlist.insert(index, MedColObj.items[z]);
              ListItem := Main.Playlist.Items.Insert(index);
              inc(index);
              listitem.data := MedColObj.items[z];
              // Listitem.Focused:=true;
              If MedColObj.items[z].title<>'' Then ListItem.Caption := MedColObj.items[z].Artist+
                                                                       ' - '+MedColObj.items[z].
                                                                       title
              Else ListItem.Caption := extractfilename(MedColObj.items[z].path);
            End;
          z := MedColObj.GetNext;
        End;
      Until z<0;
      Listitem.MakeVisible(false);
    End;
  main.playlist.Column[0].Caption := rsPlaylist+'            ('+IntToStr(PlayerObj.playlist.
                                     ItemCount)+' Files/ '+PlayerObj.Playlist.
                                     TotalPlayTimeStr+' )';
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure artist_to_playlist;

Var tsnode: TTreeNode;
  curartist, curalbum: string;
  Listitem: TListitem;
  album_mode: boolean;
  MedColObj: TMediaCollectionClass;
  MedFileObj: TMediaFileClass;
  z, oldcount: integer;
Begin
  //TODO: add album to playlist sorted by TRACK
  tsnode := Main.ArtistTree.Selected;
  If (tsnode<>Nil) And (tsnode.Level>0) Then
    Begin
      oldcount := main.Playlist.Items.Count;
      If tsnode.level<2 Then album_mode := false
      Else album_mode := true;
      MedFileObj := TMediaFileClass(tsnode.data);
      MedColObj := MedFileObj.Collection;
      curartist := lowercase(MedFileObj.Artist);
      curalbum := lowercase(MedFileObj.Album);
      z := MedColObj.getTracks(MedFileObj.Artist, MedFileObj.index);
      Repeat
        Begin
          DebugOutLn(MedColObj.items[z].title, 3);
          If (album_mode=false) Or ((album_mode=true) And (lowercase(MedColObj.items[z].album)=
             curalbum)) Then
            Begin
              PlayerObj.playlist.add(MedColObj.items[z]);
              ListItem := Main.Playlist.Items.Add;
              listitem.data := MedColObj.items[z];
              // Listitem.Focused:=true;
              If MedColObj.items[z].title<>'' Then ListItem.Caption := MedColObj.items[z].Artist+
                                                                       ' - '+MedColObj.items[z].
                                                                       title
              Else ListItem.Caption := extractfilename(MedColObj.items[z].path);
            End;
          z := MedColObj.GetNext;
        End;
      Until z<0;
      Listitem.MakeVisible(false);
      If CactusConfig.AutostartPlay And (oldcount=0) And (main.Playlist.Items.Count>0) Then
        Begin
          main.Playlist.Selected := Main.Playlist.Items[0];
          Main.playClick(main);
        End;
    End;
  main.playlist.Column[0].Caption := rsplaylist+'            ('+IntToStr(PlayerObj.playlist.
                                     ItemCount)+' Files/ '+PlayerObj.Playlist.
                                     TotalPlayTimeStr+' )';
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure update_title_view;

Var tsnode: TTreeNode;
  curartist, curalbum: string;
  Listitem: TListItem;
  album_mode: boolean;
  MedColObj: TMediaCollectionClass;
  MedFileObj: TMediaFileClass;
  i: integer;
Begin
  tsnode := Main.ArtistTree.Selected;
  main.StatusBar1.Panels[0].Text := 'Please wait... updating...';

  DebugOutLn('', 2);
  DebugOut('## update title view...', 2);
  main.TitleTree.Selected:=nil;

  Main.TitleTree.Clear;
{$ifndef LCLGtk2}
  Main.TitleTree.BeginUpdate;
{$endif}

{$ifdef LCLGtk2}
  DebugOut(' <TODO: BeginUpdate/EndUpdate disabled in GTK2 due to some bugs in LCL> ', 2);
  //TODO: BeginUpdate/EndUpdate disabled in GTK2 due to some bugs in LCL
{$endif}

  DebugOut(' cleared items... ', 2);

  If (tsnode<>Nil) And (tsnode.level>0) Then
    Begin
      If tsnode.level=2 Then album_mode := true
      Else album_mode := false;

      MedFileObj := TMediaFileClass(tsnode.data);
      MedColObj := MedFileObj.Collection;
      curartist := lowercase(MedFileObj.Artist);
      curalbum := lowercase(MedFileObj.album);
      DebugOut(curartist, 2);

      i := MedColObj.getTracks(MedFileObj.Artist, MedFileObj.index);

      Repeat
        Begin
          If (album_mode=false) Or ((album_mode) And (curalbum=lowercase(MedColObj.items[i].album)))
            Then
            Begin
              ListItem := Main.Titletree.Items.Add;
              MedColObj.items[i].index := i;
              listitem.data := MedColObj.items[i];
              Listitem.ImageIndex := MedColObj.items[i].action;
              Listitem.caption := '';

              If MedColObj.items[i].title<>'' Then
                ListItem.SubItems.Add((MedColObj.items[i].Artist))
              Else ListItem.SubItems.Add(extractfilename(MedColObj.items[i].path));

              ListItem.SubItems.Add((MedColObj.items[i].title));
              ListItem.SubItems.Add((MedColObj.items[i].album));
              ListItem.SubItems.Add(MedColObj.items[i].track);
              ListItem.SubItems.Add(ID3Genre[MedColObj.items[i].GenreID]);
              ListItem.SubItems.Add(ExtractFileName(MedColObj.items[i].Path));
              ListItem.SubItems.Add(MedColObj.items[i].playtime);

            End;
          i := MedColObj.GetNext;
        End;
      Until (i<0);
    End;
  if CactusConfig.SortAlbumByTrack and album_mode then main.TitleTreeColumnClick(main, main.TitleTree.Column[4]);
  DebugOutLn(' finished title view ##', 2);
{$ifndef LCLGtk2}
  Main.TitleTree.EndUpdate;
{$endif}
  main.StatusBar1.Panels[0].Text := 'Ready.';

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.update_playlist;

Var
  MedfileObj: TMediaFileClass;
  i: integer;
Begin

  For i:= 0 To PlayerObj.Playlist.ItemCount-1 Do
    Begin
      MedfileObj := TMediaFileClass(playlist.Items[i].Data);
      PlayerObj.Playlist.Items[i].update(MedfileObj);

      If MedfileObj.title<>'' Then
        playlist.Items[i].caption := MedfileObj.Artist+' - '+MedfileObj.title
      Else
        playlist.Items[i].caption := extractfilename(MedfileObj.path);
    End;
  update_player_display;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TMain.disconnectDAP;

Var i : integer;
Begin
 if player_connected then begin
  DebugOutLn('### Disconnect DAP ###', 2);
  Enabled := false;
  i := 0;
  While i < playlist.Items.Count Do
    Begin
      If TMediaFileClass(playlist.Items[i].Data).collection=PlayerCol Then
        Begin
          PlayerObj.playlist.remove(i);
          If PlayerObj.Playlist.ItemCount<>0 Then Playlist.Items[i].Delete;
          dec(i);
        End;
      inc(i);
    End;
  FreeAndNil(PlayerCol);
  player_connected := false;
  For i:= 1 To MediaCollection.ItemCount-1 Do
    MediaCollection.items[i].action := -1;
  ArtistTree.Selected := Nil;
  Enabled := true;
  update_artist_view;
  update_title_view;
 end;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Function TMain.connectDAP: byte;

Var i, z: integer;
Begin
  Result := 255;
  PlayerCol := TMediaCollectionClass.create;
  PlayerCol.PathFmt := FRelative;
  DebugOutLn('### ConnectDAP  ###', 2);
  If PlayerCol.LoadFromFile(CactusConfig.DAPPath+'cactuslib')=true Then
    Begin

      sizediff := 0;
      For i:= 0 To PlayerCol.ItemCount-1 Do
        Begin
          z := 0;
          PlayerCol.items[i].action := AONPLAYER;
          While z < MediaCollection.ItemCount-1 Do
            Begin
              If MediaCollection.items[z].id=PlayerCol.items[i].id Then
                Begin
                  MediaCollection.items[z].action := AONPLAYER;
                  z := MediaCollection.ItemCount-1;
                End;
              inc(z);
            End;

        End;
      player_connected := true;
      update_artist_view;
      update_title_view;
      checkmobile.Enabled := true;
      result := 0;
    End
  Else
    freeandnil(PlayerCol);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


procedure TMain.TestPlugin1(Sender: TObject);
var
   tt :TCJ_Signals;
begin
     //TestPluginI :=CJ_Interface.GetMenu.Add(-CJ_MAINMENU_PLUGINS, 'Hello World...', @SayHello);
     //TestPluginI :=CJ_Interface.GetMenu.Add(-CJ_MAINMENU_ROOT, 'Hello World...', @SayHello);
     TestPluginI :=CJ_Interface.GetMenu.Add(-CJ_TRAYMENU_ROOT, 'Hello World...', @SayHello);
     tt :=CJ_Interface.GetSignals;
     if tt<>nil then
     begin
          tt.Connect(@SayMsgHello, 1);
          tt.Connect(@SayMsgHello2, 1);
          tt.Connect(@SayMsgHello, 1); //Test for no insertion of this....
     end;
end;


procedure TMain.SayHello(Sender: TCJ_MenuItem);
begin
     Dialogs.MessageDlg('Plugins', 'Hello World Click', mtInformation, [mbOk], 0);
     CJ_Interface.GetMenu.Remove(TestPluginI);
end;

function TMain.SayMsgHello(var Message: TMessage): Boolean;
begin
     Dialogs.MessageDlg('Plugins', 'Hello World From Messages...'+#13#10+IntToStr(Message.WParam)+' '+IntToStr(Message.LParam), mtInformation, [mbOk], 0);
     Result :=True;
end;

function TMain.SayMsgHello2(var Message: TMessage): Boolean;
begin
     Dialogs.MessageDlg('Plugins', 'Hello World 2 From Messages...'+#13#10+IntToStr(Message.WParam)+' '+IntToStr(Message.LParam), mtInformation, [mbOk], 0);
     Result :=True;
end;

procedure TMain.Button2Click(Sender: TObject);
Var
   msgHandled :Boolean;

begin
     CJ_Interface.GetSignals.Signal(1, 24, 50, msgHandled);
end;

initialization
  {$I mainform.lrs}

End.
