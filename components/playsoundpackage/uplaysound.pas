unit uplaysound;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs
  , FileUtil{$IFDEF WINDOWS}, mmsystem{$ELSE}, asyncprocess, process{$ENDIF}, aboutplaysound;

type
  TPlayStyle = (psAsync, psSync);

  Tplaysound = class(TAboutPlaySound)
  private
    { Private declarations }
    {$IFNDEF WINDOWS}
    SoundPlayerAsyncProcess: Tasyncprocess;
    SoundPlayerSyncProcess: Tprocess;
    {$ENDIF}
    fPlayCommand:String;
    fPathToSoundFile: string;
    fPlayStyle: TPlayStyle;
  protected
    { Protected declarations }
    procedure PlaySound(const szSoundFilename: string); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; reintroduce;
    // This is the default method
    procedure Execute;
  published
    { Published declarations }
    // This is normally set at runtime
    property SoundFile: string read fPathToSoundFile write fPathToSoundFile;
    // Default is Async
    property PlayStyle: TPlayStyle read fPlayStyle write fPlayStyle default psASync;
    // This is automatically determined when the component loads
    property PlayCommand:String read fPlayCommand write fPlayCommand;
  end;

procedure Register;

implementation

{$IFNDEF WINDOWS}
const // Defined in mmsystem
  SND_SYNC = 0;
  SND_ASYNC = 1;
  SND_NODEFAULT = 2;
{$ENDIF}
resourcestring
  C_UnableToPlay = 'Unable to play ';

function GetNonWindowsPlayCommand:String;
Var szNonWindowsPlayCommand: string;
begin
  szNonWindowsPlayCommand:='';
  // Try play
  if (FindDefaultExecutablePath('play') <> '') then
    szNonWindowsPlayCommand := 'play';
  // Try aplay
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('aplay') <> '') then
      szNonWindowsPlayCommand := 'aplay -q';
  // Try paplay
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('paplay') <> '') then
      szNonWindowsPlayCommand := 'paplay';
  // Try mplayer
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('mplayer') <> '') then
      szNonWindowsPlayCommand := 'mplayer -really-quiet';
  // Try CMus
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('CMus') <> '') then
      szNonWindowsPlayCommand := 'CMus';
  // Try pacat
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('pacat') <> '') then
      szNonWindowsPlayCommand := 'pacat -p';
  // Try ffplay
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('ffplay') <> '') then
      szNonWindowsPlayCommand := 'ffplay -autoexit -nodisp';
  // Try cvlc
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('cvlc') <> '') then
      szNonWindowsPlayCommand := 'cvlc -q --play-and-exit';
  // Try canberra-gtk-play
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('canberra-gtk-play') <> '') then
      szNonWindowsPlayCommand := 'canberra-gtk-play -c never -f';
  // Try Macintosh command?
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('afplay') <> '') then
      szNonWindowsPlayCommand := 'afplay';
  Result:=szNonWindowsPlayCommand;
end;


constructor Tplaysound.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPlayStyle := psASync;
  fPathToSoundFile := ProgramDirectory;
  {$IFDEF WINDOWS}
  fPlayCommand:='sndPlaySnd';
  {$ELSE}
  fPlayCommand:=GetNonWindowsPlayCommand; // Linux, Mac etc.
  {$ENDIF}
  // About Dialog properties
  AboutBoxComponentName := 'PlaySound';
  AboutBoxWidth := 400;
  AboutBoxHeight := 400;
  AboutBoxBackgroundColor := clCream;
  //AboutBoxFontName (string)
  //AboutBoxFontSize (integer)
  AboutBoxVersion := '0.0.3';
  AboutBoxAuthorname := 'Gordon Bamber';
  AboutBoxOrganisation := 'Public Domain';
  AboutBoxAuthorEmail := 'minesadorada@charcodelvalle.com';
  AboutBoxLicenseType := 'LGPL';
  AboutBoxDescription := 'Plays WAVE sounds in Windows or Linux';
end;

destructor Tplaysound.Destroy;
begin
  {$IFNDEF WINDOWS}
  FreeAndNil(SoundPlayerSyncProcess);
  FreeAndNil(SoundPlayerAsyncProcess);
  {$ENDIF}
  inherited;
end;

procedure Tplaysound.Execute;
begin
  if not FileExistsUTF8(fPathToSoundFile) then
    Exit;
  PlaySound(fPathToSoundFile);
end;

procedure Tplaysound.PlaySound(const szSoundFilename: string);
var
  flags: word;
begin
{$IFDEF WINDOWS}
  if fPlayStyle = psASync then
    flags := SND_ASYNC or SND_NODEFAULT
  else
    flags := SND_SYNC or SND_NODEFAULT;
  try
    sndPlaySound(PChar(szSoundFilename), flags);
  except
    ShowMessage(C_UnableToPlay + szSoundFilename);
  end;
{$ELSE}
  // How to play in Linux? Use generic Linux commands
  // Use asyncprocess to play sound as SND_ASYNC
  // proceed if we managed to find a valid command
  if (fPlayCommand <> '') then
  begin
    if fPlayStyle = psASync then
    begin
      if SoundPlayerAsyncProcess = nil then
        SoundPlayerAsyncProcess := Tasyncprocess.Create(nil);
      SoundPlayerAsyncProcess.CurrentDirectory := ExtractFileDir(szSoundFilename);
      SoundPlayerAsyncProcess.Executable :=
        FindDefaultExecutablePath(fPlayCommand);
      SoundPlayerAsyncProcess.Parameters.Clear;
      SoundPlayerAsyncProcess.Parameters.Add(szSoundFilename);
      try
        SoundPlayerAsyncProcess.Execute;
      except
        On E: Exception do
          E.CreateFmt('Playstyle=paASync: ' + C_UnableToPlay +
            '%s Message:%s', [szSoundFilename, E.Message]);
      end;
    end
    else
    begin
      if SoundPlayerSyncProcess = nil then
        SoundPlayerSyncProcess := Tprocess.Create(nil);
      SoundPlayerSyncProcess.CurrentDirectory := ExtractFileDir(szSoundFilename);
      SoundPlayerSyncProcess.Executable :=
        FindDefaultExecutablePath(fPlayCommand);
      SoundPlayersyncProcess.Parameters.Clear;
      SoundPlayerSyncProcess.Parameters.Add(szSoundFilename);
      try
        SoundPlayerSyncProcess.Execute;
        SoundPlayersyncProcess.WaitOnExit;
      except
        On E: Exception do
          E.CreateFmt('Playstyle=paSync: ' + C_UnableToPlay +
            '%s Message:%s', [szSoundFilename, E.Message]);
      end;
    end;
  end
  else
    raise Exception.CreateFmt('The play command %s does not work on your system',
      [fPlayCommand]);
{$ENDIF}
end;

procedure Register;
begin
  RegisterComponents('LazControls', [Tplaysound]);
  {$I playsound_icon.lrs}
end;

end.