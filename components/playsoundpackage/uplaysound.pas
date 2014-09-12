unit uplaysound;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs
  ,FileUtil{$IFDEF WINDOWS},mmsystem{$ELSE},asyncprocess,process{$ENDIF},aboutplaysound;

type
  TPlayStyle = (psAsync,psSync);
  Tplaysound = class(TAboutPlaySound)
  private
    { Private declarations }
    {$IFDEF LINUX}
    SoundPlayerAsyncProcess:Tasyncprocess;
    SoundPlayerSyncProcess:Tprocess;
    {$ENDIF}
    fPathToSoundFile:String;
    fPlayStyle:TPlayStyle;
  protected
    { Protected declarations }
    procedure PlaySound(Const szSoundFilename:String); virtual;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; reintroduce;
    procedure Execute;
  published
    { Published declarations }
    Property SoundFile:String read fPathToSoundFile write fPathToSoundFile;
    Property PlayStyle:TPlayStyle read fPlayStyle write fPlayStyle default psASync;
  end;

procedure Register;

implementation
{$IFDEF LINUX}
CONST // Defined in mmsystem
  SND_SYNC=0;
  SND_ASYNC=1;
  SND_NODEFAULT=2;
{$ENDIF}
resourcestring
  C_UnableToPlay = 'Unable to play ';
Constructor Tplaysound.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPlayStyle:=psASync;
  fPathToSoundFile:=ProgramDirectory;

  // About Dialog properties
  AboutBoxComponentName:='PlaySound';
  AboutBoxWidth:=400;
  AboutBoxHeight:=400;
  AboutBoxBackgroundColor:=clCream;
  //AboutBoxFontName (string)
  //AboutBoxFontSize (integer)
  AboutBoxVersion:='0.0.1';
  AboutBoxAuthorname:='Gordon Bamber';
  AboutBoxOrganisation:='Public Domain';
  AboutBoxAuthorEmail:='minesadorada@charcodelvalle.com';
  AboutBoxLicenseType:='LGPL';
  AboutBoxDescription:='Plays WAVE sounds in Windows or Linux';
end;
Destructor Tplaysound.Destroy;
begin
  {$IFDEF LINUX}
  FreeAndNil(SoundPlayerSyncProcess);
  FreeAndNil(SoundPlayerAsyncProcess);
  {$ENDIF}
  inherited;
end;
procedure Tplaysound.Execute;
begin
  If Not FileExistsUTF8(fPathToSoundFile) then Exit;
  PlaySound(fPathToSoundFile);
end;

procedure Tplaysound.PlaySound(Const szSoundFilename:String);
Var
  flags:Word;
  linuxplaycommand:String;
begin
linuxplaycommand:='';
{$IFDEF WINDOWS}
        If fPlayStyle = psASync then flags:=SND_ASYNC OR SND_NODEFAULT
        else flags:=SND_SYNC OR SND_NODEFAULT;
        TRY
        sndPlaySound(PChar(szSoundFilename),flags);
        except
          ShowMessage(C_UnableToPlay + szSoundFilename);
        end;
{$ELSE}
       // How to play in Linux? Use generic Linux commands
       // Use asyncprocess to play sound as SND_ASYNC

// Try play
If (FindDefaultExecutablePath('play') <> '') then linuxplaycommand:='play';
// Try aplay
If (linuxplaycommand='') then
   If (FindDefaultExecutablePath('aplay') <> '') Then linuxplaycommand:='aplay';
// Try paplay
If (linuxplaycommand='') then
   If (FindDefaultExecutablePath('paplay') <> '') Then linuxplaycommand:='paplay';
// proceed if we managed to find a valid command
If (linuxplaycommand <> '') then
BEGIN
       If fPlayStyle = psASync then
       begin
            If SoundPlayerAsyncProcess=Nil then SoundPlayerAsyncProcess:=Tasyncprocess.Create(Nil);
            SoundPlayerAsyncProcess.CurrentDirectory:=ExtractFileDir(szSoundFilename);
            SoundPlayerAsyncProcess.Executable:=FindDefaultExecutablePath(linuxplaycommand);
            SoundPlayerAsyncProcess.Parameters.Clear;
            SoundPlayerAsyncProcess.Parameters.Add(szSoundFilename);
            TRY
            SoundPlayerAsyncProcess.Execute;
            except
              ShowMessage('Playstyle=paASync: ' + C_UnableToPlay + szSoundFilename);
            end;
       end
       else
       begin
           If SoundPlayerSyncProcess=Nil then SoundPlayerSyncProcess:=Tprocess.Create(Nil);
           SoundPlayerSyncProcess.CurrentDirectory:=ExtractFileDir(szSoundFilename);
           SoundPlayerSyncProcess.Executable:=FindDefaultExecutablePath(linuxplaycommand);
           SoundPlayersyncProcess.Parameters.Clear;
           SoundPlayerSyncProcess.Parameters.Add(szSoundFilename);
           TRY
           SoundPlayerSyncProcess.Execute;
           SoundPlayersyncProcess.WaitOnExit;
           except
             ShowMessage('Playstyle=paSyncSync: ' + C_UnableToPlay + szSoundFilename);
           end;
       end;
END;
{$ENDIF}
end;

procedure Register;
begin
  RegisterComponents('LazControls',[Tplaysound]);
  {$I playsound_icon.lrs}
end;

end.
