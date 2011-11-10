{
  Cactus Jukebox

  Main Project File.

  written by Sebastian Kraft
  sebastian_kraft@gmx.de

  This software is free under the GNU Public License

  (c)2005-2008
}

program cactusjukebox;

{$mode objfpc}{$H+}

{$ifdef CPU86}          //compile with fmod support enabled by default on i386
   {$define fmod}
{$endif}

uses
 {$ifdef Unix}
   cthreads,
 {$endif}
  global_vars, Interfaces, SysUtils, Forms, status, settings, player, graphics,
  laz_synapse, editid3, directories, skin, cdrip, mediacol, BigCoverImg,
  mainform, cddb, debug, config, addradio, streamcol, playerclass, CleanLibrary,
  fmoddyn, lnetbase, guesstag;

var
  invalid_param, skip_config: boolean;
  i:integer;


  {$i cactus_const.inc}

{$R *.res}

begin
  Application.Title:='cactus';
 {$ifdef CactusDebug}
   {$ifdef CactusDebugVerbose}
   CVerbosityLevel:=8;
   {$else}
     {$ifdef Windows}
     CVerbosityLevel:=0;
     {$else}
     CVerbosityLevel:=1;
     {$endif}
   {$endif}
 {$else}
   {$ifdef Windows}
   CVerbosityLevel:=0;
   {$else}
   CVerbosityLevel:=1;
   {$endif}
 {$endif}

  DebugOutLn('', 1);

  DebugOutLn('Cactus Jukebox v'+CACTUS_VERSION, 1);
  DebugOutLn('written by Sebastian Kraft, (c) 2004-2008', 1);
  DebugOutLn('', 1);

  for i:= 1 to paramcount do if (paramstr(i)='-h') or (paramstr(i)='--help') then begin

        DebugOutLn('cactus_jukebox <OPTIONS> FILE', 1);
        DebugOutLn('', 1);
        DebugOutLn(' Command line options:', 1);
        DebugOutLn('    -c      skip config file, overwrite with standard settings', 1);
        DebugOutLn('    -m      MPLayer audio backend', 1);
        {$ifdef fmod}DebugOutLn('    -f      LibFmod audio backend', 1);{$endif}
        DebugOutLn('    -p      start in playermode', 1);
        DebugOutLn('', 1);
        DebugOutLn('    -h/--help  show this help', 1);
        DebugOutLn('', 1);
        halt;
      end;
  DebugOutLn('##### Application init  #####', 2);
  Application.Initialize;

(*
//   Init config object
{$ifdef CactusRPM}
   CactusConfig:=TConfigObject.create(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'.cactusjukebox'+DirectorySeparator+configname);
   CactusConfig.homeDir:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'));
   CactusConfig.DataPrefix:='/usr/share/cactusjukebox/';
   CactusConfig.ConfigPrefix:=CactusConfig.HomeDir+'.cactusjukebox/';
   DebugOutLn('This is Cactus RPM.', 2);
 {$else}
   SetCurrentDir(ExtractFilePath(ParamStr(0)));
   CactusConfig:=TConfigObject.create(CONFIGNAME);
   CactusConfig.HomeDir:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'));
   CactusConfig.DataPrefix:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
   CactusConfig.ConfigPrefix:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
   if DirectoryExists('lib')=false then  mkdir('lib');
{$endif}
*)
//   Init config object
{$ifdef CactusRPM}
   CactusConfig:=TConfigObject.create(PATH_Config+CONFIGNAME);
   CactusConfig.HomeDir:=PATH_Home;
   CactusConfig.DataPrefix:=PATH_Data;
   CactusConfig.ConfigPrefix:=PATH_Config;
   DebugOutLn('This is Cactus RPM.', 2);
 {$else}
   SetCurrentDir(ExtractFilePath(ParamStr(0)));
   CactusConfig:=TConfigObject.create(CONFIGNAME);
   CactusConfig.HomeDir:=PATH_Home;
   CactusConfig.DataPrefix:=PATH_Data;
   CactusConfig.ConfigPrefix:=PATH_Config;
   if DirectoryExists('lib')=false then  mkdir('lib');
{$endif}

  skip_config:=false;
  for i:= 1 to paramcount do if paramstr(i)='-c' then begin
        skip_config:=true
     end;
  if skip_config then begin
                 DebugOutLn('--> Removing old config file', 1);
                 CactusConfig.Clear;
              end;

   if DirectoryExists(CactusConfig.ConfigPrefix)=false then mkdir(CactusConfig.ConfigPrefix);
   if DirectoryExists(CactusConfig.ConfigPrefix+'lib')=false then  mkdir(CactusConfig.ConfigPrefix+'lib');
// end config

  MediaCollection:=TMediaCollectionClass.create;
  StreamCollection:=TStreamCollectionClass.create;
  SkinData:=TSkin.Create('default.xml', CactusConfig.DataPrefix);

  for i:= 1 to paramcount do if (paramstr(i)<>'-c') and (paramstr(i)<>'-p') and (paramstr(i)<>'-f') and (paramstr(i)<>'-m') and (paramstr(i)<>'-h') and (paramstr(i)<>'--help') then begin
        if FileExists(paramstr(i)) then begin
                                         CactusConfig.LoadOnStart:=paramstr(i);
                                      end
                                     else
                                       begin
                                         DebugOutLn('ERROR: file not found: '+paramstr(i), 1);
                                         halt;
                                   end;
     end;

  for i:= 1 to paramcount do if paramstr(i)='-f' then begin
      CactusConfig.AudioBackend:=FMODBACK;
     end;

  for i:= 1 to paramcount do if paramstr(i)='-m' then begin
      CactusConfig.AudioBackend:=MPLAYERBACK;
     end;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(Tplaywin, playwin);
  Application.CreateForm(TEditID3, editid3win);
  EditID3win.Hide;

  main.show;
  main.playermode:=false;
  for i:= 1 to paramcount do if paramstr(i)='-p' then begin
      playwin.show;
      main.playermode:=true;
      main.hide;
      DebugOutLn('starting in player mode', 2);
     end;

  Register_skins;
  DebugOutLn('--> loading skin '+CactusConfig.DataPrefix+'skins/'+CactusConfig.CurrentSkin, 2);
  SkinData.load_skin(CactusConfig.CurrentSkin);


  if CactusConfig.background_scan then begin
    ScanThread:=TScanThread.Create(true);
    ScanThread.tmpcollection.Assign(MediaCollection);
    ScanThread.PTargetCollection:=MediaCollection;
    ScanThread.Resume;
    DebugOutLn('starting scan thread...', 2);
  end;
  {$ifdef win32}
  main.Width:=CactusConfig.WWidth;
  main.Height:=CactusConfig.WHeight;
  {$endif}

  (*
// Search for Plugins, create Pluginlist
  DebugOutLn('##### searching plugins  #####', 2);
  CactusPlugins:=TPluginListClass.Create;
  CactusPlugins.PluginFolder:=CactusConfig.DataPrefix+'plugins'+DirectorySeparator;
//  CactusPlugins.autoload:=true;
  If CactusConfig.PluginsEnabled then CactusPlugins.ScanPluginFolder;
  DebugOut(CactusPlugins.Count, 5);DebugOutLn(' plugins found', 5);
  *)
  DebugOutLn('##### Application running  #####', 2);
  Application.CreateForm(TaddRadioForm, addRadioForm);
  Application.CreateForm(TFrmCleanLibrary, FrmCleanLibrary);
  Application.Run;
end.

