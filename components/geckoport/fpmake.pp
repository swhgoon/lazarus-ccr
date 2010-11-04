{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('gecko');

    P.Version:='0.9.0-1';
    P.OSes:=AllUnixOSes+[Win32,Win64];
    P.Author := 'Takanori Ito';
    P.License := 'MPL 1.1';
    P.HomepageURL := 'http://wiki.lazarus.freepascal.org/GeckoPort';
    P.Email := 'joshyfun@gmail.com';
    P.Description := 'Gecko headers and components to use Gecko in Free Pascal or Lazarus using XPCom.';
    P.Options.add('-Sm');
    P.Options.add('-Sd');

    P.Dependencies.Add('fcl-registry',AllWindowsOSes);

    P.Targets.AddUnit('nsTypes.pas');
    P.Targets.AddUnit('nsCID.pas');
    P.Targets.AddUnit('nsConsts.pas');

    with P.Targets.AddUnit('nsGeckoStrings.pas') do
      begin
      Dependencies.AddUnit('nsInit');
      Dependencies.AddUnit('nsMemory');
      Dependencies.AddUnit('nsError');
      end;

    with P.Targets.AddUnit('nsInit.pas') do
      begin
      Dependencies.AddUnit('nsXPCOM');
      Dependencies.AddUnit('nsConsts');
      Dependencies.AddUnit('nsTypes');
      Dependencies.AddUnit('nsGeckoStrings');
      end;

    with P.Targets.AddUnit('nsXPCOM.pas') do
      begin
      Dependencies.AddUnit('nsGeckoStrings');
      Dependencies.AddUnit('nsTypes');
      end;

    with P.Targets.AddUnit('nsError.pas') do
      begin
      Dependencies.AddUnit('nsTypes');
      end;

    with P.Targets.AddUnit('nsErrorUtils.pas') do
      begin
      Dependencies.AddUnit('nsTypes');
      Dependencies.AddUnit('nsInit');
      Dependencies.AddUnit('nsXPCOMGlue');
      Dependencies.AddUnit('nsXPCOM');
      Dependencies.AddUnit('nsConsts');
      Dependencies.AddUnit('nsError');
      end;

    with P.Targets.AddUnit('nsMemory.pas') do
      begin
      Dependencies.AddUnit('nsXPCOM');
      Dependencies.AddUnit('nsError');
      Dependencies.AddUnit('nsInit');
      end;

    with P.Targets.AddUnit('nsXPCOMGlue.pas') do
      begin
      Dependencies.AddUnit('nsXPCOM');
      Dependencies.AddUnit('nsTypes');
      ResourceStrings := true;
      end;

    // Is this unit used at all?!?
    with P.Targets.AddUnit('nsProfile.pas',AllWindowsOSes) do
      begin
      Dependencies.AddUnit('nsXPCOM');
      Dependencies.AddUnit('nsXPCOMGlue');
      Dependencies.AddUnit('nsTypes');
      end;

    with P.Targets.AddUnit('nsEnumerators.pas') do
      begin
      Dependencies.AddUnit('nsTypes');
      Dependencies.AddUnit('nsXPCOM');
      Dependencies.AddUnit('nsXPCOM_std19');
      end;

    with P.Targets.AddUnit('each-version\nsXPCOM_std19.pas') do
      begin
      Dependencies.AddUnit('nsTypes');
      Dependencies.AddUnit('nsGeckoStrings');
      end;

    with P.Targets.AddUnit('nsNetUtil.pas') do
      begin
      Dependencies.AddUnit('nsTypes');
      Dependencies.AddUnit('nsXPCOM');
      Dependencies.AddUnit('nsGeckoStrings');
      Dependencies.AddUnit('nsConsts');
      Dependencies.AddUnit('nsXPCOMGlue');
      Dependencies.AddUnit('nsError');
      Dependencies.AddUnit('nsInit');
      ResourceStrings := true;
      end;

    with P.Targets.AddUnit('nsStream.pas') do
      begin
      Dependencies.AddUnit('nsTypes');
      Dependencies.AddUnit('nsXPCOM');
      Dependencies.AddUnit('nsError');
      end;

    with P.Targets.AddUnit('nsXRE.pas') do
      begin
      Dependencies.AddUnit('nsTypes');
      Dependencies.AddUnit('nsXPCOM');
      Dependencies.AddUnit('nsGeckoStrings');
      Dependencies.AddUnit('nsError');
      Dependencies.AddUnit('nsInit');
      end;

{    with P.Targets.AddUnit('nsThreadUtil.pas') do
      begin
      Dependencies.AddUnit('nsTypes');
      Dependencies.AddUnit('nsXPCOM');
      Dependencies.AddUnit('nsGeckoStrings');
      Dependencies.AddUnit('nsError');
      end;}

{    with P.Targets.AddUnit('each-version\nsXPCOM_safe19.pas') do
      begin
      Dependencies.AddUnit('nsTypes');
      Dependencies.AddUnit('nsConsts');
      Dependencies.AddUnit('nsGeckoStrings');
      end;}

    P.Targets.AddExampleProgram('SampleApps/ChromeWin.dpr');
    P.Sources.AddExample('SampleApps/ChromeWin.dpr','examples');
    P.Sources.AddExample('SampleApps/ChromeWin.lpi','examples');
    P.Sources.AddExample('SampleApps/gec10.pas','examples');
    P.Sources.AddExample('SampleApps/gec10.lfm','examples');
    P.Sources.AddExample('SampleApps/lb-cw-cocoa.sh','examples');
    P.Sources.AddExample('SampleApps/lb-gb-cocoa.sh','examples');
    P.Sources.AddExample('SampleApps/run-bw-mac.sh','examples');
    P.Sources.AddExample('SampleApps/run-cw-mac.sh','examples');
    P.Sources.AddExample('SampleApps/run-gb-mac.sh','examples');
    P.Sources.AddExample('SampleApps/GBrowser.lpi','examples');
    P.Sources.AddExample('SampleApps/GBrowser.dpr','examples');

    P.Sources.AddDoc('Components/GeckoComponentsFP.lpk','lazaruspackage');
    P.Sources.AddDoc('Components/BrowserSupports.pas','lazaruspackage');
    P.Sources.AddDoc('Components/CallbackInterfaces.pas','lazaruspackage');
    P.Sources.AddDoc('Components/GeckoBrowser.pas','lazaruspackage');
    P.Sources.AddDoc('Components/GeckoBrowser.lrs','lazaruspackage');
    P.Sources.AddDoc('Components/GeckoChromeWindow.pas','lazaruspackage');
    P.Sources.AddDoc('Components/GeckoChromeWindow.lfm','lazaruspackage');
    P.Sources.AddDoc('Components/GeckoChromeWindow.lrs','lazaruspackage');
    P.Sources.AddDoc('Components/GeckoComponents.pas','lazaruspackage');
    P.Sources.AddDoc('Components/GeckoDirectoryService.pas','lazaruspackage');
    P.Sources.AddDoc('Components/GeckoInit.pas','lazaruspackage');
    P.Sources.AddDoc('Components/GeckoSimpleProfile.pas','lazaruspackage');

    Run;
    end;
end.

