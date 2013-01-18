{$mode objfpc}{$H+}
program fpmake;

{$IFDEF COMPILED_BY_FPPKG}
{$IFNDEF HAS_PACKAGE_LAZMKUNIT}
{$FATAL This package depends on the lazmkunit package which is not installed}
{$ENDIF}
{$ENDIF}

// By default build for Gecko9, because embedding Gecko 10 and 11 is impossible
// on linux due to bug https://bugzilla.mozilla.org/show_bug.cgi?id=720682
{$define gecko9}

uses fpmkunit, sysutils, lazmkunit;

Var
  P : TLazPackage;
  T : TTarget;

begin
  With Installer(TLazInstaller) do
    begin
    P:=AddPackage('gecko') as TLazPackage;
    p.AfterInstall := @TLazInstaller(Installer).DoRegisterLazarusPackages;

    P.Version:='2.9.0-2';
    P.OSes:=AllUnixOSes+[Win32,Win64];
    P.Author := 'Takanori Ito';
    P.License := 'MPL 1.1';
    P.HomepageURL := 'http://wiki.lazarus.freepascal.org/GeckoPort';
    P.Email := 'joshyfun@gmail.com';
    P.Description := 'Gecko headers and components to use Gecko in Free Pascal or Lazarus using XPCom.';
    P.Options.add('-Sm');
    P.Options.add('-Sd');


    {$ifdef gecko11}
    P.Options.add('-dgecko11');
    P.IncludePath.Add('gecko11');
    {$endif gecko11}
    {$ifdef gecko9}
    P.IncludePath.Add('gecko9');
    {$endif gecko9}
    P.IncludePath.Add('gecko10');

    P.Dependencies.Add('lazmkunit');
    P.Dependencies.Add('fcl-registry',AllWindowsOSes);
    // Due to a bug in fpcmake, the dependencies of fcl-registry aren't processed
    P.Dependencies.Add('fcl-base',AllWindowsOSes); 

    P.Targets.AddUnit('nsTypes.pas');
    P.Targets.AddUnit('nsCID.pas');
    P.Targets.AddUnit('nsConsts.pas');

    with P.Targets.AddUnit('nsGeckoStrings.pas') do
      begin
      Dependencies.AddUnit('nsInit');
      Dependencies.AddUnit('nsMemory');
      Dependencies.AddUnit('nsError');
      Dependencies.AddInclude('domstubs.inc');
      Dependencies.AddInclude('forwarddecl.inc');

      Dependencies.AddInclude('nsidomdocument.inc');
      Dependencies.AddInclude('nsidomelement.inc');
      Dependencies.AddInclude('nsishentry.inc');
      Dependencies.AddInclude('nsiwebprogresslistener.inc');
      Dependencies.AddInclude('nsidomnodeiterator.inc');
      Dependencies.AddInclude('nsioutputstream.inc');
      Dependencies.AddInclude('nsidomcharacterdata.inc');
      Dependencies.AddInclude('nsidomwindow.inc');
      Dependencies.AddInclude('nsicontentsecuritypolicy.inc');
      Dependencies.AddInclude('nsidomfileerror.inc');
      Dependencies.AddInclude('nsidomeventtarget.inc');
      Dependencies.AddInclude('nsiwebbrowsersetup.inc');
      Dependencies.AddInclude('nsiprintsession.inc');
      Dependencies.AddInclude('nsiwebbrowserchromefocus.inc');
      Dependencies.AddInclude('nsiinterfacerequestor.inc');
      Dependencies.AddInclude('nsidomscreen.inc');
      Dependencies.AddInclude('nsidommediaquerylist.inc');
      Dependencies.AddInclude('nsidomofflineresourcelist.inc');
      Dependencies.AddInclude('nsicomponentregistrar.inc');
      Dependencies.AddInclude('nsipromptservice2.inc');
      Dependencies.AddInclude('imgidecoderobserver.inc');
      Dependencies.AddInclude('nsidommimetype.inc');
      Dependencies.AddInclude('nsicollection.inc');
      Dependencies.AddInclude('nsidomeventlistener.inc');
      Dependencies.AddInclude('nsidomnamednodemap.inc');
      Dependencies.AddInclude('nsidocshellloadinfo.inc');
      Dependencies.AddInclude('nsibfcacheentry.inc');
      Dependencies.AddInclude('nsipromptservice.inc');
      Dependencies.AddInclude('nsiioservice.inc');
      Dependencies.AddInclude('nsiwebnavigation.inc');
      Dependencies.AddInclude('nsiprincipal.inc');
      Dependencies.AddInclude('nsidomevent.inc');
      Dependencies.AddInclude('nsidompkcs11.inc');
      Dependencies.AddInclude('nsidomstylesheet.inc');
      Dependencies.AddInclude('nsiobjectinputstream.inc');
      Dependencies.AddInclude('nsibinaryinputstream.inc');
      Dependencies.AddInclude('nsiservicemanager.inc');
      Dependencies.AddInclude('nsicontentviewer.inc');
      Dependencies.AddInclude('nsiauthpromptcallback.inc');
      Dependencies.AddInclude('nsidomlocation.inc');
      Dependencies.AddInclude('imgirequest.inc');
      Dependencies.AddInclude('nsicontroller.inc');
      Dependencies.AddInclude('nsiprotocolhandler.inc');
      Dependencies.AddInclude('nsishistory.inc');
      Dependencies.AddInclude('nsilocalfile.inc');
      Dependencies.AddInclude('nsidomcdatasection.inc');
      Dependencies.AddInclude('nsidomstoragelist.inc');
      Dependencies.AddInclude('nsidomnavigator.inc');
      Dependencies.AddInclude('nsidomnodelist.inc');
      Dependencies.AddInclude('nsidomstorageitem.inc');
      Dependencies.AddInclude('nsiwebbrowserfind.inc');
      Dependencies.AddInclude('nsiprompt.inc');
      Dependencies.AddInclude('nsidomdocumenttype.inc');
      Dependencies.AddInclude('nsibinaryoutputstream.inc');
      Dependencies.AddInclude('nsidocshelltreeitem.inc');
      Dependencies.AddInclude('nsidocumentcharsetinfo.inc');
      Dependencies.AddInclude('nsidocshell.inc');
      Dependencies.AddInclude('nsistreamlistener.inc');
      Dependencies.AddInclude('nsirequest.inc');
      Dependencies.AddInclude('nsisupportsarray.inc');
      Dependencies.AddInclude('nsicommandparams.inc');
      Dependencies.AddInclude('nsiweakreference.inc');
      Dependencies.AddInclude('nsihttpheadervisitor.inc');
      Dependencies.AddInclude('nsidomnodefilter.inc');
      Dependencies.AddInclude('nsiauthinformation.inc');
      Dependencies.AddInclude('nsiserializable.inc');
      Dependencies.AddInclude('nsidomxulcommanddispatcher.inc');
      Dependencies.AddInclude('nsicomponentmanager.inc');
      Dependencies.AddInclude('nsidomperformancenavigation.inc');
      Dependencies.AddInclude('nsidomperformance.inc');
      Dependencies.AddInclude('nsiloadgroup.inc');
      Dependencies.AddInclude('nsidommimetypearray.inc');
      Dependencies.AddInclude('nsidomprocessinginstruction.inc');
      Dependencies.AddInclude('nsiwindowcreator.inc');
      Dependencies.AddInclude('nsidomfile.inc');
      Dependencies.AddInclude('nsidomdomstringlist.inc');
      Dependencies.AddInclude('nsiembeddingsitewindow.inc');
      Dependencies.AddInclude('nsidomhistory.inc');
      Dependencies.AddInclude('nsichannel.inc');
      Dependencies.AddInclude('nsidebug.inc');
      Dependencies.AddInclude('nsicancelable.inc');
      Dependencies.AddInclude('imgicontainerobserver.inc');
      Dependencies.AddInclude('nsidomattr.inc');
      Dependencies.AddInclude('nsidocshelltreenode.inc');
      Dependencies.AddInclude('nsienumerator.inc');
      Dependencies.AddInclude('nsidomstorageobsolete.inc');
      Dependencies.AddInclude('nsidirectoryservice.inc');
      Dependencies.AddInclude('nsiwebbrowser.inc');
      Dependencies.AddInclude('nsidomnode.inc');
      Dependencies.AddInclude('nsiproperties.inc');
      Dependencies.AddInclude('nsisimpleenumerator.inc');
      Dependencies.AddInclude('nsidomcrypto.inc');
      Dependencies.AddInclude('nsifile.inc');
      Dependencies.AddInclude('nsiprintsettings.inc');
      Dependencies.AddInclude('nsirequestobserver.inc');
      Dependencies.AddInclude('nsidomcomment.inc');
      Dependencies.AddInclude('nsiwebprogress.inc');
      Dependencies.AddInclude('nsishistorylistener.inc');
      Dependencies.AddInclude('nsimemory.inc');
      Dependencies.AddInclude('nsiwebbrowserchrome.inc');
      Dependencies.AddInclude('nsianimationframelistener.inc');
      Dependencies.AddInclude('nsidomuserdatahandler.inc');
      Dependencies.AddInclude('nsivariant.inc');
      Dependencies.AddInclude('nsiwebbrowserfocus.inc');
      Dependencies.AddInclude('nsidompluginarray.inc');
      Dependencies.AddInclude('nsiinputstream.inc');
      Dependencies.AddInclude('nsiwebbrowserprint.inc');
      Dependencies.AddInclude('nsihistoryentry.inc');
      Dependencies.AddInclude('nsidomplugin.inc');
      Dependencies.AddInclude('nsiwindowwatcher.inc');
      Dependencies.AddInclude('nsitracerefcnt.inc');
      Dependencies.AddInclude('nsidomperformancetiming.inc');
      Dependencies.AddInclude('nsidomstylesheetlist.inc');
      Dependencies.AddInclude('nsidombarprop.inc');
      Dependencies.AddInclude('nsiatom.inc');
      Dependencies.AddInclude('nsitooltiplistener.inc');
      Dependencies.AddInclude('nsiselection.inc');
      Dependencies.AddInclude('nsidomtreewalker.inc');
      Dependencies.AddInclude('nsidomdocumentfragment.inc');
      Dependencies.AddInclude('imgicontainer.inc');
      Dependencies.AddInclude('nsiuri.inc');
      Dependencies.AddInclude('nsiauthprompt.inc');
      Dependencies.AddInclude('nsimodule.inc');
      Dependencies.AddInclude('nsicontrollers.inc');
      Dependencies.AddInclude('nsidomwindowcollection.inc');
      Dependencies.AddInclude('nsifactory.inc');
      Dependencies.AddInclude('nsidommedialist.inc');
      Dependencies.AddInclude('nsiobserver.inc');
      Dependencies.AddInclude('nsidocshelltreeowner.inc');
      Dependencies.AddInclude('nsiuricontentlistener.inc');
      Dependencies.AddInclude('nsieventsource.inc');
      Dependencies.AddInclude('nsidomdomimplementation.inc');
      Dependencies.AddInclude('nsihttpchannel.inc');
      Dependencies.AddInclude('nsibasewindow.inc');
      Dependencies.AddInclude('nsisecurebrowserui.inc');
      Dependencies.AddInclude('nsiobjectoutputstream.inc');
      Dependencies.AddInclude('nsidomstorage.inc');
      Dependencies.AddInclude('nsicontextmenulistener2.inc');
      Dependencies.AddInclude('nsidomtext.inc');
      {$ifdef gecko11}
      Dependencies.AddInclude('nsidomdomtokenlist.inc');
      Dependencies.AddInclude('nsidomdomclientrect.inc');
      Dependencies.AddInclude('nsidomclientrectlist.inc');
      Dependencies.AddInclude('nsiframerequestcallback.inc');
      {$endif gecko11}
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

{   This part does not compile and is not used anywhere...
    P.Sources.AddSrc('nsProfile.pas');
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
}

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

    P.LazPackageFiles.AddLazPackageTemplate('Components/GeckoComponents.template');
    P.LazPackageFiles.AddLazFile('Components/BrowserSupports.pas');
    P.LazPackageFiles.AddLazFile('Components/CallbackInterfaces.pas');
    P.LazPackageFiles.AddLazFile('Components/GeckoBrowser.pas');
    P.LazPackageFiles.AddLazFile('Components/GeckoBrowser.lrs');
    P.LazPackageFiles.AddLazFile('Components/GeckoPromptService.pas');
    P.LazPackageFiles.AddLazFile('Components/GeckoChromeWindow.pas');
    P.LazPackageFiles.AddLazFile('Components/GeckoChromeWindow.lfm');
    P.LazPackageFiles.AddLazFile('Components/GeckoChromeWindow.lrs');
    P.LazPackageFiles.AddLazFile('Components/GeckoComponents.pas');
    P.LazPackageFiles.AddLazFile('Components/GeckoDirectoryService.pas');
    P.LazPackageFiles.AddLazFile('Components/GeckoInit.pas');
    P.LazPackageFiles.AddLazFile('Components/GeckoSimpleProfile.pas');
    P.LazPackageFiles.AddLazFile('Components/geckoresources.rc');
    P.LazPackageFiles.AddLazFile('Components/geckologo.png');

    Run;
    end;
end.

