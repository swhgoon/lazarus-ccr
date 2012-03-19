unit nsXPCOM;

{$mode objfpc}{$H+}

{$MACRO on}

{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall}
{$ELSE Windows}
  {$DEFINE extdecl:=cdecl}
{$ENDIF}


interface

uses
  nsTypes;

{$i forwarddecl.inc}

type
  nsIDomPlugin = interface;

type
  nsISupports = interface
  ['{00000000-0000-0000-c000-000000000046}']
  end;

  nsIScriptContext = interface
  end;

  nsIContent = interface
  end;

  nsPIDOMWindow = interface
  end;

  nsILayoutHistoryState = interface
  end;

  nsIStructuredCloneContainer = interface
  end;


  JSContext = record
  end;

  JSPrincipals = record
  end;

  nsPresContext = pointer;
  nsIPresShell = pointer;
  nsSHEntryShared = pointer;

{$i domstubs.inc}

{$i nsiwebbrowser.inc}
{$i nsiwebbrowserchrome.inc}
{$i nsiinterfacerequestor.inc}
{$i nsiuricontentlistener.inc}
{$i nsIDOMNamedNodeMap.inc}
{$i nsidomnodelist.inc}
{$i nsidommimetype.inc}
{$i nsidomplugin.inc}
{$i nsidompluginarray.inc}
{$i nsidommimetypearray.inc}
{$i nsidomnode.inc}
{$i nsidomcharacterdata.inc}
{$i nsidomtext.inc}
{$i nsidomcdatasection.inc}
{$i nsidomcomment.inc}
{$i nsidomattr.inc}
{$i nsidomelement.inc}
{$i nsidomdocumentfragment.inc}
{$i nsidomdocumenttype.inc}
{$i nsidomdomimplementation.inc}
{$i nsidomprocessinginstruction.inc}
{$i nsidommedialist.inc}
{$i nsidomstylesheet.inc}
{$i nsidomstylesheetlist.inc}
{$i nsidomdocument.inc}
{$i nsidomnodefilter.inc}
{$i nsidomtreewalker.inc}
{$i nsidomhistory.inc}
{$i nsidombarprop.inc}
{$i nsidomnavigator.inc}
{$i nsidomdomstringlist.inc}
{$i nsidomwindowcollection.inc}
{$i nsiweakreference.inc}
{$i nsirequest.inc}
{$i nsirequestobserver.inc}
{$i nsistreamlistener.inc}
{$i nsiuri.inc}
{$i nsivariant.inc}
{$i nsidomuserdatahandler.inc}
{$i nsidomnodeiterator.inc}
{$i nsidomlocation.inc}
{$i nsianimationframelistener.inc}
{$i nsicontroller.inc}
{$i nsicontrollers.inc}
{$i nsidommediaquerylist.inc}
{$i nsidomstorage.inc}
{$i nsiprompt.inc}
{$i nsiselection.inc}
{$i nsidomperformance.inc}
{$i nsicommandparams.inc}
{$i nsiloadgroup.inc}
{$i nsidomofflineresourcelist.inc}
{$i nsisimpleenumerator.inc}
{$i nsidomstorageobsolete.inc}
{$i nsidomstoragelist.inc}
{$i nsidomstorageitem.inc}
{$i nsidomperformancetiming.inc}
{$i nsidomperformancenavigation.inc}
{$i nsidomxulcommanddispatcher.inc}
{$i nsidomevent.inc}
{$i nsidomcrypto.inc}
{$i nsidompkcs11.inc}
{$i nsidomeventtarget.inc}
{$i nsieventsource.inc}
{$i nsiserializable.inc}
{$i nsiprincipal.inc}
{$i nsidomfile.inc}
{$i nsidomfileerror.inc}
{$i nsiinputstream.inc}
{$i nsioutputstream.inc}
{$i nsibinaryoutputstream.inc}
{$i nsibinaryinputstream.inc}
{$i nsiobjectoutputstream.inc}
{$i nsiobjectinputstream.inc}
{$i nsicontentsecuritypolicy.inc}
{$i nsidomscreen.inc}
{$i nsidomwindow.inc}
{$i nsichannel.inc}
{$i nsihttpchannel.inc}
{$i nsidocshell.inc}
{$i nsihttpheadervisitor.inc}
{$i nsicontentviewer.inc}
{$i nsiprintsettings.inc}
{$i nsidocshellloadinfo.inc}
{$i nsiprintsession.inc}
{$i nsiatom.inc}
{$i nsidocumentcharsetinfo.inc}
{$i nsiwebnavigation.inc}
{$i nsihistoryentry.inc}
{$i nsishistory.inc}
{$i nsienumerator.inc}
{$i nsicollection.inc}
{$i nsisupportsarray.inc}
{$i nsibfcacheentry.inc}
{$i nsishentry.inc}
{$i nsidomeventlistener.inc}
{$i nsisecurebrowserui.inc}
{$i nsidocshelltreenode.inc}
{$i nsidocshelltreeitem.inc}
{$i nsidocshelltreeowner.inc}
{$i nsiwebbrowserprint.inc}
{$i nsishistorylistener.inc}
{$i nsiwebprogresslistener.inc}
{$i nsiwebprogress.inc}
{$i nsifile.inc}
{$i nsilocalfile.inc}
{$i nsidirectoryservice.inc}
{$i nsiservicemanager.inc}
{$i nsicomponentmanager.inc}
{$i nsifactory.inc}
{$i nsicomponentregistrar.inc}
{$i nsimemory.inc}
{$i nsidebug.inc}
{$i nsitracerefcnt.inc}
{$i nsimodule.inc}
{$i nsiproperties.inc}
{$i nsiembeddingsitewindow.inc}
{$i nsiwindowcreator.inc}
{$i nsiwindowwatcher.inc}
{$i nsiobserver.inc}
{$i nsiauthprompt.inc}
{$i nsiwebbrowserfind.inc}
{$i nsiwebbrowserchromefocus.inc}
{$i nsitooltiplistener.inc}
{$i nsiioservice.inc}
{$i nsiprotocolhandler.inc}
{$i nsiwebbrowserfocus.inc}
{$i nsibasewindow.inc}
{$i nsicontextmenulistener2.inc}
{$i nsiwebbrowsersetup.inc}
{$i nsiauthinformation.inc}
{$i nsiauthpromptcallback.inc}
{$i nsipromptservice.inc}
{$i nsipromptservice2.inc}
{$i nsicancelable.inc}

{$i imgicontainer.inc}
{$i imgicontainerobserver.inc}
{$i imgidecoderobserver.inc}
{$i imgirequest.inc}


implementation

end.

