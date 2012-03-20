#!/bin/sh
if [ "$#" -ne 1 ] 
then
  echo "Please supply the directory of the .idl packages"
  exit
fi
if [ ! -d $1 ] 
then
  echo "Directory not found"
  exit
fi

idltopas $1/nsIDOMDocument.idl \
    $1/nsIDOMElement.idl \
    $1/nsISHEntry.idl \
    -m idltypemap.cfg -p -f forwarddecl.inc -o

idltopas $1/nsIWebProgressListener.idl \
    $1/nsIDOMNodeIterator.idl \
    $1/nsIOutputStream.idl \
    $1/nsIDOMCharacterData.idl \
    $1/nsIDOMWindow.idl \
    $1/nsIContentSecurityPolicy.idl \
    $1/nsIDOMFileError.idl \
    $1/nsIDOMEventTarget.idl \
    $1/nsIWebBrowserSetup.idl \
    $1/nsIPrintSession.idl \
    $1/nsIWebBrowserChromeFocus.idl \
    $1/nsIInterfaceRequestor.idl \
    $1/nsIDOMScreen.idl \
    $1/nsIDOMMediaQueryList.idl \
    $1/nsIDOMOfflineResourceList.idl \
    $1/nsIComponentRegistrar.idl \
    $1/nsIPromptService2.idl \
    $1/imgIDecoderObserver.idl \
    $1/nsIDOMMimeType.idl \
    $1/nsICollection.idl \
    $1/nsIDOMEventListener.idl \
    $1/nsIDOMNamedNodeMap.idl \
    $1/nsIDocShellLoadInfo.idl \
    $1/nsIBFCacheEntry.idl \
    $1/nsIPromptService.idl \
    $1/nsIIOService.idl \
    $1/nsIWebNavigation.idl \
    $1/nsIPrincipal.idl \
   $1/nsIDOMEvent.idl \
   $1/nsIDOMPkcs11.idl \
   $1/nsIDOMStyleSheet.idl \
   $1/nsIObjectInputStream.idl \
   $1/nsIBinaryInputStream.idl \
   $1/nsIServiceManager.idl \
   $1/nsIContentViewer.idl \
   $1/nsIAuthPromptCallback.idl \
   $1/nsIDOMLocation.idl \
   $1/imgIRequest.idl \
   $1/nsIController.idl \
   $1/nsIProtocolHandler.idl \
   $1/nsISHistory.idl \
   $1/nsILocalFile.idl \
   $1/nsIDOMCDATASection.idl \
   $1/nsIDOMStorageList.idl \
   $1/nsIDOMNavigator.idl \
   $1/nsIDOMNodeList.idl \
   $1/nsIDOMStorageItem.idl \
   $1/nsIWebBrowserFind.idl \
   $1/nsIPrompt.idl \
   $1/nsIDOMDocumentType.idl \
   $1/nsIBinaryOutputStream.idl \
   $1/nsIDocShellTreeItem.idl \
   $1/nsIDocumentCharsetInfo.idl \
   $1/nsIDocShell.idl \
   $1/nsIStreamListener.idl \
   $1/nsIRequest.idl \
   $1/nsISupportsArray.idl \
   $1/nsICommandParams.idl \
   $1/nsIWeakReference.idl \
   $1/nsIHttpHeaderVisitor.idl \
   $1/nsIDOMNodeFilter.idl \
   $1/nsIAuthInformation.idl \
   $1/nsISerializable.idl \
   $1/nsIDOMXULCommandDispatcher.idl \
   $1/nsIComponentManager.idl \
   $1/nsIDOMPerformanceNavigation.idl \
   $1/nsIDOMPerformance.idl \
   $1/nsILoadGroup.idl \
   $1/nsIDOMMimeTypeArray.idl \
   $1/nsIDOMProcessingInstruction.idl \
   $1/nsIWindowCreator.idl \
   $1/nsIDOMFile.idl \
   $1/nsIDOMDOMStringList.idl \
   $1/nsIEmbeddingSiteWindow.idl \
   $1/nsIDOMHistory.idl \
   $1/nsIChannel.idl \
   $1/nsIDebug.idl \
   $1/nsICancelable.idl \
   $1/imgIContainerObserver.idl \
   $1/nsIDOMAttr.idl \
   $1/nsIDocShellTreeNode.idl \
   $1/nsIEnumerator.idl \
   $1/nsIDOMStorageObsolete.idl \
   $1/nsIDirectoryService.idl \
   $1/nsIWebBrowser.idl \
   $1/nsIDOMNode.idl \
   $1/nsIProperties.idl \
   $1/nsISimpleEnumerator.idl \
   $1/nsIDOMCrypto.idl \
   $1/nsIFile.idl \
   $1/nsIPrintSettings.idl \
   $1/nsIRequestObserver.idl \
   $1/nsIDOMComment.idl \
   $1/nsIWebProgress.idl \
   $1/nsISHistoryListener.idl \
   $1/nsIMemory.idl \
   $1/nsIWebBrowserChrome.idl \
# This idl was removed from Gecko 11 
#  $1/nsIAnimationFrameListener.idl \
   $1/nsIDOMUserDataHandler.idl \
   $1/nsIVariant.idl \
   $1/nsIWebBrowserFocus.idl \
   $1/nsIDOMPluginArray.idl \
   $1/nsIInputStream.idl \
   $1/nsIWebBrowserPrint.idl \
   $1/nsIHistoryEntry.idl \
   $1/nsIDOMPlugin.idl \
   $1/nsIWindowWatcher.idl \
   $1/nsITraceRefcnt.idl \
   $1/nsIDOMPerformanceTiming.idl \
   $1/nsIDOMStyleSheetList.idl \
   $1/nsIDOMBarProp.idl \
   $1/nsIAtom.idl \
   $1/nsITooltipListener.idl \
   $1/nsISelection.idl \
   $1/nsIDOMTreeWalker.idl \
   $1/nsIDOMDocumentFragment.idl \
   $1/imgIContainer.idl \
   $1/nsIURI.idl \
   $1/nsIAuthPrompt.idl \
   $1/nsIModule.idl \
   $1/nsIControllers.idl \
   $1/nsIDOMWindowCollection.idl \
   $1/nsIFactory.idl \
   $1/nsIDOMMediaList.idl \
   $1/nsIObserver.idl \
   $1/nsIDocShellTreeOwner.idl \
   $1/nsIURIContentListener.idl \
   $1/nsIEventSource.idl \
   $1/nsIDOMDOMImplementation.idl \
   $1/nsIHttpChannel.idl \
   $1/nsIBaseWindow.idl \
    $1/nsISecureBrowserUI.idl \
    $1/nsIObjectOutputStream.idl \
    $1/nsIDOMStorage.idl \
    $1/nsIContextMenuListener2.idl \
    $1/nsIDOMText.idl \
# These idl's were added in Gecko 11.
    $1/nsIDOMDOMTokenList.idl \
    $1/nsIDOMClientRectList.idl \
    $1/nsIDOMClientRect.idl \
    $1/nsIFrameRequestCallback.idl \
    -m idltypemap.cfg -f forwarddecl.inc -o
