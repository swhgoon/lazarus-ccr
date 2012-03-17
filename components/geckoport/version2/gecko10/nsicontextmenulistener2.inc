type

  nsIContextMenuListener2 = interface(nsISupports)
  ['{7fb719b3-d804-4964-9596-77cf924ee314}']
    procedure onShowContextMenu(aContextFlags: idlulong; aUtils: nsIContextMenuInfo); safecall;

  end;

const
  nsIContextMenuListener2_CONTEXT_NONE=0;
  nsIContextMenuListener2_CONTEXT_LINK=1;
  nsIContextMenuListener2_CONTEXT_IMAGE=2;
  nsIContextMenuListener2_CONTEXT_DOCUMENT=4;
  nsIContextMenuListener2_CONTEXT_TEXT=8;
  nsIContextMenuListener2_CONTEXT_INPUT=16;
  nsIContextMenuListener2_CONTEXT_BACKGROUND_IMAGE=32;

type
  nsIContextMenuInfo = interface(nsISupports)
  ['{2f977d56-5485-11d4-87e2-0010a4e75ef2}']
    function GetmouseEvent(): nsIDOMEvent; safecall;
    property mouseEvent : nsIDOMEvent read GetmouseEvent;
    function GettargetNode(): nsIDOMNode; safecall;
    property targetNode : nsIDOMNode read GettargetNode;
    function GetassociatedLink(): AString; safecall;
    property associatedLink : AString read GetassociatedLink;
    function GetimageContainer(): imgIContainer; safecall;
    property imageContainer : imgIContainer read GetimageContainer;
    function GetimageSrc(): nsIURI; safecall;
    property imageSrc : nsIURI read GetimageSrc;
    function GetbackgroundImageContainer(): imgIContainer; safecall;
    property backgroundImageContainer : imgIContainer read GetbackgroundImageContainer;
    function GetbackgroundImageSrc(): nsIURI; safecall;
    property backgroundImageSrc : nsIURI read GetbackgroundImageSrc;

  end;

