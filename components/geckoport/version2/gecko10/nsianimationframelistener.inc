type

  nsIAnimationFrameListener = interface(nsISupports)
  ['{ba240e38-c15a-4fb2-802a-8a48f09331bd}']
    procedure onBeforePaint(timeStamp: DOMTimeStamp); safecall;

  end;

