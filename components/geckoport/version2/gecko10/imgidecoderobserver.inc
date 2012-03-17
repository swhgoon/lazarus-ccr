type

  imgIDecoderObserver = interface(imgIContainerObserver)
  ['{9f6bfbee-9e04-43a0-b8f6-2159973efec8}']
    procedure onStartRequest(aRequest: imgIRequest); safecall;
    procedure onStartDecode(aRequest: imgIRequest); safecall;
    procedure onStartContainer(aRequest: imgIRequest; aContainer: imgIContainer); safecall;
    procedure onStartFrame(aRequest: imgIRequest; aFrame: idlulong); safecall;
    procedure onDataAvailable(aRequest: imgIRequest; aCurrentFrame: longbool; aRect: nsIntRect); safecall;
    procedure onStopFrame(aRequest: imgIRequest; aFrame: idlulong); safecall;
    procedure onStopContainer(aRequest: imgIRequest; aContainer: imgIContainer); safecall;
    procedure onStopDecode(aRequest: imgIRequest; status: nsresult; statusArg: PWideChar); safecall;
    procedure onStopRequest(aRequest: imgIRequest; aIsLastPart: longbool); safecall;
    procedure onDiscard(aRequest: imgIRequest); safecall;

  end;

