type

  imgIDecoderObserver = interface(imgIContainerObserver)
  ['{2e5fa0c4-57f8-4d16-bda3-1daeba9caa34}']
    procedure onStartRequest(aRequest: imgIRequest); safecall;
    procedure onStartDecode(aRequest: imgIRequest); safecall;
    procedure onStartContainer(aRequest: imgIRequest; aContainer: imgIContainer); safecall;
    procedure onStartFrame(aRequest: imgIRequest; aFrame: idlulong); safecall;
    procedure onDataAvailable(aRequest: imgIRequest; aCurrentFrame: longbool; aRect: nsIntRect); safecall;
    procedure onStopFrame(aRequest: imgIRequest; aFrame: idlulong); safecall;
    procedure onStopContainer(aRequest: imgIRequest; aContainer: imgIContainer); safecall;
    procedure onImageIsAnimated(aRequest: imgIRequest); safecall;
    procedure onStopDecode(aRequest: imgIRequest; status: nsresult; statusArg: PWideChar); safecall;
    procedure onStopRequest(aRequest: imgIRequest; aIsLastPart: longbool); safecall;
    procedure onDiscard(aRequest: imgIRequest); safecall;

  end;

