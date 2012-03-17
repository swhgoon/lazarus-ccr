type

  imgIContainerObserver = interface(nsISupports)
  ['{aa2068c1-6b91-4f52-8603-487b14ac5f04}']
    procedure frameChanged(aContainer: imgIContainer; aDirtyRect: nsIntRect); safecall;

  end;

