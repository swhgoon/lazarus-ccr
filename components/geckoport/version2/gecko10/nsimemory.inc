type

  nsIMemory = interface(nsISupports)
  ['{59e7e77a-38e4-11d4-8cf5-0060b0fc14a3}']
    function alloc(size: size_t) : voidPtr; safecall;
    function realloc(ptr: voidPtr; newSize: size_t) : voidPtr; safecall;
    procedure free(ptr: voidPtr); safecall;
    procedure heapMinimize(immediate: longbool); safecall;
    function isLowMemory() : longbool; safecall;

  end;

