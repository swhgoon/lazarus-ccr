{$DEFINE USE_SERIALIZE}
{$UNDEF TRemotableTypeInitializer_Initialize}
{$UNDEF WST_TKPROCVAR}
{$UNDEF WST_UNICODESTRING}
{$UNDEF WST_SEMAPHORE_TIMEOUT}

{$WARNINGS OFF}

  {$IFDEF FPC}
    {$mode objfpc}{$H+}
    {$DEFINE HAS_QWORD}
    {$DEFINE HAS_TKBOOL}
    {$UNDEF WST_INTF_DOM}
    //{$DEFINE USE_INLINE}
    {$IF Defined(FPC_VERSION) and (FPC_VERSION = 2)  }
      {$IF Defined(FPC_RELEASE) }
        {$IF Defined(FPC_VERSION) and (FPC_RELEASE > 0) }
          {$define FPC_211}
          {$IF Defined(FPC_VERSION) and (FPC_RELEASE > 2) }
            {$DEFINE WST_TKPROCVAR}
            {$DEFINE WST_UNICODESTRING}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
    {$IF Defined(FPC_211)}
      {$DEFINE HAS_FORMAT_SETTINGS}                                              
    {$IFEND}
    {$IFDEF WINDOWS}
      {$DEFINE WST_SEMAPHORE_TIMEOUT}
    {$ENDIF WINDOWS}
  {$ENDIF}

  {$IFNDEF FPC}
    {$DEFINE WST_DELPHI}
    {$DEFINE DELPHI}
    {$UNDEF HAS_QWORD}
    {$UNDEF HAS_TKBOOL}
    {$UNDEF USE_INLINE}
    {$DEFINE WST_RECORD_RTTI}
    {$DEFINE WST_INTF_DOM}
    {$IFDEF VER150}
      {$DEFINE HAS_FORMAT_SETTINGS}
    {$ENDIF}
    {$IFDEF VER200} // Delphi 2009
      {$DEFINE WST_UNICODESTRING}
      {$DEFINE USE_INLINE}
    {$ENDIF}
    {$DEFINE WST_SEMAPHORE_TIMEOUT}
    {$IFDEF MSWINDOWS}
      {$IFNDEF WINDOWS}
        {$DEFINE WINDOWS}
      {$ENDIF WINDOWS}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CPU86}
    {$DEFINE HAS_COMP}
  {$ENDIF}

{$WARNINGS ON}

