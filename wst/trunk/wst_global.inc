{$DEFINE USE_SERIALIZE}
{$UNDEF TRemotableTypeInitializer_Initialize}

{$WARNINGS OFF}

  {$IFDEF FPC}
    {$mode objfpc}{$H+}
    {$DEFINE HAS_QWORD}
    {$DEFINE HAS_TKBOOL}
    {$UNDEF WST_INTF_DOM}
    //{$DEFINE USE_INLINE}
    {$IF Defined(FPC_VERSION) and (FPC_VERSION = 2)  }
      {$IF Defined(FPC_RELEASE) and (FPC_RELEASE > 0) }
        {$define FPC_211}
      {$IFEND}
    {$IFEND}
    {$IF Defined(FPC_211)}
      {$DEFINE HAS_FORMAT_SETTINGS}                                              
    {$IFEND}
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
  {$ENDIF}

  {$IFDEF CPU86}
    {$DEFINE HAS_COMP}
  {$ENDIF}

{$WARNINGS ON}

