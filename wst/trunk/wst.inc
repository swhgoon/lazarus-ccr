{$IFNDEF FPC}
  const FPC_RELEASE = 0; 
  const FPC_VERSION = 0;
{$ENDIF}
{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$DEFINE HAS_QWORD}
  {$DEFINE USE_INLINE}
  {$IF( (FPC_VERSION = 2) and (FPC_RELEASE > 0) ) }
    {$define FPC_211}
  {$IFEND}
{$ELSE}
  {$UNDEF HAS_QWORD}
  {$UNDEF USE_INLINE}
{$ENDIF}
