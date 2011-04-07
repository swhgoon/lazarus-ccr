{
This unit has been produced by ws_helper.
  Input unit name : "IDelphiChess".
  This unit name  : "IDelphiChess_proxy".
  Date            : "10/17/10 08:24:54 AM".
}

Unit IDelphiChess_proxy;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, IDelphiChessWSDL;

type
  TDelphiChess_Proxy=class(TBaseProxy,IDelphiChess)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function XML_GetNextMove(
      const  Position : string; 
      const  WhiteMovesNext : boolean; 
      const  SearchDepth : integer
    ):string;
  end;

  Function wst_CreateInstance_IDelphiChess(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'; const AAddress : string = ''):IDelphiChess;

Implementation

uses wst_resources_imp, metadata_repository;

Function wst_CreateInstance_IDelphiChess(const AFormat : string; const ATransport : string; const AAddress : string):IDelphiChess;
Var
  locAdr : string;
Begin
  locAdr := AAddress;
  if ( locAdr = '' ) then
    locAdr := GetServiceDefaultAddress(TypeInfo(IDelphiChess));
  Result := TDelphiChess_Proxy.Create('IDelphiChess',AFormat+GetServiceDefaultFormatProperties(TypeInfo(IDelphiChess)),ATransport + 'address=' + locAdr);
End;

{ TDelphiChess_Proxy implementation }

class function TDelphiChess_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(IDelphiChess);
end;

function TDelphiChess_Proxy.XML_GetNextMove(
  const  Position : string; 
  const  WhiteMovesNext : boolean; 
  const  SearchDepth : integer
):string;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('XML_GetNextMove', GetTarget(),locCallContext);
      locSerializer.Put('Position', TypeInfo(string), Position);
      locSerializer.Put('WhiteMovesNext', TypeInfo(boolean), WhiteMovesNext);
      locSerializer.Put('SearchDepth', TypeInfo(integer), SearchDepth);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(string), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i IDelphiChess.wst}

  {$IF DECLARED(Register_IDelphiChess_ServiceMetadata)}
  Register_IDelphiChess_ServiceMetadata();
  {$IFEND}
End.
