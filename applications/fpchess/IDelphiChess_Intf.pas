unit IDelphiChess_Intf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IDelphiChessWSDL, IDelphiChess_proxy, chessgame, httpsend;

procedure GetNextMoveFromBorlandWS();

implementation

function ReadEntry(const APromp : string):string ;
begin
  Result := '';
  Write(APromp);
  while True do begin
    ReadLn(Result);
    Result := Trim(Result);
    if ( Length(Result) > 0 ) then
      Break;
  end;
end;

procedure GetNextMoveFromBorlandWS();
var
  locService : IDelphiChess;
  rsps : string;
begin
  Register_IDelphiChess_ServiceMetadata();
  //  SYNAPSE_RegisterHTTP_Transport();

  locService := wst_CreateInstance_IDelphiChess();

  rsps := locService.XML_GetNextMove(
    '', True, 5);
end;

end.

