{
This unit has been produced by ws_helper.
  Input unit name : "IDelphiChess".
  This unit name  : "IDelphiChess".
  Date            : "10/17/10 08:24:54 AM".
}
unit IDelphiChessWSDL;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$IFNDEF FPC}
  {$DEFINE WST_RECORD_RTTI}
{$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'http://eBob42.org/';
  sUNIT_NAME = 'IDelphiChess';

type
  IDelphiChess = interface(IInvokable)
    ['{ECE02B6C-B051-2815-AE82-B2969FFEDC2A}']
    function XML_GetNextMove(
      const  Position : string; 
      const  WhiteMovesNext : boolean; 
      const  SearchDepth : integer
    ):string;
  end;

procedure Register_IDelphiChess_ServiceMetadata();

Implementation

uses metadata_repository, record_rtti, wst_types;

procedure Register_IDelphiChess_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'IDelphiChess',
    'TRANSPORT_Address',
    'http://www.bobswart.nl/cgi-bin/ChessISAPIServer.dll/soap/IDelphiChess'
  );
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'IDelphiChess',
    'FORMAT_Style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IDelphiChess',
    'XML_GetNextMove',
    '_E_N_',
    'XML_GetNextMove'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IDelphiChess',
    'XML_GetNextMove',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IDelphiChess',
    'XML_GetNextMove',
    'TRANSPORT_soapAction',
    'urn:DelphiChess-IDelphiChess#XML_GetNextMove'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IDelphiChess',
    'XML_GetNextMove',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'IDelphiChess',
    'XML_GetNextMove',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
end;

var
  typeRegistryInstance : TTypeRegistry = nil;
initialization
  typeRegistryInstance := GetTypeRegistry();
end.
