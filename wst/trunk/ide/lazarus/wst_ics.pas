{ Ce fichier a été automatiquement créé par Lazarus. Ne pas l'éditer !
  '
  +'Cette source est seulement employée pour compiler et installer le '
  +'paquet.
 }

unit wst_ics;

interface

uses
ics_http_protocol, ics_tcp_protocol, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('wst_ics', @Register); 
end.
