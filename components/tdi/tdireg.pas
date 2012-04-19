unit TDIReg ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources ;

procedure Register;

implementation

Uses TDIClass ;

procedure Register;
begin
  RegisterComponents('TDI', [ TTDINoteBook ]);
end ;


end.

