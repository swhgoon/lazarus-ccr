unit CToPasExp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, prsExpression;

function PascalizeCExp(CExp: TExpEntity; PascalExps : TList): Boolean;

implementation

function PascalizeCExp(CExp: TExpEntity; PascalExps : TList): Boolean;
begin
  Result:=False;
end;

end.

