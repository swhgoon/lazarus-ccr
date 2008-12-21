unit uUtils;

{$mode objfpc}{$H+}

interface

function Iif(Cond:boolean; const TrueResult:String; const FalseResult:string):string;overload;
function Iif(Cond:boolean; const TrueResult:integer; const FalseResult:integer):integer;overload;

function IfEmpty(const S:String; const ThenReplace:string):string;

implementation

function IfEmpty(const S:String; const ThenReplace:string):string;
begin
  if S = '' then Result:=ThenReplace else Result:=S;
end;

function Iif(Cond:boolean; const TrueResult:String; const FalseResult:string):string;overload;
begin
  if Cond then Result:=TrueResult else Result:=FalseResult;
end;

function Iif(Cond:boolean; const TrueResult:integer; const FalseResult:integer):integer;overload;
begin
  if Cond then Result:=TrueResult else Result:=FalseResult;
end;

end.

