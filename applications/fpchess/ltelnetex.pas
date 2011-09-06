unit ltelnetex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  lTelnet, lnet,
  sorokinregexpr,
  Forms;

type
  TLTelnetDebugOutProc = procedure (AStr: string) of object;

  { TLTelnetClientEx }

  TLTelnetClientEx = class(TLTelnetClient)
  private
    FOnDebugOut: TLTelnetDebugOutProc;
    RegexObj: TRegExpr;
  public
    LastMsg: string;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function WaitFor(FirstMatch, SecondMatch: string; ATimeOut: Cardinal): Integer;
    property OnDebugOut: TLTelnetDebugOutProc read FOnDebugOut write FOnDebugOut;
  end;

implementation

{ TLTelnetClientEx }

constructor TLTelnetClientEx.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  RegexObj := TRegExpr.Create;
end;

destructor TLTelnetClientEx.Destroy;
begin
  RegexObj.Free;
  inherited Destroy;
end;

{ Returns a zero-based index to which regular expression was found
  or -1 if none was found
}
function TLTelnetClientEx.WaitFor(FirstMatch, SecondMatch: string;
  ATimeOut: Cardinal): Integer;
var
  lMsg: string;
  lRemaining: Integer;
begin
  lRemaining := ATimeOut;
  Result := -1;
  if (FirstMatch = '') and (SecondMatch = '') then Exit;

  repeat
    if GetMessage(lMsg) > 0 then
      if Assigned(OnDebugOut) then OnDebugOut(lMsg);

    LastMsg := lMsg;

    if FirstMatch <> '' then
    begin
      RegexObj.Expression := FirstMatch;
      if RegexObj.Exec(lMsg) then Exit(0);
    end;

    if SecondMatch <> '' then
    begin
      RegexObj.Expression := SecondMatch;
      if RegexObj.Exec(lMsg) then Exit(1);
    end;

    CallAction; // don't forget to make the clock tick :)
    if lRemaining > 0 then // Don't sleep if the routine was called with ATimeOut=0
    begin
      Application.ProcessMessages;
      Sleep(100);
    end;
    Dec(lRemaining, 100);
  until lRemaining <= 0;
end;

end.

