unit cmdlinefpccond;

interface

uses
  SysUtils;

//todo: need to distingiush between cpu and os.
//      the list however, need to come externally

type
  { TFPCConditionCheck }
  TFPCConditionCheck = class(TObject)
  private
    fCndStr: string;
    cnt  : integer;
    fCnd : array of record cpu, os: string end;
    procedure AddSubCond(const cond: string);
    procedure ParseStr(const ACndStr: string);
  public
    constructor Create(const ACndStr: string);
    function isValid(const cpu, os: string): Boolean;
    property CndStr: string read fCndStr;
  end;

implementation

procedure ParseCPUOS(const cpu_os: string; var cpu, os : string);
var
  i : integer;
begin
  //todo: see todo above!
  i:=Pos('-', cpu_os);
  if i>0 then begin
    cpu:=Copy(cpu_os, 1, i-1);
    os:=Copy(cpu_os, i+1, length(cpu_os));
  end else begin
    cpu:=cpu_os;
    os:='';
  end;
end;

{ TFPCConditionCheck }

procedure TFPCConditionCheck.AddSubCond(const cond: string);
var
  os,cpu: string;
begin
  os:=''; cpu:='';
  ParseCPUOS(cond, cpu, os);
  if cpu<>'' then begin
    if cnt=length(fCnd) then begin
      if cnt=0 then SetLength(fCnd, 4)
      else SetLEngth(fCnd, cnt*2);
    end;
    fCnd[cnt].cpu:=AnsiLowerCase(cpu);
    fCnd[cnt].os:=AnsiLowerCase(os);
    inc(cnt);
  end;
end;

procedure TFPCConditionCheck.ParseStr(const ACndStr: string);
var
  i : integer;
  j : integer;
  s   : string;
begin
  j:=1;
  cnt:=0;
  i:=1;
  while i<=length(ACndStr) do begin
    if ACndStr[i] in [','] then begin
      s:=trim(Copy(ACndStr, j, i-j));
      if s<>'' then AddSubCond(s);
      j:=i+1;
    end;
    inc(i);
  end;
  if j<length(ACndStr) then
    AddSubCond( Copy(ACndStr, j, i-j));
  SetLength(fCnd, cnt);
end;

constructor TFPCConditionCheck.Create(const ACndStr: string);
begin
  inherited Create;
  ParseStr(ACndStr);
  fCndStr:=ACndStr;
end;

function TFPCConditionCheck.isValid(const cpu, os: string): Boolean;
var
  i : integer;
  a, b: string;
begin
  if length(fCnd)=0 then begin
    Result:=true;
    Exit;
  end;
  a:=AnsiLowerCase(cpu);
  b:=AnsiLowerCase(os);
  if (cpu='') and (os<>'') then begin
    a:=b;
    b:='';
  end;
  for i:=0 to length(fCnd)-1 do begin
    Result:=// complete match of os and cpu
            ((fCnd[i].os=b) and (fCnd[i].cpu=a))
            // this is the check, when only OS or only CPU is specified as a condition
            // but either CPU or OS has been passed.
            // i.e.
            // "i386" valid for "i386-linux" or "i386-Win32"
            // "darwin" valid for "arm-darwin" or "i386-darwin"
            // note, that if a condition consists only of a single string, it
            // will always be saved into "cpu" field of the check record.
            or ( (fCnd[i].os='') and ((fCnd[i].cpu=a) or (fCnd[i].cpu=b)));
    if Result then Exit;
  end;
  Result:=False;
end;

end.
