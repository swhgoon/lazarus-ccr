program testcmdlineparse;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes
  , cmdlinecfg, cmdlinecfgutils, cmdlinecfgparser, cmdlinecfgjson
  { you can add units after this };


procedure TestConfigLineParam(const cmdlinefn, conffn: string);
var
  cfg : TcmdLineCfg;
  prm : TStringList;
  result :TList;
  i      : integer;
  v   : TCmdLineOptionValue;
  p   : string;
begin
  prm:=TStringList.Create;
  result:=TList.Create;
  try
    prm.LoadFromFile(cmdlinefn);
    p:=prm.Text;
    prm.Clear;
    CmdLineParse(p, prm);
    writeln('total input arguments:  ', prm.Count);
    for i:=0 to prm.Count-1 do writeln(prm[i]);
    writeln;

    if FileExists(conffn) then begin
      cfg:=TCmdLineCfg.Create;
      try
        CmdLineCfgJSONReadFile(conffn, cfg);

        CmdLineMatchArgsToOpts(cfg, prm, result);
        writeln('known values: ');
        for i:=0 to result.Count-1 do begin
          v:=TCmdLineOptionValue(result[i]);
          if v.Option = nil then Continue;
          writeln(' ', v.Option.Key,' (',v.Option.Display,')');
          writeln(' value: ', v.Value);
        end;
        writeln;

        writeln('unknown values: ');
        for i:=0 to result.Count-1 do begin
          v:=TCmdLineOptionValue(result[i]);
          if v.Option <> nil then Continue;
          writeln(' ',v.Value);
        end;
      finally
        cfg.Free;
      end;
    end;

  finally
    prm.Free;
    result.Free;
  end;
end;

begin
  if Paramcount=0 then begin
    writeln('Please specify file name that contains command lines to be parsed');
    Exit;
  end;
  if ParamCount=1 then begin
    writeln('Simple command line parsing test');
    TestConfigLineParam(ParamStr(1), '');
  end else if PAramCount=2 then begin
    writeln('Command line to configuration parsing test');
    TestConfigLineParam(ParamSTr(1), Paramstr(2));
  end;
end.

