program testcompconfread;

{$mode delphi}{$H+}

uses
  SysUtils, Classes, cmdlinecfg, cmdlinecfgjson, cmdlinecfgutils
  { you can add units after this };

var
  cfg : TCmdLineCfg;
begin
  if Paramcount=0 then begin
    writeln('please provide the configuration file name');
    exit;
  end;
  try
    cfg := TCmdLineCfg.Create;
    try
      CmdLineCfgJSONReadFile(ParamStr(1), cfg);
      CmdLineDebug(cfg);
    finally
      cfg.Free;
    end;
  except
    on e: Exception do
      writeln(e.message);
  end;
end.

