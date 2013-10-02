program testmakeline;

{$mode delphi}{$H+}

uses
  Classes, cmdlinecfg, cmdlinecfgutils, cmdlinecfgjson;

var
  cfg : TCmdLineCfg;
  vals : TList;
begin
  if ParamCount=0 then begin
    writeln('please specify a configuration file');
    Exit;
  end;
  cfg := TCmdLineCfg.Create;
  vals:= TList.Create;
  try
    CmdLineCfgJSONReadFile(ParamStr(1), cfg);
    vals.Add(TCmdLineOptionValue.Create( cfg.FindOption('instructionset'), 'PENTIUM' ));
    vals.Add(TCmdLineOptionValue.Create( cfg.FindOption('generatelinkmap'), '-1' ));
    vals.Add(TCmdLineOptionValue.Create( cfg.FindOption('shownotes'), '1' ));
    vals.Add(TCmdLineOptionValue.Create( cfg.FindOption('showhints'), '1' ));
    writeln( CmdLineMakeOptions ( vals ));
  finally
    vals.Free;
    cfg.Free;
  end;

end.

