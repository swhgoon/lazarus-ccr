program testgetversion;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, cmdlinecfg, cmdlinecfgutils, cmdlinecfgjson;

var
  s    : string;
  list : TList;
  cfg  : TCmdLineCfg;
  exe  : string;
  i    : Integer;
begin
  exe := 'fpc';
  if ParamCount>0 then exe:=ParamStr(1);
  writeln('executable: ', exe);
  list := TList.Create;
  try
    CmdLineCfgJSONLoadFilesFromDir( GetCurrentDir, list );
    writeln('found configurations: ', list.Count);
    for i:=0 to list.Count-1 do
      writeln( TCmdLineCfg(list[i]).Version );

    cfg:=CmdLineCfgDetect( list, GetCurrentDir, exe);
    if Assigned(cfg) then begin
      writeln('version detected: ', cfg.Version);
    end else
      writeln('not found');
  finally
    list.Free;
  end;
end.

