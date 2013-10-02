program testuijson;

{$mode delphi}{$H+}

uses
  Classes, cmdlinecfgui, cmdlinecfguijson;

procedure DebugSection(sc: TLayoutSection);
var
  i : Integer;
begin
  if sc.Name<>'' then writeln(sc.Display, ' (', sc.Name,')');
  for i:=0 to sc.ElemCount-1 do begin
    if sc.Elements[i].ElementType=letSwitch then
      writeln('    ', sc.Elements[i].Name)
    else
      DebugSection(sc.Elements[i]);
  end;
end;

procedure TestUIConfFile(const fn: string);
var
  layout : TCmdLineLayoutInfo;
begin
  layout:=TCmdLineLayoutInfo.Create;
  try
    CmdLineUIJSONReadFile(fn, layout);
    DebugSection(layout.GetSection(''));
  finally
    layout.Free;
  end;
end;

begin
  if ParamCount=0 then begin
    writeln('Please specify UI configuration file');
    Exit;
  end;
  TestUIConfFile(ParamStr(1));
end.

