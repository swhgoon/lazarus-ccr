program ViewWith;

{
  Test program for ViewDoc unit.
}

{$IFDEF FPC}
 {$MODE Delphi}
{$ELSE}
 {$APPTYPE CONSOLE}
{$ENDIF} 
{$R+,Q+}

uses
  SysUtils,
  ViewDoc;
  
var
  VwrIdx   : Integer;
  Viewer   : Integer;
  Options  : TViewerOptions;
  InStr    : string;
  ErrorMsg : string;
  Done     : Boolean;

begin

  if ParamCount < 2 then
    begin
    WriteLn('Usage: ViewWith viewername docfilename [-t] [-d]');
    Exit;
    end;

  Viewer := 0;
  for VwrIdx := 1 to GetViewerCount do
    begin
    if SameText(ParamStr(1), GetViewerName(VwrIdx)) then
      Viewer := VwrIdx;
    end;
  if Viewer = 0 then
    WriteLn('Specified viewer not supported - using first viewer found');

  Options := [];
  if FindCmdLineSwitch('t', ['-'], True) then  {Treat file as template?}
    Options := Options + [ovwUseAsTemplate];

  if FindCmdLineSwitch('d', ['-'], True) then  {Delete file before exiting?}
    begin
    Options := Options + [ovwAddToDeleteList];
    Write('File will be deleted when done viewing - is this okay (Y/N)? ');
    ReadLn(InStr);
    if CompareText(InStr, 'y') <> 0 then
      Exit;
    end; 

  if not ViewDocument(ParamStr(2), Viewer, Options, ErrorMsg) then
    begin
    WriteLn(ErrorMsg);
    Exit;
    end; 

  if FindCmdLineSwitch('d', ['-'], True) and FileExists(ParamStr(2)) then
    begin
    repeat
      Write('Press Enter when ready to delete file (or Ctrl+C to exit): ');
      ReadLn(InStr);
      Done := DeleteViewedDocs;
      if not Done then
        WriteLn('  Unable to delete file - may still be open in viewer');
    until Done; 
    end;
end.
  