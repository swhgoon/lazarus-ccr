{ The unit is part of Lazarus Chelper package

  Copyright (C) 2010 Dmitry Boyarintsev skalogryz dot lists at gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit toSourceEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, LCLType, LCLIntf, Forms,
  Menus, MenuIntf, SrcEditorIntf, process, LazIDEIntf,
  extconvdialog, converteridesettings, cconvconfig;

procedure Register;

implementation

function DoExtConvert(const t: AnsiString; var EndPos: TPoint): AnsiString;
var
  p     : TProcess;
  d     : AnsiString;
  inp   : AnsiString;
  outp  : AnsiString;
  i, err  : Integer;
  fs    : TFileStream;
  st    : TStringList;
  cmd   : AnsiString;
  tm    : LongWord;
begin
  if t='' then begin
    Result:='';
    EndPos.X:=0;
    EndPos.Y:=0;
    Exit;
  end;
  EndPos.X:=-1;
  EndPos.Y:=-1;

  try
    d:=GetTempDir;
    ForceDirectories(d);
    inp:=IncludeTrailingPathDelimiter(d)+'input.txt';
    outp:=IncludeTrailingPathDelimiter(d)+'output.txt';
    try
      fs:=TFileStream.Create(inp, fmCreate or fmShareDenyNone);
      try
        fs.Write(t[1], length(t));
      finally
        fs.Free;
      end;
    except
      Result:='can''t write input';
    end;

    p:=TProcess.Create(nil);
    try
      cmd:=ExtTool+' ';

      cconvconfig.SaveToFile(ConvFile, converteridesettings.ConvSettings);
      cmd:=cmd+' -cfg "'+ ConvFile +'"';

      if (DefineFile<>'') and FileExists(DefineFile) then
        cmd:=cmd+' -defines "'+DefineFile+'" ';
      cmd:=cmd+' -o "'+outp+'" ';
      cmd:=cmd+'"'+inp+'"';

      p.CommandLine:=cmd;
      d:=p.CommandLine;
      p.Execute;

      tm:=GetTickCount;
      while p.Active and (GetTickCount-tm<ExtTimeOut) do begin
        Application.ProcessMessages;
      end;
      if p.Active then begin
        p.Terminate(1);
        Result:='timeout';
        Exit;
      end;

    finally
      p.Free;
    end;

    try
      st:=TStringList.Create;
      try
        st.LoadFromFile(outp);
        if st.Count=0 then Exit;
        d:=st[0];
        if d='' then Exit;
        i:=Pos(' ', d);
        if i>=1 then begin
          Val( copy(d, 1, i-1), EndPos.Y, err);
          Val( copy(d, i+1, length(d)), EndPos.X, err);
        end;
        st.Delete(0);
        Result:=st.Text;
      finally
        st.Free;
      end;
    except
      Result:='can''t read output file';
    end;

  except
    on E:Exception do
      Result:=e.Message;
  end;
end;

function DoConvertCode(const t: AnsiString; var EndPoint: TPoint; var txt: AnsiString): Boolean;
begin
  Result:=False;
  if UseExtTool then begin
    if not FileExists(ExtTool) then begin
      ShowMessage('No convertor binary specified');
      Exit;
    end;
    cconvconfig.SaveToFile(ConvFile, ConvSettings);
    txt:=DoExtConvert(t, EndPoint);
    Result:=(EndPoint.X>=0) and (EndPoint.Y>=0);

    if Result then cconvconfig.LoadFromFile(ConvFile, ConvSettings)
    else ShowMessage('Error: '+ txt);

  end else
    txt:='';
end;

var
  parsing : Boolean = False;

procedure TryParse;
var
  editor  : TSourceEditorInterface;
  i       : Integer;
  txt     : AnsiString;
  s       : AnsiString;
  p       : TPoint;
  st      : TPoint;
begin
  if parsing then Exit;
  if not Assigned(SourceEditorManagerIntf) or not Assigned(SourceEditorManagerIntf.ActiveEditor) then Exit;

  parsing:=True;
  try
    editor:=SourceEditorManagerIntf.ActiveEditor;

    if Assigned(CtoPasConfig) then CtoPasConfig.UIToSettings;

    i:=editor.CursorTextXY.Y;
    dec(i);
    if i<0 then i:=0;
    txt:='';
    for i:=i to editor.Lines.Count-1 do
      txt:=txt+editor.Lines[i]+#10;

    if DoConvertCode(txt, p, s) then
    begin
      inc(p.Y, editor.CursorTextXY.Y-1);
      st:=editor.CursorTextXY;
      st.X:=1;
      editor.ReplaceText(st, p, s);
      if Assigned(CtoPasConfig) then
        CtoPasConfig.SettingsToUI;
    end;
  finally
    parsing:=False;
  end;
end;

procedure OnCtoPasClick(Sender: TObject);
begin
  TryParse;
end;

procedure OnCtoPasOptionsClick(Sender: TObject);
begin
  ShowConfigDialog;
end;

procedure InitPackage;
var
  cmd : TIDEMenuCommand;
begin
  cmd:=RegisterIDEMenuCommand(itmSecondaryTools, 'CtoPas', 'C to Pascal', nil, @OnCtoPasClick);
  RegisterIDEMenuCommand(itmSecondaryTools, 'CtoPas', 'C to Pascal Options', nil, @OnCtoPasOptionsClick);
  if Assigned(cmd) and Assigned(cmd.MenuItem) then cmd.MenuItem.ShortCut:=ShortCut(VK_B, [ssCtrl]);
end;

procedure Register;
var
  pth : AnsiString;
begin
  InitPackage;
  pth:=IncludeTrailingPathDelimiter(LazIDEIntf.LazarusIDE.GetPrimaryConfigPath);
  ConvFile   := pth+'cconv.ini';
  LoadFromFile(ConvFile, ConvSettings);
  ReadIDESettings(ConvFile);
  if DefineFile='' then DefineFile:=pth+'cconvdefines.h';
end;

initialization

finalization
  SaveToFile(ConvFile, ConvSettings);
  WriteIDESettings(ConvFile);

end.

