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
  Menus, MenuIntf, SrcEditorIntf, process, LazIDEIntf, IDEMsgIntf,
  extconvdialog, converteridesettings, cconvconfig, ctopasconvert;

procedure Register;

implementation

function GetErrorInfo(const errstr: AnsiString; var error: TerrorInfo): Boolean;
var
  i   : Integer;
  d   : AnsiString;
  err : Integer;
begin
  i:=Pos('error ', errstr);
  error.isError:=i>0;
  Result:=error.isError;
  if not error.isError then Exit;

  d:=Copy(errstr, 7, length(errstr)-6);
  i:=Pos(' ', d);
  Val( copy(d, 1, i-1), error.ErrorPos.Y, err);
  d:=Copy(d, i+1, length(d));

  i:=Pos(' ', d);
  Val( copy(d, 1, i-1), error.ErrorPos.X, err);

  error.ErrorMsg:=Copy(d, i+1, length(d));
end;

function DoExtConvert(const t: AnsiString; ParseAll: Boolean; var EndPos: TPoint; var error: TErrorInfo): AnsiString;
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
      if ParseAll then cmd:=cmd+' -all ';
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
        i:=0;
        d:=st[0];
        if GetErrorInfo(d, error) then begin
          st.Delete(0);
          d:=st[0];
        end;
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

function StringFromFile(const FileName: AnsiString): AnsiString;
var
  fs  : TFileStream;
begin
  Result:='';
  if not FileExists(FileName) then Exit;
  try
    fs:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(Result, fs.Size);
      fs.Read(Result[1], fs.Size);
    finally
      fs.Free;
    end;
  except
  end;
end;

function DoConvertCode(const t: AnsiString; ParseAll: Boolean; var EndPoint: TPoint; var txt: AnsiString; var error: TErrorInfo): Boolean;
begin
  Result:=False;
  if UseExtTool then begin
    if not FileExists(ExtTool) then begin
      ShowMessage('No convertor binary specified');
      Exit;
    end;
    cconvconfig.SaveToFile(ConvFile, ConvSettings);
    txt:=DoExtConvert(t, ParseAll, EndPoint, error);
    Result:=(EndPoint.X>=0) and (EndPoint.Y>=0);

    if Result then cconvconfig.LoadFromFile(ConvFile, ConvSettings)
    else ShowMessage('Error: '+ txt);

  end else begin
    try
      if FileExists(DefineFile) then
        ConvSettings.CustomDefines:=StringFromFile(DefineFile)
      else
        ConvSettings.CustomDefines:='';
      txt:=ConvertCode(t, EndPoint, ParseAll, error, ConvSettings);
      Result:=true;

    except
      on E: Exception do begin
        ShowMessage('Error: '+e.Message);
        Result:=False;
      end;
    end;
  end;
end;

var
  parsing : Boolean = False;

procedure TryParse(ParseAll: Boolean);
var
  editor  : TSourceEditorInterface;
  i       : Integer;
  txt     : AnsiString;
  s       : AnsiString;
  p       : TPoint;
  st      : TPoint;
  err     : TErrorInfo;
  line    : TIDEMessageLine;
  parts   : TStringList;
  lcnt    : Integer;
begin
  if parsing then Exit;
  if not Assigned(SourceEditorManagerIntf) or not Assigned(SourceEditorManagerIntf.ActiveEditor) then Exit;

  parsing:=True;
  try
    editor:=SourceEditorManagerIntf.ActiveEditor;

    if Assigned(CtoPasConfig) then CtoPasConfig.UIToSettings;

    st:=editor.CursorTextXY;
    i:=st.Y-1;
    if i<0 then i:=0;
    txt:='';
    for i:=i to editor.Lines.Count-1 do
      txt:=txt+editor.Lines[i]+#10;

    if Assigned(IDEMessagesWindow) then IDEMessagesWindow.Clear;

    if DoConvertCode(txt, ParseAll, p, s, err) then
    begin
      if p.Y>0 then begin
        inc(p.Y, st.Y-1);
        st.X:=1;
        lcnt:=editor.LineCount;
        editor.ReplaceText(st, p, s);
        lcnt:=editor.LineCount-lcnt;
        if Assigned(CtoPasConfig) then
          CtoPasConfig.SettingsToUI;
      end else
        lcnt:=0;
      if err.isError then begin
        inc(err.ErrorPos.Y, st.Y-1+lcnt);
        if Assigned(IDEMessagesWindow) then begin
          with err do s:=Format('%s(%d,%d) Chelper: %s', [ExtractFileName(editor.FileName), ErrorPos.Y,ErrorPos.X, ErrorMsg]);
          parts:=TStringList.Create;
          try
            parts.Values['Type']:='Chelper';
            parts.Values['Filename']:=editor.FileName;
            parts.Values['Line']:=IntToStr(err.ErrorPos.Y);
            parts.Values['Column']:=IntToStr(err.ErrorPos.X);
            IDEMessagesWindow.AddMsg(s, ExtractFileDir(editor.FileName), -1, parts);
          finally
            parts.Free;
          end;
        end;
        editor.CursorTextXY:=err.ErrorPos;
      end;
    end;
  finally
    parsing:=False;
  end;
end;

procedure OnCtoPasClick(Sender: TObject);
begin
  TryParse(False);
end;

procedure OnCtoPasAllClick(Sender: TObject);
begin
  TryParse(True);
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
  if Assigned(cmd) and Assigned(cmd.MenuItem) then cmd.MenuItem.ShortCut:=ShortCut(VK_B, [ssCtrl]);

  cmd:=RegisterIDEMenuCommand(itmSecondaryTools, 'CtoPas', 'C to Pascal all', nil, @OnCtoPasAllClick);
  if Assigned(cmd) and Assigned(cmd.MenuItem) then cmd.MenuItem.ShortCut:=ShortCut(VK_B, [ssShift, ssCtrl]);

  RegisterIDEMenuCommand(itmSecondaryTools, 'CtoPas', 'C to Pascal Options', nil, @OnCtoPasOptionsClick);
end;


type

  { TChelperJumper }

  TChelperJumper = class(TIDEMsgQuickFixItem)
    constructor Create;
    procedure Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep); override;
    function IsApplicable(Line: TIDEMessageLine): boolean; override;
  end;

constructor TChelperJumper.Create;
begin
  inherited Create;
  Name:='Chelper code jumper';
  Caption:='Chelper code jumper';
  Steps:=[imqfoJump]
end;

function TChelperJumper.IsApplicable(Line: TIDEMessageLine): boolean;
begin
  Result:=Assigned(Line) and Assigned(Line.Parts) and (Line.Parts.Values['Type']='Chelper');
end;

procedure TChelperJumper.Execute(const Msg: TIDEMessageLine; Step: TIMQuickFixStep);
var
  fn  : AnsiString;
  ln  : Integer;
  cl  : Integer;
begin
  if Step=imqfoJump then begin
    if Msg.Parts.Values['Type']<>'Chelper' then Exit;
    Msg.GetSourcePosition(fn, ln, cl);
    LazarusIDE.DoOpenFileAndJumpToPos(fn, Point(cl, ln), -1, -1, -1, [ofOnlyIfExists,ofRegularFile,ofVirtualFile]);
  end;
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
  RegisterIDEMsgQuickFix(TChelperJumper.Create);
end;

initialization

finalization
  SaveToFile(ConvFile, ConvSettings);
  WriteIDESettings(ConvFile);

end.

