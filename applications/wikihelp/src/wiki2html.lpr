program wiki2html;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }, regex, regexpr, wikitohtml,Fileutil;

type

  { TWiki2HTML }

  TWiki2HTML = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TWiki2HTML }

procedure TWiki2HTML.DoRun;
var
  ErrorMsg: String;
  fs: TFileStream;
  ss: TStringStream;
  r: TRegexEngine;
  index: longint;
  len: longint;
  tmp: ansistring;
  Info: TSearchRec;
begin
  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Halt;
  end;
  { add your program here }
  if DirectoryExists(CleanAndExpandDirectory(GetParams(1))) then
    begin
      if FindFirst (CleanAndExpandDirectory(GetParams(1))+DirectorySeparator+'*.txt',faAnyFile,Info)=0 then
        repeat
          with Info do
            begin
              if not ((Attr and faDirectory) = faDirectory) then
                begin
                  fs := TFilestream.Create(CleanAndExpandDirectory(GetParams(1))+Info.Name,fmOpenRead);
                  ss := TStringStream.Create('');
                  ss.CopyFrom(fs,fs.Size);
                  fs.free;
                  tmp := wikitext2html(GetParams(1),ss.DataString);
                  ss.free;
                  ss := TStringStream.Create(tmp);
                  fs := TFileStream.Create(ChangeFileExt(CleanAndExpandDirectory(GetParams(1))+lowercase(Info.Name),'.html'),fmCreate);
                  fs.CopyFrom(ss,ss.Size);
                  fs.Free;
                end;
            end;
        until FindNext(info)<>0;
      FindClose(Info);
    end
  else
    begin
      fs := TFilestream.Create(GetParams(1),fmOpenRead);
      ss := TStringStream.Create('');
      ss.CopyFrom(fs,fs.Size);
      fs.free;
      tmp := wikitext2html(GetParams(1),ss.DataString);
      ss.free;
      ss := TStringStream.Create(tmp);
      fs := TFileStream.Create(ChangeFileExt(GetParams(1),'.html'),fmCreate);
      fs.CopyFrom(ss,ss.Size);
      fs.Free;
    end;
  // stop program loop
  Terminate;
end;

constructor TWiki2HTML.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TWiki2HTML.Destroy;
begin
  inherited Destroy;
end;

procedure TWiki2HTML.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' filename.txt');
end;

var
  Application: TWiki2HTML;
begin
  Application:=TWiki2HTML.Create(nil);
  Application.Title:='Wiki2HTML';
  Application.Run;
  Application.Free;
end.

