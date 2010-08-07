unit codewriter;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  { TCodeWriter }

  TCodeWriter = class(TObject)
  private
    fnewline:Boolean;
    fText       : AnsiString;
    fIdent      : AnsiString;
    fIdDelta    : AnsiString;
    newline     : Boolean;

    fCurLine    : AnsiString;
    fSection    : AnsiString;

    fMaxLen       : Integer;
    fCheckLineLen : Boolean;
  public
    constructor Create;
    procedure IncIdent;
    procedure DecIdent;
    procedure W(const s: AnsiString='');
    procedure Wln(const s: AnsiString='');
    procedure StartNewLine;
    property Section: AnsiString read fSection write fSection;
    property Text: AnsiString read fText write fText;
    property LineStarts: Boolean read fnewline;

    property MaxLineLen: Integer read fMaxLen write fMaxLen;
    property CheckLineLen: Boolean read fCheckLineLen write fCheckLineLen;
  end;

procedure SetPasSection(wr: TCodeWriter; const SectionName: AnsiString; DoIdent: Boolean=true);

implementation

procedure SetPasSection(wr: TCodeWriter; const SectionName: AnsiString; DoIdent: Boolean);
begin
  if wr.Section=SectionName then Exit;

  if (wr.Section<>'') and DoIdent then wr.DecIdent;
  if SectionName<>'' then wr.Wln(SectionName);
  wr.Section:=SectionName;
  if (wr.Section<>'') and DoIdent then wr.IncIdent;
end;

{ TCodeWriter }

constructor TCodeWriter.Create;
begin
  fIdDelta:='  ';
  newline:=True;
  fMaxLen:=80;
end;

procedure TCodeWriter.IncIdent;
begin
  fIdent:=fIdent+fIdDelta;
end;

procedure TCodeWriter.DecIdent;
begin
  fIdent:=Copy(fIdent, 1, length(fIdent)-length(fIdDelta));
end;

procedure TCodeWriter.W(const s:String);
var
  AutoBreak: Boolean;
begin
  //todo: check eoln symbols in s
  if s ='' then Exit;

  AutoBreak:=CheckLineLen and (fCurLine<>'') and ( length(fCurLine+fIdent)+length(s) > fMaxLen);
  if AutoBreak then begin
    fText:=fText+LineEnding;
    fCurLine:='';
    fText:=fText+fIdent+fIdDelta;
  end;

  if newline then fText:=fText+fIdent;
  fText:=fText+s;
  fCurLine:=fCurLine+s;
  newline:=False;
end;

procedure TCodeWriter.Wln(const s:String);
begin
  W(s+LineEnding);
  newline:=True;
  fCurLine:='';
end;

procedure TCodeWriter.StartNewLine;
begin
  if not newline then Wln;
end;

end.

