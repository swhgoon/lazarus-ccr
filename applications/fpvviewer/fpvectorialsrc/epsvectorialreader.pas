{
Reads EPS files

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho

Documentation: http://www.tailrecursive.org/postscript/postscript.html
}
unit epsvectorialreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, contnrs,
  fpvectorial, fpimage, fpvutils;

type
  TPSTokenType = (ttComment, ttFloat);

  TPSTokens = TFPList;// TPSToken;

  TPSToken = class
    StrValue: string;
    FloatValue: double;
    IntValue: Integer;
    Childs: TPSTokens;
    Line: Integer; // To help debugging
  end;

  TCommentToken = class(TPSToken)
  end;

  TDefinitionToken = class(TPSToken)
  end;

  TGroupToken = class(TPSToken)
    Levels: Integer; // Used to count groups inside groups and find the end of a top-level group
  end;

  TExpressionToken = class(TPSToken)
  end;

  TPostScriptScannerState = (ssSearchingToken, ssInComment, ssInDefinition, ssInGroup, ssInExpressionElement);

  { TPSTokenizer }

  TPSTokenizer = class
  public
    Tokens: TPSTokens;
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream);
    procedure DebugOut();
    function IsValidPostScriptChar(AChar: Byte): Boolean;
    function IsPostScriptSpace(AChar: Byte): Boolean;
    function IsEndOfLine(ACurChar: Byte; AStream: TStream): Boolean;
  end;

  { TvEPSVectorialReader }

  TvEPSVectorialReader = class(TvCustomVectorialReader)
  private
    FPointSeparator: TFormatSettings;
    procedure RunPostScript(ATokens: TPsTokens; AData: TvVectorialDocument);
    procedure PostScriptCoordsToFPVectorialCoords(AParam1, AParam2: TPSToken; var APosX, APosY: Double);
    function IsExpressionOperand(AToken: TExpressionToken): Boolean;
  public
    { General reading methods }
    Tokenizer: TPSTokenizer;
    constructor Create; override;
    Destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

{$DEFINE FPVECTORIALDEBUG}

{ TPSTokenizer }

constructor TPSTokenizer.Create;
begin
  inherited Create;
  Tokens := TPSTokens.Create;
end;

destructor TPSTokenizer.Destroy;
begin
  Tokens.Free;
  inherited Destroy;
end;

{@@ Rules for parsing PostScript files:

* Coments go from the first occurence of % outside a line to the next new line
* The only accepted characters are printable ASCII ones, plus spacing ASCII chars
  See IsValidPostScriptChar about that
}
procedure TPSTokenizer.ReadFromStream(AStream: TStream);
var
  i: Integer;
  CurChar: Char;
  CurLine: Integer = 1;
  State: TPostScriptScannerState = ssSearchingToken;
  CommentToken: TCommentToken;
  DefinitionToken: TDefinitionToken;
  GroupToken: TGroupToken;
  ExpressionToken: TExpressionToken;
  Len: Integer;
  lIsEndOfLine: Boolean;
begin
  while AStream.Position < AStream.Size do
  begin
    CurChar := Char(AStream.ReadByte());
//    {$ifdef FPVECTORIALDEBUG}
//    WriteLn(Format('Obtained token %s', [CurChar]));
//    {$endif}
    if not IsValidPostScriptChar(Byte(CurChar)) then
      raise Exception.Create('[TPSTokenizer.ReadFromStream] Invalid char: ' + IntToHex(Byte(CurChar), 2));

    lIsEndOfLine := IsEndOfLine(Byte(CurChar), AStream);
    if lIsEndOfLine then Inc(CurLine);

    case State of
      { Searching for a token }
      ssSearchingToken:
      begin
        if CurChar = '%' then
        begin
          CommentToken := TCommentToken.Create;
          CommentToken.Line := CurLine;
          State := ssInComment;
//          {$ifdef FPVECTORIALDEBUG}
//          WriteLn(Format('Starting Comment at Line %d', [CurLine]));
//          {$endif}
        end
        else if CurChar = '/' then
        begin
          DefinitionToken := TDefinitionToken.Create;
          DefinitionToken.Line := CurLine;
          State := ssInDefinition;
        end
        else if CurChar = '{' then
        begin
          GroupToken := TGroupToken.Create;
          GroupToken.Levels := 1;
          GroupToken.Line := CurLine;
          State := ssInGroup;
        end
        else if CurChar in ['a'..'z','A'..'Z','0'..'9','-'] then
        begin
          ExpressionToken := TExpressionToken.Create;
          ExpressionToken.Line := CurLine;
          ExpressionToken.StrValue := CurChar;
          State := ssInExpressionElement;
        end
        else if lIsEndOfLine then Continue
        else if IsPostScriptSpace(Byte(CurChar)) then Continue
        else
          raise Exception.Create(Format('[TPSTokenizer.ReadFromStream] Unexpected char while searching for token: $%s in Line %d',
           [IntToHex(Byte(CurChar), 2), CurLine]));
      end;

      { Passing by comments }
      ssInComment:
      begin
        CommentToken.StrValue := CommentToken.StrValue + CurChar;
        if lIsEndOfLine then
        begin
          Tokens.Add(CommentToken);
          State := ssSearchingToken;
//          {$ifdef FPVECTORIALDEBUG}
//          WriteLn(Format('Adding Comment "%s" at Line %d', [CommentToken.StrValue, CurLine]));
//          {$endif}
        end;
      end; // ssInComment

      // Definitions are names. They start in "/" and end in a PostScript white space
      // (space, tab, line ending, etc) or in "{"
      // Definitions simply mean that the token is the name of a dictionary entry
      ssInDefinition:
      begin
        if IsPostScriptSpace(Byte(CurChar)) or (CurChar = '{') then
        begin
          Tokens.Add(DefinitionToken);
          State := ssSearchingToken;
          if (CurChar = '{') then AStream.Seek(-1, soFromCurrent);
        end
        else
          DefinitionToken.StrValue := DefinitionToken.StrValue + CurChar;
      end;

      // Starts at { and ends in }, passing over nested groups
      ssInGroup:
      begin
        if (CurChar = '{') then GroupToken.Levels := GroupToken.Levels + 1;
        if (CurChar = '}') then GroupToken.Levels := GroupToken.Levels - 1;

        if GroupToken.Levels = 0 then
        begin
          Tokens.Add(GroupToken);
          State := ssSearchingToken;
        end
        else
          GroupToken.StrValue := GroupToken.StrValue + CurChar;
      end;

      // Goes until a space comes
      ssInExpressionElement:
      begin
        if IsPostScriptSpace(Byte(CurChar)) then
        begin
          Tokens.Add(ExpressionToken);
          State := ssSearchingToken;
        end
        else
          ExpressionToken.StrValue := ExpressionToken.StrValue + CurChar;
      end;

    end; // case
  end; // while
end;

procedure TPSTokenizer.DebugOut();
var
  i: Integer;
  Token: TPSToken;
begin
  for i := 0 to Tokens.Count - 1 do
  begin
    Token := TPSToken(Tokens.Items[i]);

    if Token is TCommentToken then
    begin
      WriteLn(Format('TCommentToken StrValue=%s', [Token.StrValue]));
    end
    else if Token is TDefinitionToken then
    begin
      WriteLn(Format('TDefinitionToken StrValue=%s', [Token.StrValue]));
    end
    else if Token is TGroupToken then
    begin
      WriteLn(Format('TGroupToken StrValue=%s', [Token.StrValue]));
    end
    else if Token is TExpressionToken then
    begin
      WriteLn(Format('TExpressionToken StrValue=%s', [Token.StrValue]));
    end;
  end;
end;

{@@ Valid PostScript Chars:

All printable ASCII: a..zA..Z0..9 plus punctuation

Plus the following white spaces
000 00 0 Null (nul)
011 09 9 Tab (tab)
012 0A 10 Line feed (LF)
014 0C 12 Form feed (FF)
015 0D 13 Carriage return (CR)
040 20 32 Space (SP)
}
function TPSTokenizer.IsValidPostScriptChar(AChar: Byte): Boolean;
begin
  Result := ((AChar > 32) and (AChar < 127)) or (AChar in [0, 9, 10, 12, 13, 32]);
end;

function TPSTokenizer.IsPostScriptSpace(AChar: Byte): Boolean;
begin
  Result := AChar in [0, 9, 10, 12, 13, 32];
end;

function TPSTokenizer.IsEndOfLine(ACurChar: Byte; AStream: TStream): Boolean;
var
  HasNextChar: Boolean = False;
  NextChar: Byte;
begin
  Result := False;

  if ACurChar = 13 then
  begin
    if AStream.Position < AStream.Size then
    begin
      HasNextChar := True;
      NextChar := AStream.ReadByte();
      if NextChar <> 10 then AStream.Seek(-1, soFromCurrent); // Go back if it wasnt a #13#10
      Exit(True);
    end;
  end;

  if ACurChar = 10 then Result := True;
end;

{$ifndef Windows}
{$define FPVECTORIALDEBUG}
{$endif}

{ TvEPSVectorialReader }

procedure TvEPSVectorialReader.RunPostScript(ATokens: TPsTokens;
  AData: TvVectorialDocument);
var
  i: Integer;
  Stack: TObjectStack;
  Dictionary: TStringList;
  CurToken, Param1, Param2: TPSToken;
  PosX, PosY: Double;
begin
  Stack := TObjectStack.Create;
  Dictionary := TStringList.Create;

  for i := 0 to ATokens.Count - 1 do
  begin
    CurToken := TPSToken(ATokens.Items[i]);

    if CurToken is TCommentToken then
    begin
//      ProcessCommentToken(CurToken as TCommentToken, AData);
      Continue;
    end;

    if CurToken is TDefinitionToken then
    begin
      Stack.Push(CurToken);
      Continue;
    end;

    if CurToken is TGroupToken then
    begin
      Stack.Push(CurToken);
      Continue;
    end;

    if CurToken is TExpressionToken then
    begin
      if IsExpressionOperand(TExpressionToken(CurToken)) then
      begin
        Stack.Push(CurToken);
      end
      else if CurToken.StrValue = 'moveto' then
      begin
        Param1 := TPSToken(Stack.Pop);
        Param2 := TPSToken(Stack.Pop);
        PostScriptCoordsToFPVectorialCoords(Param1, Param2, PosX, PosY);
        AData.StartPath();
      end
      // Adds a dictionary definition
      else if CurToken.StrValue = 'def' then
      begin
        Param1 := TPSToken(Stack.Pop);
        Param2 := TPSToken(Stack.Pop);
        Dictionary.AddObject(Param2.StrValue, Param1);
      end
      // ???? Commands ignored for now
      else if (CurToken.StrValue = 'ndf') or (CurToken.StrValue = 'load') then
      begin
      end
      // bind can be ignored
      else if CurToken.StrValue = 'bind' then
      begin
      end
      else
        raise Exception.Create(Format('[TvEPSVectorialReader.RunPostScript] Unknown PostScript Command "%s" in Line %d',
          [CurToken.StrValue, CurToken.Line]));
    end;
  end;

  Stack.Free;
  Dictionary.Free;
end;

procedure TvEPSVectorialReader.PostScriptCoordsToFPVectorialCoords(AParam1,
  AParam2: TPSToken; var APosX, APosY: Double);
begin
  APosX := SysUtils.StrToFloat(AParam2.StrValue, FPointSeparator);
  APosY := SysUtils.StrToFloat(AParam1.StrValue, FPointSeparator);
end;

function TvEPSVectorialReader.IsExpressionOperand(AToken: TExpressionToken
  ): Boolean;
begin
  if AToken.StrValue = '' then Exit(False);
  Result := AToken.StrValue[1] in ['0'..'9','-'];
end;

constructor TvEPSVectorialReader.Create;
begin
  inherited Create;

  FPointSeparator := SysUtils.DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := ',';

  Tokenizer := TPSTokenizer.Create;
end;

destructor TvEPSVectorialReader.Destroy;
begin
  Tokenizer.Free;
  inherited Destroy;
end;

procedure TvEPSVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
begin
  Tokenizer.ReadFromStream(AStream);
  Tokenizer.DebugOut();
  RunPostScript(Tokenizer.Tokens, AData);
end;

initialization

  RegisterVectorialReader(TvEPSVectorialReader, vfEncapsulatedPostScript);

end.

