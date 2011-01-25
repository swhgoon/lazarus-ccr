{
Reads DXF files

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho

DXF is composed by records written in ASCII with the following structure:

0
SECTION
section_number
SECTION_NAME
<data>
0
ENDSEC
0

after all sections there is:

EOF

}
unit dxfvectorialreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpvectorial;

type

  { Used by tcutils.SeparateString }
  T10Strings = array[0..9] of shortstring;

  TDXFToken = class;

  TDXFTokens = TFPList;// TDXFToken;

  TDXFToken = class
    GroupCode: Integer;
    StrValue: string;
    FloatValue: double;
    IntValue: Integer;
    Childs: TDXFTokens;
    constructor Create;
    Destructor Destroy; override;
  end;

  { TDXFTokenizer }

  TDXFTokenizer = class
  public
    Tokens: TDXFTokens;
    constructor Create;
    Destructor Destroy; override;
    procedure ReadFromStrings(AStrings: TStrings);
    function  IsENTITIES_Subsection(AStr: string): Boolean;
  end;

  { TvDXFVectorialReader }

  TvDXFVectorialReader = class(TvCustomVectorialReader)
  private
    LineStartX, LineStartY, LineStartZ: Double;
    LineEndX, LineEndY, LineEndZ: Double;
    function  SeparateString(AString: string; ASeparator: Char): T10Strings;
    procedure ReadENTITIES(ATokens: TDXFTokens; AData: TvVectorialDocument);
    function  ReadENTITIES_LINE(AStrings: TStrings; var AIndex: Integer; AData: TvVectorialDocument): Boolean;
    function  GetCoordinate(AStr: shortstring): Integer;
    function  GetCoordinateValue(AStr: shortstring): Double;
  public
    { General reading methods }
    Tokenizer: TDXFTokenizer;
    constructor Create;
    Destructor Destroy; override;
    procedure ReadFromStrings(AStrings: TStrings; AData: TvVectorialDocument); override;
  end;

implementation

{$define FPVECTORIALDEBUG}

const
  // Group Codes for ENTITIES
  DXF_ENTITIES_TYPE = 0;
  DXF_ENTITIES_HANDLE = 5;
  DXF_ENTITIES_APPLICATION_GROUP = 102;
  DXF_ENTITIES_AcDbEntity = 100;
  DXF_ENTITIES_MODEL_OR_PAPER_SPACE = 67; // default=0=model, 1=paper
  DXF_ENTITIES_VISIBILITY = 60; // default=0 = Visible, 1 = Invisible

{ TDXFToken }

constructor TDXFToken.Create;
begin
  inherited Create;

  Childs := TDXFTokens.Create;
end;

destructor TDXFToken.Destroy;
begin
  Childs.Free;

  inherited Destroy;
end;

{ TDXFTokenizer }

constructor TDXFTokenizer.Create;
begin
  inherited Create;

  Tokens := TDXFTokens.Create;
end;

destructor TDXFTokenizer.Destroy;
begin
  Tokens.Free;

  inherited Destroy;
end;

procedure TDXFTokenizer.ReadFromStrings(AStrings: TStrings);
var
  i: Integer;
  StrSectionGroupCode, StrSectionName: string;
  IntSectionGroupCode: Integer;
  CurTokenBase, NextTokenBase, ENTITIESTokenBase: TDXFTokens;
  NewToken: TDXFToken;
  ParserState: Integer;
begin
  //  Tokens.ForEachCall(); deletecallback
  Tokens.Clear;

  CurTokenBase := Tokens;
  NextTokenBase := Tokens;
  i := 0;
  ParserState := 0;

  while i < AStrings.Count - 1 do
  begin
    CurTokenBase := NextTokenBase;

    // Now read and process the section name
    StrSectionGroupCode := AStrings.Strings[i];
    IntSectionGroupCode := StrToInt(Trim(StrSectionGroupCode));
    StrSectionName := AStrings.Strings[i+1];

    NewToken := TDXFToken.Create;
    NewToken.GroupCode := IntSectionGroupCode;
    NewToken.StrValue := StrSectionName;

    // Waiting for a section
    if ParserState = 0 then
    begin
      if (StrSectionName = 'SECTION') then
      begin
        ParserState := 1;
        NextTokenBase := NewToken.Childs;
      end
      else
      begin
        raise Exception.Create(Format(
          'TDXFTokenizer.ReadFromStrings: Expected SECTION, but got: %s', [StrSectionname]));
      end;
    end
    // Processing the section name
    else if ParserState = 1 then
    begin
      if (StrSectionName = 'HEADER') or
        (StrSectionName = 'CLASSES') or
        (StrSectionName = 'TABLES') or
        (StrSectionName = 'BLOCKS') or
        (StrSectionName = 'OBJECTS') or
        (StrSectionName = 'THUMBNAILIMAGE') then
      begin
        ParserState := 2;
      end
      else if (StrSectionName = 'ENTITIES') then
      begin
        ParserState := 3;
        ENTITIESTokenBase := CurTokenBase;
      end
      else if (StrSectionName = 'EOF') then
      begin
        Exit;
      end
      else
      begin
        raise Exception.Create(Format(
          'TDXFTokenizer.ReadFromStrings: Invalid section name: %s', [StrSectionname]));
      end;
    end
    // Reading a generic section
    else if ParserState = 2 then
    begin
      if StrSectionName = 'ENDSEC' then
      begin
        ParserState := 0;
        NextTokenBase := Tokens;
      end;
    end
    // Reading the ENTITIES section
    else if ParserState = 3 then
    begin
      if IsENTITIES_Subsection(StrSectionName) then
      begin
        CurTokenBase := ENTITIESTokenBase;
        NextTokenBase := NewToken.Childs;
      end
    end;

    CurTokenBase.Add(NewToken);

    Inc(i, 2);
  end;
end;

function TDXFTokenizer.IsENTITIES_Subsection(AStr: string): Boolean;
begin
  Result :=
    (AStr = '3DFACE') or
    (AStr = '3DSOLID') or
    (AStr = 'ACAD_PROXY_ENTITY') or
    (AStr = 'ARC') or
    (AStr = 'ATTDEF') or
    (AStr = 'ATTRIB') or
    (AStr = 'BODY') or
    (AStr = 'CIRCLE') or
    (AStr = 'DIMENSION') or
    (AStr = 'ELLIPSE') or
    (AStr = 'HATCH') or
    (AStr = 'IMAGE') or
    (AStr = 'INSERT') or
    (AStr = 'LEADER') or
    (AStr = 'LINE') or
    (AStr = 'LWPOLYLINE') or
    (AStr = 'MLINE') or
    (AStr = 'MTEXT') or
    (AStr = 'OLEFRAME') or
    (AStr = 'OLE2FRAME') or
    (AStr = 'POINT') or
    (AStr = 'POLYLINE') or
    (AStr = 'RAY') or
    (AStr = 'REGION') or
    (AStr = 'SEQEND') or
    (AStr = 'SHAPE') or
    (AStr = 'SOLID') or
    (AStr = 'SPLINE') or
    (AStr = 'TEXT') or
    (AStr = 'TOLERANCE') or
    (AStr = 'TRACE') or
    (AStr = 'VERTEX') or
    (AStr = 'VIEWPORT') or
    (AStr = 'XLINE');
end;

{ TvDXFVectorialReader }

{@@
  Reads a string and separates it in substring
  using ASeparator to delimite them.

  Limits:

  Number of substrings: 10 (indexed 0 to 9)
  Length of each substring: 255 (they are shortstrings)
}
function TvDXFVectorialReader.SeparateString(AString: string; ASeparator: Char): T10Strings;
var
  i, CurrentPart: Integer;
begin
  CurrentPart := 0;

  { Clears the result }
  for i := 0 to 9 do Result[i] := '';

  { Iterates througth the string, filling strings }
  for i := 1 to Length(AString) do
  begin
    if Copy(AString, i, 1) = ASeparator then
    begin
      Inc(CurrentPart);

      { Verifies if the string capacity wasn't exceeded }
      if CurrentPart > 9 then Exit;
    end
    else
      Result[CurrentPart] := Result[CurrentPart] + Copy(AString, i, 1);
  end;
end;

procedure TvDXFVectorialReader.ReadENTITIES(ATokens: TDXFTokens; AData: TvVectorialDocument);
var
  i: Integer;
  CurToken: TDXFToken;
begin
  for i := 0 to ATokens.Count - 1 do
  begin
    CurToken := TDXFToken(ATokens.Items[i]);
    if CurToken.StrValue = 'ELLIPSE' then
    begin
      // ...
    end
    else if CurToken.StrValue = 'LINE' then
    begin
      // Initial values
      LineStartX := 0;
      LineStartY := 0;
      LineStartZ := 0;
      LineEndX := 0;
      LineEndY := 0;
      LineEndZ := 0;

      // Read the data of the line
//      Inc(AIndex, 2);
//      while not ReadENTITIES_LINE(AStrings, AIndex, AData) do ;

      // And now write it
      {$ifdef FPVECTORIALDEBUG}
      WriteLn(Format('Adding Line from %f,%f to %f,%f', [LineStartX, LineStartY, LineEndX, LineEndY]));
      {$endif}
//      AData.StartPath(LineStartX, LineStartY);
//      AData.AddLineToPath(LineEndX, LineEndY);
//      AData.EndPath();
    end
    else if CurToken.StrValue = 'TEXT' then
    begin
      // ...
    end;
  end;
end;

function TvDXFVectorialReader.ReadENTITIES_LINE(AStrings: TStrings;
  var AIndex: Integer; AData: TvVectorialDocument): Boolean;
var
  StrSectionNum, StrSectionValue: string;
  IntSectionNum: Integer;
  FloatSectionValue: double;
begin
  Result := False;

{  // Now read and process the item name
  StrSectionNum := AStrings.Strings[AIndex];
  StrSectionValue := AStrings.Strings[AIndex+1];

  if IsENTITIES_Subsection(StrSectionValue) or
     (StrSectionValue = 'ENDSEC') then
  begin
    Exit(True);
  end
  else
  begin
    Inc(AIndex, 2);

    IntSectionNum := StrToInt(Trim(StrSectionNum));
    FloatSectionValue := StrToFloat(Trim(StrSectionValue));

    case IntSectionNum of
      10: LineStartX := FloatSectionValue;
      20: LineStartY := FloatSectionValue;
      30: LineStartZ := FloatSectionValue;
      11: LineEndX := FloatSectionValue;
      21: LineEndY := FloatSectionValue;
      31: LineEndZ := FloatSectionValue;
    end;
  end;}
end;

function TvDXFVectorialReader.GetCoordinate(AStr: shortstring): Integer;
begin
{  Result := INT_COORDINATE_NONE;

  if AStr = '' then Exit
  else if AStr[1] = 'X' then Result := INT_COORDINATE_X
  else if AStr[1] = 'Y' then Result := INT_COORDINATE_Y
  else if AStr[1] = 'Z' then Result := INT_COORDINATE_Z;}
end;

function TvDXFVectorialReader.GetCoordinateValue(AStr: shortstring): Double;
begin
  Result := 0.0;

{  if Length(AStr) <= 1 then Exit;

  Result := StrToFloat(Copy(AStr, 2, Length(AStr) - 1));}
end;

constructor TvDXFVectorialReader.Create;
begin
  inherited Create;

  Tokenizer := TDXFTokenizer.Create;
end;

destructor TvDXFVectorialReader.Destroy;
begin
  Tokenizer.Free;

  inherited Destroy;
end;

{@@
  The information of each separate path is lost in G-Code files
  Only one path uniting all of them is created when reading G-Code
}
procedure TvDXFVectorialReader.ReadFromStrings(AStrings: TStrings;
  AData: TvVectorialDocument);
var
  i: Integer;
  CurToken, CurTokenFirstChild: TDXFToken;
begin
  Tokenizer.ReadFromStrings(AStrings);

  for i := 0 to Tokenizer.Tokens.Count - 1 do
  begin
    CurToken := TDXFToken(Tokenizer.Tokens.Items[i]);
    CurTokenFirstChild := TDXFToken(CurToken.Childs.Items[0]);

    if CurTokenFirstChild.StrValue = 'ENTITIES' then
      ReadENTITIES(CurToken.Childs, AData);
  end;
end;

initialization

  RegisterVectorialReader(TvDXFVectorialReader, vfDXF);

end.

