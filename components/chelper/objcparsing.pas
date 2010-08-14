unit
  objcparsing;

interface

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

uses
  Classes, SysUtils, cparsertypes;

type

  { TObjCClasses }

  TObjCClasses = class(TEntity)
    Classes : TStringList;
    constructor Create(AOffset: Integer=-1); override;
    destructor Destroy; override;
  end;

  { TObjCMethod }

  TObjCMethodArg = record
    RetType  : TEntity;
    TypeName : TNamePart;
    Name     : AnsiString;
  end;

  TObjCMethod = class(TEntity)
  public
    isClassMethod : Boolean;
    Name      : TStringList;
    RetType   : TEntity;
    RetName   : TNamePart;
    Args      : array of TObjCMethodArg;
    constructor Create(AOffset: Integer=-1); override;
    destructor Destroy; override;
    procedure AddArg(const ArgType: TEntity; ArgTypeName: TNamePart; const Name: AnsiString);
  end;

  { TObjCInterface }

  TObjCInterface = class(TEntity)
  public
    Name        : AnsiString;
    SuperClass  : AnsiString;
    isCategory  : Boolean;
    Protocols   : TStringList;
    ProtVars    : TList;
    PrivVars    : TList;
    PubVars     : TList;
    PackVars    : TList;
    Methods     : TList;
    constructor Create(AOffset: Integer=-1); override;
    destructor Destroy; override;
  end;

function ParseClassList(AParser: TTextParser): TObjCClasses;
function ParseInterface(AParser: TTextParser): TObjCInterface;
function ParseMethod(AParser: TTextParser): TObjCMethod;
function ParseMethods(AParser: TTextParser; MethodsList: TList; const EndToken: AnsiString): Boolean;
function ParserProtocol(AParser: TTextParser): TEntity;

function ParseNextObjCEntity(AParser: TTextParser): TEntity;

function isObjCKeyword(const token: AnsiString): Boolean; inline;
function GetObjCKeyword(const token: AnsiString): AnsiString;

implementation

function isObjCKeyword(const token: AnsiString): Boolean; inline;
begin
  Result:=(token<>'') and (token[1]='@');
end;

function GetObjCKeyword(const token: AnsiString): AnsiString;
begin
  if isObjCKeyword(token) then Result:=Copy(token, 2, length(token)-1)
  else Result:=token;
end;

function ParseClassList(AParser: TTextParser): TObjCClasses;
var
  cl  : TObjCClasses;
begin
  Result:=nil;
  if AParser.Token<>'@class' then Exit;
  cl:=TObjCClasses.Create(AParser.TokenPos);
  AParser.NextToken;
  while AParser.Token<>';' do begin
    if AParser.TokenType<>tt_Ident then begin
      ErrorExpect(AParser,'identifier');
      cl.Free;
      Exit;
    end;
    cl.Classes.Add(AParser.Token);
    AParser.NextToken;
    if AParser.Token=',' then
      AParser.NextToken
    else if AParser.Token<>';' then begin
      ErrorExpect(AParser,';');
      cl.Free;
      Exit;
    end;
  end;
  Result:=cl;
end;

function ParseInstVars(AParser: TTextParser; itf: TObjCInterface): Boolean;
var
  vars  : TList;
  v     : TVarFuncEntity;
  s     : AnsiString;
begin
  Result:=True;
  if AParser.Token<>'{' then Exit;

  Result:=False;
  AParser.NextToken;
  vars:=itf.ProtVars;

  while AParser.Token<>'}' do begin
    if isObjCKeyword(AParser.Token) then begin
      s:=GetObjCKeyword(APArser.Token);
      if s='protected' then vars:=itf.ProtVars
      else if s='private' then vars:=itf.PrivVars
      else if s='public' then vars:=itf.PubVars
      else if s='package' then vars:=itf.PackVars
      else begin
        ErrorExpect(AParser,'}');
        Exit;
      end;
      AParser.NextToken;
    end else begin
      v:=TVarFuncEntity.Create(APArser.TokenPos);
      if not ParseNames(AParser, v.RetType, v.Names) then Exit;
      vars.Add(v);
      if AParser.Token=';' then
        AParser.NextToken;
    end;
  end;
  AParser.NextToken;

  Result:=True;
end;

function ParseInterface(AParser: TTextParser): TObjCInterface;
var
  itf : TObjCInterface;
  i   : Integer;
  nm  : AnsiString;
begin
  Result:=nil;
  if AParser.Token<>'@interface' then Exit;
  i:=AParser.TokenPos;
  AParser.NextToken;

  if not ConsumeIdentifier(AParser, nm) then Exit;

  itf:=TObjCInterface.Create(i);
  try
    itf.Name:=nm;
    itf.isCategory:=AParser.Token='(';
    if itf.isCategory then begin
       AParser.NextToken;
       if not ConsumeIdentifier(AParser, itf.SuperClass) and ConsumeToken(AParser, ')') then
        Exit;
    end else begin

      // super-class
      if AParser.Token=':' then begin
        AParser.NextToken;
        if not ConsumeIdentifier(AParser, itf.SuperClass) then Exit;
        //writeln('SuperClass = ', itf.SuperClass);
      end;

      // protocols
      if AParser.Token='<' then begin
        AParser.NextToken;
        while AParser.Token<>'>' do begin
          if not ConsumeIdentifier(AParser, nm) then Exit;
          //writeln('Protos = ', nm);
          itf.Protocols.Add(nm);
          if AParser.Token=',' then AParser.NextToken
          else if AParser.Token<>'>' then begin
            ErrorExpect(AParser, '>');
            Exit;
          end;
        end;
        AParser.NextToken;
      end;

      //writeln('parsing vars1 ', AParser.Token);
      ParseInstVars(AParser, itf);
      //writeln('parsing vars2 ', AParser.Token);
    end;

    //writeln('parsing methods1 ', AParser.Token);
    if not ParseMethods(AParser, itf.Methods, '@end') then Exit;
    //writeln('parsing methods2 ', AParser.Token);

    if AParser.Token='@end' then AParser.NextToken;

    Result:=itf;
  finally
    if not Assigned(Result) then itf.Free;
  end;
end;

function ParserProtocol(AParser: TTextParser): TEntity;
begin
  Result:=nil;
end;

var
  PrevParseNextEntity : function (AParser: TTextParser): TEntity = nil;

function ParseNextObjCEntity(AParser: TTextParser): TEntity;
var
  t   : AnsiString;
begin
  if AParser.Token[1]='@' then begin
    t:=GetObjCKeyword(AParser.Token);
    if t='class' then Result:=ParseClassList(AParser)
    else if t='interface' then Result:=ParseInterface(AParser)
    else if t='protocol' then Result:=ParserProtocol(AParser);
  end else begin
    if Assigned(PrevParseNextEntity) then
      Result:=PrevParseNextEntity(AParser)
    else
      Result:=nil;
  end;
end;

{ TObjCClasses }

constructor TObjCClasses.Create(AOffset:Integer);
begin
  inherited Create(AOffset);
  Classes := TStringList.Create;
end;

destructor TObjCClasses.Destroy;
begin
  Classes.Free;
  inherited Destroy;
end;

{ TObjCInterface }

constructor TObjCInterface.Create(AOffset:Integer);
begin
  ProtVars := TList.Create;
  PrivVars := TList.Create;
  PubVars  := TList.Create;
  PackVars := TList.Create;
  Methods  := TList.Create;
  Protocols := TStringList.Create;
  inherited Create(AOffset);
end;

destructor TObjCInterface.Destroy;
var
  i : Integer;
begin
  for i:=0 to ProtVars.Count-1 do TObject(ProtVars[i]).Free;
  for i:=0 to PrivVars.Count-1 do TObject(PrivVars[i]).Free;
  for i:=0 to PubVars.Count-1 do TObject(PubVars[i]).Free;
  for i:=0 to PackVars.Count-1 do TObject(PubVars[i]).Free;
  PrivVars.Free;
  PubVars.Free;
  ProtVars.Free;
  PackVars.Free;

  for i:=0 to Methods.Count-1 do TObject(Methods[i]).Free;
  Methods.Free;

  Protocols.Free;
  inherited Destroy;
end;

function ParseMethod(AParser: TTextParser): TObjCMethod;
var
  m       : TObjCMethod;
  nm      : AnsiString;
  atype   : TEntity;
  atname  : TNamePart;
  aname   : Ansistring;
  prm     : Boolean;
begin
  Result:=nil;
  if (AParser.Token<>'+') and (AParser.Token<>'-') then Exit;
  //writeln('in method: ', AParser.Token);
  m:=TObjCMethod.Create(AParser.TokenPos);
  try
    AParser.NextToken;

    //writeln('in method2: ', AParser.Token);
    if AParser.Token='(' then begin
      AParser.NextToken;
      if not ParseName(AParser,  m.RetType, m.RetName) then Exit;
      if not ConsumeToken(AParser, ')') then Exit;
    end;

    //writeln('in method3: ', AParser.Token);
    if not ConsumeIdentifier(AParser, nm) then Exit;

    //writeln('in method3: ', AParser.Token);
    if (AParser.Token=':') then begin
      m.Name.Add(nm+':');
      AParser.NextToken;

      //writeln('in method4: ', AParser.Token);
      while AParser.Token<>';' do begin
        prm:=ConsumeToken(AParser, '(') and
             ParseName(APArser, atype, atname) and
             ConsumeToken(AParser, ')') and
             ConsumeIdentifier(AParser, aname);
        if not prm then Exit;
        m.AddArg(atype, atname, aname);

        if AParser.TokenType=tt_Ident then ConsumeIdentifier(AParser, nm) else nm:='';
        if AParser.Token<>';' then begin
          if not ConsumeToken(AParser,':') then Exit;
          m.Name.Add(nm+':');
        end;
      end;
      AParser.NextToken;
    end else begin
      m.Name.Add(nm);
      if not ConsumeToken(AParser, ';') then Exit;
    end;

    //writeln('in method5: ', AParser.Token);
    Result:=m;
  finally
    if not Assigned(Result) then m.Free;
  end;
end;

function ParseMethods(AParser: TTextParser; MethodsList: TList; const EndToken: AnsiString = '@end'): Boolean;
var
  m : TObjCMethod;
begin
  Result:=False;
  if not Assigned(MethodsList) or not Assigned(AParser) then Exit;
  while (AParser.Token<>EndToken) and (AParser.Token<>'') and (AParser.Token[1] in ['+','-']) do begin
    //writeln('AParser.Token = ', AParser.Token);
    m:=ParseMethod(AParser);
    //writeln('m = ', Integer(m));
    if not Assigned(m) then Exit;
    MethodsList.Add(m);
  end;
  Result:=True;
end;

{ TObjCMethod }

constructor TObjCMethod.Create(AOffset:Integer);
begin
  inherited Create(AOffset);
  Name      := TStringList.Create;
  RetType   := TVarFuncEntity.Create;
end;

destructor TObjCMethod.Destroy;
var
  i : Integer;
begin
  Name.Free;
  RetType.Free;
  RetName.Free;
  for i:=0 to length(Args)-1 do begin
    Args[i].RetType.Free;
    Args[i].TypeName.Free;
  end;
  inherited Destroy;
end;

procedure TObjCMethod.AddArg(const ArgType:TEntity;ArgTypeName:TNamePart;const Name:AnsiString);
var
  i : Integer;
begin
  i:=length(Args);
  SetLength(Args, i+1);
  Args[i].Name:=Name;
  Args[i].RetType:=ArgType;
  Args[i].TypeName:=ArgTypeName;
end;

initialization
  PrevParseNextEntity:=ParseNextEntity;
  ParseNextEntity:=ParseNextObjCEntity;

end.

