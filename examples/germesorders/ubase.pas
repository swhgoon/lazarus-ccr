unit uBase;

{$mode objfpc}{$H+}

interface
uses Classes, sqlite3ds, variants, Db;

type

  { TDatabaseConnect }

  TStringsFillEvent = procedure (D:TDataset) of object;

  TDatabaseConnect = class(TComponent)
    private
      BaseFileName:string;
      function GetOption(const OptId: string): string;
      procedure SetOption(const OptId: string; const AValue: string);
    public
      constructor Create;reintroduce;
      destructor Destroy;override;
      
      procedure ConnectToBase(D:TSqlite3Dataset);
      
      function DatasetCreate(const SQL:string; OpenDataset:boolean=true):TSqlite3Dataset;overload;
      function DatasetCreate(const Table, PKField:string;  OpenDataset:boolean=true):TSqlite3Dataset;overload;

      //заполнение списка по шаблону
      //имена полей задаются через %FIELDNAME%
      //пока что поле может входить в шаблое только один раз
      procedure StringsFill(const SQL:string; const Template:string; List:TStrings; OnFill:TStringsFillEvent = nil; ClearList:boolean=true);

      function DLookup(const SQL, Column:string):Variant;overload;
      function DLookup(const SQL:string; Params:array of const; const Column:string):Variant;overload;
      procedure SQLExec(const S:String);overload;
      procedure SQLExec(const S:String; Args:array of const);overload;
      
      property OptionUser[const OptId:string]:string read GetOption write SetOption;
  end;
  
  function BaseConnect:TDatabaseConnect;

implementation
uses SysUtils, uConfig, uDebug;

var BaseObj:TDatabaseConnect=nil;
function BaseConnect:TDatabaseConnect;
begin
  if BaseObj = nil then
    begin
      BaseObj:=TDatabaseConnect.Create;
    end;
  Result:=BaseObj;
end;

{ TDatabaseConnect }

function TDatabaseConnect.GetOption(const OptId: string): string;
begin
  Result:=VarToStr(DLookup(Format('select OptValue from Options where Name=''%s''', [OptId]), 'OptValue'));
end;

procedure TDatabaseConnect.SetOption(const OptId: string; const AValue: string);
begin
  SQLExec('DELETE FROM Options WHERE Name=''%s''',[OptId]);
  SQLExec('INSERT INTO Options (Name, OptValue) VALUES(''%s'', ''%s'')', [OptId, AValue]);
end;

constructor TDatabaseConnect.Create;
begin
  inherited Create(nil);
  BaseFileName:=GlobalConfig.BaseFile;
end;

destructor TDatabaseConnect.Destroy;
begin
  inherited Destroy;
end;

procedure TDatabaseConnect.ConnectToBase(D: TSqlite3Dataset);
begin
  if D.Active then D.Close;
  D.FileName:=BaseFileName;
end;

function TDatabaseConnect.DatasetCreate(const SQL: string; OpenDataset:boolean): TSqlite3Dataset;
begin
  Result:=TSqlite3Dataset.Create(Self);
  Result.FileName:=BaseFileName;
  Result.SQL:=SQL;
  try
    if OpenDataset then Result.Open;
  except
    Result.Free;
    raise;
  end;
end;

function TDatabaseConnect.DatasetCreate(const Table, PKField: string;
  OpenDataset: boolean): TSqlite3Dataset;
begin
  Result:=TSqlite3Dataset.Create(Self);
  Result.FileName:=BaseFileName;
  Result.TableName:=Table;
  Result.AutoIncrementKey:=true;
  Result.PrimaryKey:=PKField;
  try
    if OpenDataset then Result.Open;
  except
    Result.Free;
    raise;
  end;
end;

procedure TDatabaseConnect.StringsFill(const SQL: string; const Template: string; List: TStrings;
 OnFill:TStringsFillEvent; ClearList:boolean);
var D:TSqlite3Dataset;
  Strpos, WStrPos:array of integer;
  i, j:integer;
  FS, TemplatePrepared, S:String;
begin
  D:=DatasetCreate(SQL);
  List.BeginUpdate;
  try
    SetLength(Strpos, D.FieldCount);
    TemplatePrepared:=Template;
    for i:=0 to D.FieldCount-1 do
      begin
        FS:='%'+ D.Fields.Fields[i].FieldName + '%';
        StrPos[i]:=Pos(FS, TemplatePrepared);
        if StrPos[i]<>0 then
          begin
            Delete(TemplatePrepared, StrPos[i], Length(FS));
            //цикл коррекции предыдущих найденных позиций
            for j:=0 to i-1 do
              if StrPos[j] > StrPos[i] then Dec(StrPos[j], Length(FS));
          end;
      end;

    SetLength(WStrPos, Length(Strpos));
    if ClearList then List.Clear;
    while not D.EOF do
    begin
      //инициализация массива текущих позиций
      Move(Strpos[0], WStrPos[0], Length(StrPos)*SizeOf(StrPos[0]));
      S:=TemplatePrepared;
      for i:=0 to D.FieldCount-1 do
        if WStrPos[i] > 0 then
          begin
            FS:=D.Fields.Fields[i].AsString;
            Insert(FS, S, WStrPos[i]);
            //цикл коррекции
            for j:=i+1 to High(WStrPos) do
              if WStrPos[j] > WStrPos[i] then Inc(WStrPos[j], Length(FS));
          end;
    
      if OnFill <> nil then
        OnFill(D);
        
      List.Add(S);
      D.Next;
    end;

  finally
    D.Free;
    List.EndUpdate;
  end;
end;

function TDatabaseConnect.DLookup(const SQL, Column: string): Variant;
begin
  Result:=null;
  with DatasetCreate(SQL) do
  try
    First;
    if not EOF then
      Result:=FieldByName(Column).AsVariant;
  finally
    Free;
  end;
end;

function TDatabaseConnect.DLookup(const SQL: string; Params: array of const;
  const Column: string): Variant;
begin
  Result:=DLookup(Format(SQL, Params), Column);
end;

procedure TDatabaseConnect.SQLExec(const S: String);
var PostDS:TSqlite3Dataset;
begin
  PostDS:=TSqlite3Dataset.Create(nil);
  try
    PostDS.FileName:=BaseFileName;
    //PostDS.ExecuteDirect(S);
    PostDS.ExecSQL(S);
  finally
    PostDS.Free;
  end;
end;

procedure TDatabaseConnect.SQLExec(const S: String; Args: array of const);
begin
  SQLExec(Format(S, Args));
end;

finalization
  BaseObj.Free;

end.

