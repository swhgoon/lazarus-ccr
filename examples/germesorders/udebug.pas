unit uDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db;

type
  ILogger = interface
    procedure Log(const S:string);overload;
    procedure Log(const S:string; const Fmt:array of const);overload;
    
    procedure LogException(E:Exception);
    Procedure ExceptionHandler(Sender : TObject; E : Exception);
    procedure LogDatasetFieldNames(const DatasetName:string; D:TDataset);
  end;

  function GlobalLogger:ILogger;
implementation
uses uConfig, functions_file, Forms;

var Logger:ILogger;

type

  { TLogger }

  TLogger = class(TInterfacedObject, ILogger)
    private
      //F:TFileStream;
      F:TextFile;
      ExceptionCaught:boolean;
    public
      constructor Create;
      destructor Destroy;override;
    
      procedure Log(const S:string);overload;
      procedure Log(const S:string; const Fmt:array of const);overload;
      
      procedure LogException(E:Exception);
      Procedure ExceptionHandler(Sender : TObject; E : Exception);
      procedure LogDatasetFieldNames(const DatasetName:string; D:TDataset);
  end;

{ TLogger }

constructor TLogger.Create;
begin
  inherited;
  
  ExceptionCaught:=false;
  //F:=TFileStream.Create( GlobalConfig.DebugLogFile, fmCreate or fmOpenWrite );
  AssignFile(F, GlobalConfig.DebugLogFile);
  Rewrite(F);
end;

destructor TLogger.Destroy;
var D, DLog:String;
begin
  //F.Free;
  CloseFile(F);
  if ExceptionCaught then
    try
      //сохраняем в отдельной папке логов
      D:=GlobalConfig.DebugLogDirectory;
      fb_CreateDirectoryStructure( D );
      
      DLog:=Format('%s/log-%s.log', [D, FormatDateTime('dd-mm-yyyy-hh-mm-ss', Now)]);
      //DLog:=Format('%s/log.log', [D]);
      fb_CopyFile(GlobalConfig.DebugLogFile, DLog, false, false);
    except
    end;
  
  inherited Destroy;
end;

procedure TLogger.Log(const S: string);
var Buf:string;
begin
  //Buf:=S + #13#10;
  //F.Write( PChar(@Buf[1])^, Length(Buf) );
  WriteLn(F, S);
  Flush(F);
end;

procedure TLogger.Log(const S: string; const Fmt: array of const);
begin
  Log( Format(S, Fmt) );
end;

procedure TLogger.LogException(E: Exception);
begin
  ExceptionCaught:=true;

  Log('***Исключение***: ' + E.Message);
  Log('***Исключение*** стек: ');
  DumpExceptionBackTrace(F);
  Flush(F);
end;

procedure TLogger.ExceptionHandler(Sender: TObject; E: Exception);
begin
  LogException(E);
  //Halt(1);
  Application.Terminate;
end;

procedure TLogger.LogDatasetFieldNames(const DatasetName:string; D: TDataset);
var S:String;
  n:Integer;
begin
  S:=DatasetName + ':';
  for n:=0 to D.FieldCount-1 do
    S:=S + Format(' %s(%s)', [D.Fields.Fields[n].FieldName, UpperCase(D.Fields.Fields[n].FieldName)]);
  Log(S);
end;

//==============================================================================
function GlobalLogger:ILogger;
begin
  if Logger = nil then
    begin
      Logger:=TLogger.Create;
    end;
  Result:=Logger;
end;

finalization
  Logger:=nil;

end.

