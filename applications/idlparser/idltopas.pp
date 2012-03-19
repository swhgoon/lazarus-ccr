program idltopas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  idlParser, idlGenPascal;

type

  { TIDLToPascal }

  TIDLToPascal = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TIDLToPascal }

procedure HandleIDLFile(AFilename: string; AnOutput, ForwardOutput: TStrings; TypeConvList: TStrings; AlwaysAddPrefixToParam: boolean);
var
  AnIDLList: TIDLList;
  AnInput: TStrings;
begin
  AnInput := TStringList.Create;
  AnInput.LoadFromFile(AFilename);
  try
    AnIDLList := TIDLList.create;
    try
      ParseFile(AnIDLList, AnInput);
      GeneratePascalSource(AnIDLList,AnOutput,TypeConvList, AlwaysAddPrefixToParam, ForwardOutput);
    finally
      AnIDLList.Free;
    end;
  finally
    AnInput.Free;
  end;
end;

procedure TIDLToPascal.DoRun;
var
  ErrorMsg: String;
  i: integer;
  filenames: TStrings;
  output, forwardoutput: TStringList;
  OutputToFile: boolean;
  OutputFilename: string;
  TypeMapList: TStrings;
  AlwaysAddPrefixToParam: boolean;

begin
  filenames := TStringList.Create;
  TypeMapList := TStringList.Create;
  try
    // quick check parameters
    ErrorMsg:=CheckOptions('hpo::m:f:',nil,nil,filenames);
    if ErrorMsg<>'' then
      begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
      end;

    // parse parameters
    if (HasOption('h','help')) or (filenames.Count=0) then
      begin
      WriteHelp;
      Terminate;
      Exit;
      end;

    if HasOption('m') then
      begin
      TypeMapList.LoadFromFile(GetOptionValue('m'));
      end;


    OutputToFile := HasOption('o');
    OutputFilename := GetOptionValue('o');
    AlwaysAddPrefixToParam := HasOption('p');

    if HasOption('f') then
      begin
      forwardoutput := TStringList.Create;
      forwardoutput.CaseSensitive:=false;
      forwardoutput.Sorted:=true;
      forwardoutput.Duplicates:=dupIgnore;
      end
    else
      forwardoutput := nil;

    output := TStringList.Create;
    try
      for i := 0 to filenames.Count-1 do
        begin
        if OutputToFile and (OutputFilename='') then
          Output.Clear;

        HandleIDLFile(filenames.Strings[i], output, forwardoutput, TypeMapList, AlwaysAddPrefixToParam);

        if OutputToFile and (OutputFilename='') then
          output.SaveToFile(LowerCase(ExtractFileName(ChangeFileExt(filenames.Strings[i],'.inc'))));
        end;
      if not OutputToFile then
        writeln(output.Text)
      else if OutputFilename<>'' then
        output.SaveToFile(OutputFilename);

      if assigned(forwardoutput) and (forwardoutput.Count<>0) then
        begin
        forwardoutput.Sorted:=false;
        forwardoutput.Insert(0,'type');
        forwardoutput.SaveToFile(GetOptionValue('f'));

        end;

    finally
      output.Free;
      forwardoutput.Free;
    end;

  finally
    filenames.Free;
    TypeMapList.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TIDLToPascal.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TIDLToPascal.Destroy;
begin
  inherited Destroy;
end;

procedure TIDLToPascal.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ApplicationName,' [OPTION] SOURCEFILES');
  writeln('Convert idl-SOURCEFILES to Pascal include files');
  writeln('');
  writeln(' -h           Shows this help message');
  writeln(' -p           Always add the ''a'' or ''an'' prefix to method-parameters names');
  writeln(' -o           Write the output to file(s). The file(s) are written to the current');
  writeln('              directory with the same filenames name as the input files but with');
  writeln('              the ''.inc'' extension');
  writeln(' -o filename  Write the output to one file called ''filename''');
  writeln(' -m filename  Read ''filename'' for a list of mappings between idl-type names');
  writeln('              and their Pascal counterpart');
  writeln(' -f filename  Place all forward declarations into one file called ''filename''');

end;

var
  Application: TIDLToPascal;
begin
  Application:=TIDLToPascal.Create(nil);
  Application.Title:='IDL to Pascal parser';
  Application.Run;
  Application.Free;
end.

