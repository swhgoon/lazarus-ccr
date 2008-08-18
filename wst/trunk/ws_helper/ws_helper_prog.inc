
resourcestring
  sUSAGE = 'ws_helper [-uMODE] [-p] [-b] [-i] [-w] [-x] [-y] [-d] [-oPATH] [-aPATH] inputFilename' + sNEW_LINE +
           '  -u MODE Generate the pascal translation of the WSDL input file ' + sNEW_LINE +
           '       MODE value may be U for used types or A for all types' + sNEW_LINE +
           '  -p  Generate service proxy' + sNEW_LINE +
           '  -b  Generate service binder' + sNEW_LINE +
           '  -i  Generate service minimal implementation. This will erase any existing implementation file!' + sNEW_LINE +
           '  -o  PATH  Relative output directory' + sNEW_LINE +
           '  -a  PATH  Absolute output directory' + sNEW_LINE +
           '  -w  Generate WSDL file; Can be used to get wsdl from pascal' + sNEW_LINE +
           '  -x  Generate XSD file; Can be used to get xsd from pascal' + sNEW_LINE +
           '  -y  Generate easy access interface for wrapped parameters' + sNEW_LINE +
           '  -d  Generate documentation as comment in the interface file.';
  sCOPYRIGHT = 'ws_helper, Web Service Toolkit 0.6 Copyright (c) 2006, 2007, 2008 by Inoussa OUEDRAOGO';

type
  TSourceFileType = ( sftPascal, sftWSDL, sftXsd );
  
var
  inFileName,outPath,errStr : string;
  srcMngr : ISourceManager;
  AppOptions : TComandLineOptions;
  NextParam : Integer;
  sourceType : TSourceFileType;
  symtable : TwstPasTreeContainer;
  parserMode : TParserMode;
  
  osParam, targetParam : string;

  function ProcessCmdLine():boolean;
  begin
    NextParam := ParseCmdLineOptions(AppOptions);
    if ( NextParam <= Paramcount ) then begin
      inFileName := ParamStr(NextParam);
    end;
    Result := FileExists(ExpandFileName(inFileName));
    if AnsiSameText(ExtractFileExt(inFileName),'.PAS') or
       AnsiSameText(ExtractFileExt(inFileName),'.PP')
    then begin
      sourceType := sftPascal;
    end else if AnsiSameText(ExtractFileExt(inFileName),'.WSDL') then begin
      sourceType := sftWSDL;
    end else if AnsiSameText(ExtractFileExt(inFileName),'.XSD') then begin
      sourceType := sftXsd;
    end;
    if Result then begin
      if ( AppOptions = [] ) then begin
        Include(AppOptions,cloProxy);
      end;
    end else begin
      errStr := Format('File not Found : "%s"',[inFileName]);
    end;
    if ( cloOutPutDirAbsolute in AppOptions ) then begin
      outPath := Trim(GetOptionArg(cloOutPutDirAbsolute));
    end else begin
      outPath := ExtractFilePath(inFileName);
      if ( cloOutPutDirRelative in AppOptions ) then begin
        outPath := outPath + Trim(GetOptionArg(cloOutPutDirRelative));
      end;
    end;
    outPath := IncludeTrailingPathDelimiter(outPath);
    parserMode := pmUsedTypes;
    if AnsiSameText('A',Trim(GetOptionArg(cloInterface))) then begin
      parserMode := pmAllTypes;
    end;

    if ( sourceType = sftXsd ) then begin
      AppOptions := AppOptions - [ cloProxy, cloImp, cloBinder, cloWsdl ];
    end;
  end;

  function GenerateSymbolTable() : Boolean ;

    procedure ParsePascalFile();
    begin
      ParseSource(symtable,inFileName,osParam,targetParam);
    end;
    
    procedure ParseWsdlFile();
    var
      locDoc : TXMLDocument;
      prsrW : IParser;
    begin
      ReadXMLFile(locDoc,inFileName);
{$IFNDEF WST_INTF_DOM}
      try
{$ENDIF}
        prsrW := TWsdlParser.Create(locDoc,symtable);// as IParser;
        prsrW.Execute(parserMode,ChangeFileExt(ExtractFileName(inFileName),''));
{$IFNDEF WST_INTF_DOM}
      finally
        prsrW := nil;
        ReleaseDomNode(locDoc);
      end;
{$ENDIF}
    end;

    procedure ParseXsdFile();
    var
      locDoc : TXMLDocument;
      prsr : IXsdPaser;
    begin
      prsr := nil;
      ReadXMLFile(locDoc,inFileName);
{$IFNDEF WST_INTF_DOM}
      try
{$ENDIF}
      prsr := TXsdParser.Create(locDoc,symtable,ChangeFileExt(ExtractFileName(inFileName),'')) as IXsdPaser;
      prsr.ParseTypes();
{$IFNDEF WST_INTF_DOM}
      finally
        ReleaseDomNode(locDoc);
      end;
{$ENDIF}
    end;

  begin
    try
      WriteLn('Parsing the file : ', inFileName);
      case sourceType of
        sftPascal : ParsePascalFile();
        sftWSDL   : ParseWsdlFile();
        sftXsd    : ParseXsdFile();
      end;
      Result := True;
    except
      on e : Exception do begin
        Result := False;
        errStr := e.Message;
      end;
    end;
  end;

  procedure GenerateWSDLFromTree(ASymbol : TwstPasTreeContainer; ADest : TStream);
  var
    doc : TXMLDocument;
    g : IGenerator;
  begin
    doc := CreateDoc();
    try
      g := TWsdlGenerator.Create(doc);
      g.Execute(ASymbol,ASymbol.CurrentModule.Name);
      WriteXML(doc,ADest);
    finally
      ReleaseDomNode(doc);
    end;
  end;

  procedure GenerateXsdFromTree(ASymbol : TwstPasTreeContainer; ADest : TStream);
  var
    doc : TXMLDocument;
    gnrtr : IGenerator;
  begin
    doc := CreateDoc();
    try
      gnrtr := TXsdGenerator.Create(doc);
      gnrtr.Execute(ASymbol,ASymbol.CurrentModule.Name);
      WriteXML(doc,ADest);
    finally
      ReleaseDomNode(doc);
    end;
  end;
  
  function ProcessFile():Boolean;
  Var
    mtdaFS: TMemoryStream;
    g : TBaseGenerator;
    mg : TMetadataGenerator;
    rsrcStrm : TMemoryStream;
    strStream : TStringStream;
    wrappedParams : Boolean;
  begin
    Result := False;
    wrappedParams := ( cloHandleWrappedParameters in AppOptions );
    strStream := nil;
    rsrcStrm := nil;
    mtdaFS := nil;
    mg := nil;
    g := Nil;
    try
      try
        if ( cloInterface in AppOptions ) then begin
          WriteLn('Interface file generation...');
          g := TInftGenerator.Create(symtable,srcMngr);
          if wrappedParams then
            g.Options := g.Options + [goDocumentWrappedParameter];
          g.Execute();
          FreeAndNil(g);
        end;

        If ( cloProxy in AppOptions ) Then Begin
          WriteLn('Proxy file generation...');
          g := TProxyGenerator.Create(symtable,srcMngr);
          if wrappedParams then
            g.Options := g.Options + [goDocumentWrappedParameter];
          g.Execute();
          FreeAndNil(g);
        End;

        If ( cloBinder in AppOptions ) Then Begin
          WriteLn('Binder file generation...');
          g := TBinderGenerator.Create(symtable,srcMngr);
          if wrappedParams then
            g.Options := g.Options + [goDocumentWrappedParameter];
          g.Execute();
          FreeAndNil(g);
        End;

        If ( cloImp in AppOptions ) Then Begin
          WriteLn('Implementation file generation...');
          g := TImplementationGenerator.Create(symtable,srcMngr);
          if wrappedParams then
            g.Options := g.Options + [goDocumentWrappedParameter];
          g.Execute();
          FreeAndNil(g);
        End;

        if ( [cloBinder,cloProxy]*AppOptions  <> [] ) then begin
          WriteLn('Metadata file generation...');
          mtdaFS := TMemoryStream.Create();
          mg := TMetadataGenerator.Create(symtable,CreateBinaryWriter(mtdaFS));
          mg.Execute();
          //mtdaFS.SaveToFile(ChangeFileExt(inFileName,'.' + sWST_META));
          rsrcStrm := TMemoryStream.Create();
          mtdaFS.Position := 0;
          BinToWstRessource(UpperCase(symtable.CurrentModule.Name),mtdaFS,rsrcStrm);
          rsrcStrm.SaveToFile(outPath + ChangeFileExt(ExtractFileName(inFileName),'.' + sWST_EXTENSION));
        end;
        
        if ( cloWsdl in AppOptions ) then begin
          strStream := TStringStream.Create('');
          GenerateWSDLFromTree(symtable,strStream);
          if not IsStrEmpty(strStream.DataString) then begin
            strStream.Position := 0;
            srcMngr.CreateItem(ChangeFileExt(ExtractFileName(inFileName),'.wsdl')).Write(strStream.DataString);
          end;
        end;

        if ( cloXsd in AppOptions ) then begin
          strStream := TStringStream.Create('');
          GenerateXsdFromTree(symtable,strStream);
          if not IsStrEmpty(strStream.DataString) then begin
            strStream.Position := 0;
            srcMngr.CreateItem(ChangeFileExt(ExtractFileName(inFileName),'.xsd')).Write(strStream.DataString);
          end;
        end;
        
        Result := True;
      except
        on E : Exception do begin
          Result := False;
          errStr := E.Message;
        end;
      end;
    finally
      strStream.Free();
      rsrcStrm.Free();
      mg.Free();
      mtdaFS.Free();
      g.Free();
    end;
  end;

begin
{$IFDEF FPC}
  {$IF Declared(SetHeapTraceOutput) }
    SetHeapTraceOutput('heapOut.txt');
  {$IFEND}
{$ENDIF}
  osParam := 'windows';
  targetParam := 'x86';
  
  SetLogger(TSimpleConsoleLogger.Create());
  
  symtable := nil;
  try
    try
      Writeln(sCOPYRIGHT);
      If ( ParamCount = 0 ) Then Begin
        WriteLn(sUSAGE);
        Exit;
      End;

      if not ProcessCmdLine() then begin
        WriteLn(errStr);
        Exit;
      end;
      symtable := TwstPasTreeContainer.Create();
      srcMngr := CreateSourceManager();

      if not GenerateSymbolTable() then begin
        WriteLn(errStr);
        Exit;
      end;

      If Not ProcessFile() Then Begin
        WriteLn(errStr);
        Exit;
      End;

      srcMngr.SaveToFile(outPath);
      if ( GetLogger().GetMessageCount(mtError) = 0 ) then begin
        WriteLn(Format('File "%s" parsed succesfully.',[inFileName]));
      end else begin
        WriteLn(Format('Paring complete with %d error(s).',[GetLogger().GetMessageCount(mtError)]));
      end;
    finally
      FreeAndNil(symtable);
      SetLogger(nil);
    end;
  except
    on e:exception Do
      Writeln('Exception : ' + e.Message)
  end;
end.
