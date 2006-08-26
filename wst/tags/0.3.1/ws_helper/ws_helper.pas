{
    This unit is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}


program ws_helper;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, LResources,
  parserdefs, ws_parser, generator, parserutils, source_utils,
  command_line_parser, metadata_generator, binary_streamer;

resourcestring
  sUSAGE = 'ws_helper [-p] [-b] [-i] [-oPATH] inputFilename' + sNEW_LINE +
           '  -p  Generate service proxy' + sNEW_LINE +
           '  -b  Generate service binder' + sNEW_LINE +
           '  -i  Generate service minimal implementation' + sNEW_LINE +
           '  -o  PATH  Output directory' + sNEW_LINE;
  sCOPYRIGHT = 'ws_helper, Web Service Toolkit 0.3 Copyright (c) 2006 by Inoussa OUEDRAOGO';

const
  sWST_META = 'wst_meta';
  
Var
  inFileName,outPath,errStr : string;
  srcMngr : ISourceManager;
  AppOptions : TComandLineOptions;
  NextParam : Integer;

  function ProcessCmdLine():boolean;
  begin
    NextParam := ParseCmdLineOptions(AppOptions);
    If ( NextParam <= Paramcount ) Then
      inFileName := ParamStr(NextParam);
    Result := FileExists(ExpandFileName(inFileName));
    If Result Then Begin
      If ( AppOptions = [] ) Then
        Include(AppOptions,cloProxy);
    End Else
      errStr := Format('File not Found : "%s"',[inFileName]);
    outPath := ExtractFilePath(inFileName);
    If ( cloOutPutDir in AppOptions ) Then Begin
      outPath := outPath + Trim(GetOptionArg(cloOutPutDir));
      outPath := IncludeTrailingPathDelimiter(outPath);
    End;
  end;

  function ProcessFile():Boolean;
  Var
    p : TPascalParser;
    s : TFileStream;
    mtdaFS: TMemoryStream;
    g : TBaseGenerator;
    mg : TMetadataGenerator;
    rsrcStrm : TMemoryStream;
  begin
    Result := False;
    rsrcStrm := nil;
    mtdaFS := nil;
    mg := nil;
    g := Nil;
    s := Nil;
    p := Nil;
    Try
      Try
        s := TFileStream.Create(inFileName,fmOpenRead);
        p := TPascalParser.Create(s);
        If Not p.Parse() Then
          p.Error(p.ErrorMessage);
        If ( cloProxy in AppOptions ) Then Begin
          g := TProxyGenerator.Create(p.SymbolTable,srcMngr);
          g.Execute();
          FreeAndNil(g);
        End;
        
        If ( cloBinder in AppOptions ) Then Begin
          g := TBinderGenerator.Create(p.SymbolTable,srcMngr);
          g.Execute();
          FreeAndNil(g);
        End;

        If ( cloImp in AppOptions ) Then Begin
          g := TImplementationGenerator.Create(p.SymbolTable,srcMngr);
          g.Execute();
          FreeAndNil(g);
        End;

        if ( [cloBinder,cloProxy]*AppOptions  <> [] ) then begin
          mtdaFS := TMemoryStream.Create();
          mg := TMetadataGenerator.Create(p.SymbolTable,CreateBinaryWriter(mtdaFS));
          mg.Execute();
          mtdaFS.SaveToFile(ChangeFileExt(inFileName,'.' + sWST_META));
          rsrcStrm := TMemoryStream.Create();
          mtdaFS.Position := 0;
          BinaryToLazarusResourceCode(mtdaFS,rsrcStrm,UpperCase(p.SymbolTable.Name),sWST_META);
          rsrcStrm.SaveToFile(outPath + ChangeFileExt(ExtractFileName(inFileName),'.lrs'));
        end;
        
        Result := True;
      Except
        On E : Exception Do Begin
          Result := False;
          errStr := Format('"%s" at line %d',[E.Message,p.SourceLine]) ;
        End;
      End;
    Finally
      rsrcStrm.Free();
      mg.Free();;
      mtdaFS.Free();;
      g.Free();
      p.Free();
      s.Free();
    End;
  end;


begin
  Try
    Writeln(sCOPYRIGHT);
    If ( ParamCount = 0 ) Then Begin
      WriteLn(sUSAGE);
      Exit;
    End;


    srcMngr := CreateSourceManager();
    If Not ProcessCmdLine() Then Begin
      WriteLn(errStr);
      Exit;
    End;

    If Not ProcessFile() Then Begin
      WriteLn(errStr);
      Exit;
    End;

    srcMngr.SaveToFile(outPath);
    WriteLn(Format('File "%s" parsed succesfully.',[inFileName]));
  except
    on e:exception Do
      Writeln('Exception : ' + e.Message)
  end;
end.
