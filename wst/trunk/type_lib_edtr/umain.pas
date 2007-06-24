unit umain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Menus, ActnList,
  pastree, pascal_parser_intf, logger_intf,
  SynHighlighterPas, SynEdit, StdCtrls;

type

  { TfMain }

  TfMain = class(TForm)
    actExit: TAction;
    actExport: TAction;
    actAbout: TAction;
    actEnumCreate: TAction;
    actCompoundCreate: TAction;
    actIntfCreate: TAction;
    actFullExpand: TAction;
    actFullCollapse: TAction;
    actNewFile: TAction;
    actRefreshView: TAction;
    actUpdateObject: TAction;
    actSaveAs: TAction;
    actOpenFile: TAction;
    AL: TActionList;
    MainMenu1: TMainMenu;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mmoLog: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    OD: TOpenDialog;
    PC: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SD: TSaveDialog;
    SDD: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    srcInterface: TSynEdit;
    SB: TStatusBar;
    srcImp: TSynEdit;
    srcBinder: TSynEdit;
    srcProxy: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    tsLog: TTabSheet;
    tsImp: TTabSheet;
    tsBinder: TTabSheet;
    tsInterface: TTabSheet;
    tsProxy: TTabSheet;
    trvSchema: TTreeView;
    procedure actAboutExecute(Sender: TObject);
    procedure actCompoundCreateExecute(Sender: TObject);
    procedure actEnumCreateExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actExportUpdate(Sender: TObject);
    procedure actFullCollapseExecute(Sender: TObject);
    procedure actFullExpandExecute(Sender: TObject);
    procedure actIntfCreateExecute(Sender: TObject);
    procedure actNewFileExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actRefreshViewExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actUpdateObjectExecute(Sender: TObject);
    procedure actUpdateObjectUpdate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSymbolTable : TwstPasTreeContainer;
    FStatusMessageTag : PtrInt;
  private
    function GetTypeNode() : TTreeNode;
    function GetInterfaceNode() : TTreeNode;
  private
    procedure ShowStatusMessage(const AMsgType : TMessageType;const AMsg : string);
    procedure RenderSymbols();
    procedure RenderSources();
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy();override;
  end; 

var
  fMain: TfMain;

implementation
uses view_helper, DOM, XMLRead, XMLWrite, HeapTrc,
     wsdl2pas_imp, source_utils, command_line_parser, generator, metadata_generator,
     binary_streamer, wst_resources_utils, wsdl_generator,
     uabout, edit_helper, udm;


const
  DEF_FILE_NAME = 'library1';
  sWST_META = 'wst_meta';
  
type
  TSourceType = cloInterface .. cloBinder;
  TSourceTypes = set of TSourceType;
  
function ParseWsdlFile(
  const AFileName : string;
  const ANotifier : TOnParserMessage
):TwstPasTreeContainer;
var
  locDoc : TXMLDocument;
  prsr : TWsdlParser;
  symName : string;
begin
  Result := nil;
  if FileExists(AFileName) then begin
    symName := ChangeFileExt(ExtractFileName(AFileName),'');
    if ( symName[Length(symName)] = '.' ) then begin
      Delete(symName,Length(symName),1);
    end;
    prsr := nil;
    ReadXMLFile(locDoc,AFileName);
    try
      Result := TwstPasTreeContainer.Create();
      try
        prsr := TWsdlParser.Create(locDoc,Result);
        prsr.OnMessage := ANotifier;
        prsr.Parse(pmAllTypes,symName);
      except
        FreeAndNil(Result);
        raise;
      end;
    finally
      FreeAndNil(prsr);
      FreeAndNil(locDoc);
    end;
  end;
end;

type TOutputType = ( otMemory, otFileSystem );
function GenerateSource(
        ASymbolTable : TwstPasTreeContainer;
        AOptions     : TSourceTypes;
  const AOutputType  : TOutputType;
  const AOutPath     : string;
  const ANotifier    : TOnParserMessage
) : ISourceManager;

  procedure Notify(const AMsg : string);
  begin
    if Assigned(ANotifier) then begin
      ANotifier(mtInfo, AMsg);
    end;
  end;
  
var
  mtdaFS: TMemoryStream;
  g : TBaseGenerator;
  mg : TMetadataGenerator;
  rsrcStrm : TMemoryStream;
begin
  Result := CreateSourceManager();
  rsrcStrm := nil;
  mtdaFS := nil;
  mg := nil;
  g := Nil;
  try
  
    if ( cloInterface in AOptions ) then begin
      Notify('Interface file generation...');
      g := TInftGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( cloProxy in AOptions ) then begin
      Notify('Proxy file generation...');
      g := TProxyGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( cloBinder in AOptions ) then begin
      Notify('Binder file generation...');
      g := TBinderGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( cloImp in AOptions ) then begin
      Notify('Implementation file generation...');
      g := TImplementationGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( AOutputType = otFileSystem ) and ( [cloBinder,cloProxy]*AOptions  <> [] ) then begin
      Notify('Metadata file generation...');
      mtdaFS := TMemoryStream.Create();
      mg := TMetadataGenerator.Create(ASymbolTable,CreateBinaryWriter(mtdaFS));
      mg.Execute();
      mtdaFS.SaveToFile(AOutPath + Format('%s.%s',[ASymbolTable.CurrentModule.Name,sWST_META]));
      rsrcStrm := TMemoryStream.Create();
      mtdaFS.Position := 0;
      BinToWstRessource(UpperCase(ASymbolTable.CurrentModule.Name),mtdaFS,rsrcStrm);
      rsrcStrm.SaveToFile(AOutPath + Format('%s.%s',[ASymbolTable.CurrentModule.Name,sWST_EXTENSION]));
    end;
    
    if ( AOutputType = otFileSystem ) then begin
      Result.SaveToFile(AOutPath);
    end;
  finally
    rsrcStrm.Free();
    mg.Free();;
    mtdaFS.Free();;
    g.Free();
  end;
end;

procedure GenerateWSDL_ToStream(ASymbol : TwstPasTreeContainer; ADest : TStream);
var
  doc : TXMLDocument;
begin
  doc := TXMLDocument.Create();
  try
    GenerateWSDL(ASymbol,doc);
    WriteXML(doc,ADest);
  finally
    FreeAndNil(doc);
  end;
end;

function CreateSymbolTable(const AName : string):TwstPasTreeContainer ;
begin
  Result := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(Result);
    Result.CreateElement(TPasModule,AName,Result.Package,visDefault,'',0);
    Result.CurrentModule.InterfaceSection := TPasSection(Result.CreateElement(TPasSection,'',Result.CurrentModule,visDefault,'',0));
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TfMain }

procedure TfMain.actOpenFileExecute(Sender: TObject);
var
  tmpTable : TwstPasTreeContainer;
  curLok : IInterface;
begin
  if OD.Execute() then begin
    mmoLog.Clear();
    PC.ActivePage := tsLog;
    curLok := SetCursorHourGlass();
    tmpTable := ParseWsdlFile(OD.FileName,@ShowStatusMessage);
    if Assigned(tmpTable) then begin
      trvSchema.Items.Clear();
      FreeAndNil(FSymbolTable);
      FSymbolTable := tmpTable;
      RenderSymbols();
      PC.ActivePage := tsInterface;
    end;
  end;
  curLok := nil;
end;

procedure TfMain.actRefreshViewExecute(Sender: TObject);
begin
  RenderSymbols();
end;

procedure TfMain.actSaveAsExecute(Sender: TObject);
var
  mstrm : TMemoryStream;
begin
  if SD.Execute() then begin
    mstrm := TMemoryStream.Create();
    try
      GenerateWSDL_ToStream(FSymbolTable,mstrm);
      mstrm.SaveToFile(SD.FileName);
    finally
      FreeAndNil(mstrm);
    end;
  end;
end;

procedure TfMain.actUpdateObjectExecute(Sender: TObject);
var
  o : TPasElement;
  nd, nd_1 : TTreeNode;
begin
  nd := trvSchema.Selected;
  if Assigned(nd) and Assigned(nd.Data) then begin
    o := TPasElement(nd.Data);
    if HasEditor(o) then begin
      UpdateObject(o,FSymbolTable);
      nd_1 := nd;
      trvSchema.BeginUpdate();
      try
        nd := FindPainter(o).Paint(FSymbolTable,o,GetTypeNode());
        nd.MoveTo(nd_1,naInsertBehind);
        FreeAndNil(nd_1);
      finally
        trvSchema.EndUpdate();
      end;
    end;
  end;
end;

procedure TfMain.actUpdateObjectUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    Assigned(trvSchema.Selected) and
    Assigned(trvSchema.Selected.Data) and
    HasEditor(TPasElement(trvSchema.Selected.Data));
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  RenderSymbols();
end;

function TfMain.GetTypeNode(): TTreeNode;
begin
  Result := trvSchema.TopItem.GetFirstChild().Items[1];
end;

function TfMain.GetInterfaceNode(): TTreeNode;
begin
  Result := trvSchema.TopItem.GetFirstChild().Items[2];
end;

procedure TfMain.ShowStatusMessage(const AMsgType : TMessageType;const AMsg: string);
begin
  mmoLog.Lines.Add(Format('%s : %s',[MessageTypeNames[AMsgType],AMsg]));
  SB.Panels[1].Text := AMsg;
  Inc(FStatusMessageTag);
  if ( (FStatusMessageTag) > 23 ) then begin
    FStatusMessageTag := 0;
    Application.ProcessMessages();
  end;
end;

procedure TfMain.actExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TfMain.actAboutExecute(Sender: TObject);
var
  fa : TfAbout;
begin
  fa := TfAbout.Create(Self);
  try
    fa.ShowModal();
  finally
    fa.Release();
  end;
end;

procedure TfMain.actCompoundCreateExecute(Sender: TObject);
var
  e : TPasClassType;
begin
  e := CreateCompoundObject(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetTypeNode());
  end;
end;

procedure TfMain.actEnumCreateExecute(Sender: TObject);
var
  e : TPasEnumType;
begin
  e := CreateEnum(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetTypeNode());
  end;
end;

procedure TfMain.actExportExecute(Sender: TObject);
var
  curLok : IInterface;
begin
  if SDD.Execute() then begin
    curLok := SetCursorHourGlass();
    GenerateSource(
      FSymbolTable,
      [cloInterface,cloProxy,cloImp,cloBinder],
      otFileSystem,
      IncludeTrailingBackslash(SDD.FileName),
      nil
    );
    curLok := nil;
  end;
end;

procedure TfMain.actExportUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(FSymbolTable) and ( FSymbolTable.CurrentModule.InterfaceSection.Declarations.Count > 0 );
end;

procedure TfMain.actFullCollapseExecute(Sender: TObject);
begin
  trvSchema.FullCollapse();
end;

procedure TfMain.actFullExpandExecute(Sender: TObject);
begin
  trvSchema.FullExpand();
end;

procedure TfMain.actIntfCreateExecute(Sender: TObject);
var
  e : TPasClassType;
begin
  e := CreateInterface(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetInterfaceNode());
  end;
end;

procedure TfMain.actNewFileExecute(Sender: TObject);
var
  res : Integer;
begin
  res := MessageDlg(Application.Title,'Save the current file before ?',mtConfirmation,mbYesNoCancel,0,mbYes);
  if ( res = mrCancel ) then begin
    Exit;
  end;
  if ( res = mrYes ) then begin
    actSaveAs.Execute();
  end;
  FreeAndNil(FSymbolTable);
  FSymbolTable := CreateSymbolTable(ExtractFileName(DEF_FILE_NAME));
  RenderSymbols();
end;

procedure TfMain.RenderSymbols();
var
  objPtr : ISymbolPainter;
  nd : TTreeNode;
begin
  trvSchema.BeginUpdate();
  try
    trvSchema.Items.Clear();
    srcInterface.ClearAll();
    nd := trvSchema.Items.AddFirst(nil,'Type Library Editor');
    nd.ImageIndex := -1;
    nd.StateIndex := -1;
    nd.SelectedIndex := -1;
    if Assigned(FSymbolTable) then begin
      objPtr := FindPainter(FSymbolTable.Package);
      if Assigned(objPtr) then begin
        objPtr.Paint(FSymbolTable,FSymbolTable.Package,trvSchema.TopItem);
      end;
      RenderSources();
    end;
    trvSchema.Items[0].Expand(False);
    trvSchema.Items[0].Items[0].Expand(False);
  finally
    trvSchema.EndUpdate();
  end;
  ShowStatusMessage(mtInfo,'');
end;

procedure TfMain.RenderSources();

  procedure LoadText(const AList : TStrings; ASrc : ISourceStream);
  var
    srcItemSV : ISavableSourceStream;
  begin
    if Supports(ASrc,ISavableSourceStream,srcItemSV) then begin
      srcItemSV.GetStream().Position := 0;
      AList.LoadFromStream(srcItemSV.GetStream());
    end;
  end;

var
  srcMngr : ISourceManager;
begin
  if Assigned(FSymbolTable) then begin
    if ( FSymbolTable.CurrentModule.InterfaceSection.Declarations.Count > 0 ) then begin
      srcMngr := GenerateSource(FSymbolTable,[cloInterface,cloProxy,cloBinder,cloImp],otMemory,'',@ShowStatusMessage);
      if Assigned(srcMngr) and ( srcMngr.GetCount() > 0 ) then begin
        LoadText(srcInterface.Lines,srcMngr.GetItem(0));
        LoadText(srcProxy.Lines,srcMngr.GetItem(1));
        LoadText(srcBinder.Lines,srcMngr.GetItem(2));
        LoadText(srcImp.Lines,srcMngr.GetItem(3));
      end;
    end else begin
      srcInterface.ClearAll();
      srcProxy.ClearAll();
      srcBinder.ClearAll();
      srcImp.ClearAll();
    end;
  end;
  ShowStatusMessage(mtInfo,'');
end;

constructor TfMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSymbolTable := CreateSymbolTable(ExtractFileName(DEF_FILE_NAME));
  trvSchema.Images := DM.IM;
end;

destructor TfMain.Destroy();
begin
  trvSchema.Items.Clear();
  FreeAndNil(FSymbolTable);
  inherited Destroy();
end;

initialization
  SetHeapTraceOutput('heap_trace.txt');
  
  {$I umain.lrs}

end.

