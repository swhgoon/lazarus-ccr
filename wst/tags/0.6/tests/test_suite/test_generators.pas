{   This file is part of the Web Service Toolkit
    Copyright (c) 2006, 2007, 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit test_generators;

interface
uses
  Classes, SysUtils,
{$IFDEF FPC}
  fpcunit, testutils, testregistry, DOM, XmlRead, XmlWrite, wst_fpc_xml,
{$ELSE}
  TestFrameWork, xmldom, wst_delphi_xml,
{$ENDIF}
  pastree, pascal_parser_intf, xsd_generator, wsdl_generator;

type

  TPropertyType = ( ptField, ptAttribute );

  { TTest_CustomXsdGenerator }

  TTest_CustomXsdGenerator = class(TTestCase)
  protected
    function CreateGenerator(const ADoc : TXMLDocument) : IXsdGenerator;virtual;abstract;
    function LoadXmlFromFilesList(const AFileName : string) : TXMLDocument;
  published
    procedure class_properties_default();
    procedure class_properties_extended_metadata();
    procedure class_extent_native_type();
    procedure class_headerblock_derived();
    procedure class_headerblock_simplecontent_derived();
    procedure class_widestring_property();
{$IFDEF WST_UNICODESTRING}
    procedure class_unicodestring_property();
{$ENDIF WST_UNICODESTRING}
    procedure class_ansichar_property();
    procedure class_widechar_property();
    procedure class_currency_property();
    
    procedure array_sequence_collection();
    procedure class_sequence_open_type_any();
    procedure class_sequence_open_type_any_attribute();
    procedure class_sequence_open_type_any_any_attribute();

    procedure type_alias_widestring();
    procedure type_hint_array_item();
    procedure type_hint_record_item();
  end;

  TTest_XsdGenerator = class(TTest_CustomXsdGenerator)
  protected
    function CreateGenerator(const ADoc : TXMLDocument) : IXsdGenerator;override;
  end;

  { TTest_WsdlGenerator }

  TTest_WsdlGenerator = class(TTestCase)
  protected
    function CreateGenerator(const ADoc : TXMLDocument) : IGenerator;
    function LoadXmlFromFilesList(const AFileName : string) : TXMLDocument;
  published
    procedure message_parts_type_hint();
  end;

implementation

uses
  test_suite_utils, xsd_consts;

{ TTest_CustomXsdGenerator }

procedure TTest_CustomXsdGenerator.class_properties_default();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  procedure AddProperty(
    const AName,
          ATypeName,
          ADefault   : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
{$IFDEF HAS_EXP_TREE}
    p.DefaultExpr := TPrimitiveExpr.Create(p,pekString,ADefault);
{$ELSE HAS_EXP_TREE}
    p.DefaultValue := ADefault;
{$ENDIF HAS_EXP_TREE}
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'class_properties_default',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TClassSampleType',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty('intField','int','1210',ptField);
      AddProperty('strField','string','azerty',ptField);
      AddProperty('floatField','float','1234',ptField);
      AddProperty('strAtt','string','attribute azerty',ptAttribute);
      AddProperty('intAtt','int','789',ptAttribute);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,'.\class_properties_default.xsd');
    locExistDoc := LoadXmlFromFilesList('class_properties_default.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.class_properties_extended_metadata();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  function AddProperty(
    const AName,
          ATypeName,
          ADefault   : string;
    const AKind      : TPropertyType;
    const AExtMetadataName,
          AExtMetadataValue : string
  ) : TPasProperty;
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
{$IFDEF HAS_EXP_TREE}
    p.DefaultExpr := TPrimitiveExpr.Create(p,pekString,ADefault);
{$ELSE HAS_EXP_TREE}
    p.DefaultValue := ADefault;
{$ENDIF HAS_EXP_TREE}
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
    if ( Length(AExtMetadataName) > 0 ) then
      tr.Properties.SetValue(p,AExtMetadataName,AExtMetadataValue);
    Result := p;
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
  p : TPasProperty;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'urn:wst-test',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TClassSampleType',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      p := AddProperty('intField','int','',ptField,'uri-4#a','1210');
        tr.Properties.SetValue(p,'uri-4#b','uri-5#xx');
      AddProperty('strField','string','azerty',ptField,'uri-4#a','http://www.w3.org/2001/XMLSchema#int');
      AddProperty('floatField','float','',ptField,'','');
      AddProperty('strAtt','string','attribute azerty',ptAttribute,'uri-4#a','optional');
      AddProperty('intAtt','int','',ptAttribute,'','');

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,'.\class_properties_extended_metadata.xsd');
    locExistDoc := LoadXmlFromFilesList('class_properties_extended_metadata.xsd');
    Check(CompareNodes(locExistDoc,locDoc),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.class_extent_native_type();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  procedure AddProperty(
    const AName,
          ATypeName,
          ADefault   : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
{$IFDEF HAS_EXP_TREE}
    p.DefaultExpr := TPrimitiveExpr.Create(p,pekString,ADefault);
{$ELSE HAS_EXP_TREE}
    p.DefaultValue := ADefault;
{$ENDIF HAS_EXP_TREE}
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'class_extent_native_type',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TExtendString',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TComplexStringContentRemotable',sXSD_NS) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty('intAtt','int','',ptAttribute);

    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TExtendBase64String',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TBase64StringExtRemotable',sXSD_NS) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty('strAtt','string','',ptAttribute);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,'.\class_extent_native_type.xsd');
    locExistDoc := LoadXmlFromFilesList('class_extent_native_type.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.class_headerblock_derived();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  procedure AddProperty(
    const AName,
          ATypeName,
          ADefault   : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
{$IFDEF HAS_EXP_TREE}
    p.DefaultExpr := TPrimitiveExpr.Create(p,pekString,ADefault);
{$ELSE HAS_EXP_TREE}
    p.DefaultValue := ADefault;
{$ENDIF HAS_EXP_TREE}
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'class_headerblock_derived',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TEmptyHeader',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('THeaderBlock',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);

    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TSampleHeader',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('THeaderBlock',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty('intField','int','',ptField);
      AddProperty('strField','string','',ptField);
      AddProperty('floatField','float','',ptField);
      AddProperty('strAtt','string','',ptAttribute);
      AddProperty('intAtt','int','',ptAttribute);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,'.\class_headerblock_derived.xsd');
    locExistDoc := LoadXmlFromFilesList('class_headerblock_derived.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.class_headerblock_simplecontent_derived();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  procedure AddProperty(
    const AName,
          ATypeName,
          ADefault   : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
{$IFDEF HAS_EXP_TREE}
    p.DefaultExpr := TPrimitiveExpr.Create(p,pekString,ADefault);
{$ELSE HAS_EXP_TREE}
    p.DefaultValue := ADefault;
{$ENDIF HAS_EXP_TREE}
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'class_headerblock_simplecontent_derived',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TSampleHeader',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TSimpleContentHeaderBlock',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty('strAtt','string','',ptAttribute);
      AddProperty('intAtt','int','',ptAttribute);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,'.\class_headerblock_simplecontent_derived.xsd');
    locExistDoc := LoadXmlFromFilesList('class_headerblock_simplecontent_derived.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.class_widestring_property();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  procedure AddProperty(
    const AName,
          ATypeName,
          ADefault   : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
{$IFDEF HAS_EXP_TREE}
    p.DefaultExpr := TPrimitiveExpr.Create(p,pekString,ADefault);
{$ELSE HAS_EXP_TREE}
    p.DefaultValue := ADefault;
{$ENDIF HAS_EXP_TREE}
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'class_widestring_property',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TSampleClass',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty('elementProp','WideString','',ptField);
      AddProperty('elementAtt','WideString','',ptAttribute);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,'.\class_widestring_property.xsd');
    locExistDoc := LoadXmlFromFilesList('class_widestring_property.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

{$IFDEF WST_UNICODESTRING}
procedure TTest_CustomXsdGenerator.class_unicodestring_property();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  procedure AddProperty(
    const AName,
          ATypeName,
          ADefault   : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
{$IFDEF HAS_EXP_TREE}
    p.DefaultExpr := TPrimitiveExpr.Create(p,pekString,ADefault);
{$ELSE HAS_EXP_TREE}
    p.DefaultValue := ADefault;
{$ENDIF HAS_EXP_TREE}
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'class_unicodestring_property',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TSampleClass',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty('elementProp','UnicodeString','',ptField);
      AddProperty('elementAtt','UnicodeString','',ptAttribute);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,'.\class_unicodestring_property.xsd');
    locExistDoc := LoadXmlFromFilesList('class_unicodestring_property.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;
{$ENDIF WST_UNICODESTRING}

procedure TTest_CustomXsdGenerator.array_sequence_collection();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
  arrayTyp : TPasArrayType;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'array_sequence_collection',tr.Package,visDefault,'',0));
    tr.RegisterExternalAlias(mdl,'urn:wst-test');
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexType',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TBaseComplexRemotable',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
    arrayTyp := tr.CreateArray('TCollectionComplexType',cltyp,'field','field',asScoped);
      tr.SetCollectionFlag(arrayTyp,True);
      mdl.InterfaceSection.Declarations.Add(arrayTyp);
      mdl.InterfaceSection.Types.Add(arrayTyp);
      
    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,'array_sequence_collection.xsd');
    locExistDoc := LoadXmlFromFilesList('array_sequence_collection.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.class_sequence_open_type_any();
var
  tr : TwstPasTreeContainer;

  procedure AddProperty(
          AClassType : TPasClassType;
    const AName,
          ATypeName  : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,AClassType,visDefault,'',0));
    AClassType.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  mdl : TPasModule;
  cltyp : TPasClassType;
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'open_type_module',tr.Package,visDefault,'',0));
    tr.RegisterExternalAlias(mdl,'urn:wst-test');
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexType',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TBaseComplexRemotable',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      tr.Properties.SetValue(cltyp,Format('%s#%s',[s_xs,s_any]),'processContents=lax;minOccurs=0;maxOccurs=unbounded');

    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexType2',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TBaseComplexRemotable',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      tr.Properties.SetValue(cltyp,Format('%s#%s',[s_xs,s_any]),'processContents=lax;minOccurs=0;maxOccurs=unbounded');
      AddProperty(cltyp,'strField','string',ptField);

    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexTypeParent',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TBaseComplexRemotable',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty(cltyp,'strFieldParent','string',ptField);
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexTypeChild',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElement('TComplexTypeParent') as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      tr.Properties.SetValue(cltyp,Format('%s#%s',[s_xs,s_any]),'processContents=skip;minOccurs=2;maxOccurs=10');
      AddProperty(cltyp,'strFieldChild','string',ptField);


    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXML(locDoc,'gen_class_sequence_open_type_any.xsd');
    locExistDoc := LoadXmlFromFilesList('gen_class_sequence_open_type_any.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.class_sequence_open_type_any_attribute();
var
  tr : TwstPasTreeContainer;

  procedure AddProperty(
          AClassType : TPasClassType;
    const AName,
          ATypeName  : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,AClassType,visDefault,'',0));
    AClassType.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  mdl : TPasModule;
  cltyp : TPasClassType;
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'open_type_module',tr.Package,visDefault,'',0));
    tr.RegisterExternalAlias(mdl,'urn:wst-test');
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexType',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TBaseComplexRemotable',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      tr.Properties.SetValue(cltyp,Format('%s#%s',[s_xs,s_anyAttribute]),'processContents=lax');

    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexType2',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TBaseComplexRemotable',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      tr.Properties.SetValue(cltyp,Format('%s#%s',[s_xs,s_anyAttribute]),'processContents=strict');
      AddProperty(cltyp,'strField','string',ptField);
      AddProperty(cltyp,'strFieldAtt','string',ptField);

    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexTypeParent',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TBaseComplexRemotable',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty(cltyp,'strFieldParent','string',ptField);
      AddProperty(cltyp,'strFieldParentAtt','string',ptField);
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexTypeChild',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElement('TComplexTypeParent') as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      tr.Properties.SetValue(cltyp,Format('%s#%s',[s_xs,s_anyAttribute]),'processContents=skip');
      AddProperty(cltyp,'strFieldChild','string',ptField);
      AddProperty(cltyp,'strFieldChildAtt','string',ptField);


    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXML(locDoc,'gen_class_sequence_open_type_any_attribute.xsd');
    locExistDoc := LoadXmlFromFilesList('gen_class_sequence_open_type_any_attribute.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.class_sequence_open_type_any_any_attribute();
var
  tr : TwstPasTreeContainer;

  procedure AddProperty(
          AClassType : TPasClassType;
    const AName,
          ATypeName  : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,AClassType,visDefault,'',0));
    AClassType.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  mdl : TPasModule;
  cltyp : TPasClassType;
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'open_type_module',tr.Package,visDefault,'',0));
    tr.RegisterExternalAlias(mdl,'urn:wst-test');
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexType',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TBaseComplexRemotable',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      tr.Properties.SetValue(cltyp,Format('%s#%s',[s_xs,s_anyAttribute]),'processContents=lax');
      tr.Properties.SetValue(cltyp,Format('%s#%s',[s_xs,s_any]),'processContents=lax;minOccurs=0;maxOccurs=unbounded');

    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexType2',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TBaseComplexRemotable',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      tr.Properties.SetValue(cltyp,Format('%s#%s',[s_xs,s_anyAttribute]),'processContents=strict');
      tr.Properties.SetValue(cltyp,Format('%s#%s',[s_xs,s_any]),'processContents=lax;minOccurs=0;maxOccurs=unbounded');
      AddProperty(cltyp,'strField','string',ptField);
      AddProperty(cltyp,'strFieldAtt','string',ptField);

    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexTypeParent',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElementNS('TBaseComplexRemotable',s_xs) as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty(cltyp,'strFieldParent','string',ptField);
      AddProperty(cltyp,'strFieldParentAtt','string',ptField);
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TComplexTypeChild',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      cltyp.AncestorType := tr.FindElement('TComplexTypeParent') as TPasType;
      cltyp.AncestorType.AddRef();
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      tr.Properties.SetValue(cltyp,Format('%s#%s',[s_xs,s_anyAttribute]),'processContents=skip');
      tr.Properties.SetValue(cltyp,Format('%s#%s',[s_xs,s_any]),'processContents=skip;minOccurs=2;maxOccurs=10');
      AddProperty(cltyp,'strFieldChild','string',ptField);
      AddProperty(cltyp,'strFieldChildAtt','string',ptField);


    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXML(locDoc,'gen_class_sequence_open_type_any_anyatt.xsd');
    locExistDoc := LoadXmlFromFilesList('gen_class_sequence_open_type_any_anyatt.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.type_alias_widestring();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  aliasType : TPasAliasType;
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'type_alias_widestring',tr.Package,visDefault,'',0));
    tr.RegisterExternalAlias(mdl,'urn:wst-test');
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    aliasType := TPasAliasType(tr.CreateElement(TPasAliasType,'AliasedType',mdl.InterfaceSection,visDefault,'',0));
      aliasType.DestType := tr.FindElementNS('WideString',s_xs) as TPasType;
      aliasType.DestType.AddRef();
      mdl.InterfaceSection.Declarations.Add(aliasType);
      mdl.InterfaceSection.Types.Add(aliasType);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,wstExpandLocalFileName('type_alias_widestring.xsd'));
    locExistDoc := LoadXmlFromFilesList('type_alias_widestring.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.type_hint_array_item();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  aliasType : TPasArrayType;
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'type_hint_array_item',tr.Package,visDefault,'',0));
    tr.RegisterExternalAlias(mdl,'urn:wst-test');
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    aliasType := tr.CreateArray('AliasedType',tr.FindElementNS('WideString',s_xs) as TPasType,'Item','Item',asScoped);
      mdl.InterfaceSection.Declarations.Add(aliasType);
      mdl.InterfaceSection.Types.Add(aliasType);
    aliasType := tr.CreateArray('EmbeddedAliasedType',tr.FindElementNS('WideString',s_xs) as TPasType,'EmbeddedItem','EmbeddedItem',asScoped);
      mdl.InterfaceSection.Declarations.Add(aliasType);
      mdl.InterfaceSection.Types.Add(aliasType);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,wstExpandLocalFileName('type_hint_array_item.xsd'));
    locExistDoc := LoadXmlFromFilesList('type_hint_array_item.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.type_hint_record_item();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasRecordType;

  procedure AddProperty(
    const AName,
          ATypeName  : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasVariable;
  begin
    p := TPasVariable(tr.CreateElement(TPasVariable,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.Name := AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'type_hint_record_item',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasRecordType(tr.CreateElement(TPasRecordType,'TSampleRecord',mdl.InterfaceSection,visDefault,'',0));
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty('elementProp','WideString',ptField);
      AddProperty('elementAtt','WideString',ptAttribute);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,wstExpandLocalFileName('type_hint_record_item.xsd'));
    locExistDoc := LoadXmlFromFilesList('type_hint_record_item.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

function TTest_CustomXsdGenerator.LoadXmlFromFilesList(const AFileName: string): TXMLDocument;
begin
  ReadXMLFile(Result,wstExpandLocalFileName(TestFilesPath + AFileName));
end;

procedure TTest_CustomXsdGenerator.class_ansichar_property();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  procedure AddProperty(
    const AName,
          ATypeName,
          ADefault   : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
{$IFDEF HAS_EXP_TREE}
    p.DefaultExpr := TPrimitiveExpr.Create(p,pekString,ADefault);
{$ELSE HAS_EXP_TREE}
    p.DefaultValue := ADefault;
{$ENDIF HAS_EXP_TREE}
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'class_ansichar_property',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TSampleClass',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty('elementProp','AnsiChar','',ptField);
      AddProperty('elementAtt','AnsiChar','',ptAttribute);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,'.\class_ansichar_property.xsd');
    locExistDoc := LoadXmlFromFilesList('class_ansichar_property.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.class_widechar_property();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  procedure AddProperty(
    const AName,
          ATypeName,
          ADefault   : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
{$IFDEF HAS_EXP_TREE}
    p.DefaultExpr := TPrimitiveExpr.Create(p,pekString,ADefault);
{$ELSE HAS_EXP_TREE}
    p.DefaultValue := ADefault;
{$ENDIF HAS_EXP_TREE}
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'class_widechar_property',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TSampleClass',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty('elementProp','WideChar','',ptField);
      AddProperty('elementAtt','WideChar','',ptAttribute);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,'.\class_widechar_property.xsd');
    locExistDoc := LoadXmlFromFilesList('class_widechar_property.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

procedure TTest_CustomXsdGenerator.class_currency_property(); 
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  procedure AddProperty(
    const AName,
          ATypeName,
          ADefault   : string;
    const AKind      : TPropertyType
  );
  var
    p : TPasProperty;
  begin
    p := TPasProperty(tr.CreateElement(TPasProperty,AName,cltyp,visDefault,'',0));
    cltyp.Members.Add(p);
    p.ReadAccessorName := 'F' + AName;
    p.WriteAccessorName := 'F' + AName;
    p.VarType := tr.FindElement(ATypeName) as TPasType;
    Check( (p.VarType <> nil), Format('Type not found : "%s".',[ATypeName]));
    p.VarType.AddRef();
{$IFDEF HAS_EXP_TREE}
    p.DefaultExpr := TPrimitiveExpr.Create(p,pekString,ADefault);
{$ELSE HAS_EXP_TREE}
    p.DefaultValue := ADefault;
{$ENDIF HAS_EXP_TREE}
    p.Visibility := visPublished;
    p.StoredAccessorName := 'True';
    if ( AKind = ptAttribute ) then
      tr.SetPropertyAsAttribute(p,True);
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'class_currency_property',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'TSampleClass',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okClass;
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      AddProperty('elementProp','Currency','',ptField);
      AddProperty('elementAtt','Currency','',ptAttribute);

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,'.\class_currency_property.xsd');
    locExistDoc := LoadXmlFromFilesList('class_currency_property.xsd');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;  
end;

{ TTest_XsdGenerator }

function TTest_XsdGenerator.CreateGenerator(const ADoc: TXMLDocument): IXsdGenerator;
begin
  Result := TXsdGenerator.Create(ADoc) as IXsdGenerator;
end;

{ TTest_WsdlGenerator }

function TTest_WsdlGenerator.CreateGenerator(const ADoc : TXMLDocument) : IGenerator;
begin
  Result := TWsdlGenerator.Create(ADoc) as IGenerator;
end;

function TTest_WsdlGenerator.LoadXmlFromFilesList(const AFileName : string) : TXMLDocument;
begin
  ReadXMLFile(Result,wstExpandLocalFileName(TestFilesPath + AFileName));
end;

procedure TTest_WsdlGenerator.message_parts_type_hint();
var
  tr : TwstPasTreeContainer;
  mdl : TPasModule;
  cltyp : TPasClassType;

  procedure AddMethod_EchoWideString();
  var
    p : TPasFunction;
    pt : TPasFunctionType;
    prmDef : TPasArgument;
    prmTypeDef : TPasType;
  begin
    p := TPasFunction(tr.CreateElement(TPasFunction,'EchoWideString',cltyp,visDefault,'',0));
    pt := tr.CreateFunctionType('','result',p,False,'',0);
      pt.ResultEl.ResultType := tr.FindElementNS('WideString',s_xs) as TPasType;
      pt.ResultEl.ResultType.AddRef();
    p.ProcType := pt;

    cltyp.Members.Add(p);
    prmTypeDef := tr.FindElementNS('WideString',s_xs) as TPasType;
    prmDef := TPasArgument(tr.CreateElement(TPasArgument,'AValue',pt,visDefault,'',0));
    pt.Args.Add(prmDef);
    prmDef.ArgType := prmTypeDef;
    prmTypeDef.AddRef();
    prmDef.Access := argConst;
  end;

var
  g : IGenerator;
  locDoc, locExistDoc : TXMLDocument;
begin
  locDoc := nil;
  locExistDoc := nil;
  tr := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(tr);
    mdl := TPasModule(tr.CreateElement(TPasModule,'echo_service',tr.Package,visDefault,'',0));
    tr.Package.Modules.Add(mdl);
    tr.RegisterExternalAlias(mdl,'uri:echo-service');
    mdl.InterfaceSection := TInterfaceSection(tr.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    cltyp := TPasClassType(tr.CreateElement(TPasClassType,'IEchoService',mdl.InterfaceSection,visDefault,'',0));
      cltyp.ObjKind := okInterface;
{$IFDEF HAS_EXP_TREE}
      cltyp.GUIDExpr := TPrimitiveExpr.Create(cltyp,pekString,'{FCD0F68F-3023-46C6-AD09-1DDA4A2989EB}');
{$ELSE HAS_EXP_TREE}
      cltyp.InterfaceGUID := '{FCD0F68F-3023-46C6-AD09-1DDA4A2989EB}';
{$ENDIF HAS_EXP_TREE}
      mdl.InterfaceSection.Declarations.Add(cltyp);
      mdl.InterfaceSection.Types.Add(cltyp);
      tr.AddBinding('IEchoServiceBinding',cltyp);
      AddMethod_EchoWideString();

    locDoc := CreateDoc();
    g := CreateGenerator(locDoc);
    g.Execute(tr,mdl.Name);
    //WriteXMLFile(locDoc,wstExpandLocalFileName('echo_service.wsdl'));
    locExistDoc := LoadXmlFromFilesList('echo_service.wsdl');
    Check(CompareNodes(locExistDoc.DocumentElement,locDoc.DocumentElement),'generated document differs from the existent one.');
  finally
    ReleaseDomNode(locExistDoc);
    ReleaseDomNode(locDoc);
    FreeAndNil(tr);
  end;
end;

initialization
  RegisterTest('XSD-WSDL generator',TTest_XsdGenerator.Suite);
  RegisterTest('XSD-WSDL generator',TTest_WsdlGenerator.Suite);

end.
