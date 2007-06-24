unit uinterfaceedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ActnList,
  ExtCtrls, ComCtrls, StdCtrls, Buttons,
  pastree, pascal_parser_intf,
  edit_helper;

type

  { TfInterfaceEdit }

  TfInterfaceEdit = class(TForm)
    actOK: TAction;
    AL: TActionList;
    Button1: TButton;
    Button2: TButton;
    edtName: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    PC: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    trvMethods: TTreeView;
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasClassType;
    FSymbolTable : TwstPasTreeContainer;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure LoadMethod(AMthDef : TPasProcedure);
    procedure LoadFromObject();
    procedure SaveToObject();
  public
    function UpdateObject(
      var   AObject     : TPasClassType;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;
  end; 

var
  fInterfaceEdit: TfInterfaceEdit;

implementation
uses view_helper, parserutils, udm;

{ TfInterfaceEdit }

procedure TfInterfaceEdit.actOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := not IsStrEmpty(ExtractIdentifier(edtName.Text));
end;

procedure TfInterfaceEdit.FormCreate(Sender: TObject);
begin
  trvMethods.Images := DM.IM;
end;

procedure TfInterfaceEdit.actOKExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfInterfaceEdit.LoadMethod(AMthDef: TPasProcedure);
var
  topNode : TTreeNode;
begin
  topNode := trvMethods.Items[0];
  FindPainter(AMthDef).Paint(FSymbolTable,AMthDef,topNode);
end;

procedure TfInterfaceEdit.LoadFromObject();
var
  i : Integer;
  mthd : TPasProcedure;
  extName : string;
begin
  edtName.Text := '';
  trvMethods.BeginUpdate();
  try
    trvMethods.Items.Clear();
    trvMethods.Items.AddFirst(nil,'Methods');
    if Assigned(FObject) then begin
      extName := FSymbolTable.GetExternalName(FObject);
      Self.Caption := extName;
      edtName.Text := extName;
      for i := 0 to Pred(FObject.Members.Count) do begin
        if TPasElement(FObject.Members[i]).InheritsFrom(TPasProcedure) then begin
          mthd := TPasProcedure(FObject.Members[i]);
          LoadMethod(mthd);
        end;
      end;
    end else begin
      Self.Caption := 'New';
    end;
    trvMethods.Items[0].Expand(False);
  finally
    trvMethods.EndUpdate();
  end;
end;

procedure TfInterfaceEdit.SaveToObject();
var
  typExtName, typIntName : string;
begin
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  FObject.Name := typIntName;
  FSymbolTable.RegisterExternalAlias(FObject,typExtName);
end;

function TfInterfaceEdit.UpdateObject(
  var   AObject      : TPasClassType;
  const AUpdateType  : TEditType;
        ASymbolTable : TwstPasTreeContainer
): Boolean;
var
  intName : string;
  i : Integer;
begin
  Assert(Assigned(ASymbolTable));
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  if ( FUpdateType = etCreate ) then begin
    i := 1;
    intName := 'ISampleService';
    while ( FSymbolTable.FindElementInModule(intName,FSymbolTable.CurrentModule) <> nil ) do begin
      intName := 'ISampleService' + IntToStr(i);
      Inc(i);
    end;
    FObject := TPasClassType(FSymbolTable.CreateElement(TPasClassType,intName,FSymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    FObject.ObjKind := okInterface;
    FSymbolTable.CurrentModule.InterfaceSection.Declarations.Add(FObject);
    FSymbolTable.CurrentModule.InterfaceSection.Types.Add(FObject);
  end;
  try
    LoadFromObject();
    Result := ( ShowModal() = mrOK );
    if Result then begin
      SaveToObject();
      if ( AUpdateType = etCreate ) then begin
        AObject := FObject;
      end;
    end;
  except
    if ( FUpdateType = etCreate ) then begin
      FSymbolTable.CurrentModule.InterfaceSection.Declarations.Extract(FObject);
      FSymbolTable.CurrentModule.InterfaceSection.Types.Extract(FObject);
      FObject.Release();
      AObject := nil;
    end;
    raise;
  end;
end;

initialization
  {$I uinterfaceedit.lrs}

end.

