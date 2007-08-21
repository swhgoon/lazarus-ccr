{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit ufrecordedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, ActnList, Menus,
  pastree, pascal_parser_intf,
  edit_helper;

type

  { TfRecordEdit }

  TfRecordEdit = class(TForm)
    actPropAdd : TAction;
    actPropEdit : TAction;
    actPropDelete : TAction;
    actOK : TAction;
    ActionList1 : TActionList;
    Button1 : TButton;
    Button2 : TButton;
    Button3 : TButton;
    Button4 : TButton;
    Button5 : TButton;
    edtName : TEdit;
    GroupBox1 : TGroupBox;
    Label1 : TLabel;
    edtFields : TListView;
    MenuItem1 : TMenuItem;
    MenuItem2 : TMenuItem;
    MenuItem3 : TMenuItem;
    PageControl1 : TPageControl;
    Panel1 : TPanel;
    PopupMenu1 : TPopupMenu;
    TabSheet1 : TTabSheet;
    procedure actOKExecute(Sender : TObject);
    procedure actOKUpdate(Sender : TObject);
    procedure actPropAddExecute(Sender : TObject);
    procedure actPropDeleteExecute(Sender : TObject);
    procedure actPropEditExecute(Sender : TObject);
    procedure actPropEditUpdate(Sender : TObject);
  private
    FUpdateType : TEditType;
    FObject : TPasRecordType;
    FSymbolTable : TwstPasTreeContainer;
  private
    property UpdateType : TEditType read FUpdateType;
  private
    procedure LoadField(AFieldDef : TPasVariable);
    procedure LoadFromObject();
    procedure SaveToObject();
  public
    function UpdateObject(
      var   AObject     : TPasRecordType;
      const AUpdateType : TEditType;
            ASymbolTable : TwstPasTreeContainer
    ):Boolean;
  end; 

var
  fRecordEdit : TfRecordEdit;

implementation
uses common_gui_utils, parserutils, ufpropedit;

{ TfRecordEdit }

procedure TfRecordEdit.actOKUpdate(Sender : TObject);
begin
  TAction(Sender).Enabled := not IsStrEmpty(ExtractIdentifier(edtName.Text));
end;

procedure TfRecordEdit.actPropAddExecute(Sender : TObject);
var
  prp : TPasVariable;
begin
  prp := CreateProperty(FObject,FSymbolTable);
  if Assigned(prp) then begin
    LoadField(prp);
  end;
end;

procedure TfRecordEdit.actPropDeleteExecute(Sender : TObject);
var
  prop : TPasVariable;
begin
  prop := TPasVariable(edtFields.ItemFocused.Data);
  FObject.Members.Extract(prop);
  prop.Release();
  edtFields.ItemFocused.Free();
end;

procedure TfRecordEdit.actPropEditExecute(Sender : TObject);
var
  prp : TPasVariable;
  itm : TListItem;
begin
  itm := edtFields.ItemFocused;
  if Assigned(itm) then begin
    prp := TPasVariable(itm.Data);
    if UpdateProperty(prp,FSymbolTable) then begin
      itm.Free();
      LoadField(prp);
    end;
  end;
end;

procedure TfRecordEdit.actPropEditUpdate(Sender : TObject);
begin
  TAction(Sender).Enabled := Assigned(edtFields.ItemFocused);
end;

procedure TfRecordEdit.actOKExecute(Sender : TObject);
begin
  ModalResult := mrOk;
end;

procedure TfRecordEdit.LoadField(AFieldDef : TPasVariable);
var
  itm : TListItem;
  s, extName : string;
begin
  extName := FSymbolTable.GetExternalName(AFieldDef);
  itm := FindItem(extName,edtFields.Items);
  if ( itm = nil ) then begin
    itm := edtFields.Items.Add();
  end;
  itm.Caption := extName;
  itm.SubItems.Add(FSymbolTable.GetExternalName(AFieldDef.VarType));
  if FSymbolTable.IsAttributeProperty(AFieldDef) then begin
    s := 'Y';
  end else begin
    s := 'N';
  end;
  itm.SubItems.Add(s);
  itm.Data := AFieldDef;
end;

procedure TfRecordEdit.LoadFromObject();
var
  i : Integer;
  prp : TPasVariable;
  extName : string;
begin
  edtName.Text := '';
  edtFields.Clear();
  if Assigned(FObject) then begin
    extName := FSymbolTable.GetExternalName(FObject);
    Self.Caption := extName;
    edtName.Text := extName;
    for i := 0 to Pred(FObject.Members.Count) do begin
      if TPasElement(FObject.Members[i]).InheritsFrom(TPasVariable) then begin
        prp := TPasVariable(FObject.Members[i]);
        LoadField(prp);
      end;
    end;
  end else begin
    Self.Caption := 'New';
  end;
end;

procedure TfRecordEdit.SaveToObject();
var
  typExtName, typIntName : string;
  locObj : TPasRecordType;
begin
  locObj := nil;
  typExtName := ExtractIdentifier(edtName.Text);
  typIntName := MakeInternalSymbolNameFrom(typExtName);
  locObj := FObject;
  locObj.Name := typIntName;
  FSymbolTable.RegisterExternalAlias(locObj,typExtName);
end;

function TfRecordEdit.UpdateObject(
  var   AObject      : TPasRecordType;
  const AUpdateType  : TEditType;
        ASymbolTable : TwstPasTreeContainer
) : Boolean;
begin
  Assert(Assigned(ASymbolTable));
  FSymbolTable := ASymbolTable;
  FUpdateType := AUpdateType;
  FObject := AObject;
  if ( UpdateType = etCreate ) and ( FObject = nil ) then begin
    FObject := TPasRecordType(FSymbolTable.CreateElement(TPasRecordType,'new_record',FSymbolTable.CurrentModule.InterfaceSection,visDefault,'',0));
    FSymbolTable.CurrentModule.InterfaceSection.Declarations.Add(FObject);
    FSymbolTable.CurrentModule.InterfaceSection.Types.Add(FObject);
  end;
  try
    LoadFromObject();
    Result := ( ShowModal() = mrOK );
    if Result then begin
      try
        SaveToObject();
        if ( AUpdateType = etCreate ) then begin
          AObject := FObject;
        end;
      except
        Result := False;
        raise;
      end;
    end;
  finally
    if ( not Result ) and ( UpdateType = etCreate ) and ( AObject = nil ) then begin
      FSymbolTable.CurrentModule.InterfaceSection.Declarations.Extract(FObject);
      FSymbolTable.CurrentModule.InterfaceSection.Types.Extract(FObject);
      FObject.Release();
      FObject := nil;
    end;
  end;
end;

initialization
  {$I ufrecordedit.lrs}

end.

