unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  VirtualTrees, FakeActiveX;

type

  { TMainForm }

  TMainForm = class(TForm)
    ListBox1: TListBox;
    VirtualStringTree1: TVirtualStringTree;
    procedure ListBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure VirtualStringTree1DragDrop(Sender: TBaseVirtualTree;
      Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
      Shift: TShiftState; const Pt: TPoint; var Effect: Integer; Mode: TDropMode
      );
    procedure VirtualStringTree1DragOver(Sender: TBaseVirtualTree;
      Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
      Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure VirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VirtualStringTree1InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

type

  TNodeData = record
    Title: String;
  end;

  PNodeData = ^TNodeData;

{ TMainForm }

procedure TMainForm.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  CellText := PNodeData(Sender.GetNodeData(Node))^.Title;
end;

procedure TMainForm.VirtualStringTree1InitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  PNodeData(Sender.GetNodeData(Node))^.Title := 'VTV Item ' + IntToStr(Node^.Index);
end;

procedure TMainForm.ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = VirtualStringTree1) or (Source = ListBox1);
end;

procedure TMainForm.ListBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  if Source = VirtualStringTree1 then
  begin
    Node := VirtualStringTree1.FocusedNode;
    if Node <> nil then
      ListBox1.Items.Append(VirtualStringTree1.Text[Node, 0]);
  end;
end;

procedure TMainForm.VirtualStringTree1DragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  Node: PVirtualNode;
  S: String;
begin
  if Source = ListBox1 then
  begin
    Node := Sender.AddChild(Sender.DropTargetNode);
    if ListBox1.ItemIndex = -1 then
      S := 'Unknow Item from List'
    else
      S := ListBox1.Items[ListBox1.ItemIndex];
    Sender.ValidateNode(Node, True);
    PNodeData(Sender.GetNodeData(Node))^.Title := S;
  end
  else if Source = Sender then
  begin
    Node := Sender.AddChild(Sender.DropTargetNode);
    Sender.ValidateNode(Node, True);
    PNodeData(Sender.GetNodeData(Node))^.Title := VirtualStringTree1.Text[Sender.FocusedNode, 0];
  end;
end;

procedure TMainForm.VirtualStringTree1DragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := (Sender = VirtualStringTree1) or (Source = ListBox1);
end;

procedure TMainForm.VirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

end.

