unit dxftokentotree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,
  dxfvectorialreader;

procedure ConvertDXFTokensToTreeView(ATokens: TDXFTokens; ATreeView: TTreeView);
procedure ConvertDXFTokensToTreeNodes(ATokens: TDXFTokens; ATreeNodes: TTreeNodes; ABaseNode: TTreeNode);

implementation

procedure ConvertDXFTokensToTreeView(ATokens: TDXFTokens; ATreeView: TTreeView);
begin
  ATreeView.Items.Clear;
  ConvertDXFTokensToTreeNodes(ATokens, ATreeView.Items, ATreeView.Items.GetFirstNode);
end;

procedure ConvertDXFTokensToTreeNodes(ATokens: TDXFTokens;
  ATreeNodes: TTreeNodes; ABaseNode: TTreeNode);
var
  AToken: TDXFToken;
  NodeStr: string;
  NewNode: TTreeNode;
  i: Integer;
begin
  if ATokens = nil then Exit;

  ATreeNodes.BeginUpdate(); // Greatly speeds up the operation
  try
    for i := 0 to ATokens.Count - 1 do
    begin
      AToken := TDXFToken(ATokens.Items[i]);
      NodeStr := Format('(%d %s)', [AToken.GroupCode, AToken.StrValue]);
      NewNode := ATreeNodes.AddChild(ABaseNode, NodeStr);
      ConvertDXFTokensToTreeNodes(AToken.Childs, NewNode.TreeNodes, NewNode);
    end;
  finally
    ATreeNodes.EndUpdate();
  end;
end;

end.

