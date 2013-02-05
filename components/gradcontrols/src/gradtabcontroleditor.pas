unit gradtabcontroleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, ugradtabcontrol, ComponentEditors, Menus, PropEdits;

type
  { TGradTabControlComponentEditor
  The default component editor for TGradTabControl. }
  TGradTabControlComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure AddNewPageToDesigner(Index: integer); virtual;
    procedure DoAddPage; virtual;
    procedure DoInsertPage; virtual;
    procedure DoDeletePage; virtual;
    procedure DoMoveActivePageLeft; virtual;
    procedure DoMoveActivePageRight; virtual;
    procedure DoMovePage(CurIndex, NewIndex: Integer); virtual;
    procedure AddMenuItemsForPages(ParentMenuItem: TMenuItem); virtual;
    procedure ShowPageMenuItemClick(Sender: TObject);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
    function GradTabControl: TGradTabControl; virtual;
  end;

  { TPageComponentEditor
  The default component editor for TCustomPage. }
  TGradTabPageComponentEditor = class(TGradTabControlComponentEditor)
  protected
  public
    function GradTabControl: TGradTabControl; override;
    function Page: TGradTabPage; virtual;
  end;

implementation

uses
    ObjInspStrConsts;

{ TNotebookComponentEditor }

const
  nbvAddPage       = 0;
  nbvInsertPage    = 1;
  nbvDeletePage    = 2;
  nbvMovePageLeft  = 3;
  nbvMovePageRight = 4;
  nbvShowPage      = 5;

procedure TGradTabControlComponentEditor.ShowPageMenuItemClick(Sender: TObject);
var
  AMenuItem: TMenuItem;
  NewPageIndex: integer;
begin
  AMenuItem:=TMenuItem(Sender);
  if (AMenuItem=nil) or (not (AMenuItem is TMenuItem)) then exit;
  NewPageIndex:=AMenuItem.MenuIndex;
  if (NewPageIndex<0) or (NewPageIndex>=GradTabControl.PageCount) then exit;
  GradTabControl.PageIndex:=NewPageIndex;
  GetDesigner.SelectOnlyThisComponent(GradTabControl.Page[GradTabControl.PageIndex]);
end;

procedure TGradTabControlComponentEditor.AddNewPageToDesigner(Index: integer);
var
  Hook: TPropertyEditorHook;
  NewPage: TGradTabPage;
  NewName: string;
begin
  Hook:=nil;
  if not GetHook(Hook) then exit;
  NewPage:=GradTabControl.Page[Index];
  NewName:=GetDesigner.CreateUniqueComponentName(NewPage.ClassName);
  NewPage.Caption:=NewName;
  NewPage.Name:=NewName;
  GradTabControl.PageIndex:=Index;
  Hook.PersistentAdded(NewPage,true);
  Modified;
end;

procedure TGradTabControlComponentEditor.DoAddPage;
begin
  if not HasHook then exit;
  GradTabControl.Tabs.Add('');
  AddNewPageToDesigner(GradTabControl.PageCount-1);
end;

procedure TGradTabControlComponentEditor.DoInsertPage;
var
  NewIndex: integer;
begin
  if not HasHook then exit;
  NewIndex:=GradTabControl.PageIndex;
  if NewIndex<0 then NewIndex:=0;
  GradTabControl.Tabs.Insert(NewIndex,'');
  AddNewPageToDesigner(NewIndex);
end;

procedure TGradTabControlComponentEditor.DoDeletePage;
var
  Hook: TPropertyEditorHook;
  OldIndex: integer;
  PageComponent: TComponent;
begin
  OldIndex:=GradTabControl.PageIndex;
  if (OldIndex>=0) and (OldIndex<GradTabControl.PageCount) then begin
    if not GetHook(Hook) then exit;
    PageComponent:=TComponent(GradTabControl.Tabs.Objects[OldIndex]);
    Hook.DeletePersistent(TPersistent(PageComponent));
  end;
end;

procedure TGradTabControlComponentEditor.DoMoveActivePageLeft;
var
  Index: integer;
begin
  Index:=GradTabControl.PageIndex;
  if (Index<0) then exit;
  DoMovePage(Index,Index-1);
end;

procedure TGradTabControlComponentEditor.DoMoveActivePageRight;
var
  Index: integer;
begin
  Index:=GradTabControl.PageIndex;
  if (Index>=0)
  and (Index>=GradTabControl.PageCount-1) then exit;
  DoMovePage(Index,Index+1);
end;

procedure TGradTabControlComponentEditor.DoMovePage(
  CurIndex, NewIndex: Integer);
begin
  GradTabControl.Tabs.Move(CurIndex,NewIndex);
  Modified;
end;

procedure TGradTabControlComponentEditor.AddMenuItemsForPages(
  ParentMenuItem: TMenuItem);
var
  i: integer;
  NewMenuItem: TMenuItem;
begin
  ParentMenuItem.Enabled:=GradTabControl.PageCount>0;
  for i:=0 to GradTabControl.PageCount-1 do begin
    NewMenuItem:=TMenuItem.Create(ParentMenuItem);
    NewMenuItem.Name:='ShowPage'+IntToStr(i);
    NewMenuItem.Caption:=GradTabControl.Page[i].Name+' "'+GradTabControl.Tabs[i]+'"';
    NewMenuItem.OnClick:=@ShowPageMenuItemClick;
    ParentMenuItem.Add(NewMenuItem);
  end;
end;

procedure TGradTabControlComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    nbvAddPage:       DoAddPage;
    nbvInsertPage:    DoInsertPage;
    nbvDeletePage:    DoDeletePage; // beware: this can free the editor itself
    nbvMovePageLeft:  DoMoveActivePageLeft;
    nbvMovePageRight: DoMoveActivePageRight;
  end;
end;

function TGradTabControlComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    nbvAddPage:       Result:=nbcesAddPage;
    nbvInsertPage:    Result:=nbcesInsertPage;
    nbvDeletePage:    Result:=nbcesDeletePage;
    nbvMovePageLeft:  Result:=nbcesMovePageLeft;
    nbvMovePageRight: Result:=nbcesMovePageRight;
    nbvShowPage:      Result:=nbcesShowPage;
  else
    Result:='';
  end;
end;

function TGradTabControlComponentEditor.GetVerbCount: Integer;
begin
  Result:=6;
end;

procedure TGradTabControlComponentEditor.PrepareItem(Index: Integer;
  const AnItem: TMenuItem);
begin
  inherited PrepareItem(Index, AnItem);
  case Index of
    nbvAddPage:       ;
    nbvInsertPage:    AnItem.Enabled:=GradTabControl.PageIndex>=0;
    nbvDeletePage:    AnItem.Enabled:=GradTabControl.PageIndex>=0;
    nbvMovePageLeft:  AnItem.Enabled:=GradTabControl.PageIndex>0;
    nbvMovePageRight: AnItem.Enabled:=GradTabControl.PageIndex<GradTabControl.PageCount-1;
    nbvShowPage:      AddMenuItemsForPages(AnItem);
  end;
end;

function TGradTabControlComponentEditor.GradTabControl: TGradTabControl;
begin
  Result:=TGradTabControl(GetComponent);
end;

function TGradTabPageComponentEditor.GradTabControl: TGradTabControl;
var
  APage: TGradTabPage;
begin
  APage:=Page;
  if (APage.Parent<>nil) and (APage.Parent is TGradTabControl) then
    Result:=TGradTabControl(APage.Parent);
end;

function TGradTabPageComponentEditor.Page: TGradTabPage;
begin
  Result:=TGradTabPage(GetComponent);
end;

end.

