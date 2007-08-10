unit folderlister;

{$I rx.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus;


type
  { TCustomFolderLister }

  TCustomFolderLister = class(TComponent)
  private
    FDefaultExt: string;
    FMenuItem: TMenuItem;
    FOnExecuteItem: TNotifyEvent;
    FFileFolder: string;
    FFileList:TStringList;
    procedure DoFind(S:string; MenuItem:TMenuItem);
    function GetCount: integer;
    function GetFiles(Item: integer): string;
    procedure SetMenuItem(const AValue: TMenuItem);
    procedure SetFileFolder(const AValue: string);
  protected
    property FileFolder:string read FFileFolder write SetFileFolder;
    property OnExecuteItem:TNotifyEvent read FOnExecuteItem write FOnExecuteItem;
    property MenuItem:TMenuItem read FMenuItem write SetMenuItem;
    property DefaultExt:string read FDefaultExt write FDefaultExt;
    procedure InternalExecute(Sender: TObject);virtual;
  public
    procedure Execute;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Files[Item:integer]:string read GetFiles;
    property Count:integer read GetCount;
  published
  end;

type
  TFolderLister = class(TCustomFolderLister)
  published
    property DefaultExt;
    property FileFolder;
    property OnExecuteItem;
    property MenuItem;
  end;
  
implementation
uses RxStrUtils, RxAppUtils;

function MenuItemStr(S:string):string;
var
  i:integer;
begin
  Result:=Copy2Symb(ExtractFileName(S), '.');
  if Result='' then exit;
  for i:=1 to Length(Result) do
  begin
    if Result[i]='\' then Result[i]:='/' else
    if Result[i]='_' then Result[i]:='.';
  end;
end;

{ TCustomFolderLister }
procedure TCustomFolderLister.DoFind(S: string; MenuItem: TMenuItem);
var
  Rec:TSearchRec;
  R:integer;
  AFileList,
  AFolderList:TStringList;

procedure CreateItems;
var
  i:integer;
  M:TMenuItem;
begin
  for I:=0 to AFileList.Count-1 do
  begin
    FFileList.Add(AFileList[i]);
    M:=TMenuItem.Create(Application.MainForm);
    M.Caption:=MenuItemStr(AFileList[i]);
    M.Hint:=MenuItemStr(AFileList[i]);
    MenuItem.Add(M);
    M.Tag:=FFileList.Count-1;
    M.OnClick:=@InternalExecute;
  end;
end;

procedure CreateSubItems;
var
  i:integer;
  M:TMenuItem;
begin
  for i:=0 to AFolderList.Count-1 do
  begin
    M:=TMenuItem.Create(Application.MainForm);
    M.Caption:=MenuItemStr(AFolderList[i]);
    MenuItem.Add(M);
    DoFind(AFolderList[i]+DirectorySeparator,M);
  end;
end;

begin
  AFolderList:=TStringList.Create;
  AFolderList.Sorted:=true;
  AFileList:=TStringList.Create;
  AFolderList.Sorted:=true;
  try
    R:=FindFirst(S+AllMask,faAnyFile, Rec);
    while R=0 do
    begin
      if ((Rec.Attr and faDirectory) <>0) and (Rec.Name<>'.') and (Rec.Name<>'..') then
        AFolderList.Add(S+Rec.Name)
      else
      begin
        if AnsiLowerCase(ExtractFileExt(Rec.Name))=AnsiLowerCase(FDefaultExt) then
          AFileList.Add(S+Rec.Name);
      end;
      R:=FindNext(Rec);
    end;
    FindClose(Rec);
    CreateSubItems;
    CreateItems;
  finally
    AFolderList.Free;
    AFileList.Free;
  end;
end;

function TCustomFolderLister.GetCount: integer;
begin
  Result:=FFileList.Count;
end;

function TCustomFolderLister.GetFiles(Item: integer): string;
begin
  Result:=FFileList[Item];
end;

procedure TCustomFolderLister.SetMenuItem(const AValue: TMenuItem);
begin
  if FMenuItem=AValue then exit;
  FMenuItem:=AValue;
end;

procedure TCustomFolderLister.SetFileFolder(const AValue: string);
begin
  if FFileFolder=AValue then exit;
  FFileFolder:=AValue;
  if FFileFolder<>'' then
    if FFileFolder[Length(FFileFolder)]<>DirectorySeparator then
      FFileFolder:=FFileFolder+DirectorySeparator;
end;

procedure TCustomFolderLister.InternalExecute(Sender: TObject);
begin
  if Assigned(FOnExecuteItem) then
    FOnExecuteItem(Sender)
end;

procedure TCustomFolderLister.Execute;
begin
  if Assigned(FMenuItem) then
    DoFind(FFileFolder, FMenuItem)
  else
    raise Exception.Create(Name+'. Not assigned property MenuItem');
end;

constructor TCustomFolderLister.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileList:=TStringList.Create;
  FFileList.Sorted:=false;
end;

destructor TCustomFolderLister.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

end.
