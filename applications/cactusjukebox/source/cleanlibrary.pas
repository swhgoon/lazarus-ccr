unit CleanLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls;

type
  strArray = array of string;
  ptrStrArray = ^strArray;
  
type

  { TFrmCleanLibrary }

  TFrmCleanLibrary = class(TForm)
    btnBack: TButton;
    btnReset: TButton;
    btnRemove: TButton;
    btnDisplay: TButton;
    btnBack2: TButton;
    btnUnselect: TButton;
    btnRemoveFromDisk: TButton;
    chkFolders: TCheckBox;
    Label3: TLabel;
    lstRemove: TListBox;
    Panel2: TPanel;
    pnlProgress: TPanel;
    txtNotToRemove: TEdit;
    Label1: TLabel;
    dirlistview: TListBox;
    Label2: TLabel;
    Panel1: TPanel;
    procedure btnBack2Click(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnDisplayClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnRemoveFromDiskClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnUnselectClick(Sender: TObject);
    procedure dirlistviewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstRemoveClick(Sender: TObject);
  private
    { private declarations }
    procedure Recursive_AddDir
              (dir: String; strExtension: ptrStrArray; var bContainesFiles: Boolean);
  public
  
    { public declarations }
  end; 

var
  FrmCleanLibrary: TFrmCleanLibrary;
  
implementation

uses
  mediacol, functions, config, mainform;
  


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TFrmCleanLibrary.FormCreate(Sender: TObject);
Var i: integer;
Begin
  dirlistview.Clear;
  For i:= 0 To MediaCollection.dirlist.Count-1 Do
    Begin
      dirlistview.Items.Add(MediaCollection.dirlist[i]);
    End;
  btnReset.Enabled := false;
  dirlistviewClick(Sender);
  txtNotToRemove.Text := CactusConfig.strCleanLibNotToRemove;
End;

procedure TFrmCleanLibrary.FormDestroy(Sender: TObject);
begin
  CactusConfig.strCleanLibNotToRemove := txtNotToRemove.Text;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.lstRemoveClick(Sender: TObject);
begin
  if (lstRemove.ItemIndex = -1 )
  then
    btnUnselect.Enabled := false
  else
    btnUnselect.Enabled := true;
    
  if lstRemove.Items.Count = 0
  then
    btnRemove.Enabled := false
  else
    btnRemove.Enabled := true;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.btnResetClick(Sender: TObject);
begin
  FormCreate(Sender);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.btnUnselectClick(Sender: TObject);
var
  i: integer;
begin
  i := lstRemove.ItemIndex;
  if i <> -1
  then
  begin
    lstRemove.Items.Delete(i);
    if lstRemove.Items.Count > i-1
    then
      lstRemove.ItemIndex := i;
    lstRemoveClick(Sender);
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.dirlistviewClick(Sender: TObject);
var i: integer;
begin
  if (dirlistview.ItemIndex = -1 )
  then
    btnRemove.Enabled := false
  else
    btnRemove.Enabled := true;
    
  if (dirlistview.Items.Count <> 0)
  then
    btnDisplay.Enabled := true;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.btnRemoveClick(Sender: TObject);
begin
  if (dirlistview.ItemIndex <> -1 )
  then
  begin
    dirlistview.Items.Delete(dirlistview.ItemIndex);
    btnReset.Enabled := true;
  end;

  if (dirlistview.Items.Count = 0)
  then
    btnDisplay.Enabled := false;
end;

procedure TFrmCleanLibrary.btnRemoveFromDiskClick(Sender: TObject);
var
  i, index: integer;
begin
  pnlProgress.Visible := true;
  btnBack2.Enabled := false;
  btnUnselect.Enabled := false;

  // remove from disk & collection
  for i := 1 to lstRemove.Items.Count do
  begin
    if DirectoryExists(lstRemove.Items[i-1])
    then
      RemoveDir(lstRemove.Items[i-1])
    else
    begin
      DeleteFile(lstRemove.Items[i-1]);
      index := MediaCollection.getIndexByPath(lstRemove.Items[i-1]);
      if index > 0 then
         MediaCollection.remove(index);
    end;
  end;
  lstRemove.Clear;

  lstRemoveClick(Sender);
  btnBack2.Enabled := true;
  pnlProgress.Visible := false;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.btnBackClick(Sender: TObject);
begin
  Close;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.btnBack2Click(Sender: TObject);
begin
  Panel2.Visible:= false;
  Panel1.Visible:= true;
  btnBack.Cancel:= true;
  btnDisplay.Default:=true;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.btnDisplayClick(Sender: TObject);
var
  strExtension: array of string;
  strTmp: string;
  i, j: integer;
  bDummy: Boolean;
begin
  Panel1.Visible:= false;
  Panel2.Visible:= true;
  btnBack2.Cancel:= true;
  btnRemoveFromDisk.Default:= true;
  lstRemove.Clear;
  btnBack2.Enabled:= false;
  pnlProgress.Visible := true;
  
  Application.ProcessMessages;
  
  // enumerate file extensions to skip
  i := 0;
  SetLength(strExtension, 0);
  strTmp := Trim(txtNotToRemove.Text);
  strTmp := lowercase(strTmp);
  while (strlen(PChar(strTmp)) <> 0) do
  begin
    inc(i);
    SetLength(strExtension, i);

    j := Pos(' ', strTmp);
    if j > 0
    then
    begin
      strExtension[i-1] := Copy(strTmp, 1, j-1);
      Delete(strTmp, 1, j)
    end
    else
    begin
      strExtension[i-1] := strTmp;
      strTmp := '';
    end;
  end;
  
  // start searching the folders
  for i := 1 to dirlistview.Items.Count do
    Recursive_AddDir(dirlistview.Items[i-1], @strExtension, bDummy);

  pnlProgress.Visible := false;
  btnBack2.Enabled:= true;
  lstRemoveClick(Sender);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TFrmCleanLibrary.Recursive_AddDir
          (dir: String; strExtension: ptrStrArray; var bContainesFiles: Boolean);

Var
  mp3search,dirsearch: TSearchrec;
  tmps: string;
  i: integer;
  bContinue: Boolean;
  bContainesFiles_lokal: Boolean;
  files, folders: TStringList;
Begin
  dir := IncludeTrailingPathDelimiter(dir);
  bContainesFiles := false;
  files := TStringList.Create;
  folders := TStringList.Create;

  // list files with non-matching extensions
  If FindFirst(dir+'*', faAnyFile - faDirectory, mp3search)=0 Then
    Repeat
    Begin
      if ((mp3search.name = '.') or (mp3search.name = '..')) then
        continue;
      tmps := lowercase(ExtractFileExt(mp3search.name));
      Delete(tmps, 1, 1);
      bContinue := false;
      for i := 1 to Length(strExtension^) do
        if tmps = strExtension^[i-1] then
        begin
          bContainesFiles := true;
          bContinue := true;
          continue;
        end;
      if bContinue then continue;
      tmps := dir + mp3search.name;
      files.Add(tmps);
    End;
    Until FindNext(mp3search)<>0;
  Findclose(mp3search);
  BubbleSort(files);
  for i := 1 to files.Count do
    lstRemove.Items.Add(files[i-1]);
  

  // walk into subfolders
  If Findfirst(dir+'*', FaDirectory, dirsearch)=0 Then
    Repeat
      If (dirsearch.name<>'..') And (dirsearch.name<>'.') Then
        if (dirsearch.attr And FaDirectory)=FaDirectory Then
        begin
          Recursive_AddDir(dir+dirsearch.name, strExtension, bContainesFiles_lokal);
          if (NOT bContainesFiles_lokal) and (chkFolders.Checked) then
            folders.Add(IncludeTrailingPathDelimiter(dir+dirsearch.name))
          else
            bContainesFiles := true;
        end;
    Until FindNext(dirsearch)<>0;
  Findclose(dirsearch);
  BubbleSort(folders);
  for i := 1 to folders.Count do
    lstRemove.Items.Add(folders[i-1]);
End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

initialization
  {$I cleanlibrary.lrs}

end.

unit CleanLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls;

type
  strArray = array of string;
  ptrStrArray = ^strArray;
  
type

  { TFrmCleanLibrary }

  TFrmCleanLibrary = class(TForm)
    btnBack: TButton;
    btnReset: TButton;
    btnRemove: TButton;
    btnDisplay: TButton;
    btnBack2: TButton;
    btnUnselect: TButton;
    btnRemoveFromDisk: TButton;
    chkFolders: TCheckBox;
    Label3: TLabel;
    lstRemove: TListBox;
    Panel2: TPanel;
    pnlProgress: TPanel;
    txtNotToRemove: TEdit;
    Label1: TLabel;
    dirlistview: TListBox;
    Label2: TLabel;
    Panel1: TPanel;
    procedure btnBack2Click(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnDisplayClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnRemoveFromDiskClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnUnselectClick(Sender: TObject);
    procedure dirlistviewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstRemoveClick(Sender: TObject);
  private
    { private declarations }
    procedure Recursive_AddDir
              (dir: String; strExtension: ptrStrArray; var bContainesFiles: Boolean);
  public
  
    { public declarations }
  end; 

var
  FrmCleanLibrary: TFrmCleanLibrary;
  
implementation

uses
  mediacol, functions, config, mainform;
  


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TFrmCleanLibrary.FormCreate(Sender: TObject);
Var i: integer;
Begin
  dirlistview.Clear;
  For i:= 0 To MediaCollection.dirlist.Count-1 Do
    Begin
      dirlistview.Items.Add(MediaCollection.dirlist[i]);
    End;
  btnReset.Enabled := false;
  dirlistviewClick(Sender);
  txtNotToRemove.Text := CactusConfig.strCleanLibNotToRemove;
End;

procedure TFrmCleanLibrary.FormDestroy(Sender: TObject);
begin
  CactusConfig.strCleanLibNotToRemove := txtNotToRemove.Text;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.lstRemoveClick(Sender: TObject);
begin
  if (lstRemove.ItemIndex = -1 )
  then
    btnUnselect.Enabled := false
  else
    btnUnselect.Enabled := true;
    
  if lstRemove.Items.Count = 0
  then
    btnRemove.Enabled := false
  else
    btnRemove.Enabled := true;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.btnResetClick(Sender: TObject);
begin
  FormCreate(Sender);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.btnUnselectClick(Sender: TObject);
var
  i: integer;
begin
  i := lstRemove.ItemIndex;
  if i <> -1
  then
  begin
    lstRemove.Items.Delete(i);
    if lstRemove.Items.Count > i-1
    then
      lstRemove.ItemIndex := i;
    lstRemoveClick(Sender);
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.dirlistviewClick(Sender: TObject);
var i: integer;
begin
  if (dirlistview.ItemIndex = -1 )
  then
    btnRemove.Enabled := false
  else
    btnRemove.Enabled := true;
    
  if (dirlistview.Items.Count <> 0)
  then
    btnDisplay.Enabled := true;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.btnRemoveClick(Sender: TObject);
begin
  if (dirlistview.ItemIndex <> -1 )
  then
  begin
    dirlistview.Items.Delete(dirlistview.ItemIndex);
    btnReset.Enabled := true;
  end;

  if (dirlistview.Items.Count = 0)
  then
    btnDisplay.Enabled := false;
end;

procedure TFrmCleanLibrary.btnRemoveFromDiskClick(Sender: TObject);
var
  i, index: integer;
begin
  pnlProgress.Visible := true;
  btnBack2.Enabled := false;
  btnUnselect.Enabled := false;

  // remove from disk & collection
  for i := 1 to lstRemove.Items.Count do
  begin
    if DirectoryExists(lstRemove.Items[i-1])
    then
      RemoveDir(lstRemove.Items[i-1])
    else
    begin
      DeleteFile(lstRemove.Items[i-1]);
      index := MediaCollection.getIndexByPath(lstRemove.Items[i-1]);
      if index > 0 then
         MediaCollection.remove(index);
    end;
  end;
  lstRemove.Clear;

  lstRemoveClick(Sender);
  btnBack2.Enabled := true;
  pnlProgress.Visible := false;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.btnBackClick(Sender: TObject);
begin
  Close;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.btnBack2Click(Sender: TObject);
begin
  Panel2.Visible:= false;
  Panel1.Visible:= true;
  btnBack.Cancel:= true;
  btnDisplay.Default:=true;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TFrmCleanLibrary.btnDisplayClick(Sender: TObject);
var
  strExtension: array of string;
  strTmp: string;
  i, j: integer;
  bDummy: Boolean;
begin
  Panel1.Visible:= false;
  Panel2.Visible:= true;
  btnBack2.Cancel:= true;
  btnRemoveFromDisk.Default:= true;
  lstRemove.Clear;
  btnBack2.Enabled:= false;
  pnlProgress.Visible := true;
  
  Application.ProcessMessages;
  
  // enumerate file extensions to skip
  i := 0;
  SetLength(strExtension, 0);
  strTmp := Trim(txtNotToRemove.Text);
  strTmp := lowercase(strTmp);
  while (strlen(PChar(strTmp)) <> 0) do
  begin
    inc(i);
    SetLength(strExtension, i);

    j := Pos(' ', strTmp);
    if j > 0
    then
    begin
      strExtension[i-1] := Copy(strTmp, 1, j-1);
      Delete(strTmp, 1, j)
    end
    else
    begin
      strExtension[i-1] := strTmp;
      strTmp := '';
    end;
  end;
  
  // start searching the folders
  for i := 1 to dirlistview.Items.Count do
    Recursive_AddDir(dirlistview.Items[i-1], @strExtension, bDummy);

  pnlProgress.Visible := false;
  btnBack2.Enabled:= true;
  lstRemoveClick(Sender);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure TFrmCleanLibrary.Recursive_AddDir
          (dir: String; strExtension: ptrStrArray; var bContainesFiles: Boolean);

Var
  mp3search,dirsearch: TSearchrec;
  tmps: string;
  i: integer;
  bContinue: Boolean;
  bContainesFiles_lokal: Boolean;
  files, folders: TStringList;
Begin
  dir := IncludeTrailingPathDelimiter(dir);
  bContainesFiles := false;
  files := TStringList.Create;
  folders := TStringList.Create;

  // list files with non-matching extensions
  If FindFirst(dir+'*', faAnyFile - faDirectory, mp3search)=0 Then
    Repeat
    Begin
      if ((mp3search.name = '.') or (mp3search.name = '..')) then
        continue;
      tmps := lowercase(ExtractFileExt(mp3search.name));
      Delete(tmps, 1, 1);
      bContinue := false;
      for i := 1 to Length(strExtension^) do
        if tmps = strExtension^[i-1] then
        begin
          bContainesFiles := true;
          bContinue := true;
          continue;
        end;
      if bContinue then continue;
      tmps := dir + mp3search.name;
      files.Add(tmps);
    End;
    Until FindNext(mp3search)<>0;
  Findclose(mp3search);
  BubbleSort(files);
  for i := 1 to files.Count do
    lstRemove.Items.Add(files[i-1]);
  

  // walk into subfolders
  If Findfirst(dir+'*', FaDirectory, dirsearch)=0 Then
    Repeat
      If (dirsearch.name<>'..') And (dirsearch.name<>'.') Then
        if (dirsearch.attr And FaDirectory)=FaDirectory Then
        begin
          Recursive_AddDir(dir+dirsearch.name, strExtension, bContainesFiles_lokal);
          if (NOT bContainesFiles_lokal) and (chkFolders.Checked) then
            folders.Add(IncludeTrailingPathDelimiter(dir+dirsearch.name))
          else
            bContainesFiles := true;
        end;
    Until FindNext(dirsearch)<>0;
  Findclose(dirsearch);
  BubbleSort(folders);
  for i := 1 to folders.Count do
    lstRemove.Items.Add(folders[i-1]);
End;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

initialization
  {$I cleanlibrary.lrs}

end.

