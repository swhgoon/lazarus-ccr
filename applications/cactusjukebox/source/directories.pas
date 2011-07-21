
{
Directory/library manager dialog for Cactus Jukebox

written by Sebastian Kraft, <c> 2006-2008

Contact the author at: sebastian_kraft@gmx.de

This Software is published under the GPL






}


Unit directories;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
Buttons, CheckLst, StdCtrls;

Type 

  { Tdirwin }

  Tdirwin = Class(TForm)
    add: TButton;
    Button1: TButton;
    dirlistview: TListBox;
    removebut: TButton;
    rescan: TButton;
    rescanall: TButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure dirlistviewClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure addClick(Sender: TObject);
    Procedure removeClick(Sender: TObject);
    Procedure rescanClick(Sender: TObject);
    Procedure rescanallClick(Sender: TObject);
    Private 
    { private declarations }
    Public 
    { public declarations }
  End;

Var 
  dirwin: Tdirwin;

  Implementation

  Uses mainform,status,mediacol, config;
{ Tdirwin }

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tdirwin.FormCreate(Sender: TObject);

Var i: integer;
Begin
  dirlistview.Clear;
  For i:= 0 To MediaCollection.dirlist.Count-1 Do
    Begin
      dirlistview.Items.Add(MediaCollection.dirlist[i]);
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tdirwin.addClick(Sender: TObject);

Var
  i: integer;
Begin
  SelectDirectoryDialog1.InitialDir := CactusConfig.HomeDir;


  If SelectDirectoryDialog1.Execute=true Then
    Begin
      For i:= 0 To MediaCollection.dirlist.Count-1 Do
        Begin
          If pos(MediaCollection.dirlist[i], SelectDirectoryDialog1.FileName)=1 Then
            Begin
              ShowMessage('Directory '+SelectDirectoryDialog1.FileName+
                          ' is still part of directorylist');
              exit;
            End;
        End;
      Caption := 'Please wait... Scanning...';
      Enabled := false;
      Application.ProcessMessages;
      MediaCollection.add_directory(SelectDirectoryDialog1.FileName);
      dirlistview.Items.Add(SelectDirectoryDialog1.FileName);

      If MediaCollection.ItemCount>1 Then
        Begin
          Main.ArtistTree.Selected := Nil;
          main.update_artist_view;
          update_title_view;
        End;
      Caption := 'Directories';
      Enabled := true;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tdirwin.removeClick(Sender: TObject);

Var removedir: string;
  i: integer;
Begin

  removedir := dirlistview.Items[dirlistview.ItemIndex];
  If removedir[length(removedir)]=DirectorySeparator Then delete(removedir,length(removedir), 1);
  i := 0;

  Repeat
    Begin
      If pos(removedir, ExtractFileDir(MediaCollection.items[i].path))=1 Then
        Begin
          MediaCollection.remove(i);
          dec(i);
        End;
      inc(i);
    End;
  Until i>=MediaCollection.ItemCount;
  MediaCollection.DirList.Delete(dirlistview.ItemIndex);
  dirlistview.Items.Delete(dirlistview.ItemIndex);

  Main.ArtistTree.Selected := Nil;
  main.update_artist_view;
  update_title_view;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tdirwin.rescanClick(Sender: TObject);

Var rescandir: string;
  i, n: integer;
Begin
  Main.clear_listClick(nil);
  For n:= 0 To dirlistview.Items.Count-1 Do
    Begin
      If dirlistview.Selected[n] Then
        Begin
          rescandir := dirlistview.Items[n];
          dirlistview.show;
          If rescandir[length(rescandir)]=DirectorySeparator Then delete(rescandir,length(rescandir)
            , 1);
          i := 0;
          Repeat
            Begin
              If pos(rescandir, ExtractFileDir(MediaCollection.items[i].path))=1 Then
                Begin
                  MediaCollection.remove(i);
                  dec(i);
                End;
              inc(i);
            End;
          Until i>=MediaCollection.ItemCount;
          MediaCollection.DirList.Delete(n);
          Caption := 'Please wait... Scanning...';
          Enabled := false;
          main.update_artist_view;
          update_title_view;
          Application.ProcessMessages;
          MediaCollection.add_directory(rescandir);
        End;

      If MediaCollection.ItemCount>1 Then
        Begin
          Main.ArtistTree.Selected := Nil;
          main.update_artist_view;
          update_title_view;
        End;
      Caption := 'Directories';
      Enabled := true;
    End;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tdirwin.rescanallClick(Sender: TObject);

Var i: integer;
Begin
  For i:= 0 To dirlistview.Items.Count-1 Do
    dirlistview.Selected[i] := true;
  rescanClick(Nil);
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tdirwin.Button1Click(Sender: TObject);
Begin
  Close;
End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Procedure Tdirwin.Button3Click(Sender: TObject);
Begin

End;

Procedure Tdirwin.dirlistviewClick(Sender: TObject);
Begin

End;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

initialization
  {$I directories.lrs}

End.
