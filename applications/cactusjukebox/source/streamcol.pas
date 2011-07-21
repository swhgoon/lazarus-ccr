
Unit streamcol;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils;

Type 

   { TStreamInfoItemClass }

  TStreamInfoItemClass = Class
    Private 
    FURL, FName, FDescription: string;
    Public 
    constructor create(URL, name: String);
    property Name: string read FName write FName;
    property URL: string read FURL write FURL;
    property Description: string read FDescription write FDescription;
  End;

   { TStreamCollectionClass }

  TStreamCollectionClass = Class(TStringList)
    Private 
    FFilename: string;
    Public 
    constructor create;
    destructor destroy;

    Function add(URL, name: String): integer;

    Procedure Delete(index: integer);
    override;

    Function SaveToFile(filename: String): boolean;
    Function LoadFromFile(filename: String): boolean;
  End;

Var StreamCollection: TStreamCollectionClass;

  Implementation

{ TStreamCollectionClass }

  constructor TStreamCollectionClass.create;
Begin
  Inherited create;
End;

destructor TStreamCollectionClass.destroy;

Var i: integer;
Begin
  For i:= 0 To Count-1 Do
    Objects[i].Free;
End;

Function TStreamCollectionClass.add(URL, name: String): integer;
Begin
  result := Inherited AddObject(name, TStreamInfoItemClass.create(URL, name));
End;

Procedure TStreamCollectionClass.Delete(index: integer);
Begin
  Objects[index].Free;
  inherited Delete(index);
End;

Function TStreamCollectionClass.SaveToFile(filename: String): boolean;

Var sfile: textfile;
  i: integer;
Begin
  Try
    system.Assign(sfile, filename);
    rewrite(sfile);
    writeln(sfile, 'This file is automaticly created by Cactus Jukebox');
    writeln(sfile, 'NEVER edit by hand!');
    writeln(sfile, '');

    For i:= 0 To Count-1 Do
      Begin
        writeln(sfile, TStreamInfoItemClass(Objects[i]).Name);
        WriteLn(sfile, TStreamInfoItemClass(Objects[i]).URL);
        WriteLn(sfile, TStreamInfoItemClass(Objects[i]).Description);
      End;
    close(sfile);
    result := true;
  Except
    result := false;
  End;

End;

Function TStreamCollectionClass.LoadFromFile(filename: String): boolean;

Var sfile: textfile;
  tmps1, tmps2: string;
  i: integer;
Begin
  Try
    system.Assign(sfile, filename);
    Reset(sfile);

    ReadLn(sfile, tmps1);
    ReadLn(sfile, tmps1);
    ReadLn(sfile, tmps1);

    While Not EOF(sfile) Do
      Begin
        ReadLn(sfile, tmps1);
        ReadLn(sfile, tmps2);
        i := add(tmps2, tmps1);
//        ReadLn(sfile, TStreamInfoItemClass(Objects[i]).Description);
        //TODO: reactivate stream collection loading
      End;
    result:=true;
  Except
    writeln('ERROR reading stream collection');
    result:=false;
  End;
End;

{ TStreamInfoItemClass }

constructor TStreamInfoItemClass.create(URL, name: String);
Begin
  FName := name;
  FURL := URL;
End;

End.
