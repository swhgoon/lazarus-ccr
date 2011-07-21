unit guesstag;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

    { TTagFromFilename }

    TTagFromFilename = class
      private
        FAlbum: string;
        FArtist: string;
        FTitle: string;
        FTrack: string;
        FYear: string;
      public
        constructor create;
        Function ReadTag(filename: string):boolean;
        property Artist: string read FArtist;
        property Title: string read FTitle;
        property Album: string read FAlbum;
        property Track: string read FTrack;
        property year: string read FYear;
     end;


implementation

{ TTagFromFilename }

constructor TTagFromFilename.create;
begin

end;

function TTagFromFilename.ReadTag(filename: string): boolean;
var FName: string;
    z: integer;
    FileExtLength: Integer;
begin
  FName:=ExtractFileName(filename);
  FileExtLength:= Length(ExtractFileExt(filename))+1;

  FTitle:='';
  FArtist:='';
  FTrack:='';
  FYear:='';

  // get track number from NN - Artist - Title.mp3 like names
  If (FName[1]<#60) And (FName[2]<#60) {And ((FName[4]=#45)) or (FName[3]=#45))} Then
    Begin
      FTrack:= (copy(FName,1,2));
      z := pos(' - ', FName);
      If z>4 Then
         Begin
           FTitle := TrimRight(copy(FName,z+3,length(FName)-z-FileExtLength));
           FArtist := TrimRight(copy(FName,4,z-4));
         End else
           FTitle := TrimRight(copy(FName,z+3,length(FName)-z));
    End
  else begin
     z := pos(' - ', FName);
     If z>3 Then
         Begin
           FTitle := TrimRight(copy(FName,z+3,length(FName)-z-FileExtLength));
           FArtist := TrimRight(copy(FName,1,z));
         end;
    End;

  // Artist--Title.mp3 like names
  z:=pos('--', FName);
  if z>0 then begin
     if FArtist='' then begin
           FArtist := TrimRight(copy(FName,1,z-1));
        end;
     if FTitle='' then begin
           FTitle := TrimRight(copy(FName,z+2,Length(FName)-z-FileExtLength));
        end;
  end;

end;

end.

