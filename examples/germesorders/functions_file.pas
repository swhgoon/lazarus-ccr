unit functions_file;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, U_ExtFileCopy;


function  fb_FindFiles( var astl_FilesList: TStringList; as_StartDir : String ; const as_FileMask: string ; const ab_CopyAll : Boolean ):Boolean;
Function fb_CopyFile ( const as_Source, as_Destination : String ; const ab_AppendFile, ab_CreateBackup : Boolean ):Integer;
function fb_InternalCopyDirectory ( const as_Source, as_Destination, as_Mask : String ; const ab_CopyStructure, ab_DestinationIsFile, ab_CopyAll, ab_CreateBackup : Boolean ; const aEfc_FileCopyComponent : TExtFileCopy ):Boolean;
function fb_InternalCopyFile ( const as_Source, as_Destination : String ; const ab_DestinationIsFile, ab_CreateBackup : Boolean ; const aEfc_FileCopyComponent : TExtFileCopy ):Boolean;
function fb_CreateDirectoryStructure ( const as_DirectoryToCreate : String ) : Boolean ;

implementation

uses StrUtils, Dialogs, Forms ;

// Recursive procedure to build a list of files
function fb_FindFiles( var astl_FilesList: TStringList; as_StartDir : String ; const as_FileMask: string ; const ab_CopyAll : Boolean  ):Boolean;
var
  SR: TSearchRec;
  IsFound: Boolean;
begin
  Result := False ;

  if astl_FilesList = nil
   then
    astl_FilesList := TstringList.Create ;

  if as_StartDir[length(as_StartDir)] <> DirectorySeparator
   then
    as_StartDir := as_StartDir + DirectorySeparator;

  { Build a list of the files in directory as_StartDir
     (not the directories!)                         }
  if ab_copyAll Then
  try
    IsFound := FindFirst(as_StartDir + '*', faDirectory, SR) = 0 ;
    while IsFound do
     begin
      if (( SR.Name <> '.' ) and ( SR.Name <> '..' ))
      and DirectoryExists ( as_StartDir + SR.Name )
       then
        Begin
          astl_FilesList.Add(as_StartDir + SR.Name);
        End ;
      IsFound := FindNext(SR) = 0;
      Result := True ;
    end;
    FindClose(SR);
  Except
    FindClose(SR);
  End ;
  try
    IsFound := FindFirst(as_StartDir+as_FileMask, faAnyFile-faDirectory, SR) = 0;
    while IsFound do
     begin
        if FileExists ( as_StartDir + SR.Name )
         Then
          astl_FilesList.Add(as_StartDir + SR.Name);
        IsFound := FindNext(SR) = 0;
        Result := True ;
      end;
    FindClose(SR);
  Except
    FindClose(SR);
  End ;

end;

Function fb_CopyFile ( const as_Source, as_Destination : String ; const ab_AppendFile, ab_CreateBackup : Boolean ):Integer;
var
  li_SizeRead,li_SizeWrite,li_TotalW  : Longint;
  li_HandleSource,li_HandleDest, li_pos : integer;
  ls_FileName, ls_FileExt,ls_Destination : String ;
  lb_FoundFile,lb_Error : Boolean;
  lsr_data : Tsearchrec;
  FBuffer  : array[0..2047] of char;
begin
  Result := CST_COPYFILES_ERROR_UNKNOWN ;
  {
  FindFirst(as_Source,faanyfile,lsr_data);
  li_TotalW := 0;
  findclose(lsr_data);
  }
  li_TotalW := 0;
  
  li_HandleSource := fileopen(as_Source,fmopenread);
  ls_Destination := as_Destination ;
  if  ab_AppendFile
  and fileexists(as_Destination)
   then
    Begin
      FindFirst(as_Destination,faanyfile,lsr_data);
      li_HandleDest := FileOpen(as_Destination, fmopenwrite );
      FileSeek ( li_HandleDest, lsr_data.Size, 0 );
      findclose(lsr_data);
    End
   Else
     Begin
      If fileexists(ls_Destination)
       then
        Begin
          FindFirst(as_Destination,faanyfile,lsr_data);
          if ( ab_CreateBackup )
           Then
            Begin
              ls_FileName := lsr_data.Name;
              ls_FileExt  := '' ;
              li_pos := 1;
              while ( PosEx ( '.', ls_FileName, li_pos + 1 ) > 0 ) Do
                li_pos := PosEx ( '.', ls_FileName, li_pos + 1 );
              if ( li_Pos > 1 ) Then
                Begin
                  ls_FileExt  := Copy ( ls_FileName, li_pos, length ( ls_FileName ) - li_pos + 1 );
                  ls_FileName := Copy ( ls_FileName, 1, li_pos - 1 );
                End ;
              li_pos := 0 ;
              while FileExists ( ls_Destination ) do
               Begin
                 inc ( li_pos );
                 ls_Destination := ExtractFilePath ( as_Destination ) + DirectorySeparator + ls_FileName + '-' + IntToStr ( li_pos ) + ls_FileExt ;
               End
            End
           Else
            Deletefile(as_Destination);
          findclose(lsr_data);
        End ;
      li_HandleDest := filecreate(ls_Destination);
     end ;
  lb_FoundFile := False;
  lb_Error := false;
  while not lb_FoundFile do
    begin
      li_SizeRead := FileRead(li_HandleSource,FBuffer,high ( Fbuffer ) + 1);
      if li_SizeRead < high ( Fbuffer ) + 1 then lb_FoundFile := True;
      li_SizeWrite := Filewrite(li_HandleDest,Fbuffer,li_SizeRead);
      inc( li_TotalW, li_SizeWrite );
      if li_SizeWrite < li_SizeRead then lb_Error := True;
    end;
  filesetdate(li_HandleDest,filegetdate(li_HandleSource));
  fileclose(li_HandleSource);
  fileclose(li_HandleDest);
  if lb_Error = False then
    Begin
      Result := 0 ;
    End ;
  //Application.ProcessMessages ;
end;

function fb_CreateDirectoryStructure ( const as_DirectoryToCreate : String ) : Boolean ;
var
  lsr_data : Tsearchrec;
  li_Pos : Integer ;
  ls_Temp : String ;
begin
  Result := False ;
  if DirectoryExists ( as_DirectoryToCreate )
   Then
    Begin
      Result := True;
    End
  Else
    try
       li_Pos := 1 ;
       while ( Posex ( DirectorySeparator, as_DirectoryToCreate, li_pos + 1 ) > 1 ) do
         li_Pos := Posex ( DirectorySeparator, as_DirectoryToCreate, li_pos + 1 );
       if ( li_pos > 1 ) Then
         ls_Temp := Copy ( as_DirectoryToCreate, 1 , li_pos - 1 )
       Else
         Exit ;
       if  not DirectoryExists ( ls_Temp ) Then
         Begin
           fb_CreateDirectoryStructure ( ls_Temp );
         End ;
       if DirectoryExists ( ls_Temp ) then
         Begin
           FindFirst ( ls_Temp,faanyfile,lsr_data);
           if ( DirectoryExists ( ls_Temp )) Then
             try
               CreateDir ( as_DirectoryToCreate );
               Result := True ;
             except
             End
            Else
             Result := False ;
           FindClose ( lsr_data );
         end;
     Finally
     End ;
End ;


function fb_InternalCopyFile ( const as_Source, as_Destination : String ; const ab_DestinationIsFile, ab_CreateBackup : Boolean ; const aEfc_FileCopyComponent : TExtFileCopy ):Boolean;
var lsr_AttrSource      : TSearchRec ;

begin
  Result := True ;
  Result := fb_CreateDirectoryStructure ( as_Destination );
  if FileExists ( as_Destination ) Then
    Begin
      if ( DirectoryExists ( as_Destination ) ) Then
        Begin
          FindFirst ( as_Source, faAnyFile, lsr_AttrSource );
          if assigned ( aEfc_FileCopyComponent ) Then
            Result := aEfc_FileCopyComponent.CopyFile ( as_Source, as_Destination + DirectorySeparator + lsr_AttrSource.Name, ab_DestinationIsFile, ab_CreateBackup ) <> 0
          Else
            Result := fb_CopyFile ( as_Source, as_Destination + DirectorySeparator + lsr_AttrSource.Name, ab_DestinationIsFile, ab_CreateBackup ) <> 0
        End
      Else
        Begin
          if assigned ( aEfc_FileCopyComponent ) Then
            Result := aEfc_FileCopyComponent.CopyFile ( as_Source, as_Destination, ab_DestinationIsFile, ab_CreateBackup ) <> 0
          else
            Result := fb_CopyFile ( as_Source, as_Destination, ab_DestinationIsFile, ab_CreateBackup ) <> 0
        End ;
    End
  Else
    if assigned ( aEfc_FileCopyComponent ) Then
      aEfc_FileCopyComponent.EventualFailure ( CST_COPYFILES_ERROR_DIRECTORY_CREATE, GS_COPYFILES_ERROR_DIRECTORY_CREATE + ' ' + as_Destination );
End ;


function fb_InternalCopyDirectory ( const as_Source, as_Destination, as_Mask : String ; const ab_CopyStructure, ab_DestinationIsFile, ab_CopyAll, ab_CreateBackup : Boolean ; const aEfc_FileCopyComponent : TExtFileCopy ):Boolean;
var li_Error, li_i : Integer ;
    ls_Source ,
    ls_destination  : String ;
    lstl_StringList : TStringList ;
    lsr_AttrSource      : Tsearchrec;
begin
  if not fb_CreateDirectoryStructure ( as_Destination ) Then
    Begin
      li_Error := CST_COPYFILES_ERROR_DIRECTORY_CREATE ;
      if assigned ( aEfc_FileCopyComponent ) Then
          Result := aEfc_FileCopyComponent.EventualFailure ( li_Error, as_Destination );
      Exit ;
    End ;
  if  assigned ( @aEfc_FileCopyComponent )
  and Assigned ( @aEfc_FileCopyComponent.OnChange )
   Then
    aEfc_FileCopyComponent.OnChange ( aEfc_FileCopyComponent, as_Source, as_Destination );
  Result := True ;
  lstl_StringList := nil ;
  if fb_FindFiles ( lstl_StringList, as_Source, as_Mask, ab_CopyAll ) Then
    for li_i := 0 to lstl_StringList.count - 1 do
      Begin
        ls_Source := lstl_StringList.Strings [ li_i ];
        FindFirst( ls_Source,faanyfile,lsr_AttrSource);
        if DirectoryExists ( ls_Source ) Then
          Begin
            if ab_CopyStructure then
              ls_destination := as_Destination + DirectorySeparator + lsr_AttrSource.Name
            Else
              ls_destination := as_Destination ;
            Result := fb_InternalCopyDirectory ( ls_Source, ls_Destination, as_Mask, ab_CopyStructure, ab_DestinationIsFile, ab_CopyAll, ab_CreateBackup, aEfc_FileCopyComponent );
          End
        Else
          if FileExists ( ls_Source ) Then
            Begin
              if assigned ( aEfc_FileCopyComponent ) Then
                Begin
                  Result := aEfc_FileCopyComponent.InternalDefaultCopyFile ( ls_Source, as_Destination + DirectorySeparator + lsr_AttrSource.Name );
                End
              Else
                Result := fb_InternalCopyFile ( ls_Source, as_Destination + DirectorySeparator + lsr_AttrSource.Name, ab_DestinationIsFile, ab_CreateBackup, aEfc_FileCopyComponent );
            End ;
      End ;
  lstl_StringList.Free ;
End ;



end.

