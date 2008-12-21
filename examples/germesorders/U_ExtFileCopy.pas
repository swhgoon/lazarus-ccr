unit U_ExtFileCopy;
{$mode objfpc}{$H+}

{
Composant TExtFileCopy

Développé par:
Matthieu GIROUX

Composant non visuel permettant de copier un fichier plus rapidement
que par le fonction copy de windows.
Compatible Linux
Attention: La gestion de la RAM étant calamiteuse sous Win9x, l'
utilisation de ce composant provoque une grosse une forte baisse de la
mémoire disponible. Sous WinNT/2000 il n' y a pas de problèmes


Version actuelle: 1.0

Mises à jour:
}
interface

uses
  SysUtils, Classes,ComCtrls, StrUtils, lresources ;
  
var GS_COPYFILES_ERROR_DIRECTORY_CREATE : String = 'Erreur à la création du répertoire' ;
    GS_COPYFILES_ERROR_IS_FILE          : String = 'Ne peut copier dans le fichier' ;
    GS_COPYFILES_ERROR_CANT_COPY        : String = 'Impossible de copier ' ;
    GS_COPYFILES_ERROR_PARTIAL_COPY     : String = 'Copie partielle du fichier ' ;
    GS_COPYFILES_ERROR_PARTIAL_COPY_SEEK: String = 'Erreur à la copie partielle du fichier ' ;
    GS_COPYFILES_ERROR_CANT_READ        : String = 'Impossible de lire le fichier ' ;
    GS_COPYFILES_ERROR_CANT_CHANGE_DATE : String = 'Impossible d''affecter la date au fichier ' ;
    GS_COPYFILES_ERROR_CANT_CREATE      : String = 'Impossible de créer le fichier ' ;
    GS_COPYFILES_ERROR_CANT_APPEND      : String = 'Impossible d''ajouter au fichier ' ;
    GS_COPYFILES_ERROR_FILE_DELETE      : String = 'Impossible d''effacer le fichier ' ;
    GS_COPYFILES_CONFIRM_FILE_DELETE    : String = 'Voulez-vous effacer le fichier ' ;
    GS_COPYFILES_CONFIRM                : String = 'Demande de confirmation' ;

type
    TECopyOption = ( cpCopyAll, cpUseFilter, cpNoStructure, cpCreateBackup, cpCreateDestination, cpDestinationIsFile );
    TECopyOptions = set of TECopyOption;
    TECopyEvent = procedure(Sender : Tobject; const BytesCopied,BytesTotal : cardinal) of object;
    TEReturnEvent = procedure(Sender : Tobject; var Continue : Boolean ) of object;
    TECopyErrorEvent = procedure(Sender : Tobject; const ErrorCode : Integer ; var ErrorMessage : AnsiString ; var ContinueCopy : Boolean ) of object;
    TECopyFinishEvent = procedure(Sender : Tobject; const ASource, ADestination : AnsiString ; const Errors : Integer ) of object;
    TEChangeDirectoryEvent = procedure(Sender : Tobject; const NewDirectory, DestinationDirectory : AnsiString ) of object;
const
  lco_Default = [cpCopyAll,cpUseFilter];
  CST_COPYFILES_ERROR_IS_READONLY = faReadOnly ;
  CST_COPYFILES_ERROR_UNKNOWN = -1 ;
  CST_COPYFILES_ERROR_IS_DIRECTORY = faDirectory ;
  CST_COPYFILES_ERROR_IS_FILE = 1 ;
  CST_COPYFILES_ERROR_DIRECTORY_CREATE = 2 ;
  CST_COPYFILES_ERROR_CANT_COPY = 3 ;
  CST_COPYFILES_ERROR_CANT_READ = 4 ;
  CST_COPYFILES_ERROR_CANT_CREATE = 5 ;
  CST_COPYFILES_ERROR_CANT_APPEND = 6 ;
  CST_COPYFILES_ERROR_FILE_DELETE = 7 ;
  CST_COPYFILES_ERROR_PARTIAL_COPY = 8 ;
  CST_COPYFILES_ERROR_PARTIAL_COPY_SEEK = 9 ;
  CST_COPYFILES_ERROR_CANT_CHANGE_DATE = 10 ;

type

    { TExtFileCopy }

    TExtFileCopy = class(TComponent)
           private
             FOnChange : TEChangeDirectoryEvent ;
             FSizeTotal : Int64 ;
             FErrors    ,
             FSizeProgress : Integer ;
             FOnSuccess : TECopyFinishEvent;
             FOnFailure : TECopyErrorEvent ;
             FBeforeCopy : TEReturnEvent ;
             FBeforeCopyBuffer ,
             FOnProgress       : TECopyEvent;
             FBufferSize : integer;
             FOptions : TECopyOptions ;
             FFilter, FSource,FDestination : string;
             FInProgress : Boolean;
             procedure SetBufferSize (Value : integer);
             procedure SetDestination(Value : String);
             procedure SetSource(Value: String);
             protected
             FBuffer : array[0..65535] of char;
             function  BeforeCopyBuffer ( var li_SizeRead, li_BytesTotal : Longint ) : Boolean ; virtual ;
             function  BeforeCopy : Boolean ; virtual ;
             procedure AfterCopyBuffer  ; virtual ;
             { Déclarations protégées }
           public
             function EventualFailure ( const ai_Error : Integer ; as_Message : AnsiString ):Boolean; virtual ;
             function InternalDefaultCopyFile  ( const as_Source, as_Destination : String ):Boolean; virtual ;
             procedure InternalFinish ( const as_Source, as_Destination : String ); virtual ;
             constructor Create(AOwner : Tcomponent);override;
             property InProgress : Boolean read FInprogress;
             Function CopyFile ( const as_Source, as_Destination : String ; const ab_AppendFile, ab_CreateBackup : Boolean ):Integer;
             Procedure CopySourceToDestination;
           published
             property BufferSize : integer read FBufferSize write SetBufferSize default 65536;
             property Source : string read FSource write SetSource;
             property Mask : string read FFilter write FFilter;
             property Destination : string read FDestination write SetDestination;
             property Options : TECopyOptions read FOptions write FOptions default lco_Default ;
             property OnSuccess : TECopyFinishEvent read FOnSuccess write FOnSuccess;
             property OnFailure : TECopyErrorEvent read FOnFailure write FOnFailure;
             property OnProgress : TECopyEvent read FOnProgress write Fonprogress;
             property OnBeforeCopyBuffer : TECopyEvent read FBeforeCopyBuffer write FBeforeCopyBuffer;
             property OnBeforeCopy : TEReturnEvent read FBeforeCopy write FBeforeCopy;
             property OnChange : TEChangeDirectoryEvent read FOnChange write FOnChange;
           end;

{TExtFilePartialCopy}

    TExtFilePartialCopy = class(TExtFileCopy)
           private
             lb_ExcludedFound  : Boolean ;
             lpch_excludeStart,
             lpch_excludeEnd  : String ;
             FExcludeStart ,
             FExcludeEnd   : String ;
             FExcludeReading : Boolean;
             protected
             function  BeforeCopyBuffer ( var li_SizeRead, li_BytesTotal : Longint ) : Boolean ; override ;
             function  BeforeCopy : Boolean ; override ;
             procedure AfterCopyBuffer  ; override ;
             { Déclarations protégées }
           public
             constructor Create(AOwner : Tcomponent);override;
           published
             property ExcludeReading : Boolean read FExcludeReading write FExcludeReading default False ;
             property ExcludeStart : String read FExcludeStart write FExcludeStart ;
             property ExcludeEnd   : String read FExcludeEnd   write FExcludeEnd ;
           end;


procedure Register;

implementation

uses functions_file, Forms, Dialogs, Controls ;

{TExtFileCopy}

constructor TExtFileCopy.Create(AOwner :Tcomponent);
begin
  inherited Create(AOwner);
  Options := lco_Default ;
  FBufferSize := 65536;
  FInProgress := False;
end;

procedure TExtFileCopy.SetBufferSize(Value : integer);
begin
  If not FInprogress
   then
    begin
      If Value > high ( FBuffer )
       then
        Value := high ( FBuffer ) + 1
       Else
        FBufferSize := Value;
    end;
end;

procedure TExtFileCopy.SetDestination(Value: String);
begin
  if FDestination <> Value Then
    Begin
      FDestination := Value;
    End;
end;

procedure TExtFileCopy.SetSource(Value: String);
begin
  if FSource <> Value Then
    Begin
      FSource := Value;
      if not ( csDesigning in ComponentState )
      and Assigned ( @FOnChange )
       Then
        FOnChange ( Self, FSource, FDestination );
    End;
end;

function TExtFileCopy.BeforeCopyBuffer(var li_SizeRead, li_BytesTotal : Longint ): Boolean;
begin
  Result := True ;
  if Assigned ( FBeforeCopyBuffer ) Then
    FBeforeCopyBuffer ( Self, li_SizeRead, li_BytesTotal );

end;

function TExtFileCopy.BeforeCopy: Boolean;
begin
  Result := True ;
  if Assigned ( FBeforeCopy ) Then
    FBeforeCopy ( Self, Result );
end;

procedure TExtFileCopy.AfterCopyBuffer;
begin

end;

Function TExtFileCopy.CopyFile ( const as_Source, as_Destination : String ; const ab_AppendFile, ab_CreateBackup : Boolean ):Integer;
var
  li_SizeRead,li_SizeWrite,li_TotalW, li_RealTotal  : Longint;
  li_SizeTotal : Int64 ;
  li_HandleSource,li_HandleDest, li_pos, li_Confirm : integer;
  ls_FileName, ls_FileExt,ls_Destination : String ;
  lb_FoundFile : Boolean;
  lsr_data : Tsearchrec;
begin
  Result := 0 ;
  li_Confirm := mrYes ;
  FindFirst(as_Source,faanyfile,lsr_data);
  li_RealTotal := lsr_data.size ;
  li_SizeTotal := lsr_data.Size;
  inc ( FSizeTotal, li_SizeTotal );
  li_TotalW := 0;
  findclose(lsr_data);
  try
    li_HandleSource := fileopen(as_Source,fmopenread);
  Except
    On E: Exception do
      Begin
        Result := CST_COPYFILES_ERROR_CANT_READ ;
        EventualFailure ( Result, GS_COPYFILES_ERROR_CANT_READ + as_Destination );
        Exit ;
      End ;
  End ;
  ls_Destination := as_Destination ;
  if  ab_AppendFile
  and fileexists(as_Destination)
   then
    try
      FindFirst(as_Destination,faanyfile,lsr_data);
      li_HandleDest := FileOpen(as_Destination, fmopenwrite );
      FileSeek ( li_HandleDest, lsr_data.Size, 0 );
      findclose(lsr_data);
    Except
      Result := CST_COPYFILES_ERROR_CANT_APPEND ;
      EventualFailure ( Result, GS_COPYFILES_ERROR_CANT_APPEND + as_Destination );
      Exit ;
    End
   Else
     Begin
      If fileexists(ls_Destination)
       then
        Begin
          FindFirst(as_Destination,faanyfile,lsr_data);
          if ( ab_CreateBackup )
           Then
            try
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
            Except
              Result := -1 ;
              EventualFailure ( Result, as_Destination );
              Exit ;
            End
           Else
            try
              if li_Confirm <> mrAll Then
                li_Confirm := MessageDlg ( GS_COPYFILES_CONFIRM, GS_COPYFILES_CONFIRM_FILE_DELETE, mtConfirmation, [mbYes,mbNo,mbAll,mbCancel], 0 );
              if li_Confirm = mrCancel Then
                Abort ;
              if li_Confirm = mrNo Then
                Exit ;
              Deletefile(as_Destination);
            Except
              Result := CST_COPYFILES_ERROR_FILE_DELETE ;
              EventualFailure ( Result, GS_COPYFILES_ERROR_FILE_DELETE + as_Destination );
              Exit ;
            End ;
          findclose(lsr_data);
        End ;
      try
        li_HandleDest := filecreate(ls_Destination);
      Except
        Result := CST_COPYFILES_ERROR_CANT_CREATE ;
        EventualFailure ( Result, GS_COPYFILES_ERROR_CANT_CREATE + as_Destination );
        Exit ;
      End
     end ;
  if not BeforeCopy Then
    Exit ;
  lb_FoundFile := False;
  while not lb_FoundFile do
    try
      li_SizeRead := FileRead(li_HandleSource,FBuffer,FbufferSize);
      if ( li_SizeRead <= 0 )
      and ( li_TotalW < li_RealTotal )
       Then
        try
          FileSeek ( li_HandleSource, 64, li_TotalW );
          Inc ( li_TotalW, 64 );
          Continue ;
        Except
          Result := CST_COPYFILES_ERROR_PARTIAL_COPY_SEEK ;
          EventualFailure ( Result, GS_COPYFILES_ERROR_PARTIAL_COPY_SEEK + as_Destination );
        End ;
      if BeforeCopyBuffer ( li_SizeRead, li_TotalW ) Then
        Begin
          li_SizeWrite := Filewrite(li_HandleDest,Fbuffer,li_SizeRead);
          Application.ProcessMessages;
          inc( li_TotalW, li_SizeWrite );
          if  ( li_SizeRead < FBufferSize )
          and ( li_TotalW >= li_RealTotal )
           then
            lb_FoundFile := True;
          if li_SizeWrite < li_SizeRead
           then
             Begin
              Result := CST_COPYFILES_ERROR_PARTIAL_COPY ;
              EventualFailure ( Result, GS_COPYFILES_ERROR_PARTIAL_COPY + as_Destination );
             End ;
          if assigned(FonProgress) then FonProgress(self, FSizeProgress + li_TotalW,FSizeTotal);
        End ;
      AfterCopyBuffer ;
    Except
      Result := CST_COPYFILES_ERROR_CANT_COPY ;
      EventualFailure ( Result, GS_COPYFILES_ERROR_CANT_COPY + '( ' + as_Source + ' -> ' + as_Destination + ' )' );
      Exit ;
    End ;
  try
    filesetdate(li_HandleDest,filegetdate(li_HandleSource));
  Except
    Result := CST_COPYFILES_ERROR_CANT_CHANGE_DATE ;
    EventualFailure ( Result, GS_COPYFILES_ERROR_CANT_CHANGE_DATE + as_Destination );
    Exit ;
  End ;
  fileclose(li_HandleSource);
  fileclose(li_HandleDest);
  if Result = 0 then
    Begin
      inc ( FSizeProgress, li_TotalW );
      InternalFinish ( as_Source, as_Destination );
      Result := 0 ;
    End ;
  Application.ProcessMessages ;
end;

function TExtFileCopy.InternalDefaultCopyFile  ( const as_Source, as_Destination : String ):Boolean;
var li_Error : Integer ;
begin
  Result := True ;
  li_Error := CopyFile ( as_Source, as_Destination, cpDestinationIsFile in FOptions, cpCreateBackup in FOptions );
  EventualFailure ( li_Error , '' );
End ;

function TExtFileCopy.EventualFailure ( const ai_Error : Integer ; as_Message : AnsiString ):Boolean;
begin
  Result := True ;
  if  ( ai_Error <> 0 ) then
    Begin
      inc ( FErrors );
      if assigned ( FOnFailure ) then
        Begin
            FOnFailure ( Self, ai_Error, as_Message, Result );
        End ;
    End ;
End ;

procedure TExtFileCopy.InternalFinish ( const as_Source, as_Destination : String );
begin
  if assigned ( @FOnSuccess ) then
    Begin
        FOnSuccess ( Self, as_Source, as_Destination, FErrors );
    End ;
End ;

procedure TExtFileCopy.CopySourceToDestination;
var
  lb_Continue : Boolean ;
begin
  Finprogress := true;
  FSizeTotal := 0 ;
  FErrors := 0 ;
  FSizeProgress := 0 ;
  if (    not FileExists ( FSource )
       and not DirectoryExists ( FSource ))
   Then
    Exit ;
  if  not DirectoryExists ( FDestination )
  and not fb_CreateDirectoryStructure ( FDestination )
   Then
    Exit ;
  try
    if ( DirectoryExists ( FSource )) Then
      Begin
        lb_Continue := fb_InternalCopyDirectory ( FSource, FDestination, FFilter, not ( cpNoStructure in FOptions ), cpDestinationIsFile in FOptions, cpCopyAll in FOptions, cpCreateBackup in FOptions, Self );
      End
    Else
      Begin
        lb_Continue := fb_InternalCopyFile ( FSource, FDestination, cpDestinationIsFile in FOptions, cpCreateBackup in FOptions, Self );
      End ;
  finally
    FinProgress := false;
  End ;
end;

{TExtFilePartialCopy}

constructor TExtFilePartialCopy.Create(AOwner :Tcomponent);
begin
  inherited Create(AOwner);
  FExcludeReading := False ;
end;


function TExtFilePartialCopy.BeforeCopyBuffer ( var li_SizeRead, li_BytesTotal : Longint ) : Boolean ;

var li_pos, li_i : Longint ;
Begin
  Result := inherited BeforeCopyBuffer ( li_SizeRead, li_BytesTotal );
  if FExcludeReading
  and ( FExcludeStart <> '' )
  and ( FExcludeEnd   <> '' )
   Then
    Begin
      li_pos := 0 ;
      li_i   := 0 ;
      while li_pos < li_SizeRead do
        if lb_ExcludedFound then
          Begin

          End
         Else
          Begin
          End;
    end;
End ;

procedure TExtFilePartialCopy.AfterCopyBuffer ;

Begin
End ;


function TExtFilePartialCopy.BeforeCopy : Boolean ;

Begin
  Result := inherited BeforeCopy ();
  if FExcludeReading
  and ( FExcludeStart <> '' )
  and ( FExcludeEnd   <> '' )
   Then
    Begin
//      lpch_excludeStart := fs_HexToString ( FExcludeStart );
//      lpch_excludeEnd   := fs_HexToString ( FExcludeEnd   );
    End ;
End ;

procedure Register;
begin
  RegisterComponents('Extended', [TExtFileCopy]);
end;

initialization
  {$i U_ExtFileCopy.lrs}
end.
