unit udm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, IniFiles;

{$IFNDEF WST_IDE}
const
  sLAST_PATH = 'LastPath';
{$ENDIF WST_IDE}

type

  { TDM }

  TDM = class(TDataModule)
    IM: TImageList;
  private
{$IFNDEF WST_IDE}
    FOptions : TMemIniFile;
    function GetOtions: TCustomIniFile;
{$ENDIF WST_IDE}
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy();override;
{$IFNDEF WST_IDE}
    property Options : TCustomIniFile read GetOtions;
{$ENDIF WST_IDE}
  end; 

var
  DM: TDM;

implementation

{ TDM }

{$IFNDEF WST_IDE}
function TDM.GetOtions: TCustomIniFile;
begin
  Result := FOptions;
end;
{$ENDIF WST_IDE}

constructor TDM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFNDEF WST_IDE}
  FOptions := TMemIniFile.Create(ChangeFileExt(GetAppConfigFile(False),'.ini'));
{$ENDIF WST_IDE}
end;

destructor TDM.Destroy();
begin
{$IFNDEF WST_IDE}
  if ( FOptions <> nil ) then begin
    if not DirectoryExists(FOptions.FileName) then
      ForceDirectories(ExtractFileDir(FOptions.FileName));
    FOptions.UpdateFile();
  end;
  FOptions.Free();
{$ENDIF WST_IDE}
  inherited Destroy();
end;

initialization
  {$I udm.lrs}

end.

