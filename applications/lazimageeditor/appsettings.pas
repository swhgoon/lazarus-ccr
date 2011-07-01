{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Felipe Monteiro de Carvalho

  Abstract:
    Unit to control the custom configurations of the application
}
unit appsettings;

{$MODE DELPHI}

interface

uses
{$IFDEF Windows}
  Windows, shlobj,
{$ENDIF}
  Classes, SysUtils, Forms, IniFiles, lieconstants;

type
  { TConfigurations }

  TConfigurations = class(TObject)
  private
    ConfigFilePath: string;
  public
    MyDirectory: string;
    Language: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromFile(Sender: TObject);
    procedure Save(Sender: TObject);
    function GetConfigFilePath: string;
    function GetMyDirectory: string;
  end;

var
  vConfigurations: TConfigurations;

implementation

{$ifdef Darwin}
uses
  MacOSAll;
{$endif}

{ TConfigurations }

constructor TConfigurations.Create;
begin
  { First we use some good defaults in case the configuration file doesn't yet exist }

//  Language := GetSystemLanguage();

  { Now identifies where the configuration file should be }
  ConfigFilePath := GetConfigFilePath();

  // Under Mac OS X we need to get the location of the bundle
  MyDirectory := GetMyDirectory();

  ReadFromFile(nil);
end;

destructor TConfigurations.Destroy;
begin
  Save(nil);

  inherited Destroy;
end;

procedure TConfigurations.ReadFromFile(Sender: TObject);
var
  MyFile: TIniFile;
begin
  if not FileExists(ConfigFilePath) then Exit;

  MyFile := TIniFile.Create(ConfigFilePath);
  try
    Language := MyFile.ReadInteger(SectionGeneral, IdentLanguage, 0);

    {$ifdef UNIX}{$ifndef DARWIN}
        MyDirectory := MyFile.ReadString(SectionUnix, IdentMyDirectory, DefaultDirectory);
    {$endif}{$endif}
  finally
    MyFile.Free;
  end;
end;

procedure TConfigurations.Save(Sender: TObject);
var
  MyFile: TIniFile;
begin
  MyFile := TIniFile.Create(ConfigFilePath);
  try
    MyFile.WriteInteger(SectionGeneral, IdentLanguage, Language);

    MyFile.WriteString(SectionUnix, IdentMyDirectory, MyDirectory);
  finally
    MyFile.Free;
  end;
end;

function TConfigurations.GetConfigFilePath: string;
begin
{$ifdef Windows}
  // First tryes to use a configuration file in the application directory
  Result := ExtractFilePath(Application.EXEName) + 'lazimageeditor.ini';
{$endif}
{$ifdef Unix}
  Result := GetEnvironmentVariable('HOME') + '/.lazimageeditor.ini';
{$endif}
end;

function TConfigurations.GetMyDirectory: string;
{$ifdef Darwin}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$endif}
begin
{$ifdef UNIX}
{$ifdef Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourcesDirectory;
{$else}
  Result := DefaultDirectory;
{$endif}
{$endif}

{$ifdef Windows}
  Result := ExtractFilePath(Application.EXEName);
{$endif}
end;

initialization

  vConfigurations := TConfigurations.Create;

finalization

  FreeAndNil(vConfigurations);

end.
