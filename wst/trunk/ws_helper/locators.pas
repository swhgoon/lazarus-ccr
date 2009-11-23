{
    This file is part of the Web Service Toolkit
    Copyright (c) 2009 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit locators;

interface

uses
  Classes, SysUtils
{$IFDEF WST_DELPHI}
  , xmldom, wst_delphi_xml
{$ENDIF WST_DELPHI}
{$IFDEF FPC}
  , DOM, XMLRead
{$ENDIF FPC}
  , xsd_parser;

type

  { TFileDocumentLocator }

  TFileDocumentLocator = class(TInterfacedObject,IDocumentLocator)
  private
    FBasePath : string;
  protected
    property BasePath : string read FBasePath;
  protected
    function Find(
      const ADocLocation : string;
      out   ADoc : TXMLDocument
    ) : Boolean;
  public
    constructor Create(const ABasePath : string);
  end;

implementation

{ TFileDocumentLocator }

function TFileDocumentLocator.Find(
  const ADocLocation: string;
  out   ADoc: TXMLDocument
) : Boolean;
var
  locFileName : string;
begin
  locFileName := BasePath + ExtractFileName(ADocLocation);
  locFileName := ExpandFileName(locFileName);
  Result := FileExists(locFileName);
  if Result then
    ReadXMLFile(ADoc,locFileName);
end;

constructor TFileDocumentLocator.Create(const ABasePath: string);
begin
  FBasePath := IncludeTrailingPathDelimiter(ABasePath);
end;

end.

