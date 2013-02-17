unit iOSXIBResource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLMemManager, forms,
  dom, XMLRead,XMLWrite,
  ProjectIntf, UnitResources;

type

  { TXIBResourcefileFormat }

  TXIBResourcefileFormat = class(TUnitResourcefileFormat)
  public
    class function FindResourceDirective(Source: TObject): boolean; override;
    class function ResourceDirectiveFilename: string; override;
    class function GetUnitResourceFilename(AUnitFilename: string; Loading: boolean): string; override;
    class procedure TextStreamToBinStream(ATxtStream, ABinStream: TExtMemoryStream); override;
    class procedure BinStreamToTextStream(ABinStream, ATextStream: TExtMemoryStream); override;
    class function GetClassNameFromStream(s: TStream; out IsInherited: Boolean): shortstring; override;
    class function CreateReader(s: TStream; var DestroyDriver: boolean): TReader; override;
    class function CreateWriter(s: TStream; var DestroyDriver: boolean): TWriter; override;
    class function QuickCheckResourceBuffer(PascalBuffer, LFMBuffer: TObject;
      out LFMType, LFMComponentName, LFMClassName: string; out
      LCLVersion: string; out MissingClasses: TStrings): TModalResult; override;
  end;

implementation

uses
  CodeCache,
  CodeToolManager,
  BasicCodeTools,
  ios_views;

{ TXIBResourcefileFormat }

class function TXIBResourcefileFormat.FindResourceDirective(Source: TObject): boolean;
var
  cb: TCodeBuffer;
  nx,ny,nt: integer;
  r,p: integer;
begin
  r := FindNextCompilerDirectiveWithName((source as TCodeBuffer).Source, -1, 'FakeResource', False, p);
  result := (r > -1)
end;

class function TXIBResourcefileFormat.ResourceDirectiveFilename: string;
begin
  result := '*.xib';
end;

class function TXIBResourcefileFormat.GetUnitResourceFilename(
  AUnitFilename: string; Loading: boolean): string;
begin
  result := ChangeFileExt(AUnitFilename,'.xib');
end;

class procedure TXIBResourcefileFormat.TextStreamToBinStream(ATxtStream,
  ABinStream: TExtMemoryStream);
begin
  ABinStream.LoadFromStream(ATxtStream);
end;

class procedure TXIBResourcefileFormat.BinStreamToTextStream(ABinStream,
  ATextStream: TExtMemoryStream);
begin
  ATextStream.LoadFromStream(ABinStream);
end;

class function TXIBResourcefileFormat.GetClassNameFromStream(s: TStream; out
  IsInherited: Boolean): shortstring;
begin
  result := 'TAppDelegate_iPhone';
end;

class function TXIBResourcefileFormat.CreateReader(s: TStream;
  var DestroyDriver: boolean): TReader;
begin
  result := TXIBReader.Create(S, 4096);
end;

class function TXIBResourcefileFormat.CreateWriter(s: TStream;
  var DestroyDriver: boolean): TWriter;
begin
  result := TWriter.Create(TNIBObjectWriter.Create(s));
end;

class function TXIBResourcefileFormat.QuickCheckResourceBuffer(PascalBuffer,
  LFMBuffer: TObject; out LFMType, LFMComponentName, LFMClassName: string; out
  LCLVersion: string; out MissingClasses: TStrings): TModalResult;
var
  ms: TStream;
  XMLDocument: TXMLDocument;
  RO, CR, OO, MO : TDOMElement;
  i: int64;
  b: boolean;
  AnElement: TDOMElement;

begin
  ms := TStringStream.Create((LFMBuffer as TCodeBuffer).Source);
  try
    ObtainBaseObjectInfoFromXIB(ms, XMLDocument, RO, CR, OO, MO,i, b);
    try
      AnElement := FindKeyNode(MO,'string','objectName');
      if assigned(AnElement) then
        LFMComponentName:=AnElement.TextContent;
    finally
      XMLDocument.Free;
    end;
  finally
    ms.Free;
  end;

  LCLVersion:='1.1';
  LFMType:='unknown';
  LFMClassName:='TAppDelegate_iPhone';
end;

end.

