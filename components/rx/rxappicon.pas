unit rxappicon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type

  { TRxAppIcon }

  TRxAppIcon = class(TComponent)
  private
    FIcon:TIcon;
    FIconStream:TMemoryStream;
    function GetAppIcon: TIcon;
    procedure SetAppIcon(const AValue: TIcon);
    { Private declarations }
  protected
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyIcon;
    procedure LoadFromFile(AFileName:string);
    property Icon:TIcon read GetAppIcon write SetAppIcon;
  published
  end;


implementation
{$IFDEF WIN32}
{$IFNDEF LCLGtk2} 
uses Windows, win32int, InterfaceBase, vclutils;
{$ENDIF}
{$ENDIF}


{ TRxAppIcon }

procedure TRxAppIcon.SetAppIcon(const AValue: TIcon);
begin
  FIcon.Assign(AValue);
  Application.Icon:=FIcon;
end;

procedure TRxAppIcon.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    ApplyIcon;
end;

procedure TRxAppIcon.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData, FIconStream.Size>0);
end;

procedure TRxAppIcon.ReadData(Stream: TStream);
begin
  FIconStream.LoadFromStream(Stream);
end;

procedure TRxAppIcon.WriteData(Stream: TStream);
begin
  FIconStream.SaveToStream(Stream);
end;

constructor TRxAppIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIcon:=TIcon.Create;
  FIconStream:=TMemoryStream.Create;
end;

destructor TRxAppIcon.Destroy;
begin
  FreeAndNil(FIcon);
  FreeAndNil(FIconStream);
  inherited Destroy;
end;

procedure TRxAppIcon.ApplyIcon;
{$IFDEF WIN32}
{$IFNDEF LCLGtk2} 
procedure DoApply;
var
  H:HICON;
  CI: TCursorOrIcon;
  IconSize, RequestedSize: TPoint;
begin
  try
    FIconStream.Position := 0;
    FIconStream.ReadBuffer(CI, SizeOf(CI));
    RequestedSize.x:=0;
    RequestedSize.Y:=0;
    ReadIcon(FIconStream, H, CI.Count, SizeOf(CI), RequestedSize, IconSize);
    SendMessage(TWin32WidgetSet(WidgetSet).AppHandle, WM_SETICON, 1, H);
    SetClassLong(TWin32WidgetSet(WidgetSet).AppHandle, GCL_HICON, H);
    Application.Icon:=FIcon;
    Application.Icon.Handle:=H;
  finally
  end;
end;
{$ENDIF}
{$ENDIF}
begin
  if FIconStream.Size>0 then
  begin
    Icon.LoadFromStream(FIconStream);
    {$IFDEF WIN32}
{$IFNDEF LCLGtk2} 
    DoApply;
{$ENDIF}
    {$ENDIF}
  end;
  FIconStream.Position:=0;
end;

procedure TRxAppIcon.LoadFromFile(AFileName: string);
begin
  FIconStream.LoadFromFile(AFileName);
end;

function TRxAppIcon.GetAppIcon: TIcon;
begin
  Result:=FIcon;
end;

end.
