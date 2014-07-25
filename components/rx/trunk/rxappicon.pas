{ rxappicon unit

  Copyright (C) 2005-2010 Lagunov Aleksey alexs@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit rxappicon;

{$mode objfpc}{$H+}

{$IFDEF LCLQT}
 {$DEFINE LCLGtk2}
{$ENDIF}

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
uses Windows, Win32Int, InterfaceBase, vclutils;
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
