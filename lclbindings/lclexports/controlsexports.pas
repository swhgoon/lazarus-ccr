{
controlsexports.pas

Exports the functionality from the Controls LCL unit

This file is part of the LCL Exports library.

LICENSE: The same modifyed LGPL as the Free Pascal Runtime Library
         and the Lazarus Component Library

Copyright (C) 2008 Felipe Monteiro de Carvalho
}
unit controlsexports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Controls;

{ TControl }

{ Events }
function  TControl_GetOnClick(Self: TControl): TNotifyEvent; cdecl;
procedure TControl_SetOnClick(Self: TControl; AValue: TNotifyEvent); cdecl;
{ Properties }
function  TControl_GetCaption(Self: TControl): PChar; cdecl;
procedure TControl_SetCaption(Self: TControl; AValue: PChar); cdecl;

function  TControl_GetLeft(Self: TControl): Integer; cdecl;
procedure TControl_SetLeft(Self: TControl; AValue: Integer); cdecl;
function  TControl_GetHeight(Self: TControl): Integer; cdecl;
procedure TControl_SetHeight(Self: TControl; AValue: Integer); cdecl;
function  TControl_GetHint(Self: TControl): PChar; cdecl;
procedure TControl_SetHint(Self: TControl; AValue: PChar); cdecl;
function  TControl_GetTop(Self: TControl): Integer; cdecl;
procedure TControl_SetTop(Self: TControl; AValue: Integer); cdecl;
function  TControl_GetWidth(Self: TControl): Integer; cdecl;
procedure TControl_SetWidth(Self: TControl; AValue: Integer); cdecl;

{ TWinControl }

function  TWinControl_GetParent(Self: TWinControl): TWinControl; cdecl;
procedure TWinControl_SetParent(Self: TWinControl; AValue: TWinControl); cdecl;

implementation

{ TControl }

function TControl_GetOnClick(Self: TControl): TNotifyEvent; cdecl;
begin
  Result := Self.OnClick;
end;

procedure TControl_SetOnClick(Self: TControl; AValue: TNotifyEvent); cdecl;
begin
  Self.OnClick := AValue;
end;

function TControl_GetCaption(Self: TControl): PChar; cdecl;
begin
  Result := PChar(Self.Caption);
end;

procedure TControl_SetCaption(Self: TControl; AValue: PChar); cdecl;
begin
  Self.Caption := string(AValue);
end;

function TControl_GetLeft(Self: TControl): Integer; cdecl;
begin
  Result := Self.Left;
end;

procedure TControl_SetLeft(Self: TControl; AValue: Integer); cdecl;
begin
  Self.Left := AValue;
end;

function TControl_GetHeight(Self: TControl): Integer; cdecl;
begin
  Result := Self.Height;
end;

procedure TControl_SetHeight(Self: TControl; AValue: Integer); cdecl;
begin
  Self.Height := AValue;
end;

function TControl_GetHint(Self: TControl): PChar; cdecl;
begin
  Result := PChar(Self.Hint);
end;

procedure TControl_SetHint(Self: TControl; AValue: PChar); cdecl;
begin
  Self.Hint := string(AValue);
end;

function TControl_GetTop(Self: TControl): Integer; cdecl;
begin
  Result := Self.Top;
end;

procedure TControl_SetTop(Self: TControl; AValue: Integer); cdecl;
begin
  Self.Top := AValue;
end;

function TControl_GetWidth(Self: TControl): Integer; cdecl;
begin
  Result := Self.Width;
end;

procedure TControl_SetWidth(Self: TControl; AValue: Integer); cdecl;
begin
  Self.Width := AValue;
end;

{ TWinControl }

function TWinControl_GetParent(Self: TWinControl): TWinControl; cdecl;
begin
  Result := Self.Parent;
end;

procedure TWinControl_SetParent(Self: TWinControl; AValue: TWinControl); cdecl;
begin
  Self.Parent := AValue;
end;

end.

