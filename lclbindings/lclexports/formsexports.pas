{
formsexports.pas

Exports the functionality from the Forms LCL unit

This file is part of the LCL Exports library.

LICENSE: The same modifyed LGPL as the Free Pascal Runtime Library
         and the Lazarus Component Library

Copyright (C) 2008 Felipe Monteiro de Carvalho
}
unit formsexports;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms;

{ Application }
  
procedure Application_CreateForm(var Reference: Pointer); cdecl;
procedure Application_Initialize; cdecl;
procedure Application_Run; cdecl;

{ TCustomForm }

procedure TCustomForm_Close(Self: TCustomForm); cdecl;
procedure TCustomForm_Show(Self: TCustomForm); cdecl;

{ TForm }

function  TForm_Create(TheOwner: TComponent): TForm; cdecl;

implementation

{ Application }

procedure Application_CreateForm(var Reference: Pointer); cdecl;
begin
  Application.CreateForm(TForm, Reference);
end;

procedure Application_Initialize; cdecl;
begin
  Application.Initialize;
end;

procedure Application_Run; cdecl;
begin
  Application.Run;
end;

{ TCustomForm }

procedure TCustomForm_Close(Self: TCustomForm); cdecl;
begin
  Self.Close;
end;

procedure TCustomForm_Show(Self: TCustomForm); cdecl;
begin
  Self.Show;
end;

{ TForm }

function TForm_Create(TheOwner: TComponent): TForm; cdecl;
begin
  Result := TForm.Create(TheOwner);
end;

end.

