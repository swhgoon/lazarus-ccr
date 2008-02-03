{
stdctrlsexports.pas

Exports the functionality from the StdCtrls LCL unit

This file is part of the LCL Exports library.

LICENSE: The same modifyed LGPL as the Free Pascal Runtime Library
         and the Lazarus Component Library

Copyright (C) 2008 Felipe Monteiro de Carvalho
}
unit stdctrlsexports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  StdCtrls;

{ TButton }

function TButton_Create(TheOwner: TComponent): TButton; cdecl;

{ TLabel }

function TLabel_Create(TheOwner: TComponent): TLabel; cdecl;

implementation

{ TButton }

function TButton_Create(TheOwner: TComponent): TButton; cdecl;
begin
  Result := TButton.Create(TheOwner);
end;

{ TLabel }

function TLabel_Create(TheOwner: TComponent): TLabel; cdecl;
begin
  Result := TLabel.Create(TheOwner);
end;

end.

