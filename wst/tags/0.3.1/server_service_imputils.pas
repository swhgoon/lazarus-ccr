{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).
    

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit server_service_imputils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo,
  server_service_intf;
  
Type

  { TRequestBuffer }

  TRequestBuffer = class(TInterfacedObject,IRequestBuffer)
  private
    FTargetService : string;
    FContentType   : string;
    //FLength        : Integer;
    FContent       : TStream;
    FResponse      : TStream;
  protected
    function GetTargetService():string;
    function GetContentType():string;
    //function GetLength():Integer;
    function GetContent():TStream;
    function GetResponse():TStream;
  public
    constructor Create(
      ATargetService : string;
      AContentType   : string;
      //ALength        : Integer;
      AContent       : TStream;
      AResponse      : TStream
    );
  End;


  function IsStrEmpty(Const AStr:String):Boolean;

implementation

function IsStrEmpty(Const AStr:String):Boolean;
begin
  Result := ( Length(Trim(AStr)) = 0 );
end;

{ TRequestBuffer }

function TRequestBuffer.GetTargetService(): string;
begin
  Result := FTargetService;
end;

function TRequestBuffer.GetContentType(): string;
begin
  Result := FContentType;
end;

{function TRequestBuffer.GetLength(): Integer;
begin
  Result := FLength;
end;}

function TRequestBuffer.GetContent(): TStream;
begin
  Result := FContent;
end;

function TRequestBuffer.GetResponse(): TStream;
begin
  Result := FResponse;
end;

constructor TRequestBuffer.Create(
  ATargetService : string;
  AContentType   : string;
  //ALength        : Integer;
  AContent       : TStream;
  AResponse      : TStream
);
begin
  FTargetService := ATargetService;
  FContentType   := AContentType;
  //FLength        := ALength;
  FContent       := AContent;
  FResponse      := AResponse;
end;




end.
