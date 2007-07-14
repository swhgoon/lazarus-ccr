{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$INCLUDE wst_global.inc}
unit server_listener;

interface
uses
  Classes, SysUtils;

{$INCLUDE wst.inc}
{$INCLUDE wst_delphi.inc}

type

  TListnerNotifyMessage = procedure(Sender : TObject; const AMsg : string) of object;

  TwstListener = class(TObject)
  private
    FOnNotifyMessage: TListnerNotifyMessage;
  public
    class function GetDescription() : string;virtual;
    procedure Start();virtual;abstract;
    procedure Stop();virtual;abstract;
    procedure NotifyMessage(const AMsg : string);
    property OnNotifyMessage : TListnerNotifyMessage read FOnNotifyMessage write FOnNotifyMessage;
  end;


implementation

{ TwstListener }

class function TwstListener.GetDescription() : string;
begin
  Result := ClassName;
end;

procedure TwstListener.NotifyMessage(const AMsg: string);
begin
  if Assigned(FOnNotifyMessage) then
    FOnNotifyMessage(Self,AMsg);
end;

end.
