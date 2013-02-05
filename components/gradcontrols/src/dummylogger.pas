unit DummyLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, variants;

type

  { TDummyLogger }

  TDummyLogger = class
  public
    procedure EnterMethod(AStr: string);
    procedure ExitMethod(AStr: string);
    procedure EnterMethod(AObj: TObject; AStr: string);
    procedure ExitMethod(AObj: TObject; AStr: string);
    procedure Send(AStr: string; AVar: Variant);
    procedure Watch(AStr: string; AVar: Variant);
  end;

var
  Logger : TDummyLogger = nil;

implementation

{ TDummyLogger }

procedure TDummyLogger.EnterMethod(AStr: string);
begin

end;

procedure TDummyLogger.ExitMethod(AStr: string);
begin

end;

procedure TDummyLogger.EnterMethod(AObj: TObject; AStr: string);
begin

end;

procedure TDummyLogger.ExitMethod(AObj: TObject; AStr: string);
begin

end;

procedure TDummyLogger.Send(AStr: string; AVar: Variant);
begin

end;

procedure TDummyLogger.Watch(AStr: string; AVar: Variant);
begin

end;

initialization
  Logger := TDummyLogger.Create;

finalization
  Logger.Free;

end.

