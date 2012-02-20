unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, DateUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnCronometer: TButton;
    btnStart: TButton;
    btnStop: TButton;
    btnReset: TButton;
    labelCronometer: TLabel;
    Notebook: TNotebook;
    Page1: TPage;
    pageCronometer: TPage;
    timerCronometer: TTimer;
    timerClock: TTimer;
    procedure btnCronometerClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure timerClockTimer(Sender: TObject);
    procedure timerCronometerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    CronometerStarted: Boolean;
    CronometerTime, CronometerLastUpdate: TTime;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.timerClockTimer(Sender: TObject);
begin

end;

procedure TForm1.timerCronometerTimer(Sender: TObject);
var
  lNow: TDateTime;
  lFormatSettings: TFormatSettings;
begin
  lNow := Now();
  CronometerTime := CronometerTime + CronometerLastUpdate - lNow;
  CronometerLastUpdate := lNow;
  lFormatSettings := SysUtils.FormatSettings;
  lFormatSettings.LongTimeFormat := 'hh:nn:ss:zzz';
  labelCronometer.Caption := SysUtils.TimeToStr(CronometerTime, lFormatSettings);
end;

procedure TForm1.btnCronometerClick(Sender: TObject);
begin
  Notebook.PageIndex := 1;
end;

procedure TForm1.btnResetClick(Sender: TObject);
begin
  CronometerTime := DateUtils.EncodeTimeInterval(0, 0, 0, 0);
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  CronometerStarted := True;
  timerCronometer.Enabled := True;
  CronometerLastUpdate := Now();
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  CronometerStarted := False;
  timerCronometer.Enabled := False;
end;

end.

