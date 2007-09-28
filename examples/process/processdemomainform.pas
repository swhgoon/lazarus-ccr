unit ProcessDemoMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, Process, LCLProc, ComCtrls;

type

  { TMultipleProcessDemoForm }

  TMultipleProcessDemoForm = class(TForm)
    Process2:TProcess;
    Process2GroupBox:TGroupBox;
    Process2InputLabel:TLabel;
    Process2InputMemo:TMemo;
    Process2OutputLabel:TLabel;
    Process2OutputMemo:TMemo;
    Process2SendInputButton:TButton;
    Process2StartButton:TButton;
    CommandLine2LabeledEdit:TLabeledEdit;
    ReadOutputIdleTimer:TIdleTimer;
    Process1:TProcess;
    Process1SendInputButton:TButton;
    Process1StartButton:TButton;
    CloseButton:TButton;
    CommandLine1LabeledEdit:TLabeledEdit;
    Process1InputLabel:TLabel;
    Process1OutputLabel1:TLabel;
    Process1InputMemo:TMemo;
    Process1GroupBox1:TGroupBox;
    Process1OutputMemo:TMemo;
    ProcessPanel:TPanel;
    ProcessSplitter:TSplitter;
    StatusBar1:TStatusBar;
    procedure CloseButtonClick(Sender:TObject);
    procedure MultipleProcessDemoFormClose(Sender:TObject;
      var CloseAction:TCloseAction);
    procedure ProcessSendInputButtonClick(Sender:TObject);
    procedure ProcessStartButtonClick(Sender:TObject);
    procedure ReadOutputIdleTimerTimer(Sender:TObject);
  private
    { private declarations }
    procedure StartProcess(Process: TProcess; StartButton: TButton;
      const CommandLine: string);
    procedure SendInput(Process: TProcess; InputMemo: TMemo);
  public
    { public declarations }
  end;

var
  MultipleProcessDemoForm: TMultipleProcessDemoForm;

implementation

{ TMultipleProcessDemoForm }

procedure TMultipleProcessDemoForm.CloseButtonClick(Sender:TObject);
begin
  Close;
end;

procedure TMultipleProcessDemoForm.MultipleProcessDemoFormClose(Sender:TObject;
  var CloseAction:TCloseAction);
begin
  if Process1.Running then
    Process1.Terminate(0);
  if Process2.Running then
    Process2.Terminate(0);
end;

procedure TMultipleProcessDemoForm.ProcessSendInputButtonClick(Sender:TObject);
begin
  if Sender=Process1SendInputButton then
    SendInput(Process1, Process1InputMemo);
  if Sender=Process2SendInputButton then
    SendInput(Process2, Process2InputMemo);
end;

procedure TMultipleProcessDemoForm.ProcessStartButtonClick(Sender:TObject);
begin
  if Sender=Process1StartButton then
    StartProcess(Process1, Process1StartButton, CommandLine1LabeledEdit.Text);
  if Sender=Process2StartButton then
    StartProcess(Process2, Process2StartButton, CommandLine2LabeledEdit.Text);
end;

procedure TMultipleProcessDemoForm.ReadOutputIdleTimerTimer(Sender:TObject);
var
  NoMoreOutput: boolean;
  
  procedure DoStuffForProcess(Process: TProcess; StartButton: TButton;
    OutputMemo: TMemo);
  var
    Buffer: string;
    BytesAvailable: DWord;
    BytesRead:LongInt;
  begin
    if not StartButton.Enabled then
      StartButton.Enabled := not Process.Running;
    if Process.Running then
    begin
      BytesAvailable := Process.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable>0 do
      begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := Process.OutPut.Read(Buffer[1], BytesAvailable);
        OutputMemo.Text := OutputMemo.Text + copy(Buffer,1, BytesRead);
        BytesAvailable := Process.Output.NumBytesAvailable;
        NoMoreOutput := false;
      end;
      if BytesRead>0 then
        OutputMemo.SelStart := Length(OutputMemo.Text);
    end;
  end;
begin
  repeat
    NoMoreOutput := true;
    DoStuffForProcess(Process1, Process1StartButton, Process1OutputMemo);
    DoStuffForProcess(Process2, Process2StartButton, Process2OutputMemo);
  until noMoreOutput;
end;

procedure TMultipleProcessDemoForm.StartProcess(Process:TProcess;
  StartButton: TButton; const CommandLine: string);
begin
  if not Process.Running then begin
    StartButton.Enabled := false;
    Process.CommandLine := CommandLine;
    Process.Execute;
  end;
end;

procedure TMultipleProcessDemoForm.SendInput(Process:TProcess;InputMemo:TMemo);
var
  InputStrings: string;
begin
  if Process.Running then begin
    InputStrings := InputMemo.Lines.Text;
    Process.Input.Write(InputStrings[1], length(InputStrings));
  end;
end;

initialization
  {$I processdemomainform.lrs}

end.

