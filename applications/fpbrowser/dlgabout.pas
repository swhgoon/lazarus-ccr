unit dlgabout;

interface

uses
  LclIntf, LMessages, LclType, LResources, LCLVersion,
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  //
  browserviewer, browserconstants;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    BitBtn1: TBitBtn;
    labelTitle: TLabel;
    Label2: TLabel;
    labelEngine: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor CreateIt(Owner: TComponent; const ProgName, CompName: string);
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.lfm}

constructor TAboutBox.CreateIt(Owner: TComponent; const ProgName, CompName: string);
begin
  inherited Create(Owner);

  labelTitle.Caption := 'FPBrowser ' + BrowserVersion;
  labelEngine.Caption := '     Rendering Engine: ' + GetCurrentBrowserViewer().ViewerName;
end;

end.
