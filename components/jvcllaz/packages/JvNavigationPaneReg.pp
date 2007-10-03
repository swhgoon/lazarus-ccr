unit JvNavigationPaneReg; 

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils;

procedure Register;

implementation
  uses JvNavigationPane;

procedure Register;
begin
  RegisterComponents('JvNavPane',[TJvNavigationPane, TJvNavIconButton,
    TJvNavPanelButton, TJvNavPanelHeader, TJvNavPanelDivider, TJvOutlookSplitter,
    TJvNavPaneStyleManager, TJvNavPaneToolPanel]);
end;

initialization
  {$I JvNavigationPaneLaz.lrs}

end.

