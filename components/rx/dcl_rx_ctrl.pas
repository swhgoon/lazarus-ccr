{ Этот файл был автоматически создан Lazarus. Н�
  � редактировать!
  Исходный код используется только для комп�
    �ляции и установки пакета.
 }

unit dcl_rx_ctrl; 

interface

uses
  register_rxctrl, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('register_rxctrl', @register_rxctrl.Register); 
end; 

initialization
  RegisterPackage('dcl_rx_ctrl', @Register); 
end.
