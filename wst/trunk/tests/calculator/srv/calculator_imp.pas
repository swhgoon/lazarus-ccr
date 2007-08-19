{
This unit has been produced by ws_helper.
  Input unit name : "calculator".
  This unit name  : "calculator_imp".
  Date            : "15/08/2007 16:34:20".
}
Unit calculator_imp;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, 
     base_service_intf, server_service_intf, server_service_imputils, calculator;

Type


  TCalculator_ServiceImp=class(TBaseServiceImplementation,ICalculator)
  Protected
    function AddInt(
      const  A : Integer; 
      const  B : Integer
    ):TBinaryArgsResult;
    function DivInt(
      const  A : Integer; 
      const  B : Integer
    ):Integer;
    function DoAllOperations(
      const  A : Integer; 
      const  B : Integer
    ):TBinaryArgsResultArray;
    function DoOperation(
      const  A : Integer; 
      const  B : Integer; 
      const  AOperation : TCalc_Op
    ):TBinaryArgsResult;
  End;


  procedure RegisterCalculatorImplementationFactory();

Implementation
uses config_objects;

{ TCalculator_ServiceImp implementation }
function TCalculator_ServiceImp.AddInt(
  const  A : Integer; 
  const  B : Integer
):TBinaryArgsResult;
Begin
// your code here
End;

function TCalculator_ServiceImp.DivInt(
  const  A : Integer; 
  const  B : Integer
):Integer;
Begin
// your code here
End;

function TCalculator_ServiceImp.DoAllOperations(
  const  A : Integer; 
  const  B : Integer
):TBinaryArgsResultArray;
Begin
// your code here
End;

function TCalculator_ServiceImp.DoOperation(
  const  A : Integer; 
  const  B : Integer; 
  const  AOperation : TCalc_Op
):TBinaryArgsResult;
Begin
// your code here
End;



procedure RegisterCalculatorImplementationFactory();
Begin
  GetServiceImplementationRegistry().Register('ICalculator',TImplementationFactory.Create(TCalculator_ServiceImp,wst_GetServiceConfigText('ICalculator')) as IServiceImplementationFactory);
End;

End.
