(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
$Log: acs_procs.pas,v $
Revision 1.4  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.1  2005/12/19 18:34:35  z0m3ie
*** empty log message ***

Revision 1.2  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.2  2005/08/22 20:17:01  z0m3ie
changed Headers to log
changed mail adress

}

unit acs_procs;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  SysUtils, ACS_Types, Math;

type

  TACSFilterWindowType = (fwHamming, fwHann, fwBlackman);

{$IFDEF LINUX}
  function FindLibs(const Pattern : String) : String;
{$ENDIF}


  // Direction = 1 - forward FFT, Direction = -1 - inverse FFT.
  procedure ComplexFFT(Data : PACSComplexArray; DataSize, Direction : Integer);

  procedure HannWindow(OutData : PACSDoubleArray; Width : Integer; Symmetric : Boolean);

  procedure HammingWindow(OutData : PACSDoubleArray; Width : Integer; Symmetric : Boolean);

  procedure BlackmanWindow(OutData : PACSDoubleArray; Width : Integer; Symmetric : Boolean);

  procedure CalculateSincKernel(OutData : PACSDoubleArray; CutOff : Double; Width : Integer; WType : TACSFilterWindowType);

  procedure SmallIntArrayToDouble(InData : PSmallInt; OutData : PDouble; DataSize : Integer);

  procedure SmallIntArrayToComplex(InData : PSmallInt; OutData : PACSComplex; DataSize : Integer);


  // Computes Op2[i] = Op1[i]*Op2[i], i = [0..DataSize-1]

  procedure MultDoubleArrays(Op1, Op2 : PDouble; DataSize : Integer);

  (*
    Performs calculation of
                   /
                  | Lg(Abs(InData[i])) + Shift, if Lg(Abs(InData[i])) + Shift >= 0
    OutData[i] = <  0, if Lg(Abs(InData[i])) + Shift < 0
                  | 0, if Abs(InData[i]) = 0.
                  \
    i = [0..DataSize-1]
  *)
  procedure LgMagnitude(InData : PACSComplex; OutData : PDouble; DataSize, Shift : Integer);

implementation

{$IFDEF LINUX}
  function FindLibs(const Pattern : String) : String;
  var
    Path : String;
    SR : TSearchRec;
  begin
    Path := '/usr/lib/';
    if FindFirst(Path + Pattern, faAnyFile, SR) = 0 then
    begin
      Result := Path+SR.Name;
      FindClose(SR);
      Exit;
    end;
    Path := '/usr/lib/x86_64-linux-gnu/';
    if FindFirst(Path + Pattern, faAnyFile, SR) = 0 then
    begin
      Result := Path+SR.Name;
      FindClose(SR);
      Exit;
    end;
    Path := '/usr/local/lib/';
    if FindFirst(Path + Pattern, faAnyFile, SR) = 0 then
    begin
      Result := Path+SR.Name;
      FindClose(SR);
      Exit;
    end;
    Result := '';
  end;
{$ENDIF}


  (* This routine is converted from the original C code by P. Burke
   Direction = 1 - forward FFT, Direction = -1 - inverse FFT. *)
  procedure ComplexFFT(Data : PACSComplexArray; DataSize, Direction : Integer);
  var
    i, i1, j, k, i2, l, l1, l2, Log2n : Integer;
    c1, c2, tx, ty, t1, t2, u1, u2, z  : Double;
  begin
    Log2n := Trunc(Log2(DataSize));
    // Do the bit reversal
    i2 := DataSize shr 1;
    j := 0;
    for i := 0 to DataSize-2 do
    begin
      if i < j then
      begin
        tx := Data[i].Re;
        ty := Data[i].Im;
        Data[i].Re := Data[j].Re;
        Data[i].Im := Data[j].Im;
        Data[j].Re := tx;
        Data[j].Im := ty;
      end;
      k := i2;
      while k <= j do
      begin
        Dec(j, k);
        k := k shr 1;
      end;
      Inc(j, k);
    end;
    // Compute the FFT
    c1 := -1.0;
    c2 := 0.0;
    l2 := 1;
    for l := 0 to Log2n-1 do
    begin
      l1 := l2;
      l2 := l2 shl 1;
      u1 := 1.0;
      u2 := 0.0;
      for j := 0 to l1-1 do
      begin
        i := j;
        while i < DataSize do
        begin
          i1 := i + l1;
          t1 := u1 * Data[i1].Re - u2 * Data[i1].Im;
          t2 := u1 * Data[i1].Im + u2 * Data[i1].Re;
          Data[i1].Re := Data[i].Re - t1;
          Data[i1].Im := Data[i].Im - t2;
          Data[i].Re := Data[i].Re + t1;
          Data[i].Im := Data[i].Im + t2;
          Inc(i, l2);
        end;
        z :=  u1*c1 - u2*c2;
        u2 := u1*c2 + u2*c1;
        u1 := z;
      end;
      c2 := Sqrt((1.0 - c1)/2.0);
      if Direction = 1 then c2 := -c2;
      c1 := Sqrt((1.0 + c1)/2.0);
    end;

    // Scaling for forward transform
    if Direction = 1 then
    for i := 0 to DataSize-1 do
    begin
      Data[i].Re := Data[i].Re/DataSize;
      Data[i].Im := Data[i].Im/DataSize;
    end;
  end;

  procedure HannWindow(OutData : PACSDoubleArray; Width : Integer; Symmetric : Boolean);
  var
    i, n : Integer;
  begin
    if Symmetric then n := Width-1
    else n := Width;
    for i := 0 to Width-1 do OutData[i] := (1-Cos(TwoPi*i/n))/2;
  end;

  procedure HammingWindow(OutData : PACSDoubleArray; Width : Integer; Symmetric : Boolean);
  var
    i, n : Integer;
  begin
    if Symmetric then n := Width-1
    else n := Width;
    for i := 0 to Width-1 do OutData[i] := 0.54-0.46*Cos(TwoPi*i/n);
  end;

  procedure BlackmanWindow(OutData : PACSDoubleArray; Width : Integer; Symmetric : Boolean);
  var
    i, n : Integer;
  begin
    if Symmetric then n := Width-1
    else n := Width;
    for i := 0 to Width-1 do OutData[i] := 0.42-0.5*Cos(TwoPi*i/n) + 0.08*Cos(2*TwoPi*i/n);
  end;

  procedure CalculateSincKernel(OutData : PACSDoubleArray; CutOff : Double; Width : Integer; WType : TACSFilterWindowType);
  var
    i : Integer;
    S : Double;
    Window : array of Double;
  begin
//    SetLength(OutData, Width);
    SetLength(Window, Width);
    case WType of
      fwHamming : HammingWindow(@Window[0], Width, False);
      fwHann : HannWindow(@Window[0], Width, False);
      fwBlackman : BlackmanWindow(@Window[0], Width, False);
    end;
    S := 0;
    for i := 0 to Width-1 do
    begin
      if i-(Width shr 1) <> 0 then
      OutData[i] := Sin(TwoPi*CutOff*(i-(Width shr 1)))/(i-(Width shr 1))*Window[i]
      else OutData[i] := TwoPi*CutOff*Window[i];
      S := S + OutData[i];
    end;
    for i := 0 to Width-1 do OutData[i] := OutData[i]/S;
  end;

  procedure SmallIntArrayToDouble(InData : PSmallInt; OutData : PDouble; DataSize : Integer);
  begin
    {$IFDEF CPU32}
    asm
                MOV EDX, DataSize;
                SHL EDX, 3;
                MOV ECX, OutData;
                ADD EDX, ECX;
                MOV EAX, InData;
      @test:    CMP EDX, ECX;
                JE @out;
                FILD WORD[EAX];
                ADD EAX, 2;
                FSTP QWORD[ECX];
                ADD ECX, 8;
                JMP @test;
      @out:     ;
    end;
    {$ENDIF}
  end;

  procedure SmallIntArrayToComplex(InData : PSmallInt; OutData : PACSComplex; DataSize : Integer);
  begin
    {$IFDEF CPU32}
    asm
                MOV EDX, DataSize;
                SHR EDX, 4;
                MOV ECX, OutData;
                ADD EDX, ECX;
                MOV EAX, InData;
      @test:    CMP EDX, ECX;
                JE @out;
                FILD WORD[EAX];
                ADD EAX, 2;
                FSTP QWORD[EAX];
                ADD ECX, 16;
                JMP @test;
      @out:     ;
    end;
    {$ENDIF}
  end;

  procedure MultDoubleArrays(Op1, Op2 : PDouble; DataSize : Integer);
  begin
    {$IFDEF CPU32}
    asm
                MOV EDX, DataSize;
                SHL EDX, 3;
                MOV ECX, Op1;
                ADD EDX, ECX;
                MOV EAX, Op2;
      @test:    CMP EDX, ECX;
                JE @out;
                FLD QWORD[ECX];
                FLD QWORD[EAX];
                FMUL;
                FSTP QWORD[EAX];
                ADD ECX, 8;
                ADD EAX, 8;
                JMP @test;
      @out:     ;
    end;
    {$ENDIF}
  end;

  procedure LgMagnitude(InData : PACSComplex; OutData : PDouble; DataSize, Shift : Integer);
  var
    LogBase  : Double;
  begin
    {$IFDEF CPU32}
    asm
                FLD1;
                FLDL2T;
                FDIVP;
                FSTP LogBase;
                MOV EDX, DataSize;
                SHL EDX, 3;
                MOV ECX, OutData;
                ADD EDX, ECX;
                MOV EAX, InData;
      @test:    CMP EDX, ECX;
                JE @out;
                FLD QWORD[EAX];
                FMUL ST(0), ST(0);
                ADD EAX, 8;
                FLD QWORD[EAX];
                FMUL ST(0), ST(0);
                FADDP;
                FSQRT;
                FTST;
                PUSH EAX;
                FSTSW AX;
                SAHF;
                JE @skip;
                FLD LogBase;
                FXCH;
                FYL2X;
                FIADD Shift;
                FTST;
                FSTSW AX;
                SAHF;
                JAE @skip;
                FSTP QWORD[ECX];
                FLDZ;
      @skip:    POP EAX;
                ADD EAX, 8;
                FSTP QWORD[ECX];
                ADD ECX, 8;
                JMP @test;
      @out:     ;
    end;
    {$ENDIF}
  end;
end.
