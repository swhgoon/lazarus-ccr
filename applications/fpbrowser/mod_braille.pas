(*
  The function to_braille from this unit translates a string from UTF8 chars to Braille chars.
  The dictionaries were taken from http://www.ibc.gov.br/?catid=110&blogid=1&itemid=479
  and from http://www.braillevirtual.fe.usp.br/pt/Portugues/braille.html

  Copyright 2011
*)

unit mod_braille;

interface

{$mode objfpc}{$H+}

uses
  browsermodules, lclproc;

type

  { TBrailleBrowserModule }

  TBrailleBrowserModule = class(TBrowserModule)
  public
    constructor Create; override;
    function HandleOnPageLoad(AInput: string; out AOutput: string): Boolean; override;
  end;

function ConvertUTF8TextToBraille(Line: string): string;
function ConvertUTF8HtmlTextToBraille(AInput: string): string;

implementation

type
  dictionary = array[1..32] of string;

const

  number_signal = chr($e2) + chr($a0) + chr($bc);
  caps_signal = chr($e2) + chr($a0) + chr($a8);

  d1: dictionary = ({!} chr($96), {"} chr($a6), {# TODO} chr($80), {(*$*)} chr($b0),
    {%} chr($b8) + chr($e2) + chr($a0) + chr($b4), {&} chr($af), {'} chr($84),
    {(} chr($a3) + chr($e2) + chr($a0) + chr($84), {)} chr($a0) + chr($e2) + chr($a0) + chr($9c),
    {*} chr($94), {+} chr($96), {,} chr($82), {-} chr($a4), {.} chr($84),
    {/} chr($90) + chr($e2) + chr($a0) + chr($b2), {0} chr($9a), {1} chr($81),
    {2} chr($83), {3} chr($89), {4} chr($99), {5} chr($91), {6} chr($8b),
    {7} chr($9b), {8} chr($93), {9} chr($8a), {:} chr($92), {;} chr($86),
    {<} chr($aa), {=} chr($b6), {>} chr($95), {?} chr($a2), {@ TODO} chr($80));

  d2: dictionary = ({a} chr($81), {b} chr($83), {c} chr($89), {d} chr($99),
    {e} chr($91), {f} chr($8b), {g} chr($9b), {h} chr($93), {i} chr($8a),
    {j} chr($9a), {k} chr($85), {l} chr($87), {m} chr($8d), {n} chr($9d),
    {o} chr($95), {p} chr($8f), {q} chr($9f), {r} chr($97), {s} chr($8e),
    {t} chr($9e), {u} chr($a5), {v} chr($a7), {w} chr($ba), {x} chr($ad),
    {y} chr($bd), {z} chr($b5), '', '', '', '', '', '');

  d3: dictionary = ({a + grave} chr($ab), {a + acute} chr($b7),
    {a + circumflex} chr($a1), {a + tilde} chr($9c), {a + diaeresis TODO} chr($80),
    {a + ring above TODO} chr($80), {ae TODO} chr($80), {c + cedilla} chr($af),
    {e + grave} chr($ae), {e + acute} chr($bf), {e + circumflex} chr($a3),
    {e + diaeresis TODO} chr($80), {i + grave} chr($a9), {i + acute}  chr($8c),
    {i + circumflex TODO} chr($80), {i + diaeresis} chr($bb), {eth TODO} chr($80),
    {n + tilde TODO} chr($80), {o + grave} chr($ba), {o + acute} chr($ac),
    {o + circumflex} chr($b9), {o + tilde} chr($aa), {o + diaeresis TODO} chr($80),
    {division sign TODO} chr($80), {o + stroke TODO} chr($80), {u + grave} chr($b1),
    {u + acute} chr($be), {u + circumflex TODO} chr($80), {u + diaeresis} chr($b3),
    {y + acute TODO} chr($80), {thorn TODO} chr($80), {y + diaeresis TODO} chr($80));

function ConvertUTF8TextToBraille(Line: string): string;
var
  lCharSize, count, n, next_n, len: integer;
  Braille_string: string;
  Pline: Pchar;
  num, caps: boolean;

begin
  Braille_string := '';
  num := False;
  caps := False;
  count := 1;
  if Line = '' then Result := '';

  len := length(Line);
  Pline := PChar(Line);

  while count <= len do
  begin

    lCharSize := LCLProc.UTF8CharacterLength(PLine);

    if (lCharSize = 1) then
    begin

      n := Ord(Line[count]);

      if (n = 9) then Braille_string := Braille_string + #9

      else
      if (n = 10) then Braille_string := Braille_string + #10

      else
      if (n = 32) then Braille_string := Braille_string + #32

      else
      begin

      if ((n >= 97) and (n <= 122)) then            {a lower case letter}
        Braille_string := Braille_string + chr($e2) + chr($a0) + d2[n - 96]

      else
      begin

      if ((n >= 65) and (n <= 90)) then           {an upper case letter}
      begin
        if not caps then
        begin
          Braille_string := Braille_string + caps_signal;
          if (count + 2 < length(Line)) then
          begin
            next_n := Ord(Line[count + 1]);
            if ((next_n >= 65) and (next_n <= 90)) or ((next_n = 195) and ((Ord(Line[count + 2]) >= 128) and (Ord(Line[count + 2]) <= 159))) then {if the next char is also upper case, add another caps signal}
            begin
              Braille_string := Braille_string + caps_signal;
              caps := True;
            end;
          end;
        end;
        Braille_string := Braille_string + chr($e2) + chr($a0) + d2[n - 64];
        if (count + 1 <= length(Line)) then
        begin
          next_n := Ord(Line[count + 1]);
          if not(((next_n >= 65) and (next_n <= 90)) or ((next_n = 195) and ((Ord(Line[count + 2]) >= 128) and (Ord(Line[count + 2]) <= 159)))) then caps := False;   {if the next char is not upper case, unflag <caps>}
        end;
      end

      else
      begin

      if (n >= 48) and (n <= 57) then           {a number}
      begin
        if not num then Braille_string := Braille_string + number_signal;       {first algarism of a number, add a number signal}
        num := True;
        Braille_string := Braille_string + chr($e2) + chr($a0) + d1[n - 31];
        if (count + 1 <= length(Line)) then
        begin
          next_n := Ord(Line[count + 1]);
          if not ((next_n >= 48) and (next_n <= 57) or (next_n = 44) or (next_n = 46)) then num := False; {the next char is not a number, nor ',', nor '.', then unflag <num>}
        end;
      end

      else
      begin

      if (n >= 33) and (n <= 64) then        {a char from the first dictionary}
      begin
        Braille_string := Braille_string + chr($e2) + chr($a0) + d1[n - 32];
        if (count + 1 <= length(Line)) then
        begin
          next_n := Ord(Line[count + 1]);
          if ((n = 44) or (n = 46)) and not ((next_n >= 48) and (next_n <= 57)) then num := False; {if the char is a ',' or a '.' but the next is not a number, unflag <num>}
        end;
      end;
      end;
      end;
      end;
      end;
    end

    else
    begin

      if (lCharSize = 2) then
      begin
        n := Ord(Line[count]);
        if (n = 195) then
        begin
          n := Ord(Line[count + 1]);

          if (n >= 128) and (n <= 159) then      {upper case accented char}
          begin
            if not caps then
            begin
              Braille_string := Braille_string + caps_signal;
              if (count + 2 <= length(Line)) then
              begin
                next_n := Ord(Line[count + 2]);
                if ((next_n >= 65) and (next_n <= 90)) or ((next_n = 195) and ((Ord(Line[count + 2]) >= 128) and (Ord(Line[count + 2]) <= 159))) then {if the next char is also upper case, add another caps signal}
                begin
                  Braille_string := Braille_string + caps_signal;
                  caps := True;
                end;
              end;
            end;
            Braille_string := Braille_string + chr($e2) + chr($a0) + d3[n - 127];
            if (count + 3 <= length(Line)) then
            begin
              next_n := Ord(Line[count + 2]);
              if not(((next_n >= 65) and (next_n <= 90)) or ((next_n = 195) and ((Ord(Line[count + 3]) >= 128) and (Ord(Line[count + 3]) <= 159)))) then {if the next char is not upper case, unflag <caps>}
                caps := False;
            end;
          end

          else
          if (n >= 160) and (n <= 191) then Braille_string := Braille_string + chr($e2) + chr($a0) + d3[n - 159];   {lower case accented letter}
        end;
      end;
    end;
    count := count + lCharSize;
    Inc(Pline, lCharSize);
  end;
  Result := Braille_string;
end;

function ConvertUTF8HtmlTextToBraille(AInput: string): string;
var
  i, lCharSize: integer;
  output, aux_string, end_string: string;
  is_text: boolean;
  Pline : PChar;

begin
  i := 1;
  output := '';
  aux_string := '';
  is_text := False;
  Pline := PChar(AInput);

  while i <= length(AInput) do
  begin
    end_string := '';
    while is_text and (i <= length(AInput)) do
    begin
      if (AInput[i] = '<')  then            { an instruction comes next }
      begin
        is_text := False;
        i := i + 1;
        Inc(Pline, 1);
        end_string := '<';
        if (copy(AInput, i, 6) = 'script') then   { if it's a script, go through it and
                                                     keep reading the text}
        begin
          i := i + 6;
          Inc(Pline, 6);
          end_string := end_string + 'script';
          while (copy(AInput, i, 9) <> '</script>') do
          begin
            end_string := end_string + AInput[i];
            i := i + 1;
            Inc(Pline, 1);
          end;
          is_text := True;
        end;
        break;
      end;

      { Read the next UTF8 character add it to aux_string }
      lCharSize := LCLProc.UTF8CharacterLength(Pline);
      aux_string := aux_string + copy(AInput, i, lCharSize);

      i := i + lCharSize;
      Inc(Pline, lCharSize);
    end;

    { Translate aux_string to Braille and add it to output }
    output := output + ConvertUTF8TextToBraille(aux_string) + end_string;
    aux_string := '';

    while (not is_text) and (i <= length(AInput))do
    begin
      if (AInput[i] = '>') then   { End of instruction }
      begin
        is_text := True;
        i := i + 1;
        Inc(Pline, 1);
        output := output + '>';
        break
      end;

      { Add the next UTF8 character straight to output, without translating it }

      lCharSize := LCLProc.UTF8CharacterLength(Pline);
      output := output + copy(AInput, i, lCharSize);
      i := i + lCharSize;
      Inc(Pline, lCharSize);
    end;
  end;
  ConvertUTF8HtmlTextToBraille := output;

end;

{ TBrailleBrowserModule }

constructor TBrailleBrowserModule.Create;
begin
  inherited Create;

  ShortDescription := 'Braille Module';
end;

function TBrailleBrowserModule.HandleOnPageLoad(AInput: string; out
  AOutput: string): Boolean;
begin
  AOutput := ConvertUTF8HtmlTextToBraille(AInput);
  Result := True;
end;

initialization
  RegisterBrowserModule(TBrailleBrowserModule.Create());
end.

