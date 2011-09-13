unit utf8_braille;

{$mode delphi}

interface
    Type
        dictionary = array[1..32] of string;
	dictionary_pointer = ^dictionary;

    Const
         number_signal = chr($e2) + chr($a0) + chr($bc);
         caps_signal = chr($e2) + chr($a0) + chr($a8);
         d1:dictionary = ({!} chr($96), {"} chr($a6) , {#} 'TODO', {(*$*)} chr($b0) , {%} chr($b8)+chr($e2)+chr($a0)+chr($b4), {&} chr($af), {'} chr($84), {(} chr($a3)+chr($e2)+chr($a0)+chr($84), {)} chr($a0)+chr($e2)+chr($a0)+chr($9c), {*} chr($94), {+} chr($96), {,} chr($82), {-} chr($a4), {.} chr($84), {/} chr($90)+chr($e2)+chr($a0)+chr($b2), {0} chr($9a), {1} chr($81), {2} chr($83), {3} chr($89), {4} chr($99), {5} chr($91), {6} chr($8b), {7} chr($9b), {8} chr($93), {9} chr($8a), {:} chr($92), {;} chr($86), {<} chr($aa), {=} chr($b6), {>} chr($95), {?} chr($a2), {@} 'TODO');
         d2:dictionary = ({a} chr($81), {b} chr($83), {c} chr($89), {d} chr($99), {e} chr($91), {f} chr($8b), {g} chr($9b), {h} chr($93), {i} chr($8a), {j} chr($9a), {k} chr($85), {l} chr($87), {m} chr($8d), {n} chr($9d), {o} chr($95), {p} chr($8f), {q} chr($9f), {r} chr($97), {s} chr($8e), {t} chr($9e), {u} chr($a5), {v} chr($a7), {w} chr($ba), {x} chr($ad), {y} chr($bd), {z} chr($b5),'','','','','','');
         d3:dictionary = ({a + grave} chr($ab), {a + acute} chr($b7), {a + circumflex} chr($a1), {a + tilde} chr($9c), {a + diaeresis} 'TODO', {a + ring above} 'TODO', {ae} 'TODO', {c + cedilla} chr($af), {e + grave} chr($ae), {e + acute} chr($bf), {e + circumflex} chr($a3), {e + diaeresis} 'TODO', {i + grave} chr($a9), {i + acute}  chr($8c), {i + circumflex} 'TODO', {i + diaeresis} chr($bb), {eth} 'TODO', {n + tilde} 'TODO', {o + grave} chr($ba), {o + acute} chr($ac), {o + circumflex} chr($b9), {o + tilde} chr($aa), {o + diaeresis} 'TODO', {division sign} 'TODO', {o + stroke} 'TODO', {u + grave} chr($b1), {u + acute} chr($be), {u + circumflex} 'TODO', {u + diaeresis} chr($b3), {y + acute} 'TODO', {thorn} 'TODO', {y + diaeresis} 'TODO');
uses
  Classes, SysUtils; 

implementation

Function Braille (Word: string): string;

Var
   count, count_aux, n, n_aux, decr:integer;
   Braille_string, string_aux:string;
   num, accented:boolean;
   dict_p:dictionary_pointer;

   begin
   Braille_string := '';
   num := False;
   accented := False;
   New(dict_p);
   decr := 0;
   count := 1;
   n_aux := 0;
   while count <= length(Word) do
   begin
        if Word[count] = ' ' then   {reproduce the empty space and go to next iteration}
           begin
           Braille_string := Braille_string + chr($e2)+chr($a0)+chr($80);
           count := count + 1;
           continue
           end;
        if accented then {if the last iteration found n = 195, it means we have an accented character. Print it and go to next iteration}
           begin
           n := ord(Word[count]);
           Braille_string := Braille_string + chr($e2) + chr($a0) + (dict_p^)[n-decr];
           accented := False;
	   count := count + 1;
           continue
           end;

           n := ord(Word[count]);

        if ((n >= 97) and (n <= 122)) then       {if the char is a letter, choose d2}
	   begin
	   dict_p^ := d2;
	   decr := 96
	   end
	else
	    if n = 195 then   {if the char is a accented letter, choose d3 and the next iteration will only print the next char}
	       begin
	       dict_p^ := d3;
	       decr := 159;
	       accented := True;
	       count := count + 1;
	       continue
	       end

	    else
                if n = 195 then {if the char is a accented letter, choose d3 and the next iteration will only print the next char}
		   begin
	           dict_p^ := d3;
	           decr := 159;
                   accented := True;
                   count := count + 1;
                   continue
                   end

                else
                    if ((n >= 65) and (n <= 90)) then      {if it's a capital letter, choose d2 and see if it's only the first letter or if there are more capital letters}
		       begin
                       dict_p^ := d2;
                       decr := 96 - 32;
                       string_aux := chr($e2) + chr($a0) + (dict_p^)[n-decr];
                       count_aux := count + 1;
                       if count_aux > length(Word) then
                          begin
                          Braille_string := Braille_string + caps_signal + string_aux;
                          break
                          end;
                       n_aux := ord(Word[count_aux]);
                       while ((n_aux >= 65) and (n_aux <= 90)) do
                             begin
                             string_aux := string_aux + chr($e2) + chr($a0) + (dict_p^)[n_aux-decr];
                             count_aux := count_aux + 1;
                             if count_aux > length(Word) then
                                break;
                             n_aux := ord(Word[count_aux]);
                             end;
                       if length(string_aux) > 3 then
                          Braille_string := Braille_string + caps_signal + caps_signal + string_aux
                       else
                           Braille_string := Braille_string + caps_signal + string_aux;
                       count := count_aux;
                       continue
                       end
                    else  {or else, choose d1}
                          begin
                               dict_p^ := d1;
                               decr := 32
                          end;

        if num and not (((n >= 48) and (n <= 57)) or (n = 46) or (n = 44)) then		{if the whole number has already been printed, change num to False again}
           num:=False;

        if not num and ((n >= 48) and (n <= 57)) then		{if it's the first number to appear, print the number signal first}
           begin
                Braille_string := Braille_string + number_signal;
                num := True;
           end;

        Braille_string := Braille_string + chr($e2) + chr($a0) + (dict_p^)[n-decr];

        count := count + 1;
        end;

   Braille := Braille_string + '\n';

   end;
end.

