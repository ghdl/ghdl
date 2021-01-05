with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Ghdlversion is
   Line : String (1 .. 128);
   Len : Natural;
   Pos : Natural;
   E : Natural;
begin
   loop
      exit when End_Of_File;
      Get_Line (Line, Len);
      
      --  Search GHDL
      Pos := Index (Line (1 .. Len), "GHDL ");
      if Pos /= 0 then
	 Pos := Pos + 5;
	 E := Pos;
	 while Line (E) in '0' .. '9'
	   or Line (E) in 'a' .. 'z'
	   or Line (E) = '.'
	 loop
	    exit when E = Len;
	    E := E + 1;
	 end loop;
	 Put_Line ("!define VERSION """ & Line (Pos .. E - 1) & """");
	 return;
      end if;
   end loop;
end Ghdlversion;
