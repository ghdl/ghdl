use std.textio.all;
use			work.corelib.all;

entity Integer_List_tb is
end entity;

architecture test of Integer_List_tb is
	-- shared variable	GlobalStdOut	: T_STDOUT;
	shared variable List1					: Integer_List;
begin
	process
		variable index		: INTEGER;
		variable element	: INTEGER;
	begin
		List1.Init;
		
		for i in 0 to 67 loop
			element := i + 1;
			report "Append " & INTEGER'image(element);
			index		:= List1.Append(element);
			report "  index= " & INTEGER'image(index);
			
			index		:= List1.IndexOf(element);
			List1.Set(index, element + 100);
			element	:= List1.Get(i);
			report "  IndexOf -> " & INTEGER'image(index);
			report "  Get -> " & INTEGER'image(element);
		end loop;
		for i in 54 downto 27 loop
                  report "RemoveAt " & INTEGER'image(i);
			List1.RemoveAt(i);
		end loop;
		for i in 20 downto 7 loop
                  report "Remove " & INTEGER'image(i + 101);
			List1.Remove(i + 101);
		end loop;
		wait;
	end process;
end architecture;
