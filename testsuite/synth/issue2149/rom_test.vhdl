library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity rom_test is
	generic (
		CONFIG : std_ulogic_vector(0 to 7)
	);
	port (
		clk     :  in std_ulogic;
		reset_n :  in std_ulogic;
		valid   : out std_ulogic
	);
end rom_test;

architecture struct of rom_test is
	type entry_t is record
		value : unsigned(7 downto 0);
		valid : std_ulogic;
	end record;

	type rom_t is array(natural range<>) of entry_t;

	function generate_rom(input : std_ulogic_vector) return rom_t is
		variable ret : rom_t(0 to input'length-1);
	begin
		for i in input'range loop
			ret(i).valid := input(i);
			if input(i) = '1' then
				ret(i).value := to_unsigned(i, 8);
			end if;
		end loop;
		return ret;
	end function;

	constant ROM : rom_t := generate_rom(CONFIG);

	signal index : natural range 0 to ROM'length-1;
	signal entry : entry_t;

begin
	process(clk, reset_n)
	begin
		if reset_n = '0' then
			index <= 0;
		elsif rising_edge(clk) then
			if index = ROM'length-1 then
				index <= 0;
			else
				index <= index + 1;
			end if;
		end if;
	end process;

	entry <= ROM(index);
	valid <= entry.valid;
	
end architecture;
