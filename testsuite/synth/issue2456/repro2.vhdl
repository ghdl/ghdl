library ieee;
use ieee.std_logic_1164.all;

entity repro2 is
	port(
		a_in: in std_logic_vector(7 downto 0);
		c_out: out std_logic_vector(7 downto 0)
	);
end;

architecture formal of repro2 is
begin
	-- If this process is commented out, no assertin is seen with sby.
	process(all)
	begin
		assert a_in(0) = '1';
		c_out <= a_in;
	end process;
end;
