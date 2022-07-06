library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity isx is
    port (
        s : in std_ulogic_vector(1 downto 0);
        u : in signed(1 downto 0)
	);
end entity isx;

architecture rtl of isx is
begin
    test1 : process(all)
    begin
	if is_X(s) then
	    report "std_ulogic test" severity FAILURE;
	end if;
	if is_X(u) then
	    report "unsigned test" severity FAILURE;
	end if;
    end process;
end;
