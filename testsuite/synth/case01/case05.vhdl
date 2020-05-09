library ieee;
use ieee.std_logic_1164.all;

entity case05 is
    port (
	in_en  : std_logic;
	in_v  : std_logic_vector(3 downto 0)
	);
end entity case05;

architecture behav of case05 is
begin
    process(in_en, in_v)
	variable l : boolean;
    begin
	if in_en = '1' then
	    case in_v is

	    when "0010" =>
		l := in_v = "0000";
	    when others =>
		report "illegal";
	    end case;
	end if;
    end process;
end architecture behav;
