library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;

entity repro3 is
	port (
		i : in std_ulogic_vector(1 downto 0);
                o : out std_ulogic_vector (3 downto 0)
	);
end entity repro3;

architecture behav of repro3 is
    function func (v : std_ulogic_vector (1 downto 0)) return std_ulogic_vector is
    begin
       case v is
       when "01" =>
         null;
       when others =>
         return "0000";
       end case;
       return "1111";
    end;
begin
  o <= func (i);
end architecture behav;
