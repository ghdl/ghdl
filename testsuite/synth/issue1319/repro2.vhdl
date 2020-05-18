library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;

entity repro2 is
	port (
		i : in std_ulogic_vector(1 downto 0);
                o : out std_ulogic_vector (3 downto 0)
	);
end entity repro2;

architecture behav of repro2 is
    function func (v : std_ulogic_vector (1 downto 0)) return std_ulogic_vector is
       variable res : std_ulogic_vector (3 downto 0);
    begin
       case v is
       when "01" =>
           res := "1111";
       when others =>
           res := "0000";
           return "0000";
       end case;
       return res;
    end;

begin
  o <= func (i);
end architecture behav;
