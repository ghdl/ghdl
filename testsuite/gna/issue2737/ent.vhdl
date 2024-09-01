library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
    port (
        clk : in std_logic := '0'
    );
end entity ent;

architecture arch of ent is
    signal dummy : std_logic := '0';
    constant delay : natural := 5;
begin
   default clock is rising_edge(clk);

   assertFail : assert always dummy -> next[delay] (not dummy);

end architecture arch;
