entity crash is

end entity crash;
library ieee;
use ieee.std_logic_1164.all;
architecture test of crash is
    type t_mem2D is array
        (natural range <>,  -- Address, specifies one word
         natural range <>   -- Bit positions within a word
        )
        of std_logic;
begin  -- architecture test
    process is
        variable var_array : t_mem2D(0 to 5, 7 downto 0);
    begin  -- process
        assert var_array = t_mem2D'(X"DEAD", X"BEEF") report "var array error" severity error;
        wait;
    end process;
end architecture test;

