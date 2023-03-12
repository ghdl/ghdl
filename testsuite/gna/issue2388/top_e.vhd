
library ieee;
use ieee.std_logic_1164.all;

entity unit_a is
    port (
        clk   : in  std_logic;
        reset : in  std_logic;

        a     : in  std_logic;
        b     : out std_logic
    );
end entity;

library ieee;
use ieee.std_logic_1164.all;

entity unit_b is
    port (
        clk   : in std_logic;
        reset : in std_logic;

        a     : in  std_logic;
        b     : out std_logic
    );
end entity;

architecture rtl of unit_a is
begin
    my_inst: entity work.unit_b
      port map (
        clk => clk,
        reset => reset,
        a => a,
        b => b
      );
end architecture;
