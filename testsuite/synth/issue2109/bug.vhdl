library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
generic(
    tmp : std_ulogic_vector(0 downto 1) := ""
);
port(
    val : out std_ulogic_vector(0 downto 1)
);
end entity;

architecture rtl of bug is
begin
   val <= tmp;
end architecture;
