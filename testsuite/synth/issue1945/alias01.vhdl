library ieee;
use ieee.STD_LOGIC_1164.all;

entity alias01 is
    port(
        i : in  std_logic_vector(7 downto 0);
        o : out std_logic
    );
end entity;

architecture rtl of alias01 is
    alias i_alias : std_logic_vector(7 downto 2) is i(6 downto 1);
    alias lower   : std_logic_vector(3 downto 0) is i_alias(6 downto 3);
begin
  o <= '1' when lower = "0000" else '0';
end architecture;
