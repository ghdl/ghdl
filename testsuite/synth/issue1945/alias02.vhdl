library ieee;
use ieee.STD_LOGIC_1164.all;

entity alias02 is
    port(
        i : in  std_logic;
        o : out std_logic_vector(7 downto 0)
    );
end entity;

architecture rtl of alias02 is
    alias o_alias : std_logic_vector(7 downto 2) is o(6 downto 1);
    alias o_alias2   : std_logic_vector(3 downto 0) is o_alias(6 downto 3);
begin
  o (7) <= '1';
  o (0) <= '1';
  o_alias (7) <= '1';
  o_alias (2) <= '1';
  o_alias2 <= i & i & i & i;
end architecture;
