library ieee;
use ieee.std_logic_1164.all;

entity err3 is
end;

architecture arch of err3 is
  component comp is
    port (
      a1 : in std_logic;
      a2 : in std_logic_vector ( 2 downto 0 );
      a3 : in std_logic
    );
    end component comp;
begin
  inst: comp
    port map (
      a1 => 'X',
      a2 => (others => 'X'),
      a3 => (others => 'X')
      );
end architecture arch;
