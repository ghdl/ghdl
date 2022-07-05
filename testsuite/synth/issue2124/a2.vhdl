library ieee;
use ieee.std_logic_1164.all;

entity a2 is
end entity;

architecture behaviour of a2 is

  component b
    port (
      c : in  std_logic;
      q : out std_logic
      );
  end component;

  for inst : b use entity work.b;
begin

  inst : b
    port map (
      c => '0',
      q => open
      );
  
end architecture;
