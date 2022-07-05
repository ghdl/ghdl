library ieee;
use ieee.std_logic_1164.all;

entity a is
end entity;

architecture behaviour of a is

  component b
    port (
      c : in  std_logic;
      q : out std_logic
      );
  end component;
  
begin

  inst : b
    port map (
      c => '0',
      q => open
      );
  
end architecture;
