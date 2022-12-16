library ieee;
use ieee.std_logic_1164.all;

entity ent is
end entity;

architecture behaviour of ent is
  component comp is
    port (
      d : in  std_logic;
      q : out std_logic
      );
    end component;
begin

  comp : comp
    port map (
      d => '0',
      q => open
      );
end architecture;
