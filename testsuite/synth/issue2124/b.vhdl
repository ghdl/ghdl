library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (
    c : in  std_logic;
    d : in  std_logic;
    q : out std_logic
    );
end entity;

architecture behaviour of b is
begin

  process (c)
  begin
    if rising_edge(c)
    then
      q <= d;
    end if;
  end process;
  
end architecture;
