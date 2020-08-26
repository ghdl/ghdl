library ieee;
use ieee.std_logic_1164.all;

entity rec1 is
  port (a : out std_logic;
        b : std_logic_vector(7 downto 0));
end;

architecture behav of rec1 is
  type my_rec is record
    a : std_logic;
    b : std_logic;
  end record;

  signal s : my_rec;
begin
  s.b <= s.a;
  a <= s.b;

  s.a <= '1' when b /= x"00" else '0';

  s.b <= '0';
end behav;
