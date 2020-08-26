library ieee;
use ieee.std_logic_1164.all;

entity rec3 is
  port (a : out std_logic;
        b : std_logic_vector(7 downto 0));
end;

architecture behav of rec3 is
  type my_rec is record
    a : std_logic;
    b : std_logic;
    c : std_logic;
  end record;

  signal s : my_rec;
begin
  s.a <= b (0);
  s.b <= b (1);
  s.c <= b (2);
  a <= s.a;

  s.b <= '0';
end behav;
