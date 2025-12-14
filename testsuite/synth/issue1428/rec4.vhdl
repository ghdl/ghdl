library ieee;
use ieee.std_logic_1164.all;

entity rec4 is
  port (a : out std_logic;
        b : std_logic_vector(7 downto 0));
end;

architecture behav of rec4 is
  type my_rec is record
    a : std_logic;
    b : std_logic_vector(3 downto 0);
    c : std_logic_vector(3 downto 0);
  end record;

  signal s : my_rec;
begin
  (s.b (3), s.c(0)) <= std_logic_vector'("01");

  s.b <= b(3 downto 0);
  s.c <= b(7 downto 4);
  a <= s.a;

  s.a <= '1' when s.b /= x"0" else '0';

  -- Assignments to net s:
  --  bits 0 - 0 (w=1)
  --  bits 1 - 8 (w=8)
  --  bits 2 - 2 (w=1)

end behav;
