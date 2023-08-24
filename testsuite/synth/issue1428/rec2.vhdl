library ieee;
use ieee.std_logic_1164.all;

entity rec2 is
  port (a : out std_logic;
        b : std_logic_vector(7 downto 0));
end;

architecture behav of rec2 is
  type my_rec is record
    a : std_logic;
    b : std_logic_vector(7 downto 0);
  end record;

  signal s : my_rec;
begin
  s.b <= b;
  a <= s.a;

  s.a <= '1' when s.b /= x"00" else '0';

  s.b (1) <= '0';

  -- Assignments to net s:
  --  bits 0 - 0 (w=1)
  --  bits 1 - 8 (w=8)
  --  bits 2 - 2 (w=1)

end behav;
