library ieee;
use ieee.std_logic_1164.all;

entity nomem1 is
  generic (l : natural := 1);
  port (inp : std_logic_vector(l - 1 downto 0);
        v : std_logic;
        off : natural;
        res : out std_logic_vector(l - 1 downto 0));
end;

architecture arch of nomem1 is
begin
  process (inp, v, off)
    variable t : std_logic_vector(l - 1 downto 0);
  begin
    t := inp;
    t (off) := v;
    res <= t;
  end process;
end;
