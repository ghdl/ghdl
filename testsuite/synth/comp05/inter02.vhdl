library ieee;
use ieee.std_logic_1164.all;

package inter02_pkg is
  type my_rec is record
    v : std_logic_vector;
    c : character;
  end record;
end;
  
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.inter02_pkg.all;
entity inter02_sub is
  port (clk : std_logic;
        inp : my_rec;
        o : out character);
end inter02_sub;

architecture behav of inter02_sub is
begin
  process (clk) is
  begin
    if rising_edge(clk) then
      if inp.c >= 'a' and inp.c <= 'z' then
        o <= character'val(character'pos(inp.c) - 32);
      else
        o <= inp.c;
      end if;
    end if;
  end process;
end behav;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity inter02 is
  port (clk : std_logic;
        a : std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end inter02;

architecture behav of inter02 is
  signal cin, cout : character;
begin
  cin <= character'val(to_integer(unsigned(a)));
  dut : entity work.inter02_sub
    port map (clk => clk, inp.c => cin, inp.v => "00", o => cout);
  o <= std_logic_vector(to_unsigned(character'pos(cout), 8));
end behav;
