library ieee;
use ieee.std_logic_1164.all;

package inter03_pkg is
  type my_rec is record
    v : std_logic_vector;
    w : std_logic_vector;
    c : character;
  end record;
end;
  
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.inter03_pkg.all;
entity inter03_sub is
  port (clk : std_logic;
        inp : my_rec;
        o : out character);
end inter03_sub;

architecture behav of inter03_sub is
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

entity inter03 is
  port (clk : std_logic;
        a : std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end inter03;

architecture behav of inter03 is
  signal cin, cout : character;
begin
  cin <= character'val(to_integer(unsigned(a)));
  dut : entity work.inter03_sub
    port map (clk => clk, inp.c => cin, inp.v => "00", inp.w => "001", o => cout);
  dut2 : entity work.inter03_sub
    port map (clk => clk, inp.c => cin, inp.v => "010", inp.w => "11", o => open);
  o <= std_logic_vector(to_unsigned(character'pos(cout), 8));
end behav;
