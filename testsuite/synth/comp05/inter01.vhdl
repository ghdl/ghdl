library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity inter01_sub is
  port (clk : std_logic;
        inp : character;
        o : out character);
end inter01_sub;

architecture behav of inter01_sub is
begin
  process (clk) is
  begin
    if rising_edge(clk) then
      if inp >= 'a' and inp <= 'z' then
        o <= character'val(character'pos(inp) - 32);
      else
        o <= inp;
      end if;
    end if;
  end process;
end behav;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity inter01 is
  port (clk : std_logic;
        a : std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end inter01;

architecture behav of inter01 is
  signal cin, cout : character;
begin
  cin <= character'val(to_integer(unsigned(a)));
  dut : entity work.inter01_sub
    port map (clk => clk, inp => cin, o => cout);
  o <= std_logic_vector(to_unsigned(character'pos(cout), 8));
end behav;
