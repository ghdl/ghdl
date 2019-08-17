library ieee;
use ieee.std_logic_1164.all;

entity sub_rng1 is
  port (
    clk : in std_logic;
    a : in natural range 0 to 7;
    b : out natural range 0 to 7
  );
end sub_rng1;

architecture rtl of sub_rng1 is
begin
  process(clk)
  begin
    if rising_edge(clk) then
      b <= a;
    end if;
  end process;
end rtl;


library ieee;
use ieee.std_logic_1164.all;

entity repro_rng1 is
  port (
    clk : in std_logic;
    a : in natural range 0 to 7;
    b : out natural range 0 to 7
  );
end repro_rng1;

architecture rtl of repro_rng1 is
begin
  i_sub_rng1 : entity work.sub_rng1
  port map (
    clk => clk,
    a => a,
    b => b
  );
end rtl;
