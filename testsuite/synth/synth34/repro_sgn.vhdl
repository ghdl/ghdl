library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sub_sgn is
  port (
    clk : in std_logic;
    a : in signed(7 downto 0);
    b : out signed(7 downto 0)
  );
end sub_sgn;

architecture rtl of sub_sgn is
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
use ieee.numeric_std.all;

entity repro_sgn is
  port (
    clk : in std_logic;
    a : in signed(7 downto 0);
    b : out signed(7 downto 0)
  );
end repro_sgn;

architecture rtl of repro_sgn is
begin
  i_sub_sgn : entity work.sub_sgn
  port map (
    clk => clk,
    a => a,
    b => b
  );
end rtl;
