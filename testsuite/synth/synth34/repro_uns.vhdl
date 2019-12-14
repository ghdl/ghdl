library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sub_uns is
  port (
    clk : in std_logic;
    a : in unsigned(7 downto 0);
    b : out unsigned(7 downto 0)
  );
end sub_uns;

architecture rtl of sub_uns is
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

entity repro_uns is
  port (
    clk : in std_logic;
    a : in unsigned(7 downto 0);
    b : out unsigned(7 downto 0)
  );
end repro_uns;

architecture rtl of repro_uns is
begin
  i_sub_uns : entity work.sub_uns
  port map (
    clk => clk,
    a => a,
    b => b
  );
end rtl;
