library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.numeric_std.all;

entity ent is
  generic (
    g_NumberOfChannels : natural := 4;
    g_BitsPerChannel   : natural := 16
  );
  port (
    CLK     : in  std_logic;
    RST     : in  std_logic
  );
end entity;

architecture arch of ent is

  signal do_out : STD_LOGIC_VECTOR(15 DOWNTO 0);

begin

  process(RST, CLK)
    variable cnt: natural range 0 to g_NumberOfChannels-1;
  begin
    if RST then
      cnt := 0;
    elsif rising_edge(CLK) then
      cnt := cnt + 1 when cnt<g_NumberOfChannels-1 else 0;
    end if;
  end process;

end architecture;
