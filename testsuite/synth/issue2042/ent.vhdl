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
    RST     : in  std_logic;
    DATA    : out std_logic_vector(g_NumberOfChannels*g_BitsPerChannel-1 downto 0)
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
      DATA((cnt+1)*g_BitsPerChannel-1 downto cnt*g_BitsPerChannel) <= do_out(do_out'left downto 1+do_out'left-g_BitsPerChannel);
      cnt := cnt + 1 when cnt<g_NumberOfChannels-1 else 0;
    end if;
  end process;

end architecture;
