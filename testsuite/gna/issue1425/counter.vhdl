library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity counter is
  generic( N : integer := 8 );
  port (
    rst    : in  std_logic;
    clk    : in  std_logic;
    ena    : in  std_logic;
    count  : out std_logic_vector(N-1 downto 0)
  );
end counter;

architecture counter_arch of counter is

  signal cont, n_cont : unsigned(N-1 downto 0);

begin

  count <= std_logic_vector(cont);

  comb: process (rst)
  begin
    if ena = '1' then
      n_cont <= cont + 1;
    else
      n_cont <= cont;
    end if;
  end process;

  sync: process (rst, clk)
  begin
    if rst = '1' then
      cont <= (others => '0');
    elsif rising_edge(clk) then
      cont <= n_cont;
    end if;
  end process;

end counter_arch;

