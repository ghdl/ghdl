-- The analysis of this file should not throw any sensitivity list warnings
-- The sensitivity of the "comb" process contains an array, of which one of its
-- fields is actually used in the process
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity arrays is
  generic( N : integer := 8 );
  port (
    rst    : in  std_logic;
    clk    : in  std_logic;
    ena    : in  std_logic;
    count  : out std_logic_vector(N-1 downto 0)
  );
end arrays;

architecture arrays_arch of arrays is

  type myarray is array (0 to 3) of unsigned(N-1 downto 0);

  signal cont, n_cont : myarray;

begin

  count <= std_logic_vector(cont(0));

  comb: process (ena, cont)
  begin
    if ena = '1' then
      n_cont(0) <= cont(0) + 1;
    else
      n_cont(0) <= cont(0);
    end if;
  end process;

  sync: process (rst, clk)
  begin
    if rst = '1' then
      cont <= (others => (others => '0'));
    elsif rising_edge(clk) then
      cont <= n_cont;
    end if;
  end process;

end arrays_arch;
