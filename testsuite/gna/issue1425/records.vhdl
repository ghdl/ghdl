-- The analysis of this file should not throw any sensitivity list warnings
-- The sensitivity of the "comb" process contains a record, of which one of its
-- fields is actually used in the process
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity records is
  generic( N : integer := 8 );
  port (
    rst    : in  std_logic;
    clk    : in  std_logic;
    ena    : in  std_logic;
    count  : out std_logic_vector(N-1 downto 0)
  );
end records;

architecture records_arch of records is

  type myrecord is record
    data: unsigned(N-1 downto 0);
    sat: std_logic;
  end record;

  signal cont, n_cont : myrecord;

begin

  count <= std_logic_vector(cont.data);

  comb: process (ena, cont)
  begin
    if ena = '1' then
      n_cont.data <= cont.data + 1;
    else
      n_cont.data <= cont.data;
    end if;
  end process;

  sync: process (rst, clk)
  begin
    if rst = '1' then
      cont <= (sat => '0', data => (others => '0'));
    elsif rising_edge(clk) then
      cont <= n_cont;
    end if;
  end process;

end records_arch;
