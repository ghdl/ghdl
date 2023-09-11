library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity alias1 is
  generic( N : integer := 8 );
  port (
    rst    : in  std_logic;
    clk    : in  std_logic;
    carry  : out std_logic;
    count  : out std_logic_vector(N-1 downto 0)
  );
end;

architecture arch of alias1 is

  signal cont : unsigned(N downto 0);
  alias carr is cont(N);
  alias val is cont(N-1 downto 0);

begin

  process (carr)
  begin
    carry <= carr;
  end process;
  
  count <= std_logic_vector(val);

  sync: process (rst, clk)
  begin
    if rst = '1' then
      cont <= (others => '0');
    elsif rising_edge(clk) then
      cont <= cont + 1;
    end if;
  end process;

end arch;

