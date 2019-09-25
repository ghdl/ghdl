library ieee;
  use ieee.std_logic_1164.all;

entity int_test2 is
  generic (
    INT_MIN : integer range 1 to 8 := 1;
    INT_MAX : integer range 1 to 8 := 8
  );
  port (
    clk : in std_logic;
    a : in integer range INT_MIN to INT_MAX;
    b : out integer range INT_MIN to INT_MAX
  );
end int_test2;

architecture rtl of int_test2 is
  signal int : integer range INT_MIN to INT_MAX;
begin
  process (clk)
  begin
    if rising_edge (clk) then
      if a < INT_MAX then
        int <= a + 1;
      else
        int <= INT_MIN * 2;
      end if;
    end if;
  end process;
  b <= int;
end rtl;

