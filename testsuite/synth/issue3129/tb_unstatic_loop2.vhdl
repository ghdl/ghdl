library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity tb_unstatic_loop2 is
end entity tb_unstatic_loop2;

architecture test of tb_unstatic_loop2 is
  constant TABLE : integer_vector(0 to 15)
    := (1, 1, 2, 3,
        5, 8, 13, 21,
        34, 55, 89, 144,
        233, 377, 610, 987);
  -- Fibonacci sequence for something non-trivial

  signal val   : unsigned(15 downto 0);
  signal width : unsigned(7 downto 0);
begin

  unstatic_loop2_1: entity work.unstatic_loop2
    port map (
      val   => val,
      width => width);
  
  p_table : process is
    variable w, v : natural;
  begin
    for i in table'range loop
      v := TABLE(i);
      val <= to_unsigned(v, val'length);
      wait for 1 ns;
      w := to_integer(width);
      report "ceillog2(" & natural'image(v) & ") = " & natural'image(w);
      assert v = 1 or v <= 2**w severity failure;
      assert v = 1 or v > 2**(w-1) severity failure;
    end loop;
    wait;
  end process p_table;

end architecture test;
