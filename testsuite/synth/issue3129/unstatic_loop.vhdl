library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity ghdl_unstatic_loop is
  
  port (
    idx   : in  unsigned(3 downto 0);
    val   : out unsigned(15 downto 0);
    width : out unsigned(7 downto 0);
    clk   : in  std_ulogic);

end entity ghdl_unstatic_loop;

architecture test of ghdl_unstatic_loop is

  constant TABLE : integer_vector(0 to 15)
    := (1, 1, 2, 3,
        5, 8, 13, 21,
        34, 55, 89, 144,
        233, 377, 610, 987);
  -- Fibonacci sequence for something non-trivial

  --------------------
  function ceillog2 (num : natural) return natural is
    variable cur_2_to_x : natural := 2;
    variable cur_x      : natural := 1;
  begin
    if num < 2 then
      return 1;
    end if;

    while cur_2_to_x < num loop
      cur_2_to_x := cur_2_to_x * 2;
      cur_x      := cur_x + 1;
    end loop;
    return cur_x;
  end function ceillog2;
  --------------------
  
  
begin

  p_table : process (clk) is
    variable idx_v : natural;
    variable val_v : natural;
  begin
    if rising_edge(clk) then
      idx_v := to_integer(to_01(idx));
      val_v := TABLE(idx_v);
      val   <= to_unsigned(val_v, val'length);
      width <= to_unsigned(ceillog2(val_v), width'length);
    end if;
  end process p_table;

end architecture test;
