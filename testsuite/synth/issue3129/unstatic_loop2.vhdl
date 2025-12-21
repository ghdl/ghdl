library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity unstatic_loop2 is
  port (val   : in unsigned(15 downto 0);
        width : out unsigned(7 downto 0));
end entity unstatic_loop2;

architecture rtl of unstatic_loop2 is
  --------------------
  function ceillog2 (num : natural) return natural is
    variable cur_x      : natural := 2;
  begin
    for i in 1 to 16 loop
      if cur_x >= num then
        return i;
      end if;
      cur_x := cur_x * 2;
    end loop;
  end function ceillog2;
  --------------------
  
  
begin
  width <= to_unsigned(ceillog2 (to_integer(val)), width'length);
end;
