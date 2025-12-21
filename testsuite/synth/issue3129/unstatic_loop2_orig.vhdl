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
  
  
begin
  width <= to_unsigned(ceillog2 (to_integer(val)), width'length);
end;
