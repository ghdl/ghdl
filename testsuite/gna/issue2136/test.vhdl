library ieee;
use ieee.std_logic_1164.all;
--use ieee.std_logic_arith.all;
--use ieee.std_logic_unsigned.all;

entity test is
end;

architecture BHV of test is
  type matrixType is array(natural range <>) of std_logic_vector;

  function get_min( a : matrixType) return std_logic_vector is
    variable res    : a'element;
  begin
    report "a: " & natural'image(a'left) & natural'image (a'right);
    res:=a(a'left);
    for i in a'range loop
      if (a(i)<res) then
        res:=a(i);
      end if;
    end loop;
    return res;
  end function get_min;

  signal matrix1 : matrixType(0 to 1)(7 downto 0):=(x"80",x"10");
  signal min1    : std_logic_vector  (7 downto 0);

begin

  min1 <= get_min(matrix1);

end;
