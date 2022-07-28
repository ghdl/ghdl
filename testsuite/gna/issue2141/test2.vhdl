library ieee;
use ieee.std_logic_1164.all;

entity test2 is
end;

architecture BHV of test2 is
  type array_t       is array( natural range <> ) of bit_vector;
  type arry_arry_t   is array( natural range <> ) of array_t;

  function get_min( a : arry_arry_t) return bit_vector is
    --variable res    : a'element'element; -- fail@AHDL
      variable res    : bit_vector(a'element'element'range); -- success@AHDL
    --variable res : bit_vector(7 downto 0); -- success@GHDL
    begin
      res:=a(a'left)(a'element'left);
      for i in a'range loop
        for j in a'element'range loop
          if (a(i)(j)<res) then
            res:=a(i)(j);
          end if;
        end loop;
      end loop;
      return res;
    end function get_min;

  signal m2x2  : arry_arry_t(0 to 1)(0 to 1)(7 downto 0):=((x"01",x"02"),(x"80",x"10"));
    signal m_min : m2x2'element'element; -- success@AHDL
  --signal m_min : bit_vector(7 downto 0); -- success@GHDL

begin

  m_min <= get_min(m2x2);

end;
