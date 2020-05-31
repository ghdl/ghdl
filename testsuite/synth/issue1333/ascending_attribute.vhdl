library ieee;
use ieee.std_logic_1164.all;

entity ascending_attribute is
  port (
    a : in std_logic_vector(7 downto 0);
    b : out boolean
  );
end ascending_attribute;

architecture rtl of ascending_attribute is
  function is_ascending(i : std_logic_vector) return boolean is
  begin
    return i'ASCENDING;
  end function;
begin
  b <= is_ascending(a);
  assert not is_ascending(a);
end rtl;
