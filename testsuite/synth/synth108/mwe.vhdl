library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity MWE is

  port (
    a   : in std_logic_vector(0 to 3);
    b   : in std_logic_vector(0 to 3);
    sel : in std_logic_vector(0 to 1);
    o   : out std_logic_vector(0 to 1)
  );
end MWE;

architecture behavioral of MWE is
  signal cnt : integer range 0 to 1;
begin
  cnt <= to_integer(unsigned(sel));
  o <= a(cnt*o'length to (cnt + 1)*o'length - 1) xor b(cnt*o'length to (cnt + 1)*o'length - 1);
end architecture behavioral;
