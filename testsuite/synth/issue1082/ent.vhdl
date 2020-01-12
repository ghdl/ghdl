library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
  port(
    a_in  : in std_logic_vector(31 downto 0);
    b_out : out std_logic_vector(31 downto 0)
  );
end test;

architecture rtl of test is
begin
  process(all)
  begin
    b_out <= std_logic_vector(to_unsigned((31-to_integer(unsigned(a_in))) / 4, 32));
  end process;
end;