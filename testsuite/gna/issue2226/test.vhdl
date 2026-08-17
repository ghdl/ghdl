library ieee;
use ieee.std_logic_1164.all;

entity test is
end test;

architecture rtl of test is

  type barr is array (natural range <>) of std_logic;
  type barr_arr is array (natural range <>) of barr;
  signal nt : barr_arr(0 to 2)(3 downto 0) := ("0011", "000Z", "0111");

begin

  process
    variable v_bit : bit := '1';
  begin
    report "Array(1)(0) value is: " & nt'element'element'image(nt(1)(0));
    nt(1) <= (others => '1');
    wait for 1 ns;
    report "Array(1)(0) value is: " & nt'element'element'image(nt(1)(0));
    wait;
  end process;
end rtl;
