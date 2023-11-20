library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity test1 is
end entity;

architecture rtl of test1 is
  function log2_ceil (
    val: natural)
  return natural is
  begin
    return integer(ceil(log2(real(val) + 1.0)));
  end function log2_ceil;
  signal eeprom_bit_cnt   : unsigned(16 + log2_ceil(7) - 1 downto 0);
  alias  eeprom_byte_cnt is eeprom_bit_cnt(eeprom_bit_cnt'high downto log2_ceil(7));
begin
  p_proc : process
  begin
    report to_string(eeprom_byte_cnt'length) & " " & to_string(eeprom_byte_cnt'high) & " " & to_string(eeprom_byte_cnt'low);
    assert eeprom_byte_cnt'length = 16 severity failure;
    assert eeprom_byte_cnt'high = 18 severity failure;
    assert eeprom_byte_cnt'low = 3 severity failure;
    wait;
  end process;
end architecture;
