--  Reduced from Debug.zip attached to the report (master_Bug.vhd, line 99).
--  The case choice is a constant whose value comes from a function call, so
--  it is not locally static -- which is what GHDL must say.  It used to end
--  in "build_constant: cannot handle IIR_KIND_AGGREGATE" instead.

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

package pkg_ext_i2c is
  constant led_drv_ic11_i2C_address : std_logic_vector(6 downto 0) :=
    std_logic_vector(conv_unsigned(16#60#, 7));
end package;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg_ext_i2c.all;

entity repro is
end entity;

architecture behav of repro is
  signal led_device : std_logic_vector(6 downto 0) := (others => '0');
  constant led_device_zero : std_logic_vector(6 downto 0) := (others => '0');
begin
  process (led_device)
  begin
    case led_device is
      when led_device_zero =>
        null;
      when led_drv_ic11_i2C_address =>
        null;
      when others =>
        null;
    end case;
  end process;
end architecture;
