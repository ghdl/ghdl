library ieee;
use ieee.std_logic_1164.all;

entity tb is end;
architecture sim of tb is
  type arr_a is array (natural range <>, natural range <>) of std_logic;
  type arr_b is array (natural range <>, natural range <>) of std_logic;
  constant A : arr_a(0 to 0, 7 downto 0) := (others => (others => '0'));
  constant B : arr_b(0 to 0, 7 downto 0) := arr_b(A);
begin
  process begin report "PASS"; std.env.stop(0); end process;
end;
