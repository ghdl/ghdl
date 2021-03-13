library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_fail is
  port(
    value_i: in std_ulogic_vector(7 downto 0);
    matches_o: out std_ulogic
    );
end entity;

architecture beh of test_fail is

  subtype data_t is std_ulogic_vector(7 downto 0);
  signal value_s : data_t;
  constant expected_c : data_t := "10001---";

begin

  value_s <= value_i;
  matches_o <= '1' when std_match(value_s, expected_c) else '0';

end architecture;
