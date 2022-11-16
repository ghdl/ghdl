library ieee;
use ieee.std_logic_1164.all;

entity repro3 is
end repro3;

architecture rtl of repro3 is
    signal c_a : std_logic_vector(11 downto 0) := x"FAE";

    procedure check (v : std_logic_vector) is
    begin
      report "v = " & to_hstring (v);
      assert v'ascending = false report "bad direction" severity failure;
    end check;
begin
    expected_value : process
    begin
      check ((15 downto 12 => c_a(11), 11 downto 0 => c_a));
        wait;
    end process;
end rtl;
