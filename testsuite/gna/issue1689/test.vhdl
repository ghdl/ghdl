library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity test;

architecture beh of test is
   type     t_slv_array  is array (natural range <>) of std_logic_vector;
   constant test_val : t_slv_array(0 to 1)(15 downto 0) := (others => x"BBBB");
begin
  
  process
    variable v_val : t_slv_array(0 to test_val'length)(15 downto 0);
    -- Change test_val'length to 2 and it works
  begin
    report "test_val'length " & to_string(test_val'length)
      & " v_val'length " & to_string(v_val'length);
    v_val(0) := x"AAAA";
    v_val(1 to 2) := test_val;
    assert v_val(0) = x"AAAA"
      report "Incorrect value, v_val is: " & to_hstring(v_val(0))
      & " " & to_hstring(v_val(1)) & " " & to_hstring(v_val(2))
      severity failure;
    wait;
  end process;

end architecture beh;
