library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity test;

architecture beh of test is
   type     t_slv_array  is array (natural range <>) of std_logic_vector;
   constant c_test_val : t_slv_array(0 to 1)(15 downto 0) := (others => x"BBBB");
begin
  
  process
    procedure test_proc(test_val : in t_slv_array) is
        variable v_val : t_slv_array(0 to test_val'length)(15 downto 0);
    begin
        v_val(0) := x"AAAA";
        v_val(1 to test_val'length) := test_val;
        assert v_val(0) = x"AAAA" report "Incorrect value, v_val is: " & to_hstring(v_val(0)) & " " & to_hstring(v_val(1)) & " " & to_hstring(v_val(2)) severity failure;
    end procedure;
  begin
    test_proc(c_test_val);
    wait;
  end process;

end architecture beh;
