entity test2 is
end entity test2;

architecture beh of test2 is
   type     t_bv_array  is array (natural range <>) of bit_vector;
   constant c_test_val : t_bv_array(0 to 1)(15 downto 0) := (others => x"BBBB");
begin

  process
    procedure test_proc(test_val : in t_bv_array) is
        variable v_val : t_bv_array(0 to test_val'length)(15 downto 0);
    begin
        v_val(0) := x"AAAA";
        v_val(1 to test_val'length) := test_val;
--        v_val(1) := test_val(0);
--        v_val(2) := test_val(1);
        report "v_val is: " & to_hstring(v_val(0)) & " " & to_hstring(v_val(1)) & " " & to_hstring(v_val(2));
        assert v_val(0) = x"AAAA" report "Incorrect value" severity failure;
    end procedure;
  begin
    test_proc(c_test_val);
    wait;
  end process;

end architecture beh;
