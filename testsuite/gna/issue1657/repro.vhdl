entity repro is
end;

architecture rtl of repro is
begin
  Test_Proc: process
    type bit_vector_array is array(natural range <>) of bit_vector;
    variable v : bit_vector_array(0 to 3)(7 downto 0) :=
      (others => (others => '0'));
    begin
      v := v(1 to 3) & bit_vector'(x"FF");

      for i in v'range loop
        report "V(" & natural'image(i) & ") = " & to_string (v(i));
      end loop;

      assert V(0) = x"00" severity failure;
      assert V(1) = x"00" severity failure;
      assert V(2) = x"00" severity failure;
      assert V(3) = x"FF" severity failure;

      report "TESTING" severity note;
      wait;
    end process;
end architecture rtl;
