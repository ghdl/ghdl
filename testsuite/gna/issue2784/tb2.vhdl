entity testbench is
end entity;

architecture a of testbench is
    type vector_array is array(natural range <>) of bit_vector;
    type vector_array_array is array(natural range <>) of vector_array;
--    subtype phy_data_t is vector_array(63 downto 0)(7 downto 0);
--    subtype phy_edc_t is vector_array(7 downto 0)(7 downto 0);


    constant test_pattern : vector_array_array(open)(0 to 8)(7 downto 0) := (
        (X"00", X"FF", X"00", X"00", X"00", X"00", X"00", X"00", X"00"),
        (X"00", X"FF", X"00", X"00", X"00", X"00", X"00", X"00", X"FF"),
        (X"00", X"FF", X"00", X"00", X"00", X"00", X"00", X"00", X"02")
    );

    subtype TEST_RANGE is natural range test_pattern'RANGE;
    signal test_index : TEST_RANGE := test_pattern'LOW;

begin
    process
    begin
      if test_index < test_pattern'HIGH then
        test_index <= test_index + 1;
      else
        test_index <= test_pattern'LOW;
      end if;

--            report natural'image(test_index);
--            report to_hstring(test_pattern(test_index)(8));
      report  to_hstring(test_pattern(test_index)(8));
      wait for 1 ns;
    end process;
end;
