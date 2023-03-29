entity repro1 is
end;

architecture sim of repro1 is
    type SLV_vector is array (natural range <>) of bit_vector;

    constant TEST_VAL_A : SLV_vector(3 downto 1)(7 downto 0) := (x"AD", x"12", x"DC");

    -- Broken
    constant TEST_SLICE : SLV_vector(1 downto 0) := TEST_VAL_A(3 downto 2);

    -- Works
    --constant TEST_SLICE : SLV_vector(1 downto 0)(TEST_VAL_A'element'range) := TEST_VAL_A(3 downto 2);

begin
    test_runner : process
    begin
        assert TEST_SLICE'high = 1 report "High should be 1" severity failure;
        assert TEST_SLICE'low = 0 report "Low should be 0" severity failure;
        wait;
    end process test_runner;

end architecture sim;
