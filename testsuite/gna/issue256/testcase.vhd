entity testcase is
    port(clk: in bit);
begin
    check: process is
    begin
        -- Require at least 10ns between clock edges
        assert clk'delayed'last_event >= 10 ns;
        wait on clk;
    end process check;
end entity testcase;

-- Keep the compiler happy
architecture empty of testcase is
begin
end architecture empty;
