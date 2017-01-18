entity testcase3 is
    port(clk: in bit);
begin
end entity testcase3;

-- Keep the compiler happy
architecture empty of testcase3 is
begin
    check: assert clk'delayed'last_event >= 10 ns;
end architecture empty;
