entity testcase2 is
    port(clk: in bit);
begin
    check: assert clk'delayed'last_event >= 10 ns;
end entity testcase2;
