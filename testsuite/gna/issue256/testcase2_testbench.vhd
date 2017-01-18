entity testcase2_testbench is
end entity testcase2_testbench;

architecture bench of testcase2_testbench is
    signal clk: bit;
begin
    dut: entity work.testcase2(empty) port map(clk => clk);

    stimulus: process is
    begin
        -- Valid low and high pulses
        clk <= '0';
        wait for 10 ns;
        clk <= '1';
        wait for 10 ns;
        -- Confirm that we're timing events, not transactions
        clk <= '1';
        wait for 5 ns;
        -- Now send a short pulse to make the assertion fire        
        clk <= '0';
        wait for 5 ns;
        -- Assertion should fire here, at 30ns
        clk <= '1';
        wait;
    end process stimulus;
end architecture bench;
