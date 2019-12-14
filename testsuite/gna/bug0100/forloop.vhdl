entity forloop is
end forloop;

architecture behav of forloop is
    signal clk : bit;
    signal rst : bit := '1';
    signal tx : bit;
    signal data : bit_vector (7 downto 0);
    signal valid : bit;
    signal err : bit;
begin
    process
        procedure pulse is
        begin
            clk <= '0';
            wait for 1 ns;
            clk <= '1';
            wait for 1 ns;
        end pulse;
        variable txdata : bit_vector (7 downto 0);
    begin
        rst <= '1';
        tx <= '1';
        pulse;
        rst <= '0';
        
        --  Transmit 1 byte.
        tx <= '0';
        pulse;
        assert err = '0' and valid = '0' severity error;
        txdata :
        for i in txdata'reverse_range loop
            tx <= txdata(i);
            pulse;
            assert err = '0' and valid = '0' severity error;
        end loop;
        tx <= '1';  -- parity
        pulse;
        tx <= '1';  --  stop
        pulse;
        assert valid = '1' severity error;
        assert err = '0' severity error;
        assert data = txdata;
        wait;
    end process;
end behav;
