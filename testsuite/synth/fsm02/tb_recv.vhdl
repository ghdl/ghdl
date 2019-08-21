library ieee;
use ieee.std_logic_1164.all;

entity tb_recv is
end tb_recv;

architecture behav of tb_recv is
    signal clk : std_logic;
    signal rst : std_logic := '1';
    signal tx : std_logic;
    signal data : std_logic_vector (7 downto 0);
    signal valid : std_logic;
    signal err : std_logic;
begin
    dut: entity work.recv
      port map (clk => clk,
        rst => rst,
        rx => tx,
        byte => data,
        b_err => err,
        b_en => valid);
    
    process
        procedure pulse is
        begin
            clk <= '0';
            wait for 1 ns;
            clk <= '1';
            wait for 1 ns;
        end pulse;
        variable txdata : std_logic_vector (7 downto 0);
    begin
        rst <= '1';
        tx <= '1';
        pulse;
        rst <= '0';
        
        --  Transmit 1 byte.
        tx <= '0';
        pulse;
        assert err = '0' and valid = '0' severity error;
        txdata := x"6e";
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