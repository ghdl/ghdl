-------------------------------------------------------
-- Design Name : lfsr
-- File Name   : lfsr_updown_tb.vhd
-- Function    : Linear feedback shift register
-- Coder       : Deepak Kumar Tala (Verilog)
-- Translator  : Alexander H Pham (VHDL)
-------------------------------------------------------
library ieee;
    use ieee.std_logic_1164.all;
    use ieee.std_logic_textio.all;
    use std.textio.all;
    
entity lfsr_updown_tb is
end entity;
architecture test of lfsr_updown_tb is

    constant WIDTH :integer := 8;

    signal clk       :std_logic := '0';
    signal reset     :std_logic := '1';
    signal enable    :std_logic := '0';
    signal up_down   :std_logic := '0';
    signal count     :std_logic_vector (WIDTH-1 downto 0);
    signal overflow  :std_logic;
    
    component lfsr_updown is
    generic (
        WIDTH :integer := 8
    );
    port (
        clk       :in  std_logic;                       -- Clock input
        reset     :in  std_logic;                       -- Reset input
        enable    :in  std_logic;                       -- Enable input
        up_down   :in  std_logic;                       -- Up Down input
        count     :out std_logic_vector (WIDTH-1 downto 0);   -- Count output
        overflow  :out std_logic                        -- Overflow output
    );
    end component;
    
    constant PERIOD :time := 20 ns;
    
begin
    clk     <= not clk after PERIOD/2;
    reset   <= '0' after PERIOD*10;
    enable  <= '1' after PERIOD*11;
    up_down <= '1' after PERIOD*22;
    
    -- Display the time and result
    process (reset, enable, up_down, count, overflow)
        variable wrbuf :line;
    begin
        write(wrbuf, string'("Time: "     )); 
        writeline(output, wrbuf);
	write(wrbuf, now);
        writeline(output, wrbuf);
        write(wrbuf, string'(" rst: "     )); 
        writeline(output, wrbuf);
	write(wrbuf, reset);
        writeline(output, wrbuf);
        write(wrbuf, string'(" enable: "  )); 
        writeline(output, wrbuf);
	write(wrbuf, enable);
        writeline(output, wrbuf);
        write(wrbuf, string'(" up_down: " )); 
        writeline(output, wrbuf);
	write(wrbuf, up_down);
        writeline(output, wrbuf);
        write(wrbuf, string'(" count: "   )); 
        writeline(output, wrbuf);
	write(wrbuf, count);
        writeline(output, wrbuf);
        write(wrbuf, string'(" overflow: ")); 
        writeline(output, wrbuf);
	write(wrbuf, overflow);
        writeline(output, wrbuf);
    end process;

    Inst_lfsr_updown : lfsr_updown
    port map (
        clk      => clk,
        reset    => reset,
        enable   => enable,
        up_down  => up_down,
        count    => count,
        overflow => overflow
    );

end architecture;
