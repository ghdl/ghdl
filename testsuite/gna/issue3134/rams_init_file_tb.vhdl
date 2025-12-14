-- Initializing Block RAM from external data file
-- File: rams_init_file.vhd

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use std.env.finish;

entity rams_init_file_tb is
    generic (
        AW : positive := 6;
        DW : positive := 32
    );
end rams_init_file_tb;

architecture behav of rams_init_file_tb is
    signal clk  : std_logic := '1';
    signal ce   : std_logic := '0';
    signal we   : std_logic;
    signal addr : std_logic_vector(AW-1 downto 0);
    signal din  : std_logic_vector(DW-1 downto 0);
    signal dout : std_logic_vector(DW-1 downto 0);
begin

    -- clock
    clk <= not clk after 5 ns;

    -- stimuli
    process
    begin
        report "TEST: start";
        -- idle cycle
        wait until rising_edge(clk);
        -- memory read sequence
        ce   <= '1';
        we   <= '0';
        wait until rising_edge(clk);
        for i in 0 to 2**AW-1 loop
            addr <= std_logic_vector(to_unsigned(i, AW));
            wait until rising_edge(clk);
        end loop;
        we   <= 'U';
        -- idle cycle
        ce   <= '0';
        wait until rising_edge(clk);
        -- memory write sequence
        ce   <= '1';
        we   <= '1';
        for i in 0 to 2**AW-1 loop
            addr <= std_logic_vector(to_unsigned(i, AW));
            din  <= std_logic_vector(to_unsigned(i, DW));
            wait until rising_edge(clk);
        end loop;
        we   <= 'U';
        din  <= (others => 'U');
        -- idle cycle
        ce   <= '0';
        wait until rising_edge(clk);
        -- memory read sequence
        ce   <= '1';
        we   <= '0';
        wait until rising_edge(clk);
        for i in 0 to 2**AW-1 loop
            addr <= std_logic_vector(to_unsigned(i, AW));
            wait until rising_edge(clk);
        end loop;
        we   <= 'U';
        -- idle cycle
        ce   <= '0';
        wait until rising_edge(clk);
        report "TEST: end";
        finish;
    end process;

    ram : entity work.rams_init_file(rtl)
    generic map (
        AW => AW,
        DW => DW
    )
    port map (
        clk  => clk ,
        ce   => ce  ,
        we   => we  ,
        addr => addr,
        din  => din ,
        dout => dout
    );

end behav;
