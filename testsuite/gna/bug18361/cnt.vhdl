library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity CNT_V is
Generic(clk_divisor: natural);
Port(clk      : in std_logic;
    reset     : in std_logic;
    q_o       : out std_logic);
end CNT_V;

architecture behv of CNT_V is
    --components
    --constants
    --signals
    signal q:     std_logic;
begin
    q_o <= q;
    count: process(clk, reset) is
        --variable
        variable idx: natural range 0 to clk_divisor-1;
        begin
            if reset = '1' then 
                idx:= 0;
                q <= '0';
            elsif rising_edge(clk) then
                if idx = clk_divisor - 1 then
                    q <= '1';
                    idx := 0;
                else
                    q <= '0';
                    idx := idx + 1;
                end if;
            end if;
        end process;
end behv;

-- Testbench:

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cnt_v_tb is
end cnt_v_tb;

architecture TB of cnt_v_tb is

component CNT_V is
Generic(clk_divisor: natural);
Port(clk      : in std_logic;
    reset     : in std_logic;
    q_o       : out std_logic);
end component;

--components
--constants
--signals
signal clk      : std_logic;
signal reset     : std_logic;
signal q_o       : std_logic;

begin

    DUV: cnt_v    
    --generic map(clk_divisor => 10) -- here ist the error 
    port map( clk, reset, q_o);

--stimuli here
--Stimuli for Signal "clk" 40 mhz
process
    begin
        clk <= '1';
        wait for 12.5 ns;
        clk <= '0';
        wait for 12.5 ns;
end process;

process
    begin
    --initialisation
    reset <= '1';
    wait for 20 ns;
    --stimuli
    reset <= '0';
    wait for 22 ns;
    -- do some stuff
    wait;
end process;
end TB;

