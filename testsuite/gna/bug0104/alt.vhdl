library ieee;
use ieee.std_logic_1164.all;

entity delay is
    port (
        clk : in std_logic;
        reset: in std_logic;
        start: in std_logic;
        done: out std_logic
    );
end entity delay;

architecture fast of delay is   -- The reader is unenlightened as to fast/slow
begin
end architecture fast;

architecture slow of delay is
begin
end architecture slow;

library ieee;
use ieee.std_logic_1164.all;

entity dut is
    generic (
         SPEED : string := "fast"
     );
    port(
        clk : in std_logic;
        reset: in std_logic;
        start: in std_logic;
        done:  out std_logic);
    end entity dut;

architecture dutarch of dut is

    -- component delay is   -- component declaration not needed or used here.
    --     port (
    --         clk : in std_logic;
    --         reset: in std_logic;
    --         start: in std_logic;
    --         done: out std_logic
    --     );
    -- end component delay;
    begin
d1g: 
    if SPEED = "fast" generate
d1: -- The alternative labels, if any, within an if generate statement or a 
    -- case generate statement shall all be distinct. 11.8 Generate statements
        entity work.delay(fast)
           port map (
                clk     => clk,
                reset   => reset,
                start   => start,
                done => done
            );
    else generate
d1: -- This isn't a distinct label in the else alternative
            entity work.delay(slow)
            port map (
                clk     => clk,
                reset   => reset,
                start   => start,
                done => done
            );
    end generate;
end architecture dutarch;
