
library ieee;
use ieee.std_logic_1164.all;

entity dff is
    port (
        d, clk: in  std_logic;
        q:      out std_logic
    );
end entity dff;

architecture behave of dff is
begin
    process (clk)
    begin
        if clk = '1' then
            q <= d;
        end if;
    end process;
end architecture behave; 

library ieee;
use ieee.std_logic_1164.all;

entity dff is
end entity dff;

architecture behave of dff is
    component dff is
        port (
            d, clk: in  std_logic;
            q:      out std_logic
        );
    end component;
    signal  d_in: std_logic;
    signal  clk_in: std_logic;
    signal  q_out: std_logic;
begin
d_ff: 
    dff port map ( d_in, clk_in, q_out);
no_label:
    process
    begin
        if clk_in = '1' then
            q_out <= d_in;
        end if;
    end process;
end architecture behave; 
