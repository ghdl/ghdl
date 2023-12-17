library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.ALL;

entity DELAY is
generic(
    type DELAY_TYPE;
    DELAY_DEPTH : natural := 1);
port(
    clk         : in  std_logic;
    ce          : in  std_logic;
    depth       : in  natural range 1 to DELAY_DEPTH;
    d           : in  DELAY_TYPE;
    q           : out DELAY_TYPE);
end entity;

architecture BEHAVIORAL of DELAY is
    type store is array(DELAY_DEPTH downto 0) of DELAY_TYPE;
    signal regs : store;
begin
    q <= d when depth <= 0 else
         regs(depth - 1);

    process(clk)
    begin
        if rising_edge(clk) then
            if ce = '1' then
                regs(0) <= d;
                for i in 0 to DELAY_DEPTH - 1
                loop
                    regs(i + 1) <= regs(i);
                end loop;
            end if;
        end if;
    end process;
end architecture;
