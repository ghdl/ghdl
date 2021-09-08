-- Created on    : 11/08/2021
-- Author        : Fabien Marteau <fabien.marteau@armadeus.com>
-- Copyright (c) ARMadeus systems 2015

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;


Entity rising_pulse_detector is
generic( WAITPRETRIGG_CNT: natural := 1000);
port
(
    clk : in std_logic;
    rst : in std_logic;

    inputvec : in std_logic_vector(15 downto 0);

    output_pulse : out std_logic_vector(15 downto 0)

);
end entity;

Architecture rising_pulse_detector_1 of rising_pulse_detector is

    signal inputvec_old, inputvec_pulse : std_logic_vector(15 downto 0);

begin

    output_pulse <= inputvec_pulse;

    process(clk, rst)
    begin
        if(rst = '1') then
            inputvec_old <= (others => '0');
            inputvec_pulse <= (others => '0');
        elsif(rising_edge(clk)) then
            inputvec_pulse <= (not inputvec_old) and inputvec;
            inputvec_old <= inputvec;
        end if;
    end process;

end architecture rising_pulse_detector_1;
