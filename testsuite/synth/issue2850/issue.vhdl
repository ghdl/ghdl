LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;
USE IEEE.std_logic_misc.ALL;

ENTITY issue IS
    PORT(
        clk                               :   IN    std_logic;
        reset                             :   IN    std_logic;
        state                             :   IN    std_logic;    				
        in1                               :   IN    std_logic;     
        out1                         	  :   OUT   std_logic          
    );
END issue;

ARCHITECTURE rtl OF issue IS
begin
    out1_pc: process (all)
    begin 
        if (reset='1') then 
            out1 <= state;	-- not working as expected with Synth
        elsif (clk'EVENT AND clk = '1') THEN
            out1 <= in1;
        end if;
    end process;
END rtl;

