-------------------------------------------------------------------------------
-- HEIG-VD, Haute Ecole d'Ingenierie et de Gestion du canton de Vaud
-- Institut REDS, Reconfigurable & Embedded Digital Systems
--
-- File         : sequencer.vhd
--
-- Description  : Simple sequencer to generate waveforms for a 1 bit
--                std_logic signals.
--                Inspired by 
--                https://github.com/tmeissner/psl_with_ghdl/tree/master/src/sequencer.vhd
--
-- Author       : Y. Thoma
-- Date         : 20.06.2025
-- Version      : 1.0
--
--| Modifications |------------------------------------------------------------
-- Version   Author      Date               Description
-- 1.0       YTA         see header         First version.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity sequencer is
    generic (
        seq : string
    );
    port (
        clk  : in  std_logic;
        data : out std_logic
    );
end entity sequencer;


architecture rtl of sequencer is


    function to_bit (a : in character) return std_logic is
        variable ret : std_logic;
    begin
        case a is
            when '0' | '_' => ret := '0';
            when '1' | '-' => ret := '1';
            when others    => ret := 'X';
        end case;
        return ret;
    end function to_bit;

    function to_bit(s: string) return std_logic_vector is
        variable result : std_logic_vector(s'length-1 downto 0);
    begin
        for i in result'range loop
            -- Indices of strings are 1 to n, not 0 to n-1
            result(i) := to_bit(s(i+1));
        end loop;
        return result;
    end function;


    -- We build the reset value statically to really infer a register
    constant reg_c : std_logic_vector(seq'length-1 downto 0) := to_bit(seq);
    signal reg : std_logic_vector(seq'length-1 downto 0) := reg_c;

begin


    process (clk) is
    begin
        if rising_edge(clk) then
            reg <= reg(reg'high) & reg(reg'high downto 1);
        end if;
    end process;

    data <= reg(0);

end architecture rtl;
