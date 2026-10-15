library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.wishbone_pkg.all;

entity top is
port
(
	Clock: in std_logic;
	nReset: in std_logic;

	WbIn: in wb_slave_i;
	WbOut: out wb_slave_o
);
end top;

architecture rtl of top is
    type readout_state_type is
    (
        IDLE,
        PUSH1
    );
    signal ro_state : readout_state_type;
begin
	readout: process(nReset, Clock)
	begin
        if nReset = '0' then
            ro_state <= IDLE;
--			WbOut.dat <= (others => '0'); -- yosys crashes without this
            WbOut.ack <= '0';
            WbOut.stall <= '0';
        elsif rising_edge(Clock) then
            case ro_state is
                when IDLE =>
                    WbOut.dat <= (others => '0');
                    if WbIn.cyc = '1' and WbIn.stb = '1' then
                        if WbIn.we = '0' then
                            if WbIn.adr = "111" then
                                ro_state <= PUSH1;
                            elsif WbIn.adr = "001" then
                                WbOut.dat <= "00000000";
                                WbOut.ack <= '1';
                            end if;
                        end if;
                    end if;
				when others => null;
            end case;
        end if;
	end process;
end architecture;
