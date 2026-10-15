library ieee;
use ieee.std_logic_1164.all;

entity top is
port
(
	Clock: in std_logic;
	nReset: in std_logic;

	stb : std_logic;
        --  Result:
        --  7 downto 0: dff without the reset
        --  8: adff
        --  9: latch
        res : out std_logic_vector(9 downto 0)
);
end top;

architecture rtl of top is
    type readout_state_type is
    (
        IDLE,
        PUSH1
    );
    --  Result: adff
    signal ro_state : readout_state_type;
begin
	readout: process(nReset, Clock)
	begin
        if nReset = '0' then
            ro_state <= IDLE;
--			WbOut.dat <= (others => '0'); -- yosys crashes without this
            res (8) <= '0';
            res (9) <= '0';
        elsif rising_edge(Clock) then
            case ro_state is
                when IDLE =>
                    res(7 downto 0) <= (others => '0');
                    if stb = '1' then
                                res(7 downto 0) <= "00000000";
                                res(8) <= '1';
                                ro_state <= PUSH1;
                    end if;
				when others => null;
            end case;
        end if;
	end process;
end architecture;
