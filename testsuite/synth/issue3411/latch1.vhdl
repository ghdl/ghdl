library ieee;
use ieee.std_logic_1164.all;

entity top is
port
(
	Clock: in std_logic;
	nReset: in std_logic;

	stb : std_logic;
        res : out std_logic_vector(8 downto 0)
);
end top;

architecture rtl of top is
begin
	readout: process(nReset, Clock)
	begin
        if nReset = '0' then
            res (8) <= '0';
        elsif rising_edge(Clock) then
          if stb = '1' then
            res(7 downto 0) <= "00000000";
          else
            res(7 downto 0) <= "00000001";
          end if;
        end if;
	end process;
end architecture;
