library ieee;
use ieee.std_logic_1164.all;

entity top is
port
(
  Clock: in std_logic;
  nReset: in std_logic;

  stb : std_logic;
  --  0: dff
  --  1: adff
  --  2: latch
  res : out std_logic_vector(2 downto 0)
);
end top;

architecture rtl of top is
  signal ro_state : std_logic;
begin
  readout: process(nReset, Clock)
  begin
    if nReset = '0' then
      ro_state <= '0';
      res (1) <= '0';
      res (2) <= '0';
    elsif rising_edge(Clock) then
      case ro_state is
        when '1' =>
          res(0) <= '0';
          if stb = '1' then
            res(1) <= '1';
            ro_state <= '1';
          end if;
        when others => null;
      end case;
      end if;
    end process;
end architecture;
