library ieee;
use ieee.std_logic_1164.all;

entity test is
end entity;

architecture a of test is

  constant num_ports : positive := 4;
  signal clock : std_logic := '0';

begin

  dut_inst : entity work.dut
    generic map (
      num_ports => num_ports
    )
    port map (
      clocks => (others => clock)
    );

end architecture;
