library ieee;
use ieee.std_logic_1164.all;

entity tb_afed is
end entity;

architecture behaviour of tb_afed is

  signal sig : std_logic := '1';
  signal ack : std_logic := '1';
  signal fe  : std_logic;
begin

  monitor : process (fe)
  begin
    report std_logic'image(fe);
  end process;

  simu : process
  begin
    wait for 1 ns;
    ack <= '0'; wait for 1 ns;
    sig <= '1'; wait for 1 ns;
    sig <= '0'; wait for 1 ns;
    sig <= '1'; wait for 1 ns;
    ack <= '1'; wait for 1 ns;
    wait;
  end process;

  afed : entity work.afed
    port map (
      sig => sig,
      ack => ack,
      fe  => fe
      );
  
end architecture;
