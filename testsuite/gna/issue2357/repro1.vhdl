library ieee;
use ieee.std_logic_1164.all;

entity repro1_1 is
  port (o : out std_logic;
        i1, i2 : std_logic);
end;

architecture behav of repro1_1 is
begin
  o <= i1;
  o <= i2;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity repro1_2 is
  port (o : out std_logic;
        i1, i2 : std_logic);
end;

architecture behav of repro1_2 is
begin
  dut : entity work.repro1_1
    port map (o => o, i1 => i1, i2 => i2);
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity repro1 is
end;

architecture behav of repro1 is
  signal o : std_logic;
  signal s1, s2 : std_logic;
begin
  dut: entity work.repro1_2 port map (o => o, i1 => s1, i2 => s2);

  process
  begin
    s1 <= 'Z';
    s2 <= '1';
    wait for 1 ns;
    assert o = '1' severity failure;

    wait;
  end process;
end behav;

