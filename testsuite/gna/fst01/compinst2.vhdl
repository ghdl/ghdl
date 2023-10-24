library ieee;
use ieee.std_logic_1164.all;

entity compinst2_sub is
  port (d : std_logic;
        q : out std_logic);
end;

architecture behav of compinst2_sub is
begin
  q <= not d;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity compinst2 is
end;

architecture behav of compinst2 is
  signal inp, res : std_logic;
begin
  dut : entity work.compinst2_sub
    port map (d => inp, q => res);

  inp <= '0' after 1 ns, '1' after 2 ns;
end behav;
