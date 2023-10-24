library ieee;
use ieee.std_logic_1164.all;

entity compinst1_sub is
  port (d : std_logic;
        q : out std_logic);
end;

architecture behav of compinst1_sub is
begin
  q <= not d;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity compinst1 is
end;

architecture behav of compinst1 is
  component compinst1_sub is
    port (d : std_logic;
          q : out std_logic);
  end component;
  
  signal inp, res : std_logic;
begin
  dut : compinst1_sub
    port map (d => inp, q => res);

  inp <= '0' after 1 ns, '1' after 2 ns;
end behav;
