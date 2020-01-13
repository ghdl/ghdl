library ieee;
use ieee.std_logic_1164.all;

entity uassoc02_sub is
  port (i : std_logic_vector;
        o : out std_logic_vector);
end uassoc02_sub;

architecture behav of uassoc02_sub is
begin
  o <= not i;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity uassoc02 is
  port (i1 : std_logic_vector(3 downto 0);
        i2 : std_logic_vector(7 downto 0);
        o : out std_logic_vector(3 downto 0));
end uassoc02;

architecture rtl of uassoc02 is
  component uassoc02_sub is
    port (i : std_logic_vector;
          o : out std_logic_vector);
  end component;

  signal o1: std_logic_vector(3 downto 0);
  signal o2: std_logic_vector(7 downto 0);
begin
  dut1: uassoc02_sub
    port map (i => i1, o => o1);

  dut2: uassoc02_sub
    port map (i => i2, o => o2);

  o <= o1 xor o2 (3 downto 0);
end rtl;
