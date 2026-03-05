library ieee;
use ieee.std_logic_1164.all;

entity repro1 is
  port (v : std_logic_vector (7 downto 0);
        r : out std_logic_vector (7 downto 0));
end;

architecture behav of repro1 is
  component repro1_cmp is
    generic (
      g0 : integer range 0 to 0 := 0);
    port (
      b : std_logic;
      v : std_logic_vector (7 downto 0);
      r : out std_logic_vector (7 downto 0));
  end component;

begin
  dut : repro1_cmp
    port map (v => v,
              b => v(0),
              r => r);
end behav;


