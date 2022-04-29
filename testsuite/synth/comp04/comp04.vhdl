library ieee;
use ieee.std_logic_1164.all;

entity mand is
  port (v : std_logic_vector (7 downto 0);
        b : std_logic;
        r : out std_logic_vector (7 downto 0));
end mand;

architecture behav of mand is
begin
  process (v, b)
  begin
    for i in v'range loop
      r(i) <= v (i) and b;
    end loop;
  end process;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity comp04 is
  port (v : std_logic_vector (7 downto 0);
        r : out std_logic_vector (7 downto 0));
end;

architecture behav of comp04 is
  component mand is
    port (
      b : std_logic;
      v : std_logic_vector (7 downto 0);
      r : out std_logic_vector (7 downto 0));
  end component;

begin
  dut : mand
    port map (v => v,
              b => v(0),
              r => r);
end behav;


