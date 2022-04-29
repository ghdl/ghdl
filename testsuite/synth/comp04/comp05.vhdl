library ieee;
use ieee.std_logic_1164.all;

entity mand is
  port (v : std_logic_vector (7 downto 0);
        b : out std_logic;
        r : out std_logic_vector (7 downto 0));
end mand;

architecture behav of mand is
begin
  r <= not v;
  
  process (v)
  begin
    b <= '1';
    for i in v'range loop
      if v (i) = '0' then
        b <= '0';
        exit;
      end if;
    end loop;
  end process;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity comp05 is
  port (v : std_logic_vector (7 downto 0);
        r : out std_logic_vector (7 downto 0));
end;

architecture behav of comp05 is
  component mand is
    port (
      r : out std_logic_vector (7 downto 0);
      b : out std_logic;  
      v : std_logic_vector (7 downto 0));
  end component;

  signal b : std_logic;
begin
  dut : mand
    port map (v => v,
              b => b,
              r => r);
end behav;


