library ieee;
use ieee.std_logic_1164.all;

entity ent is
  port (
   clk : std_logic;
   req : std_logic;
   val : std_logic;
   ack : out std_logic);
end ent;

architecture behav of ent is
  signal cnt : natural range 0 to 5;
begin
  process (clk)
  begin
    if rising_edge(clk) then
      if cnt < 5 then
        cnt <= cnt + 1;
      else
        cnt <= 0;
      end if;
      if req = '1' and cnt = 0 then
        ack <= '1';
      else
        ack <= '0';
      end if;
    end if;
  end process;
end behav;

