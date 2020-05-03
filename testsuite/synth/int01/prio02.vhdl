library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity prio02 is
  port (dat : std_logic_vector(15 downto 0);
        prio : out natural);
end;

architecture behav of prio02 is
  function prioritize(b : std_logic_vector(15 downto 0)) return natural
  is
    variable level : integer range 0 to 15;
  begin
    level := 0;
    for i in 15 downto 0 loop
      level := i;
      if b(i) = '1' then exit; end if;
    end loop;
    return level;
  end;
begin
  prio <= prioritize (dat);
end;

