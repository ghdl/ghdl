library ieee;
use ieee.std_logic_1164.all;

entity ent is
  port (
    rst : std_logic;
		clk : in std_logic;
		counter : out natural
	);
end entity;

architecture a of ent is
	procedure incr(signal i : inout natural) is
	begin
		i <= i + 1;
	end procedure;
begin
	process(clk)
	begin
          if rising_edge(clk) then
            if rst = '1' then
              counter <= 0;
            else
              -- works:
              --counter <= counter + 1;
              incr(counter);
            end if;
          end if;
	end process;
end;
