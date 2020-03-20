library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug2 is
	generic(
		W    : positive := 4;
		N    : positive := 4
	);
	port(
		clk     :  in std_ulogic;
		reset_n :  in std_ulogic;
                o : out std_ulogic
	);
end bug2;

architecture behav of bug2 is
	type queue_info_t is record
		dummy : integer range 0 to W-1;
		strb  : std_ulogic_vector(W-1 downto 0);
	end record;

	type queues_t is array (0 to N-1) of queue_info_t;
	signal queues : queues_t;
begin

	process(clk, reset_n)
		variable index : integer range 0 to N-1;
	begin
          if reset_n = '0' then
            index := 0;
          elsif rising_edge(clk) then
            for i in 0 to W-1 loop
              queues(index).strb(i) <= '0';
            end loop;
            index := (index + 1) mod N;
          end if;
	end process;

        o <=  queues (0).strb (0);
end architecture;
