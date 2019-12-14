library ieee;
use ieee.std_logic_1164.all;

entity mwe_entity is
  port (
        input : in std_logic;
        output : out std_logic
  );
end mwe_entity;

architecture behav of mwe_entity is
begin
	process(input)
	begin
		output <= input;
	end process;
end behav;	
