library ieee;
use ieee.std_logic_1164.all;

entity ent is
	generic (
		VAL : real := 1.5
	);
	port (
		lt  : out std_logic
	);
end;

architecture a of ent is
   constant fmul : real := val * 5.0;
   constant fneg : real := -val;
   constant fid : real := +val;
   constant fabs : real := abs val;
   constant fexp : real := val ** 2;
begin
	lt  <= '1' when VAL  < 1.5 else '0';
end;

