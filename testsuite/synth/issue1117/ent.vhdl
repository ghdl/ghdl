library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity ent is
	generic (
		G : unsigned(31 downto 0)
	);
        port (
          res : out unsigned (31 downto 0));
end;

architecture a of ent is
begin
  res <= g;
end;
