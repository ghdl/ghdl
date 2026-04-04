library ieee;
use ieee.std_logic_1164.all;

entity foo is port (
  a : inout  std_logic
);
end entity;

architecture beh0 of foo is
begin
end;

library ieee;
use ieee.std_logic_1164.all;

entity tb is port (
	a : inout std_logic
);
end entity;

architecture beh of tb is
begin
   foo_inst : entity work.foo port map (a => a);
end;
