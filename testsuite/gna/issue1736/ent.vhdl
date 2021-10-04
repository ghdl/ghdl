library ieee;
context ieee.ieee_std_context;

library mylib;
use mylib.fifo;

entity ent is
  generic ( gen : integer := 0 );
end ent;

architecture arch of ent is
begin
  fifo: entity mylib.fifo generic map ( gen  );
end architecture;
