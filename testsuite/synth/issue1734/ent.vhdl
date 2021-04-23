library ieee;
context ieee.ieee_std_context;

entity fifo is
  generic ( gen : positive := 8 );
end fifo;

architecture arch of fifo is begin end;

library ieee;
context ieee.ieee_std_context;

entity ent is
  generic ( gen : integer := 0 );
end ent;

architecture arch of ent is
begin
  fifo: entity work.fifo generic map ( gen  );
end architecture;
