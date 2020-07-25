library ieee;
use ieee.std_logic_1164.all;

entity repro is
  port (a, b : std_ulogic;
        o : out std_ulogic);
end;

architecture behav of repro is
  type table_2d is array (std_ulogic, std_ulogic) of std_ulogic;

  constant and_table : table_2d :=
  --  UX01ZWLH-
    ("UU0UUU0UU",   -- U
     "UX0XXX0XX",   -- X
     "000000000",   -- 0
     "UX01XX01X",   -- 1
     "UX0XXX0XX",   -- Z
     "UX0XXX0XX",   -- W
     "000000000",   -- L
     "UX01XX01X",   -- H
     "UX0XXX0XX"    -- -
     );
begin
  o <= and_table(a,b);
end;

