package assoc_pkg is
  type rec is record
    a : bit;
  end record;

  view vw of rec is
    a : in;
  end view;

  procedure p1 (x : out bit);  --  Comment for p1

  procedure p2 (
    --  Comment for y
    y : out bit
  );
end package;

use work.assoc_pkg.all;

entity assoc is
  port (
    --  Comment for v
    v : view vw;
    q : out bit  --  Comment for q
  );
end entity;

--  Comment for the architecture
architecture arch of assoc is
  signal s, t : bit;  --  Comment for s and t
  signal u : bit;
begin
end architecture;
