package repro1_pkg is
  signal s : bit;
  constant cst : natural := 5;
end;

use work.repro1_pkg.all;

entity repro1 is
  port (a,b : bit;
        c : out bit);
end repro1;

architecture behav of repro1 is
begin
  c <= a xor b;
end behav;
