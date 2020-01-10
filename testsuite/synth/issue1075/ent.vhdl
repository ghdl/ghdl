library ieee;
use ieee.math_real.all;

entity ent is
end ent;

architecture a of ent is
constant DELAY_NSEC : real := -1.0;
constant DELAY_TAPS_INT : integer :=
    integer(round(DELAY_NSEC / 0.078125));
begin
end a;
