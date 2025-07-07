library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

entity ent is
end entity;

architecture a of ent is
    constant NO_PORTS_C    : integer := 8;
    constant NO_MUXPORTS_C : integer := 2;
    constant NEEDS_POW : integer :=
        integer(
            ceil(
                real(NO_PORTS_C) /                -- 8.0
                ( NO_MUXPORTS_C ** real(1) )      -- 2 ** 1.0  <-- crash here
            )
        );
begin
    assert NEEDS_POW > 0
      report "NEEDS_POW = " & integer'image(NEEDS_POW);
end;

