library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue2 is
end issue2;

architecture beh of issue2 is
begin
    assert (unsigned'("1111") >  unsigned'("0111"));
    assert (unsigned'("1111") >= unsigned'("0111"));
    assert (unsigned'("0111") <  unsigned'("1111"));
    assert (unsigned'("0111") <= unsigned'("1111"));

    assert (signed'("0111") >  signed'("1111"));
    assert (signed'("0111") >= signed'("1111"));
    assert (signed'("1111") <  signed'("0111"));
    assert (signed'("1111") <= signed'("0111"));

    assert signed'("1111") = -1;
end architecture beh;
