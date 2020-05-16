library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue is
end issue;

architecture beh of issue is
begin
    assert "+"(unsigned'("0001"), unsigned'("0001")) = unsigned'("0010");
    assert "-"(unsigned'("0001"), unsigned'("0001")) = unsigned'("0000");
    assert "="(unsigned'("0001"), unsigned'("0001"));

    assert "+"(signed'("0001"), signed'("0001")) = signed'("0010");
    assert "-"(signed'("0001"), signed'("0001")) = signed'("0000");
    assert "="(signed'("0001"), signed'("0001"));
end architecture beh;
