library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cmp03 is
end;

architecture behav of cmp03 is
begin
  process
    variable c : character;
    variable s : string (1 to 2);
  begin
    c := 'i';
    assert c >= 'a' severity failure;
    assert c <= 'z' severity failure;
    assert c = 'i' severity failure;

    s := "ab";
    assert s >= "aa" severity failure;
    assert s <= "ac" severity failure;
    assert s = "ab" severity failure;

    assert s >= "a" severity failure;
    assert "a" <= s severity failure;
    wait;
  end process;
end behav;
