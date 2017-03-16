package pkg is
  function f return natural;
end pkg;

package body pkg is
  constant c : natural := 5;

  function f return natural is
  begin
    report "read " & c'path_name severity note;
    return c;
  end;
end pkg;

entity repro is
end;

architecture behav of repro is
begin
  process
  begin
    assert work.pkg.f >= 2;
    wait;
  end process;
end behav;

