--  Test release of stack2 for conditions.
entity repro1 is
end;

library ieee;
use ieee.numeric_std.all;

architecture behav of repro1 is
  function ispow2a(i : integer) return boolean is
  begin
    if to_integer(to_unsigned(i, 32) and to_unsigned(i - 1, 32)) = 0 then
      return true;
    else
      return false;
    end if;
  end;

  function ispow2b(i : integer) return boolean is
  begin
    return to_integer(to_unsigned(i, 32) and to_unsigned(i - 1, 32)) = 0;
  end;

  function ispow2c(i : integer) return boolean is
  begin
    while to_integer(to_unsigned(i, 32) and to_unsigned(i - 1, 32)) = 0 loop
      return true;
    end loop;
    return false;
  end;

  function ispow2d(i : integer) return boolean is
  begin
    loop
      exit when to_integer(to_unsigned(i, 32) and to_unsigned(i - 1, 32)) = 0;
      return False;
    end loop;
    return True;
  end;

  signal s : boolean;
begin
  assert ispow2a (64);
  assert not ispow2b (31);

  assert ispow2c (64);
  assert not ispow2c (31);

  assert ispow2d (128);
  assert not ispow2d (30);
end behav;
