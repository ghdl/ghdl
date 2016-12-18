--------------------------------------------------------------------------------
--
-- Package demo with two simple overloaded procedures
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pack is

  procedure inc(signal val:inout std_logic_vector);
  procedure inc(signal val:inout unsigned);
  procedure inc(signal val:inout signed);
  procedure inc(signal val:inout integer);
  procedure inc(variable val:inout unsigned);
  procedure inc(variable val:inout integer);
  procedure dec(signal val:inout std_logic_vector);
  procedure dec(signal val:inout unsigned);
  procedure dec(signal val:inout signed);
  procedure dec(signal val:inout integer);
  procedure dec(variable val:inout unsigned);
  procedure dec(variable val:inout integer);

end pack;

package body pack is

  procedure inc(signal val:inout std_logic_vector) is
  begin
    val<= std_logic_vector(unsigned(val) + 1);
  end;

  procedure inc(signal val:inout signed) is
  begin
    val<= val + 1;
  end;

  procedure inc(signal val:inout unsigned) is
  begin
    val<= val + 1;
  end;

  procedure inc(signal val:inout integer) is
  begin
    val<= val + 1;
  end;

  procedure inc(variable val:inout unsigned) is
  begin
    val := val + 1;
  end;

  procedure inc(variable val:inout integer) is
  begin
    val := val + 1;
  end;

  procedure dec(signal val:inout std_logic_vector) is
  begin
    val<= std_logic_vector(unsigned(val) - 1);
  end;

  procedure dec(signal val:inout unsigned) is
  begin
    val<= val - 1;
  end;

  procedure dec(signal val:inout signed) is
  begin
    val<= val - 1;
  end;

  procedure dec(signal val:inout integer) is
  begin
    val<= val - 1;
  end;

  procedure dec(variable val:inout unsigned) is
  begin
    val := val - 1;
  end;

  procedure dec(variable val:inout integer) is
  begin
    val := val - 1;
  end;

end;

