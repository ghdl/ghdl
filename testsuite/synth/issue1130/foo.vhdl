library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity foo is
  port ( encoded : in integer);
end;

architecture foo of foo is
  type some_type is (foo, bar, baz);

  function decode( constant v : integer ) return some_type is
  begin
    return some_type'val(v);
  end;

  function decode( constant v : string ) return some_type is
  begin
    return some_type'value(v);
  end;

  signal decoded_from_slv : some_type;
  signal decoded_from_string : some_type;

begin

  decoded_from_slv <= decode(encoded);
  decoded_from_string <= decode(string'("foo"));

end;
