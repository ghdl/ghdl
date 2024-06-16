library ieee;
use ieee.std_logic_1164.all;

package test is
  function gi (vector:std_logic_vector) return integer;
end package;

package body test is
  function gi (vector:std_logic_vector) return integer is
    subtype ur is natural range vector'length-1 downto vector'length/2;
  begin
    if vector(ur)/=(ur => '0') then
    --if vector(vector'length-1 downto vector'length/2)/=(vector'length-1 downto vector'length/2 => '0') then
      if vector'length/=2 then
        return gi(vector(ur));
      else
        return 1;
      end if;
    end if;
  end function;
end package body;

entity crash is
end crash;
library work;
use work.test.all;
architecture behaviour of crash is
begin
end behaviour;
