library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pack0 is

  procedure inc(signal val:inout std_logic_vector);
  procedure inc(signal val:inout unsigned);
  procedure inc(signal val:inout signed);
  procedure inc(signal val:inout integer);
  procedure dec(signal val:inout std_logic_vector);
  procedure dec(signal val:inout unsigned);
  procedure dec(signal val:inout signed);
  procedure dec(signal val:inout integer);
  
end pack0;

package body pack0 is

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

end;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pack1 is

  procedure inc(variable val:inout unsigned);
  procedure inc(variable val:inout integer);
  procedure dec(variable val:inout unsigned);
  procedure dec(variable val:inout integer);

end pack1;

package body pack1 is

  procedure inc(variable val:inout unsigned) is
  begin
    val := val + 1;
  end;

  procedure inc(variable val:inout integer) is
  begin
    val := val + 1;
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.pack0.all;
use work.pack1.all;

entity overload is
end entity;

architecture foo of overload is
    signal sig: unsigned ( 7 downto 0) := (others => '0');
    signal int: integer range 0 to 255; -- 'LEFT = 0 initial value
begin
    process
        variable isig: unsigned ( 7 downto 0) := (others => '0');
        variable iint: integer range 0 to 255;
    begin
        inc(sig);
        inc(isig);
        inc(int);
        inc(iint);
        wait for 0 ns;
        dec(sig);
        dec(isig);
        dec(int);
        dec(iint);
        wait;
    end process;
    
end architecture;
