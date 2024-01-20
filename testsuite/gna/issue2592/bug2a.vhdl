library ieee;
use ieee.numeric_std.all;

package foo_pkg is
  constant WID : positive := 8;
  subtype data_type is signed (WID-1 downto 0);
  type data_vector is array (natural range <>) of data_type;
  function even_elem(x : data_vector) return data_vector;
  function odd_elem(x : data_vector) return data_vector;
end package;

package body foo_pkg is
  function even_elem(x : data_vector) return data_vector is
    variable res : data_vector (0 to x'length/2-1);
  begin
    for i in res'range loop
      res(i) := x(2*i);
    end loop;
    return res;
  end function;
  function odd_elem(x : data_vector) return data_vector is
    variable res : data_vector (0 to x'length/2-1);
  begin
    for i in res'range loop
      res(i) := x(2*i+1);
    end loop;
    return res;
  end function;
end package body;

library ieee;
use ieee.numeric_std.all;
use work.foo_pkg.all;

entity foo is
  generic (
    DIM : positive := 4;
    p : data_vector (0 to DIM/2-1));
  port (
    x : in  data_vector (0 to DIM-1);
    y : out data_vector (0 to DIM-1));
end entity;

architecture rec of foo is
  subtype half_array is data_vector (0 to DIM/2-1);
  signal even, odd : half_array := (others => to_signed(0, WID));
begin
  stage :
  if gen_case : DIM > 1 generate
    foo_even :
      entity foo
        generic map (DIM => DIM/2, p => even_elem(p))
        port map (x => even_elem(x), y => even);
    foo_odd :
      entity foo
        generic map (DIM => DIM/2, p => even_elem(p))
        port map (x => odd_elem(x), y => odd);
    arithm :
    process (all) is
    begin
      for i in half_array'range loop
        y(i)       <= resize (even(i)*p(i), WID) + odd(i);
        y(i+DIM/2) <= resize (even(i)*p(i), WID) - odd(i);
      end loop;
    end process;
  else base_case : generate
    y <= x;
  end generate;
end architecture;

library ieee;
use ieee.numeric_std.all;
use std.textio.all;
use work.foo_pkg.all;

entity tb is
end entity;

architecture test of tb is
  constant DIM : positive := 4;
  signal x, y : data_vector(0 to DIM-1);

  function coeff(n : positive) return data_vector is
    variable res : data_vector (0 to n-1);
  begin
    for i in res'range loop
      res(i) := to_signed(i, WID);
    end loop;
    return res;
  end function;

begin
  x <= (others => to_signed(1, WID));
  foo_inst :
    entity work.foo(rec)
      generic map (DIM => DIM, p => coeff(DIM/2))
      port map (x => x, y => y);
  log :
  postponed process (all) is
    variable lin : line;
    alias s is to_string [integer return string];
    alias s is to_string [data_type return string];
  begin
    for i in y'range loop
      write(lin, "y(" & s(i) & ") = " & s(y(i)));
      writeline(output, lin);
    end loop;
  end process;
end architecture;
