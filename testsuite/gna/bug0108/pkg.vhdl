package name1 is
  constant c : natural := 4;
  function get_c return natural;
end name1;

package body name1 is
  function get_c return natural is
  begin
     return c;
  end get_c;
end name1;
