--library ieee;
--use ieee.std_logic_1164.all;

package dosomething is

  type dosomething_t is record
                       dummy1 : integer;
                       dummy2 : integer;
                       dummy3 : integer;
                     end record;

  procedure dosomething_hello (
    variable r : inout dosomething_t);

end dosomething;
