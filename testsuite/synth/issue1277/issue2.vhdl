library ieee;
use ieee.std_logic_1164.all;

entity issue2 is
    port (d : std_logic;
          q : out std_logic);
end entity issue2;

architecture beh of issue2 is
  type my_int is range 127 downto 0;
  type my_slv is array(my_int range <>) of std_logic;
  
    function fun (arg : my_int) return my_int is
    begin
        return arg - 1;
    end function;

    signal hi_c : my_slv (15-1 downto 1);
    signal hi_f : my_slv (fun(15) downto 1);

    signal foobar_c : my_slv (15 downto 0) := (others => '0');
    signal foobar_f : my_slv (fun(16) downto 0) := (others=>'0');
  signal loa, lob : my_slv(1 downto 0);
begin
  foobar_f(0) <= d;
    (hi_c, loa) <= foobar_f; -- works
    (hi_f, lob) <= foobar_c; -- crashes
  q <= hi_f(1);
end architecture;
