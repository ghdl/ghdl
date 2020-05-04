library ieee;
use ieee.std_logic_1164.all;

entity issue is
    port (lo_1 : out std_logic_vector (1 downto 0);
          lo_2 : out std_logic_vector (1 downto 0));
end entity issue;

architecture beh of issue is
    function fun (arg : integer) return integer is
    begin
        return arg - 1;
    end function;

    signal hi_c : std_logic_vector (15-1 downto 1);
    signal hi_f : std_logic_vector (fun(15) downto 1);

    signal foobar_c : std_logic_vector (15 downto 0) := (others => '0');
    signal foobar_f : std_logic_vector (fun(16) downto 0) := (others=>'0');
begin
    (hi_c, lo_1) <= foobar_f; -- works
    (hi_f, lo_2) <= foobar_c; -- crashes
end architecture;
