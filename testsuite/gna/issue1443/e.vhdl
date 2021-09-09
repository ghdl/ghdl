library ieee;
use ieee.std_logic_1164.all;

entity e is
    port (p1 : out std_logic_vector((3+1)+1 downto 0);
          p2 : out std_logic_vector((3-1)+1 downto 0);
          p3 : out std_logic_vector((3  )+1 downto 0));
end e;

architecture rtl of e is
    function f (a : integer) return integer is
    begin
        return a;
    end function;

    signal s1 : std_logic_vector(3+1 downto 0) := (others=>'0');
    signal s2 : std_logic_vector(3-1 downto 0) := (others=>'0');
    signal s3 : std_logic_vector(f(3) downto 0) := (others=>'0');
begin
    p1 <= ('0',s1);
    p2 <= ('0',s2);
    p3 <= ('0',s3);
end architecture;
