library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;

entity test is
    port (
        i : in  std_logic_vector( 127 downto 0 );
        o : out std_logic_vector( 6 downto 0 )
    );
    function onehot2int( v : std_logic_vector; efflow : integer := 0 ) return integer is
        variable mid : integer := v'length/2;
        variable upper : std_logic_vector( v'length - mid - 1 downto 0 ) := v( v'length - 1 downto mid );
        variable lower : std_logic_vector( mid - 1 downto 0 ) := v( mid - 1 downto 0 );
    begin
        if v'length = 1 then
            return efflow;
        else
            if or_reduce(upper) = '1' then
                return onehot2int(upper,efflow+mid);
            else
                return onehot2int(lower,efflow);
            end if;
        end if;
    end function;
end entity;

architecture test of test is
begin
    o <= std_logic_vector( to_unsigned ( onehot2int(i), o'length ) );
end architecture;
