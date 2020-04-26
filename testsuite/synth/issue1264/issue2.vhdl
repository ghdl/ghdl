library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue2 is
    port (uns_inp : in  unsigned (4-1 downto 0);
          uns_sll : out unsigned (8-1 downto 0);
          uns_srl : out unsigned (8-1 downto 0);
          sgn_inp : in    signed (4-1 downto 0);
          sgn_sll : out   signed (8-1 downto 0);
          sgn_srl : out   signed (8-1 downto 0));
end issue2;

architecture beh of issue2 is
begin
    uns_sll <= resize (uns_inp, sgn_srl'length) sll 1; -- work
    uns_srl <= resize (uns_inp, sgn_srl'length) srl 1; -- work
    sgn_sll <= resize (sgn_inp, sgn_srl'length) sll 1; -- error
    sgn_srl <= resize (sgn_inp, sgn_srl'length) srl 1; -- error
end architecture beh;
