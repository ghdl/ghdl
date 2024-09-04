package mypkg is
    type myenum is ( e1, e2, e3 );
end package;

use work.mypkg.all;

entity test is
    generic  (
        e : myenum
    );
    port (o : out natural);
end entity;

use work.mypkg.all;

architecture test of test is
begin
    g_case : case e generate
        when e1 => o <= 12;
        when e2 =>
        when e3 =>
    end generate;
end architecture;
