library ieee;
use ieee.std_logic_1164.all;

--  The design of top.vhdl with the slice written ascending, which is the
--  direction LRM08 5.3.2.2 e) 2) gives the formal.  This is the shape that
--  is legal, and the slice of a 'downto' actual by an ascending formal is
--  fine: an association only has to match on length.
entity handle_ok is
    port (data_i : in std_ulogic_vector);
end;

architecture arch of handle_ok is
begin
end;

library ieee;
use ieee.std_logic_1164.all;

entity top_ok is
    port (count_i : in std_ulogic_vector(5 downto 0));
end;

architecture arch of top_ok is
begin
    handle : entity work.handle_ok port map (
        data_i(0 to 5) => count_i
    );
end;
