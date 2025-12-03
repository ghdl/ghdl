library ieee;
use ieee.std_logic_1164.all;
library gaisler;
use gaisler.pkg.all;

entity Design_Entity is
    port (
        do : in  std_logic;
        -- do : in  std_logic := '0';
        b : out std_logic
    );
end entity Design_Entity;

architecture structural of Design_Entity is
    signal ahb_partial : ahb_partial_vec_t;
begin

process (all)
    variable res : std_logic := '0';
begin
    for s in 0 to ahb_partial_vec_t'high loop
        -- Some dummy operations
        for i in 0 to ahb_partial(s).hwdata'high loop
            ahb_partial(s).hwdata(i) <= do;
            b <= b xor ahb_partial(s).hwdata(i);
        end loop;

    ahb_partial(s).hready <= '1';
    end loop;

end process;

end architecture structural;
