package unbound_rec is
    type UnbRecType is record
		writedata:  bit_vector;
    end record;
end package;

use work.unbound_rec.all;
entity eut is
    port (
        form_sig:  out UnbRecType
    );
end entity;

architecture foo of eut is
begin 
end architecture;

use work.unbound_rec.all;
entity eu_tb is
end entity;

architecture fum of eu_tb is
    signal act_sig:     UnbRecType (writedata (7 downto 0));
begin
UUT:
    entity work.eut
        port map (
            form_sig => act_sig
        );
end architecture;
