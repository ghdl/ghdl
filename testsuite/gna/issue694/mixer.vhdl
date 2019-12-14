library ieee;
use ieee.std_logic_1164.all;
use work.mixer_pkg.sample_array;

entity mixer is
    generic(
        sample_bits: positive
    );
    port(
        i_samples: in sample_array(0 to 127)(sample_bits-1 downto 0)
    );
end mixer;

architecture behavioural of mixer is
begin
end behavioural;
