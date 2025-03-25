library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.fixed_pkg.all;
use ieee.fixed_float_types.all;

entity j_csr is
port (
    csr_add_noise_sig_gain          :out  ufixed(7 downto -10)
);
end entity;

architecture rtl of j_csr is

    constant zeb_add_noise_sig_gain_17_0         :ufixed(7 downto -10)           := to_ufixed(1515520, 7, -10);

begin

end architecture;
