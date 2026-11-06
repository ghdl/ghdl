library ieee;
use ieee.std_logic_1164.all;

entity v27 is
  port (clk_i, reset_n_i, d_i : in std_ulogic;
        q_o : out std_ulogic);
end entity;

architecture rtl of v27 is
  type rec_t is record
    valid  : std_ulogic;
    enable : std_ulogic;
  end record;
  signal r, nxr : rec_t := (valid => '0', enable => '1');
begin

  process (clk_i, reset_n_i) is
  begin
    if reset_n_i = '0' then
      r.valid <= '0';
    elsif rising_edge(clk_i) then
      r <= nxr;
    end if;
  end process;

  process (r, d_i) is
  begin
    nxr <= r;
    nxr.valid <= d_i;
  end process;

  q_o <= r.valid;

end architecture;

