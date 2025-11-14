library ieee;
use ieee.std_logic_1164.all;

package reprod_pkg is
  type rec_out is record
    data : std_logic_vector(3 downto 0);
  end record;
end package;

library ieee;
use ieee.std_logic_1164.all;

use work.reprod_pkg.all;

entity reprod is
  port (
    clk   : in std_ulogic;
    d_i   : in std_logic_vector(3 downto 0);
    d_o   : out rec_out;
    mode  : in std_ulogic
    );
end;

architecture rtl of reprod is
  signal data0_h : std_logic_vector(3 downto 0);
  signal d_i_data0_h : std_logic_vector(3 downto 0);
begin

  mux_input : process(d_i, mode)
  begin
    if mode = '1' then
      d_i_data0_h <= d_i;
    end if;

    if mode = '0' then
      d_i_data0_h <= not d_i;
    end if;

  end process;

  reg: process (clk)
  begin
    if rising_edge(clk) then
      data0_h <= d_i_data0_h;
    end if;
  end process;

  mux_output : process(data0_h, mode)
  begin
    if mode = '1' then
      d_o.data <= not data0_h;
    end if;

    if mode = '0' then
      d_o.data <= data0_h;
    end if;
  end process;

end architecture;

