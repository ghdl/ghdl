library ieee;
use ieee.std_logic_1164.all;

use work.reprod_pkg.all;

entity reprod is
  generic (
    arch   : integer;
    works  : integer := 0
    );
  port (
    clk   : in std_ulogic;
    d_i   : in itracebuf_in_type5;
    d_o   : out itracebuf_out_type5;
    mode  : in std_ulogic
    );
end;

architecture rtl of reprod is

  constant arch0  : integer := 0;
  constant arch1  : integer := 1;
  constant arch2  : integer := 2;
  constant addrbits : integer := 11;
  signal data0_h, data0_l : std_logic_vector(255 downto 0);
  signal d_i_data0_h, d_i_data0_l : std_logic_vector(255 downto 0);
begin

  mux_input : process(d_i, mode)
  begin
    if mode = '1' then
      d_i_data0_h <= d_i.data0;
      d_i_data0_l <= d_i.data0;
    end if;

    if mode = '0' then
      d_i_data0_h <= d_i.data0(255 downto 64) & d_i.data0(255 downto 192);
      d_i_data0_l <= d_i.data0;
    end if;

  end process;

  reg: process (clk, d_i_data0_h, d_i_data0_l)
    variable r_d_i_data0h, r_d_i_data0l  : std_logic_vector(255 downto 0);
  begin
    if rising_edge(clk) then
      r_d_i_data0h := d_i_data0_h;
      r_d_i_data0l := d_i_data0_l;
    end if;

    data0_h <= r_d_i_data0h;
    data0_l <= r_d_i_data0l;
  end process;

  mux_output : process(d_i.addr1, d_i.addr0, data0_h, data0_l, mode)
  begin
    if works = 1 then
      d_o.data <= (others => '0');
    end if;
    if arch = arch0 or (arch = arch2 and mode = '1') then
      if d_i.addr0(addrbits-1) = '1' then
        d_o.data(191 downto 0) <= data0_h(191 downto 0);
      else
        d_o.data(191 downto 0) <= data0_l(191 downto 0);
      end if;
      if works = 0 then
        d_o.data(511 downto 384) <= (others => '0');
      end if;
    end if;

    if arch = arch1 or (arch = arch2 and mode = '0') then
      d_o.data <= (data0_h(63 downto 0) & data0_l(191 downto 0)) & (data0_h(63 downto 0) & data0_l(191 downto 0));
    end if;
  end process;

end architecture;

