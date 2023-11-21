context fifo_proj is
  library ieee;
  context ieee.ieee_std_context;
  use ieee.numeric_std_unsigned.all;
end context;

-- types
library ieee;
context ieee.ieee_std_context;

package fifo_types is
  subtype logic is std_ulogic;
  subtype logic_vec is std_ulogic_vector;
end package;context work.fifo_proj;
use work.fifo_types.all;

-- fifo
entity fifo is
  generic (constant DEPTH_W : positive := 8);
  port (ireset, irclk, rd_en, iwclk, wr_en : in logic);
end entity;

architecture rtl of fifo is
  signal wr_ptr, rd_ptr : logic_vec (DEPTH_W-1 downto 0);
begin

  write_proc : process (iwclk, ireset) is
  begin
    if ireset then
      wr_ptr <= (others => '0');
    elsif rising_edge(iwclk) then
      if wr_en then
        wr_ptr <= wr_ptr + 1;
      end if;
    end if;
  end process;

  read_proc : process (irclk, ireset) is
  begin
    if ireset then
      rd_ptr  <= (others => '0');
    elsif rising_edge(irclk) then
      if rd_en then
        rd_ptr <= rd_ptr + 1;
      end if;
    end if;
  end process;

end architecture;context work.fifo_proj;

-- testbench
use work.fifo_types.all;
use std.textio.all;

entity tb is
  generic (
    constant T_wclk  : time := 5.0 ns;
    constant T_rclk  : time := 13.1 ns;
    constant DEPTH_W : positive := 8);
end entity;

architecture mixed of tb is
  signal rst, wclk, rclk, wr_req, rd_req : logic := '0';
begin

rst <= '0', '1' after 15 ns, '0' after 36 ns;

wr_clock_gen: process is
begin
  wclk <= '0';
  wait for 0.5*T_wclk;
  wclk <= '1';
  wait for 0.5*T_wclk;
end process;

rd_clock_gen: process is
begin
  rclk <= '0';
  wait for 0.5*T_rclk;
  rclk <= '1';
  wait for 0.5*T_rclk;
end process;

wr_req <= '1';
rd_req <= '1';

FIFO_inst: entity work.fifo(rtl)
  generic map (DEPTH_W => DEPTH_W)
  port map (
    ireset => rst,
    iwclk => wclk, wr_en => wr_req,
    irclk => rclk, rd_en => rd_req
  );

log: postponed process (wclk, rclk) is
  variable l: line;
begin
  swrite(l, "wclk: " & to_string(wclk) & ", rclk: " & to_string(rclk) &
            ", wptr: " & to_string(<< signal .tb.FIFO_inst.wr_ptr : logic_vec (DEPTH_W-1 downto 0) >>) &
            ", rptr: " & to_string(<< signal .tb.FIFO_inst.rd_ptr : logic_vec (DEPTH_W-1 downto 0) >>)
  );
  writeline(output, l);
end process;

end architecture;

