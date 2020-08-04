library ieee;
use ieee.std_logic_1164.all;
package repro3_pkg is
   type t_av_st is record
      data  : std_ulogic_vector;         -- src -> sink
      valid : std_ulogic;                -- src -> sink
      empty : std_ulogic_vector;         -- src -> sink
      chnnl : std_ulogic_vector;         -- src -> sink
   end record t_av_st;
   type t_arr_av_st is array (NATURAL range <>) of t_av_st;
end package repro3_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.numeric_std_unsigned.all;
use work.repro3_pkg.all;

entity repro3_sub is
   generic(
      G_SINKS            : positive;
      G_SOURCES          : positive
   );
   port(
      clk                : in    std_ulogic;
      -- Avst Sink
      avst_sink          : inout t_arr_av_st(G_SINKS   downto 1);
      avst_source        : inout t_arr_av_st(G_SOURCES downto 1);
      sink_sel           : in std_ulogic_vector(G_SINKS   downto 1);
      source_sel         : in std_ulogic_vector(G_SOURCES downto 1)
   );
end entity;

architecture rtl of repro3_sub is
   signal avst_current_sink : t_av_st(data(avst_sink(avst_sink'low).data'range), empty(avst_sink(avst_sink'low).empty'range), chnnl(avst_sink(avst_sink'low).chnnl'range));
begin

--   sink_sel_gen: for i in sink_sel'range generate
--      sink_sel_seq_proc: process(clk) is begin
--         avst_current_sink.valid <= '0';
--      end process;
--   end generate;
end architecture;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.numeric_std_unsigned.all;

use work.repro3_pkg.all;

entity repro3 is
end entity;

architecture tb of repro3 is

   constant clk_period : time := 10 ns;
   signal clk               : std_logic := '0';

   constant C_VC_SOURCES : positive := 3;
   constant C_VC_SINKS    : positive := 3;
   subtype vc_sources_range is natural range C_VC_SOURCES downto 1;
   subtype vc_sinks_range is natural range C_VC_SINKS downto 1;
   signal avst_snk : t_arr_av_st(vc_sinks_range)(data(8 downto 0), empty(0 downto 0), chnnl(0 downto 0));
   signal avst_src : t_arr_av_st(vc_sources_range)(data(8 downto 0), empty(0 downto 0), chnnl(0 downto 0));
   signal sink_sel : std_ulogic_vector(C_VC_SOURCES downto 1);
   signal source_sel : std_ulogic_vector(C_VC_SINKS downto 1);

begin

   main : process
   begin
      wait until rising_edge(clk);
   end process main;

   dut_inst: entity work.repro3_sub
   generic map (
      G_SINKS => C_VC_SOURCES,
      G_SOURCES => C_VC_SINKS)
   port map (
      clk   => clk,
      avst_sink   => avst_src,
      avst_source => avst_snk,
      sink_sel    => sink_sel,
      source_sel  => source_sel
   );

   clk <= not clk after clk_period / 2;

end architecture tb;
