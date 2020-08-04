library ieee;
use ieee.std_logic_1164.all;

package repro4_pkg is
   type t_av_st is record
      data  : std_ulogic_vector;         -- src -> sink
      valid : std_ulogic;                -- src -> sink
      empty : std_ulogic_vector;         -- src -> sink
   end record t_av_st;
   type t_arr_av_st is array (NATURAL range <>) of t_av_st;
end package repro4_pkg;

library ieee;
use ieee.std_logic_1164.all;
use work.repro4_pkg.all;

entity repro4_sub is
   generic(
      G_SINKS            : positive;
      G_SOURCES          : positive
   );
   port(
      avst_sink          : inout t_arr_av_st(G_SINKS   downto 1);
      avst_source        : inout t_arr_av_st(G_SOURCES downto 1);
      source_sel         : in std_ulogic_vector(G_SOURCES downto 1)
   );
end entity;

architecture rtl of repro4_sub is
--   signal avst_current_sink : t_av_st(data(avst_sink(avst_sink'low).data'range), empty(avst_sink(avst_sink'low).empty'range));
begin

--   sink_sel_gen: for i in sink_sel'range generate
--      sink_sel_seq_proc: process(clk) is begin
--         avst_current_sink.valid <= '0';
--      end process;
--   end generate;
end architecture;


library ieee;
use ieee.std_logic_1164.all;
use work.repro4_pkg.all;

entity repro4 is
end entity;

architecture tb of repro4 is
   constant C_VC_SOURCES : positive := 3;
   constant C_VC_SINKS    : positive := 3;

   subtype vc_sources_range is natural range C_VC_SOURCES downto 1;
   subtype vc_sinks_range is natural range C_VC_SINKS downto 1;

   signal avst_snk : t_arr_av_st(vc_sinks_range)(data(8 downto 0), empty(0 downto 0));
   signal avst_src : t_arr_av_st(vc_sources_range)(data(8 downto 0), empty(0 downto 0));
   signal source_sel : std_ulogic_vector(C_VC_SINKS downto 1);
begin
   dut_inst: entity work.repro4_sub
   generic map (
      G_SINKS => C_VC_SOURCES,
      G_SOURCES => C_VC_SINKS)
   port map (
      avst_sink   => avst_src,
      avst_source => avst_snk,
      source_sel  => source_sel
   );
end architecture tb;
