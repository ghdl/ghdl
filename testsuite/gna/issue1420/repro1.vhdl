entity repro1 is
end entity;

architecture tb of repro1 is

   type t_av_st is record
      data  : bit_vector;         -- src -> sink
      valid : bit;                -- src -> sink
      empty : bit_vector;         -- src -> sink
      chnnl : bit_vector;         -- src -> sink
   end record t_av_st;
   type t_arr_av_st is array (NATURAL range <>) of t_av_st;

   constant C_VC_SOURCES : positive := 3;
   constant C_VC_SINKS    : positive := 3;
   subtype vc_sources_range is natural range C_VC_SOURCES downto 1;
   subtype vc_sinks_range is natural range C_VC_SINKS downto 1;
   signal avst_snk : t_arr_av_st(vc_sinks_range)(data(8 downto 0), empty(0 downto 0), chnnl(0 downto 0));
   signal avst_src : t_arr_av_st(vc_sources_range)(data(8 downto 0), empty(0 downto 0), chnnl(0 downto 0));
   signal sink_sel : bit_vector(C_VC_SOURCES downto 1);
   signal source_sel : bit_vector(C_VC_SINKS downto 1);

begin

end architecture tb;
