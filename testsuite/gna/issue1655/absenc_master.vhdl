library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;


entity master is

generic
(
 CLK_FREQ: integer;
 ENABLE_ENDAT: boolean;
 ENABLE_BISS: boolean;
 ENABLE_SSI: boolean
);
port
(
 -- local clock
 clk: in std_logic;
 rst: in std_logic;

 -- generated clock for slave
 -- fdiv divides CLK_FREQ
 ma_fdiv: in unsigned;
 ma_clk: out std_logic;

 -- master out, slave in
 mosi: out std_logic;
 miso: in std_logic;

 -- gate to drive output (1 to drive it)
 gate: out std_logic;

 -- data from slave, and data length
 data: out std_logic_vector;
 len: in unsigned;

 -- encoder type
 enc_type: in integer;

 -- ssi specific control registers
 ssi_flags: in std_logic_vector;
 ssi_delay_fdiv: in unsigned
);

end entity;


architecture absenc_master_rtl of master is

constant DATA_LEN: integer := data'length;

--
-- conversion pipeline

type conv_state_t is
(
 CONV_WAIT_LATCH,
 CONV_LSB_TO_MSB,
 CONV_GRAY_TO_BIN,
 CONV_EXTEND_SIGN,
 CONV_DONE
);

constant CONV_ERR: conv_state_t := CONV_WAIT_LATCH;

signal curr_state: conv_state_t;
signal next_state: conv_state_t;

signal latched_data: std_logic_vector(data'range);
signal msb_data: std_logic_vector(data'range);
signal bin_data: std_logic_vector(data'range);
signal signed_data: std_logic_vector(data'range);
signal conv_data: std_logic_vector(data'range);
signal len_mask: std_logic_vector(data'range);

--
-- enable conversion stages

signal gray_to_bin_en: std_logic;
signal lsb_to_msb_en: std_logic;

--
-- timeout counter

constant TM_MAX: integer := work.absenc_pkg.us_to_count
 (work.absenc_pkg.MAX_TM_US, CLK_FREQ, integer'high);

constant TM_LEN: integer :=
 work.absenc_pkg.integer_length(TM_MAX);

constant DEFAULT_TM_TOP: integer := work.absenc_pkg.us_to_count
 (work.absenc_pkg.MASTER_DEFAULT_TM_US, CLK_FREQ, TM_LEN);

signal tm_val: unsigned(TM_LEN - 1 downto 0);
signal tm_top: unsigned(tm_val'range);
signal tm_match: std_logic;

--
-- general counter

signal count_val:
 unsigned(work.absenc_pkg.integer_length(DATA_LEN) - 1 downto 0);
signal count_top: unsigned(count_val'range);
signal count_top_latched: unsigned(count_val'range);
signal count_rst: std_logic;
signal count_match: std_logic;

--
-- serial in, parallel out (SIPO) register

signal sipo_val: std_logic_vector(DATA_LEN - 1 downto 0);
signal sipo_latch_redge: std_logic;
signal sipo_latch_pre: std_logic;
signal sipo_latch: std_logic;

--
-- master clock

signal ma_clk_rst_en: std_logic;
signal ma_clk_rst_val: std_logic;

signal ma_clk_val: std_logic;

signal ma_half_fdiv: unsigned(ma_fdiv'length - 1 downto 0);
signal ma_half_count: unsigned(ma_half_fdiv'length - 1 downto 0);
signal ma_half_match: std_logic;

--
-- master clock rising falling edges

signal ma_clk_redge: std_logic;
signal ma_clk_fedge: std_logic;
signal ma_clk_edge: std_logic;

--
-- encoder multiplexed signal
--
-- the mux size depends on ENABLE_xxx generics. the idea is to eliminate
-- unneeded FPGA logic by instantiating only modules enabled by the generics.
-- to do so, we have functions that translate from ENC_TYPE_xxx to ENC_MUX_xxx
-- and vice versa. ENC_TYPE_xxx represent all the possible encoders, while
-- ENC_MUX_xxx represent an encoder index in the mux, if enabled by generics.

subtype tm_val_t is unsigned(tm_val'range);
type tm_val_array_t is array(integer range<>) of tm_val_t;

subtype count_val_t is unsigned(count_val'range);
type count_val_array_t is array(integer range<>) of count_val_t;

constant enc_mux_to_type: work.absenc_pkg.enc_type_array_t :=
 work.absenc_pkg.gen_enc_mux_to_type(ENABLE_ENDAT, ENABLE_BISS, ENABLE_SSI);

constant ENC_MUX_COUNT: integer :=
 work.absenc_pkg.get_enc_mux_count(ENABLE_ENDAT, ENABLE_BISS, ENABLE_SSI);

constant ENC_MUX_ENDAT: integer :=
 work.absenc_pkg.get_enc_mux_endat(ENABLE_ENDAT, ENABLE_BISS, ENABLE_SSI);

constant ENC_MUX_BISS: integer :=
 work.absenc_pkg.get_enc_mux_biss(ENABLE_ENDAT, ENABLE_BISS, ENABLE_SSI);

constant ENC_MUX_SSI: integer :=
 work.absenc_pkg.get_enc_mux_ssi(ENABLE_ENDAT, ENABLE_BISS, ENABLE_SSI);

signal ma_clk_edge_mux: std_logic_vector(ENC_MUX_COUNT - 1 downto 0);
signal ma_clk_rst_en_mux: std_logic_vector(ENC_MUX_COUNT - 1 downto 0);
signal ma_clk_rst_val_mux: std_logic_vector(ENC_MUX_COUNT - 1 downto 0);
signal mosi_mux: std_logic_vector(ENC_MUX_COUNT - 1 downto 0);
signal gate_mux: std_logic_vector(ENC_MUX_COUNT - 1 downto 0);
signal tm_top_mux: tm_val_array_t(ENC_MUX_COUNT - 1 downto 0);
signal count_rst_mux: std_logic_vector(ENC_MUX_COUNT - 1 downto 0);
signal count_top_mux: count_val_array_t(ENC_MUX_COUNT - 1 downto 0);
signal sipo_latch_mux: std_logic_vector(ENC_MUX_COUNT - 1 downto 0);
signal gray_to_bin_en_mux: std_logic_vector(ENC_MUX_COUNT - 1 downto 0);
signal lsb_to_msb_en_mux: std_logic_vector(ENC_MUX_COUNT - 1 downto 0);


begin


--
-- assert lengths to reduce generated logic
-- ie. counter and comparator sizes

assert (ma_fdiv'length <= 16)
report "ma_fdiv'length too large" severity failure;


--
-- master clock generation
-- master clock frequency is clk divided by ma_fdiv
-- generate edges using a counter from ma_fdiv/2 to 0

ma_half_fdiv <= ma_fdiv srl 1;
ma_half_match <= '1' when ma_half_count = 1 else '0';

process
begin
 wait until rising_edge(clk);

 if ((rst or ma_clk_rst_en or ma_half_match) = '1') then
  ma_half_count <= ma_half_fdiv;
 else
  ma_half_count <= ma_half_count - 1;
 end if;

end process;


process
begin
 wait until rising_edge(clk);

 if ((rst or ma_clk_rst_en) = '1') then
  ma_clk_val <= ma_clk_rst_val;
 else
  ma_clk_val <= ma_clk_val xor ma_half_match;
 end if;

end process;

ma_clk <= ma_clk_val;


--
-- master clock edges

process
begin
 wait until rising_edge(clk);
 ma_clk_redge <= ma_half_match and (not ma_clk_val);
end process;

process
begin
 wait until rising_edge(clk);
 ma_clk_fedge <= ma_half_match and ma_clk_val;
end process;


--
-- timeout counter
-- tm_val decrement from tm_top to 0
-- tm_val reloaded at ma_clk_edge

process
begin
 wait until rising_edge(clk);

 tm_match <= '0';

 if ((rst or ma_clk_edge) = '1') then
  tm_val <= tm_top;
 elsif (tm_val = 0) then
  tm_val <= tm_top;
  tm_match <= '1';
 else
  tm_val <= tm_val - 1;
 end if;

end process;


--
-- general purpose counter
-- monotically incrementing
-- increment every master edge
-- starts from 1

process
begin

 wait until rising_edge(clk);

 if (count_rst = '1') then
  count_val <= to_unsigned(integer(1), count_val'length);
 elsif (ma_clk_edge = '1') then
  count_val <= count_val + 1;
 end if;

end process;


--
-- general purpose counter comparator
-- count_match set to one when count_val = count_top

process
begin

 wait until rising_edge(clk);

 if (count_rst = '1') then
  count_top_latched <= count_top;
 end if;

end process;

count_match <= '1' when (count_val = count_top_latched) else '0';


--
-- sipo register

process
begin

 wait until rising_edge(clk);

 if (ma_clk_edge = '1') then
  sipo_val <= sipo_val(sipo_val'length - 2 downto 0) & miso;
 end if;

end process;


--
-- sipo_latch edge detector
-- ma_clk_redge is not used as ma_clk may be disabled.
-- instead, clk is used for detecting sipo_latch edges

process
begin
 wait until rising_edge(clk);
 sipo_latch_pre <= sipo_latch;
end process;

sipo_latch_redge <= (not sipo_latch_pre) and sipo_latch;


--
-- data conversion pipeline. ordering:
-- data from slave (latched at sipo_latch redge from sipo_val)
-- lsb_to_msb
-- gray_to_bin
-- extend_sign
-- latched to data
-- bin_to_sfixed (implemented in top)

process
begin
 wait until rising_edge(clk);

 if (rst = '1') then
  curr_state <= CONV_WAIT_LATCH;
 else
  curr_state <= next_state;
 end if;

end process;


process(curr_state, sipo_latch_redge)
begin
 
 next_state <= curr_state;

 case curr_state is

  when CONV_WAIT_LATCH =>
   if (sipo_latch_redge = '1') then
    next_state <= CONV_LSB_TO_MSB;
   end if;

  when CONV_LSB_TO_MSB =>
   next_state <= CONV_GRAY_TO_BIN;

  when CONV_GRAY_TO_BIN =>
   next_state <= CONV_EXTEND_SIGN;

  when CONV_EXTEND_SIGN =>
   next_state <= CONV_DONE;

  when CONV_DONE =>
   next_state <= CONV_WAIT_LATCH;

  when others =>
   next_state <= CONV_ERR;

 end case;

end process;


process
begin
 wait until rising_edge(clk);

 latched_data <= latched_data;
 conv_data <= conv_data;

 case curr_state is

  when CONV_WAIT_LATCH =>
   latched_data <= sipo_val and len_mask;

  when CONV_LSB_TO_MSB =>

  when CONV_EXTEND_SIGN =>

  when CONV_DONE =>
   conv_data <= signed_data;

  when others =>

 end case;

end process;

data <= conv_data;


len_to_mask: work.absenc_pkg.len_to_mask
port map
(
 len => len,
 mask => len_mask
);


lsb_to_msb: work.absenc_pkg.lsb_to_msb
port map
(
 en => lsb_to_msb_en,
 data_len => len,
 lsb_data => latched_data,
 msb_data => msb_data
);


gray_to_bin: work.absenc_pkg.gray_to_bin
port map
(
 en => gray_to_bin_en,
 gray_data => msb_data,
 bin_data => bin_data
);


extend_sign: work.absenc_pkg.extend_sign
port map
(
 data_len => len,
 data_in => bin_data,
 data_out => signed_data,
 len_mask => len_mask
);


--
-- encoder master implementations

gen_endat: if ENABLE_ENDAT = TRUE generate
master_endat: work.absenc_pkg.master_endat
generic map
(
 CLK_FREQ => CLK_FREQ
)
port map
(
 clk => clk,
 rst => rst,
 ma_clk_fedge => ma_clk_fedge,
 ma_clk_redge => ma_clk_redge,
 ma_clk_edge => ma_clk_edge_mux(ENC_MUX_ENDAT),
 ma_clk_rst_en => ma_clk_rst_en_mux(ENC_MUX_ENDAT),
 ma_clk_rst_val => ma_clk_rst_val_mux(ENC_MUX_ENDAT),
 mosi => mosi_mux(ENC_MUX_ENDAT),
 miso => miso,
 gate => gate_mux(ENC_MUX_ENDAT),
 len => len,
 tm_match => tm_match,
 tm_top => tm_top_mux(ENC_MUX_ENDAT),
 count_top => count_top_mux(ENC_MUX_ENDAT),
 count_match => count_match,
 count_rst => count_rst_mux(ENC_MUX_ENDAT),
 sipo_val => sipo_val,
 sipo_latch => sipo_latch_mux(ENC_MUX_ENDAT),
 gray_to_bin_en => gray_to_bin_en_mux(ENC_MUX_ENDAT),
 lsb_to_msb_en => lsb_to_msb_en_mux(ENC_MUX_ENDAT)
);
end generate gen_endat;


gen_biss: if ENABLE_BISS = TRUE generate
master_biss: work.absenc_pkg.master_biss
generic map
(
 CLK_FREQ => CLK_FREQ
)
port map
(
 clk => clk,
 rst => rst,
 ma_clk_fedge => ma_clk_fedge,
 ma_clk_redge => ma_clk_redge,
 ma_clk_edge => ma_clk_edge_mux(ENC_MUX_BISS),
 ma_clk_rst_en => ma_clk_rst_en_mux(ENC_MUX_BISS),
 ma_clk_rst_val => ma_clk_rst_val_mux(ENC_MUX_BISS),
 mosi => mosi_mux(ENC_MUX_BISS),
 miso => miso,
 gate => gate_mux(ENC_MUX_BISS),
 len => len,
 tm_match => tm_match,
 tm_top => tm_top_mux(ENC_MUX_BISS),
 count_top => count_top_mux(ENC_MUX_BISS),
 count_match => count_match,
 count_rst => count_rst_mux(ENC_MUX_BISS),
 sipo_val => sipo_val,
 sipo_latch => sipo_latch_mux(ENC_MUX_BISS),
 gray_to_bin_en => gray_to_bin_en_mux(ENC_MUX_BISS),
 lsb_to_msb_en => lsb_to_msb_en_mux(ENC_MUX_BISS)
);
end generate gen_biss;


gen_ssi: if ENABLE_SSI = TRUE generate
master_ssi: work.absenc_pkg.master_ssi
generic map
(
 CLK_FREQ => CLK_FREQ
)
port map
(
 clk => clk,
 rst => rst,
 ma_clk_fedge => ma_clk_fedge,
 ma_clk_redge => ma_clk_redge,
 ma_clk_edge => ma_clk_edge_mux(ENC_MUX_SSI),
 ma_clk_rst_en => ma_clk_rst_en_mux(ENC_MUX_SSI),
 ma_clk_rst_val => ma_clk_rst_val_mux(ENC_MUX_SSI),
 mosi => mosi_mux(ENC_MUX_SSI),
 miso => miso,
 gate => gate_mux(ENC_MUX_SSI),
 len => len,
 tm_match => tm_match,
 tm_top => tm_top_mux(ENC_MUX_SSI),
 count_top => count_top_mux(ENC_MUX_SSI),
 count_match => count_match,
 count_rst => count_rst_mux(ENC_MUX_SSI),
 sipo_val => sipo_val,
 sipo_latch => sipo_latch_mux(ENC_MUX_SSI),
 gray_to_bin_en => gray_to_bin_en_mux(ENC_MUX_SSI),
 lsb_to_msb_en => lsb_to_msb_en_mux(ENC_MUX_SSI),
 ssi_flags => ssi_flags,
 ssi_delay_fdiv => ssi_delay_fdiv
);
end generate gen_ssi;


--
-- enc_type multiplexer

process
(
 enc_type,
 ma_clk_edge_mux,
 ma_clk_rst_en_mux,
 ma_clk_rst_val_mux,
 mosi_mux,
 gate_mux,
 tm_top_mux,
 count_rst_mux,
 count_top_mux,
 sipo_latch_mux,
 gray_to_bin_en_mux,
 lsb_to_msb_en_mux
)

begin

 ma_clk_edge <= ma_clk_redge;
 ma_clk_rst_en <= '0';
 ma_clk_rst_val <= '1';
 mosi <= '0';
 gate <= '0';
 tm_top <= to_unsigned(DEFAULT_TM_TOP, tm_top'length);
 count_rst <= '0';
 count_top <= (others => '0');
 sipo_latch <= '0';
 gray_to_bin_en <= '0';
 lsb_to_msb_en <= '0';

 for i in 0 to ENC_MUX_COUNT - 1 loop
  if enc_mux_to_type(i) = enc_type then
   ma_clk_edge <= ma_clk_edge_mux(i);
   ma_clk_rst_en <= ma_clk_rst_en_mux(i);
   ma_clk_rst_val <= ma_clk_rst_val_mux(i);
   mosi <= mosi_mux(i);
   gate <= gate_mux(i);
   tm_top <= tm_top_mux(i);
   count_rst <= count_rst_mux(i);
   count_top <= count_top_mux(i);
   sipo_latch <= sipo_latch_mux(i);
   gray_to_bin_en <= gray_to_bin_en_mux(i);
   lsb_to_msb_en <= lsb_to_msb_en_mux(i);
  end if;
 end loop;

end process;


end absenc_master_rtl; 
