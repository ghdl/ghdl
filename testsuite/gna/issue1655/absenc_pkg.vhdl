--
-- package for absolute encoder slave and master
-- supported formats: ENDAT, BISS, SSI
--
-- there is a bunch of code here. a typical user is
-- interested in the following 2 components:
-- absenc_pkg.slave
-- absenc_pkg.master
--
-- refer to sim/common/main.vhd for usage.


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
use ieee.math_real.all;


package absenc_pkg is


--
-- encoder type identifiers

constant ENC_TYPE_ENDAT: integer := 0;
constant ENC_TYPE_BISS: integer := 1;
constant ENC_TYPE_SSI: integer := 2;
constant ENC_TYPE_COUNT: integer := 3;


--
-- encoder positions in mux

function get_enc_mux_endat
(
 enable_endat: boolean;
 enable_biss: boolean;
 enable_ssi: boolean
)
return integer;


function get_enc_mux_biss
(
 enable_endat: boolean;
 enable_biss: boolean;
 enable_ssi: boolean
)
return integer;


function get_enc_mux_ssi
(
 enable_endat: boolean;
 enable_biss: boolean;
 enable_ssi: boolean
)
return integer;


--
-- count enabled encoders

function get_enc_mux_count
(
 enable_endat: boolean;
 enable_biss: boolean;
 enable_ssi: boolean
)
return integer;


--
-- mux position to encoder type

type enc_type_array_t is array(ENC_TYPE_COUNT - 1 downto 0) of integer;

function gen_enc_mux_to_type
(
 enable_endat: boolean;
 enable_biss: boolean;
 enable_ssi: boolean
)
return enc_type_array_t;


--
-- compute integer length

function integer_length
(
 i: integer
)
return integer;


--
-- microsecond to counter clocked by freq

function us_to_count
(
 us: integer;
 freq: integer;
 len: integer
)
return integer;


--
-- default timeout us
-- conditions the lowset and highest ma_clk

constant MAX_TM_US: integer := 100;
constant MASTER_DEFAULT_TM_US: integer := 60;
constant SLAVE_DEFAULT_TM_US: integer := 20;


--
-- default ssi related values
-- no spy mode
-- binary coding
-- no special terminating pattern
-- no delay

constant SSI_DEFAULT_FLAGS: std_logic_vector := "00000";
constant SSI_DEFAULT_DELAY_FDIV: unsigned(0 downto 0) := to_unsigned(0, 1);


--
-- encoder slave interface

component slave
generic
(
 -- local clock frequency
 CLK_FREQ: integer;

 -- enable a specific implementation, or all by default
 -- disabling unneeded implementations optimizes resources
 ENABLE_ENDAT: boolean := TRUE;
 ENABLE_BISS: boolean := TRUE;
 ENABLE_SSI: boolean := TRUE
);
port
(
 -- local clock and reset
 clk: in std_logic;
 rst: in std_logic;

 -- clock from master
 ma_clk: in std_logic;

 -- master in, slave out
 miso: out std_logic;
 mosi: in std_logic;

 -- gate to drive output (1 to drive it). used in PEPU
 -- first versions, set to open if not used.
 gate: out std_logic;

 -- data sent to master. typically the position
 -- important: use the smallest possible width, as
 -- this is used to derive other resource sizes
 data: in std_logic_vector;

 -- data length. typically the encoder resolution
 -- important: use the smallest possible width, as
 -- this is used to derive other resource sizes
 len: in unsigned;

 -- the selected encoder type
 enc_type: in integer;

 -- ssi specific flags
 -- set to work.absenc_pkg.SSI_DEFAULT_FLAGS if unused
 -- ssi_flags<0>: unused
 -- ssi_flags<1>: 0 or 1 for binary or gray data coding
 -- ssi_flags<2>: '.S' terminating pattern
 -- ssi_flags<3>: 'ES' terminating pattern
 -- ssi_flags<4>: 'OS' terminating pattern
 ssi_flags: in std_logic_vector
);
end component;


component slave_endat
generic
(
 CLK_FREQ: integer
);
port
(
 -- local clock
 clk: in std_logic;
 rst: in std_logic;

 -- master clock edges
 ma_clk_redge: in std_logic;
 ma_clk_fedge: in std_logic;

 -- the edge we are interested in
 ma_clk_edge: out std_logic;

 -- master in, slave out
 miso: out std_logic;
 mosi: in std_logic;

 -- gate to drive output (1 to drive it)
 gate: out std_logic;

 -- actual data to send and length
 data: in std_logic_vector;
 len: in unsigned;

 -- timeout counter
 tm_match: in std_logic;
 tm_top: out unsigned;

 -- general purpose counter
 count_top: out unsigned;
 count_match: in std_logic;
 count_rst: out std_logic;

 -- piso register
 piso_rval: in std_logic_vector;
 piso_lval: in std_logic_vector;
 piso_ini: out std_logic_vector;
 piso_load: out std_logic
);
end component;


component slave_biss
generic
(
 CLK_FREQ: integer
);
port
(
 -- local clock
 clk: in std_logic;
 rst: in std_logic;

 -- master clock edges
 ma_clk_redge: in std_logic;
 ma_clk_fedge: in std_logic;

 -- the edge we are interested in
 ma_clk_edge: out std_logic;

 -- master in, slave out
 miso: out std_logic;
 mosi: in std_logic;

 -- gate to drive output (1 to drive it)
 gate: out std_logic;

 -- actual data to send and length
 data: in std_logic_vector;
 len: in unsigned;

 -- timeout counter
 tm_match: in std_logic;
 tm_top: out unsigned;

 -- general purpose counter
 count_top: out unsigned;
 count_match: in std_logic;
 count_rst: out std_logic;

 -- piso register
 piso_rval: in std_logic_vector;
 piso_lval: in std_logic_vector;
 piso_ini: out std_logic_vector;
 piso_load: out std_logic
);
end component;


component slave_ssi
generic
(
 CLK_FREQ: integer
);
port
(
 -- local clock
 clk: in std_logic;
 rst: in std_logic;

 -- master clock edges
 ma_clk_redge: in std_logic;
 ma_clk_fedge: in std_logic;

 -- the edge we are interested in
 ma_clk_edge: out std_logic;

 -- master in, slave out
 miso: out std_logic;
 mosi: in std_logic;

 -- gate to drive output (1 to drive it)
 gate: out std_logic;

 -- actual data to send and length
 data: in std_logic_vector;
 len: in unsigned;

 -- timeout counter
 tm_match: in std_logic;
 tm_top: out unsigned;

 -- general purpose counter
 count_top: out unsigned;
 count_match: in std_logic;
 count_rst: out std_logic;

 -- piso register
 piso_rval: in std_logic_vector;
 piso_lval: in std_logic_vector;
 piso_ini: out std_logic_vector;
 piso_load: out std_logic;

 -- refer to absenc_pkg.slave for comments
 ssi_flags: in std_logic_vector
);
end component;


--
-- encoder master interface

component master
generic
(
 -- local clock frequency
 CLK_FREQ: integer;

 -- enable a specific implementation, or all by default
 -- disabling unneeded implementations optimizes resources
 ENABLE_ENDAT: boolean := TRUE;
 ENABLE_BISS: boolean := FALSE;
 ENABLE_SSI: boolean := FALSE
);
port
(
 -- local clock and reset
 clk: in std_logic;
 rst: in std_logic;

 -- ma_clk is the clock signal generated by the master
 -- to the slave. its frequency is CLK_FREQ / ma_fdiv
 ma_fdiv: in unsigned;
 ma_clk: out std_logic;

 -- master out, slave in
 mosi: out std_logic;
 miso: in std_logic;

 -- gate to drive output (1 to drive it). used in PEPU
 -- first versions, set to open if not used.
 gate: out std_logic;

 -- data received from slave. typically the position
 -- important: use the smallest possible width, as
 -- this is used to derive other resource sizes
 data: out std_logic_vector;

 -- data length. typically the encoder resolution
 -- important: use the smallest possible width, as
 -- this is used to derive other resource sizes
 len: in unsigned;

 -- the selected encoder type
 enc_type: in integer;

 -- ssi specific flags
 -- set to work.absenc_pkg.SSI_DEFAULT_FLAGS if unused
 -- ssi_flags<0>: ssi spy mode (ie. master without clock)
 -- ssi_flags<1>: 0 or 1 for binary or gray data coding
 -- ssi_flags<2>: '.S' terminating pattern
 -- ssi_flags<3>: 'ES' terminating pattern
 -- ssi_flags<4>: 'OS' terminating pattern
 ssi_flags: in std_logic_vector;

 -- ssi frame delay. divides CLK_FREQ
 -- set to work.absenc_pkg.SSI_DEFAULT_DELAY_FDIV if unsued
 ssi_delay_fdiv: in unsigned
);
end component;


component master_endat
generic
(
 CLK_FREQ: integer
);
port
(
 -- local clock
 clk: in std_logic;
 rst: in std_logic;

 -- master clock edges
 ma_clk_fedge: in std_logic;
 ma_clk_redge: in std_logic;

 -- the edge we are interested in
 ma_clk_edge: out std_logic;

 -- master clock reset
 -- if ma_clk_rst_en, use ma_clk_rst_level
 ma_clk_rst_en: out std_logic;
 ma_clk_rst_val: out std_logic;

 -- master out, slave in
 mosi: out std_logic;
 miso: in std_logic;

 -- gate to drive output (1 to drive it)
 gate: out std_logic;

 -- desired data length
 len: in unsigned;

 -- timeout counter
 tm_match: in std_logic;
 tm_top: out unsigned;

 -- general purpose counter
 count_top: out unsigned;
 count_match: in std_logic;
 count_rst: out std_logic;

 -- sipo register
 sipo_val: in std_logic_vector;
 sipo_latch: out std_logic;

 -- enable data conversion stages
 gray_to_bin_en: out std_logic;
 lsb_to_msb_en: out std_logic
);
end component;


component master_biss
generic
(
 CLK_FREQ: integer
);
port
(
 -- local clock
 clk: in std_logic;
 rst: in std_logic;

 -- master clock edges
 ma_clk_fedge: in std_logic;
 ma_clk_redge: in std_logic;

 -- the edge we are interested in
 ma_clk_edge: out std_logic;

 -- master clock reset
 -- if ma_clk_rst_en, use ma_clk_rst_level
 ma_clk_rst_en: out std_logic;
 ma_clk_rst_val: out std_logic;

 -- master out, slave in
 mosi: out std_logic;
 miso: in std_logic;

 -- gate to drive output (1 to drive it)
 gate: out std_logic;

 -- desired data length
 len: in unsigned;

 -- timeout counter
 tm_match: in std_logic;
 tm_top: out unsigned;

 -- general purpose counter
 count_top: out unsigned;
 count_match: in std_logic;
 count_rst: out std_logic;

 -- sipo register
 sipo_val: in std_logic_vector;
 sipo_latch: out std_logic;

 -- enable data conversion stages
 gray_to_bin_en: out std_logic;
 lsb_to_msb_en: out std_logic
);
end component;


component master_ssi
generic
(
 CLK_FREQ: integer
);
port
(
 -- local clock
 clk: in std_logic;
 rst: in std_logic;

 -- master clock edges
 ma_clk_fedge: in std_logic;
 ma_clk_redge: in std_logic;

 -- the edge we are interested in
 ma_clk_edge: out std_logic;

 -- master clock reset
 -- if ma_clk_rst_en, use ma_clk_rst_level
 ma_clk_rst_en: out std_logic;
 ma_clk_rst_val: out std_logic;

 -- master out, slave in
 mosi: out std_logic;
 miso: in std_logic;

 -- gate to drive output (1 to drive it)
 gate: out std_logic;

 -- desired data length
 len: in unsigned;

 -- timeout counter
 tm_match: in std_logic;
 tm_top: out unsigned;

 -- general purpose counter
 count_top: out unsigned;
 count_match: in std_logic;
 count_rst: out std_logic;

 -- sipo register
 sipo_val: in std_logic_vector;
 sipo_latch: out std_logic;

 -- enable data conversion stages
 gray_to_bin_en: out std_logic;
 lsb_to_msb_en: out std_logic;

 -- refer to absenc_pkg.master for comments
 ssi_flags: in std_logic_vector;
 ssi_delay_fdiv: in unsigned
);
end component;


component reader_hssl is
generic
(
 CLK_FREQ: integer
);
port
(
 -- local clock
 clk: in std_logic;
 rst: in std_logic;

 -- sender clock
 sclk: in std_logic;

 -- sender out, reader in
 sori: in std_logic;

 -- actual data to send and length
 data: out std_logic_vector;

 -- configuration
 len: in unsigned;
 tm_gap: in unsigned
);

end component;


--
-- utilities

component len_to_mask
port
(
 len: in unsigned;
 mask: out std_logic_vector
);
end component;


component lsb_to_msb
port
(
 en: in std_logic;
 data_len: in unsigned;
 lsb_data: in std_logic_vector;
 msb_data: out std_logic_vector
);
end component;


component bin_to_gray
port
(
 en: in std_logic;
 bin_data: in std_logic_vector;
 gray_data: out std_logic_vector
);
end component;


component gray_to_bin
port
(
 en: in std_logic;
 gray_data: in std_logic_vector;
 bin_data: out std_logic_vector
);
end component;


component extend_sign
port
(
 data_len: in unsigned;
 data_in: in std_logic_vector;
 data_out: out std_logic_vector;
 len_mask: in std_logic_vector
);
end component;


end package absenc_pkg;


package body absenc_pkg is

--
-- encoder positions in mux

function get_enc_mux_endat
(
 enable_endat: boolean;
 enable_biss: boolean;
 enable_ssi: boolean
)
return integer is
begin
 return 0;
end get_enc_mux_endat;


function get_enc_mux_biss
(
 enable_endat: boolean;
 enable_biss: boolean;
 enable_ssi: boolean
)
return integer is
 variable i: integer;
begin
 i := 0;
 if enable_endat = TRUE then i := i + 1; end if;
 return i;
end get_enc_mux_biss;


function get_enc_mux_ssi
(
 enable_endat: boolean;
 enable_biss: boolean;
 enable_ssi: boolean
)
return integer is
 variable i: integer;
begin
 i := 0;
 if enable_endat = TRUE then i := i + 1; end if;
 if enable_biss = TRUE then i := i + 1; end if;
 return i;
end get_enc_mux_ssi;


--
-- count enabled encoders

function get_enc_mux_count
(
 enable_endat: boolean;
 enable_biss: boolean;
 enable_ssi: boolean
)
return integer is
 variable n: integer;
begin
 n := 0;
 if enable_endat = TRUE then n := n + 1; end if;
 if enable_biss = TRUE then n := n + 1; end if;
 if enable_ssi = TRUE then n := n + 1; end if;
 return n;
end get_enc_mux_count;


--
-- mux position to encoder type

function gen_enc_mux_to_type
(
 enable_endat: boolean;
 enable_biss: boolean;
 enable_ssi: boolean
)
return enc_type_array_t is
 variable a: enc_type_array_t;
 variable i: integer;
begin

 i := 0;

 if enable_endat = TRUE then
  a(i) := ENC_TYPE_ENDAT;
  i := i + 1;
 end if;

 if enable_biss = TRUE then
  a(i) := ENC_TYPE_BISS;
  i := i + 1;
 end if;

 if enable_ssi = TRUE then
  a(i) := ENC_TYPE_SSI;
  i := i + 1;
 end if;

 return a;
end gen_enc_mux_to_type;


--
-- compute integer length

function integer_length
(
 i: integer
)
return integer is
begin
 return integer(ceil(log2(real(i))));
end integer_length;


--
-- microsecond to counter clocked by freq

function us_to_count
(
 us: integer;
 freq: integer;
 len: integer
)
return integer is
 variable count: integer;
begin
 count := integer(ceil(real(us) * real(freq) / 1000000.0));
 assert (integer_length(count) <= len) report "tm too high" severity failure;
 return count;
end us_to_count;


end package body absenc_pkg;
