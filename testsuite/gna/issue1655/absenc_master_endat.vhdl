library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;


entity master_endat is

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

end entity;


architecture absenc_master_endat_rtl of master_endat is


--
-- default timeout value. standard says: 10 to 30 us

constant TM_VAL: integer := work.absenc_pkg.us_to_count
 (work.absenc_pkg.MASTER_DEFAULT_TM_US, CLK_FREQ, tm_top'length);


--
-- main state machine

type endat_state_t is
(
 ENDAT_INIT,

 ENDAT_T0,
 ENDAT_T1,
 ENDAT_MODE,
 ENDAT_T3,
 ENDAT_T4,
 
 ENDAT_T5,

 ENDAT_START,
 ENDAT_F1,
 ENDAT_DATA,
 
 ENDAT_CRC5_FIRST,
 ENDAT_CRC5_CONT,

 ENDAT_DONE
);

constant ENDAT_TMOUT: endat_state_t := ENDAT_DONE;
constant ENDAT_ERR: endat_state_t := ENDAT_DONE;

signal curr_state: endat_state_t;
signal next_state: endat_state_t;


--
-- right shifted parallel in, serial out register
-- loaded values are in reverse order

constant piso_ini: std_logic_vector := "111000";
signal piso_val: std_logic_vector(piso_ini'length - 1 downto 0);
signal piso_load: std_logic;


begin


--
-- state automaton

process
begin
 wait until rising_edge(clk);

 if (rst = '1') then
  curr_state <= ENDAT_TMOUT;
 elsif ((tm_match or ma_clk_fedge) = '1') then
  curr_state <= next_state;
 end if;

end process;


process(curr_state, count_match, tm_match, miso)
begin
 
 next_state <= curr_state;

 case curr_state is

  when ENDAT_INIT =>
   next_state <= ENDAT_T0;

  when ENDAT_T0 =>
   next_state <= ENDAT_T1;

  when ENDAT_T1 =>
   next_state <= ENDAT_MODE;

  when ENDAT_MODE =>
   if (count_match = '1') then
    next_state <= ENDAT_T3;
   end if;

  when ENDAT_T3 =>
   next_state <= ENDAT_T4;

  when ENDAT_T4 =>
   next_state <= ENDAT_T5;

  when ENDAT_T5 =>
   next_state <= ENDAT_START;

  when ENDAT_START =>
   if (miso = '1') then
    next_state <= ENDAT_F1;
   elsif (tm_match = '1') then
    next_state <= ENDAT_TMOUT;
   end if;

  when ENDAT_F1 =>
   next_state <= ENDAT_DATA;

  when ENDAT_DATA =>
   if (count_match = '1') then
    next_state <= ENDAT_CRC5_FIRST;
   end if;

  when ENDAT_CRC5_FIRST =>
   next_state <= ENDAT_CRC5_CONT;

  when ENDAT_CRC5_CONT =>
   if (count_match = '1') then
    next_state <= ENDAT_DONE;
   end if;

  when ENDAT_DONE =>
   -- wait for at least one timeout
   next_state <= ENDAT_INIT;

  when others =>
   next_state <= ENDAT_ERR;

 end case;

end process;


process
begin

 wait until rising_edge(clk);

 ma_clk_rst_en <= '0';
 count_top <= (count_top'range => '0');
 count_rst <= '0';
 sipo_latch <= '0';
 gate <= '0';
 mosi <= '0';
 piso_load <= '0';

 case curr_state is

  when ENDAT_INIT =>
   gate <= '1';

  when ENDAT_T0 =>
   gate <= '1';

  when ENDAT_T1 =>
   piso_load <= '1';
   gate <= '1';
   count_top <= to_unsigned(6, count_top'length);
   count_rst <= '1';

  when ENDAT_MODE =>
   mosi <= piso_val(0);
   gate <= '1';

  when ENDAT_T3 =>
   mosi <= '0';
   gate <= '1';

  when ENDAT_T4 =>
   mosi <= '0';
   gate <= '1';

  when ENDAT_T5 =>

  when ENDAT_START =>

  when ENDAT_F1 =>
   count_top <= len(count_top'range);
   count_rst <= '1';

  when ENDAT_DATA =>
   -- sent LSb first

  when ENDAT_CRC5_FIRST =>
   count_top <= to_unsigned(integer(5 - 1), count_top'length);
   count_rst <= '1';
   sipo_latch <= '1';

  when ENDAT_CRC5_CONT =>

  when ENDAT_DONE =>
   ma_clk_rst_en <= '1';

  when others =>

 end case;

end process;


--
-- right shifted piso register
-- set at falling edge, sample by slave at redge

process
begin
 wait until rising_edge(clk);

 if (piso_load = '1') then
  piso_val <= piso_ini;
 elsif (ma_clk_fedge = '1') then
  piso_val <= '0' & piso_val(piso_val'length - 1 downto 1);
 end if;

end process;


--
-- clock reset or idle value

ma_clk_rst_val <= '1';


--
-- timeout

tm_top <= to_unsigned(TM_VAL, tm_top'length);


--
-- gray to binary disabled

gray_to_bin_en <= '0';


--
-- lsb to msb enabled

lsb_to_msb_en <= '1';


--
-- use falling edge

ma_clk_edge <= ma_clk_fedge;


end architecture;
