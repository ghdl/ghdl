----------------------------------------------------------------------
--                           VLM5030
--                      www.fpgaarcade.com
--                     All rights reserved.
--
--                     admin@fpgaarcade.com
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
----------------------------------------------------------------------
--
-- Copyright (c) 2021, Arnim Laeuger  arnim.laeuger@gmx.net
-- All rights reserved.
--
-------------------------------------------------------------------------------
--
-- Sanyo VLM5030 speech synthesizer
--
-- Gate-level recreation extracted from the die image at
--   https://siliconpr0n.org/archive/doku.php?id=ogoun:vlm5030
--
--   https://www.fpgaarcade.com/tag/vlm5030/
--
-------------------------------------------------------------------------------
--
--                 +-------,_,-------+
--        GND   -- |  1           40 | <-    RST
--        TST1  -> |  2           39 | ->    TST4
--        OSC2  ck |  3     _     38 | <-    TST3
--        OSC1  ck |  4    (_)    37 | ->    TST2
--        D0    -> |  5           36 | ->    DAO
--        D1    -> |  6           35 | <-    VREF
--        D2    -> |  7           34 | ->    MTE
--        D3    -> |  8     V     33 | ->    /ME
--        D4    -> |  9     L     32 | <-    VCU
--        D5    -> | 10     M     31 | <-    START
--        D6    -> | 11     5     30 | ->    BSY
--        D7    -> | 12     0     29 | --    Vdd
--        A0    <- | 13     3     28 | ->    A15
--        A1    <- | 14     0     27 | ->    A14
--        A2    <- | 15           26 | ->    A13
--        A3    <- | 16     _     25 | ->    A12
--        A4    <- | 17    (_)    24 | ->    A11
--        A5    <- | 18           23 | ->    A10
--        A6    <- | 19           22 | ->    A9
--        A7    <- | 20           21 | ->    A8
--                 +-----------------+
--
-- Clocking
-- ========
--
-- This model requires overclocking of OSC1/OSC2 by 2x or higher.
-- Violating this requirement results in undefined behaviour.
--
-- i_clk is supplied with a free-running clock at >= 2x OSC frequency.
--
-- i_oscen acts as clock enable for i_clk to define the 3.58 MHz clock
-- at OSC1/OSC2 of a real chip.
--
--
-- Test pins
-- =========
--
-- i_tst1 must be tied to '0' for mission mode operation.
-- Other unused test pins can be left unconnected:
-- * i_vref
-- * o_tst2
-- * i_tst3
-- * o_tst4
--
--
-- Audio output
-- ============
--
-- The VLM5030 internally generates 12 bit signed PCM audio with ~8136 Hz
-- sample rate.
--
-- During /ME=1 this PCM data is available at A[13:12], A[9:0].
-- Note that A[9] has to be inverted externally if A[9:0] is to be used as a
-- standard signed vector.
--
-- Typical applications use audio from the DAO pin. It is fed by a combination
-- of a 5 bit parallel DAC (tapping A[9:5]) and overlaid with a PWM signal
-- (controlled by A[4:2]). Bits A[1:0] are not used for DAO generation
--
-- DAO characteristics
-- -------------------
--
-- DAO is an overlay of the 8136 Hz PCM data with high frequency PWM.
-- The PWM overlay periodically increases the DAO output voltage to the next
-- higher value with a duty cycle controlled by A[4:2].
--
--   ^   new PCM value                                 new PCM value
--   |   .                PWM overlay                              .
--   |   .+---+      +---+      +---+      +---+      +---+      +-+
--   |   ++   +------+   +------+   +------+   +------+   +------+ +
--   |   |                                                         |
--   |   |                                                         +--
--   |---+
--   +------------------------------------------------------------------>
--
-- When fed through an external low-pass filter, this PWM overlay augments
-- the discreete PCM DAC voltages with fractional levels.
--
-- PWM period approx. 24.581 us, 40.682 kHz
--   PWM |  high  |  low
--  -----+--------+--------
--    7  | 20.112 |  4.470
--    6  | 17.318 |  7.263
--    5  | 13.966 | 10.615
--    4  | 11.173 | 13.408
--    3  |  7.821 | 16.760
--    2  |  4.670 | 19.553
--    1  |  1.676 | 22.905
--    0  |  0.000 | 24.581
--
-- Direct PCM output
-- -----------------
--
-- This model outputs signed 10 bit audio at o_audio in addition to DAO.
-- o_audio is equivalent to A[9:0] during /ME=1 with corrected sign bit.
--


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity vlm5030_gl is

  port (
    i_clk   : in  std_logic;
    i_oscen : in  std_logic;
    i_rst   : in  std_logic;
    i_start : in  std_logic;
    i_vcu   : in  std_logic;
    i_vref  : in  std_logic := '1';
    i_tst1  : in  std_logic;
    o_tst2  : out std_logic;
    i_tst3  : in  std_logic := '1';
    o_tst4  : out std_logic;
    i_d     : in  std_logic_vector( 7 downto 0);
    o_a     : out std_logic_vector(15 downto 0);
    o_me_l  : out std_logic;
    o_mte   : out std_logic;
    o_bsy   : out std_logic;

    -- o_dao: unsigned audio, sample rate ~8136Hz, PWM ~40.682 kHz
    o_dao    : out std_logic_vector( 5 downto 0);

    -- o_audio: signed audio, sample rate ~8136Hz
    o_audio  : out std_logic_vector(9 downto 0)
  );

end;

use work.clock_functions_pack.all;
use work.vlm5030_pack.all;

architecture gl of vlm5030_gl is

  signal osc   : r_clk;
  signal clk2  : r_clk := z_clk;
  signal nclk2 : r_clk;

  signal rst   : std_logic;


  signal dq       : std_logic_vector(7 downto 0);
  signal maskdq53 : std_logic;

  signal starttst    : std_logic;
  signal tststopclk2, tstend2ID, tstend2IE,
         tstenID2A, tstenIE2A, tstenctrl2A,
         tstenIE2DAC : std_logic;

  signal clk2divq          : std_logic_vector(10 downto 0) := (others => '0');
  signal c2d0, c2d1, -- c2d2,
         c2d3, c2d4, c2d5,
         c2d6, c2d7, c2d8,
         c2d9, c2d10       : r_clk;
  signal c2d5fin           : r_clk;
  signal c2d7fin, nc2d7fin : r_clk;
  signal c2d9fin, nc2d9fin : r_clk;
  signal clk2gd5           : r_clk;
  signal nc2d1, nc2d6,
         nc2d8, nc2d10     : r_clk;
  signal c2d3gated         : r_clk;


  signal fsromevalout : std_logic;
  signal fsromdo      : std_logic_vector(13 downto 0);
  signal fsromnorhigh,
         fsromnorlow  : std_logic;
  --
  signal clk2ctrl    : r_clk;
  signal ncen1, cen3 : std_logic;
  signal eaoen       : std_logic;
  signal xromdo7nq   : std_logic := '0';
  signal xromdo7q    : std_logic;
  signal xromdo      : std_logic_vector(36 downto 0);
  signal yromdo      : std_logic_vector( 4 downto 0);
  signal c2d3gate    : std_logic;


  signal cntdn0 : std_logic;


  signal dinalq : std_logic_vector(7 downto 0) := (others => '0');
  signal aq     : std_logic_vector(o_a'range);


  signal startrise  : std_logic;
  signal clkcntdn   : r_clk;
  signal ncntdnload : std_logic;
  signal ncntdn     : std_logic;

  signal eavcu      : std_logic;
  signal ealatchh   : std_logic;
  signal neaload    : std_logic;
  signal eainc      : std_logic;
  signal clrdinal   : std_logic;
  signal clkdin     : std_logic;
  signal nvcufinal  : std_logic;
  signal vcufinal12 : std_logic;
  signal nbsy       : std_logic;
  signal me         : std_logic;
  signal rflatchwen : std_logic;
  signal asshift2   : std_logic;
  signal updtpitch  : std_logic;
  signal enrf2ID    : std_logic;
  signal clkksa     : r_clk;
  signal ensum2ID   : std_logic;


  signal clk2ena, clk2enb : r_clk;

  signal rstdel : std_logic;


  signal ksa  : std_logic_vector(4 downto 0);
  signal nkdo : std_logic_vector(9 downto 0);


  signal rfdo        : std_logic_vector(nkdo'range);
  signal rfdo97zero  : std_logic;


  signal nID : std_logic_vector(9 downto 0);

  signal assum : std_logic_vector(nID'range);

  signal nIE : std_logic_vector(11 downto 0);

  signal idlat       : std_logic_vector(7 downto 0);
  signal idlatall1   : std_logic;
  signal enIDlinv2ID : std_logic;
  signal enIDl2IE    : std_logic;
  signal enIDlinv2IE : std_logic;


  signal pitchoverflow : std_logic;
  signal enpitchlat    : r_clk;


  signal enmem02ID : std_logic;
  signal nmem0do,
         mem0do    : std_logic_vector(nID'range);
  signal enmem12IE : std_logic;
  signal mem1do2IE : std_logic_vector(nIE'range);
  signal enmem22IE : std_logic;
  signal mem2do2IE : std_logic_vector(nIE'range);

  signal ieregdrv,
         ieregdrv4IE  : std_logic_vector(nIE'range);
  signal ieregload    : r_clk;

  signal enieregfa2IE : std_logic;

  signal c2d10xr9  : r_clk;
  signal enIE2A    : std_logic;
  signal ieaddrreg : std_logic_vector(nIE'range);

  signal pitchmod : std_logic;

  signal random : std_logic;

  signal pwmsr : std_logic;

begin

  -- MSB K-slice address bit
  -- logic exists that generates it but output is not used (patched wiring)
  ksa(4) <= '0';

  -- main clock, corresponds to oscillator output
  osc  <= (base => i_clk,
           val  => i_clk,
           rise => i_oscen,
           fall => '0');

  clk2ena <= clk2 or (cen3 nor tstenctrl2A) or ncen1;
  clk2enb <= clk2 or (cen3 nor tstenctrl2A) or ncen1 or (fsromdo(6) nor tstenctrl2A);



  -----------------------------------------------------------------------------
  -- RST / POR generation
  --
  rstdel_block : block
    signal porcnt : unsigned(7 downto 0) := (others => '1');
    signal npor   : std_logic := '0';
    signal del    : std_logic_vector(1 downto 0) := (others => '0');
  begin
    por_p : process (osc)
    begin
      if rising_edge(osc) then
        if porcnt > 0 then
          porcnt <= porcnt - 1;
        end if;
        if porcnt = 0 then              -- TODO: measure POR hold time
          npor <= '1';
        end if;
      end if;
    end process;

    o_tst4 <= not npor;

    rst <= '1' when npor = '0' and i_tst3 = '1' else i_rst;

    rstdel_p : process (clk2)
    begin
      if rising_edge(clk2) then
        del <= del(0) & rst;
      end if;
    end process;
    --
    rstdel <= del(1);
  end block;



  -----------------------------------------------------------------------------
  -- DQ registers
  --
  dq_block : block
    signal rstq   : std_logic := '0';
    signal rstclk : r_clk;
    signal ldq    : std_logic_vector(dq'range);
    signal maskdq53m, maskdq53s : std_logic := '0';

  begin
    rstclk_p : process (osc.base)
    begin
      if rising_edge(osc.base) then
        rstq <= rst;
      end if;
    end process;
    --
    rstclk <= (base => osc.base,
               val  => rstq,
               rise => not rstq and     rst,
               fall =>     rstq and not rst);

    dq_p : process (rstclk)
    begin
      if falling_edge(rstclk) then
        ldq <= i_d;
      end if;
    end process;

    maskqm_p : process(startrise, enpitchlat)
    begin
      if startrise = '1' then
        maskdq53m <= '1';
      elsif rising_edge(enpitchlat) then
        maskdq53m <= rfdo97zero;
      end if;
    end process;
    maskqs_p : process(startrise, enpitchlat)
    begin
      if startrise = '1' then
        maskdq53s <= '1';
      elsif falling_edge(enpitchlat) then
        maskdq53s <= maskdq53m;
      end if;
    end process;
    maskdq53 <= maskdq53s;

    dqmask_p : process(maskdq53, ldq)
    begin
      dq <= ldq;
      if maskdq53 = '1' then
        dq(5 downto 3) <= (others => '0');
      end if;
    end process;

  end block;



  -----------------------------------------------------------------------------
  -- TST logic
  --
  --
  -- TST1 VREF VCU START    Function
  --  0    X    X    X      Mission mode
  --  1    0    1    1      Clock stopped
  --  1    1    1    0      D to ID
  --  1    1    0    0      D to IE
  --  1    1    1    1      ID to A
  --  1    1    0    1      IE to A
  --  1    0    0    1      CTRL to A
  --  1    0    X    0      IE to DAC, forces D to IE
  --                        VCU controls pwmsel
  --
  tst_block : block
    signal ntst1, nstart, nvcu : std_logic;
    signal ntst1vref : std_logic;
  begin
    ntst1  <= not i_tst1;
    nstart <= not i_start;
    nvcu   <= not i_vcu;

    starttst <= nstart nor i_tst1;

    ntst1vref <= i_tst1 nand i_vref;

    tststopclk2 <= not( ntst1 or i_vref or  nvcu or  nstart );
    tstend2ID   <= not( ntst1vref       or  nvcu or i_start );
    tstend2IE   <= not( ntst1vref       or i_vcu or i_start ) or tstenIE2DAC;
    tstenID2A   <= not( ntst1vref       or  nvcu or  nstart );
    tstenIE2A   <= not( ntst1vref       or i_vcu or  nstart );
    tstenctrl2A <= not( ntst1 or i_vref or i_vcu or  nstart );
    tstenIE2DAC <= not( ntst1 or i_vref          or i_start );
  end block;



  -----------------------------------------------------------------------------
  -- CLK2 generation
  --
  clk2_block : block
  begin

    clk2.base <= osc.base;

    process (osc)
    begin
      if rising_edge(osc) then
        if tststopclk2 = '1' then
          clk2.val  <= '0';

        else
          clk2.val <= not clk2.val;
        end if;
      end if;
    end process;

    clk2.rise <= '0' when tststopclk2 = '1' else
                 osc.rise when clk2 = '0' else
                 '0';
    clk2.fall <= '0' when tststopclk2 = '1' else
                 osc.rise when clk2 = '1' else
                 '0';

    nclk2 <= not clk2;

  end block;



  -----------------------------------------------------------------------------
  -- CLK2 divider
  --
  clk2div_block : block
    signal c2qnor   : std_logic;
    signal feedback : std_logic;

    -- edge detection for val that will change to preval on the rising refclk edge
    function rising_edge_detect(refclk : r_clk; val, preval : std_logic) return r_clk is
    begin
      return (base => refclk.base,
              val  => val,
              rise => refclk.rise and not val and     preval,
              fall => refclk.rise and     val and not preval);
    end;

  begin
    -- the 10 bit shift register chain for CLK2DIV
    process (clk2)
    begin
      if rising_edge(clk2) then
        clk2divq <= clk2divq(9 downto 0) & feedback;
      end if;
    end process;

    c2qnor <= norf(clk2divq(9 downto 0));

    -- feedback signal for the CLK2DIV shifter
    feedback <= '0' when rstdel = '1' else c2qnor;

    c2d0  <= rising_edge_detect(refclk => clk2, val => clk2divq( 0), preval => feedback);
    c2d1  <= rising_edge_detect(refclk => clk2, val => clk2divq( 1), preval => clk2divq(0));
    -- c2d2  <= rising_edge_detect(refclk => clk2, val => clk2divq( 2), preval => clk2divq(1));
    c2d3  <= rising_edge_detect(refclk => clk2, val => clk2divq( 3), preval => clk2divq(2));
    c2d4  <= rising_edge_detect(refclk => clk2, val => clk2divq( 4), preval => clk2divq(3));
    c2d5  <= rising_edge_detect(refclk => clk2, val => clk2divq( 5), preval => clk2divq(4));
    c2d6  <= rising_edge_detect(refclk => clk2, val => clk2divq( 6), preval => clk2divq(5));
    c2d7  <= rising_edge_detect(refclk => clk2, val => clk2divq( 7), preval => clk2divq(6));
    c2d8  <= rising_edge_detect(refclk => clk2, val => clk2divq( 8), preval => clk2divq(7));
    c2d9  <= rising_edge_detect(refclk => clk2, val => clk2divq( 9), preval => clk2divq(8));
    c2d10 <= rising_edge_detect(refclk => clk2, val => clk2divq(10), preval => clk2divq(9));


    -- SR-latches
    c2d5fin_b : entity work.vlm5030_srlatchclk
      port map (
        i_clk => osc,
        i_res => c2d0,
        i_set => c2d5,
        o_q   => c2d5fin
      );
    c2d7fin_b : entity work.vlm5030_srlatchclk
      port map (
        i_clk => osc,
        i_res => c2d0,
        i_set => c2d7,
        o_q   => c2d7fin
      );
    nc2d7fin <= not c2d7fin;

    c2d9fin_b : entity work.vlm5030_srlatchclk
      port map (
        i_clk => osc,
        i_res => c2d0,
        i_set => c2d9,
        o_q   => c2d9fin
      );
    nc2d9fin <= not c2d9fin;

    clk2gd5   <= c2d0 nor (c2d5 and nclk2);
    nc2d1     <= not c2d1;
    c2d3gated <= (not c2d3) nor c2d3gate;
    nc2d6     <= not c2d6;
    nc2d8     <= not c2d8;
    nc2d10    <= not c2d10;
  end block;



  -----------------------------------------------------------------------------
  -- FSROM
  --
  fsrom_block : block
    signal fsroma : std_logic_vector(5 downto 0);
  begin

    agen_block : block
    begin
      process (clk2ena)
      begin
        if rising_edge(clk2ena) then
          if (fsromnorlow nand nvcufinal) = '0' then
            -- shift chain
            fsroma <= fsroma(4 downto 0) & (fsroma(0) xor fsroma(5));
          else
            -- reset chain
            fsroma <= (3 => not rstdel,
                       others => '0');
          end if;

        end if;
      end process;

      fsromevalout <= not(   (fsromdo( 9) and (xromdo(0) or xromdo(4)))
                          or (fsromdo(10) and (xromdo(1) or xromdo(4)))
                          or (fsromdo(11) and (xromdo(6) or xromdo(3) or xromdo(0)))
                          or (fsromdo(12) and (xromdo(5) or xromdo(2)))
                          or (fsromdo(13) and  xromdo(5))
                          or (fsromdo( 8) and (xromdo(7) or xromdo(1))) );
    end block;

    rom_block : block
      alias  a   : std_logic_vector(fsroma'range) is fsroma;
      signal na  : std_logic_vector(a'range);
      signal ndq : std_logic_vector(5 downto 3);

      -- NOTE: vectors for norf must be range 0 to N!
      signal wl : std_logic_vector(0 to 17);

    begin
      na  <= not a;
      ndq <= not dq(5 downto 3);

      -------------------------------------------------------------------------
      -- FSROM data out
      --
      -- The sequence of the word lines must match the ROM bitmaps below
      wl <= a(5)&na(5) & na(4)&a(4) & a(3)&na(3) & na(2)&a(2) & a(1)&na(1) & na(0)&a(0) & dq(3)&ndq(3) & ndq(4)&dq(4) & dq(5)&ndq(5);

      --              wl(0)           wl(17)
      --              a(5)            ndq(5)
      --                |                |
      fsromdo <= (--    |                |
        00 => norf(wl, "100110011001000000"),
        01 => norf(wl, "011010010110001010"),
        02 => norf(wl, "101001010101010110"),
        03 => norf(wl, "011001011001100100"),
        04 => norf(wl, "011010010101001001"),
        05 => norf(wl, "011010011001010101"),
        06 => norf(wl, "010110011001000000"),
        --
        07 => norf(wl, "101010011001000000"),
        08 => norf(wl, "011001100110000000"),
        09 => norf(wl, "101001100110000000"),
        10 => norf(wl, "100101100110000000"),
        11 => norf(wl, "100110100110000000"),
        12 => norf(wl, "100110010110000000"),
        13 => norf(wl, "100110011010000000"),
        others => '1');
      --
      -------------------------------------------------------------------------

      fsromnorhigh <= norf(fsromdo(13 downto 8));
      fsromnorlow  <= norf(fsromdo( 5 downto 0));
    end block;

  end block;



  -----------------------------------------------------------------------------
  -- Sequencer ROM
  --
  seqrom_block : block
    signal gseqroma : std_logic_vector(4 downto 0);
  begin

    agen_block : block
      signal ncen3 : std_logic;
      -- signal nclk2ctrlcen3 : std_logic;
      -- signal nclk2ctrlncen3 : std_logic;
      signal seqroma : std_logic_vector(4 downto 0) := "11110";

    begin
      ncen1    <= tstenctrl2A nor c2d10;

      clk2ctrl <= clk2 or ncen1;

      ncen3 <= rstdel nor xromdo(7);
      cen3  <= not ncen3;

      -- nclk2ctrlcen3  <=  cen3 nor clk2ctrl;
      -- nclk2ctrlncen3 <= ncen3 nor clk2ctrl;


      process (clk2ctrl)
      begin
        if rising_edge(clk2ctrl) then
          seqroma(0) <= rstdel nor not (cen3 or xromdo(36) or not (seqroma(4) xor not seqroma(1)));

          if cen3 = '0' then
            seqroma(1) <= not seqroma(0);
            seqroma(2) <= seqroma(1);
            seqroma(3) <= seqroma(2);
            seqroma(4) <= seqroma(3);
          end if;
          if ncen3 = '0' then
            seqroma(4 downto 1) <= (others => '1');
          end if;

          xromdo7nq <= xromdo(7);       -- unclear circuit extraction
        end if;

      end process;

      xromdo7q <= not xromdo7nq;

      gseqroma <= seqroma(4 downto 1) & not seqroma(0);

      eaoen    <= i_tst1 nor xromdo7nq;

    end block;

    rom_block : block
      alias   a0 : std_logic is gseqroma(0);
      alias   a1 : std_logic is gseqroma(1);
      alias   a2 : std_logic is gseqroma(2);
      alias   a3 : std_logic is gseqroma(3);
      alias   a4 : std_logic is gseqroma(4);
      alias  xa5 : std_logic is nc2d9fin.val;
      alias  xa6 : std_logic is c2d9fin.val;
      signal  na : std_logic_vector(gseqroma'range);
      alias  na0 : std_logic is na(0);
      alias  na1 : std_logic is na(1);
      alias  na2 : std_logic is na(2);
      alias  na3 : std_logic is na(3);
      alias  na4 : std_logic is na(4);

      signal ny : std_logic_vector(yromdo'range);

      -- NOTE: vectors for norf must be range 0 to N!
      signal xwl : std_logic_vector(0 to 11);
      signal ywl : std_logic_vector(0 to 35);

    begin
      na <= not gseqroma;

      -------------------------------------------------------------------------
      -- XROM data out
      --
      -- The sequence of the word lines must match the ROM bitmaps below
      xwl <= na0 & a0 & a1 & na1 & na2 & a2 & a3 & na3 & na4 & a4 & xa5 & xa6;

      --              xwl(0)     xwl(11)
      --                na0        xa6
      --                 |          |
      xromdo <= (--      |          |
        00 => norf(xwl, "100110100100"),
        01 => norf(xwl, "010110010100"),
        02 => norf(xwl, "011010011000"),
        03 => norf(xwl, "011001011000"),
        04 => norf(xwl, "011001101000"),
        05 => norf(xwl, "011001100100"),
        06 => norf(xwl, "101001100100"),
        07 => norf(xwl, "100101100100"),
        --
        08 => norf(xwl, "100110100100"),
        09 => norf(xwl, "011010100100"),
        10 => norf(xwl, "101001101000"),
        11 => norf(xwl, "011001010100"),
        12 => norf(xwl, "011001101010"),
        13 => norf(xwl, "011010011010"),
        14 => norf(xwl, "100110100110"),
        --
        15 => norf(xwl, "101001000110"),
        16 => norf(xwl, "011001100101"),
        17 => norf(xwl, "011001011001"),
        18 => norf(xwl, "010110010101"),
        19 => norf(xwl, "010001010100"),
        20 => norf(xwl, "010101000000"),
        21 => norf(xwl, "011001100010"),
        --
        22 => norf(xwl, "000110000110"),
        23 => norf(xwl, "011001001001"),
        24 => norf(xwl, "010110011000"),
        25 => norf(xwl, "101000101000"),
        26 => norf(xwl, "010101011000"),
        27 => norf(xwl, "011000011010"),
        28 => norf(xwl, "100101101001"),
        --
        29 => norf(xwl, "000110000100"),
        30 => norf(xwl, "011010011000"),
        31 => norf(xwl, "001010101000"),
        32 => norf(xwl, "010101010000"),
        33 => norf(xwl, "100101101010"),
        34 => norf(xwl, "101001010100"),
        35 => norf(xwl, "101010011000"),
        36 => norf(xwl, "100110011000"),
        others => '1');
      --
      -------------------------------------------------------------------------


      -------------------------------------------------------------------------
      -- YROM data out
      --
      x_to_ywl : for idx in ywl'range generate
        ywl(idx) <= xromdo(idx);
      end generate;

      -- The sequence of the word lines must match the ROM bitmap below:
      -- Leftmost word line is x0, rightmost word line is x35
      --
      --               ywl(0)                            ywl(35)
      --                 x0                                x35
      --                 |                                  |
      ny <= (--          |                                  |
        04 => norf(ywl, "000000000100000000011000110000010001"),
        03 => norf(ywl, "000000000000111111111000100010000000"),
        02 => norf(ywl, "000000001001000000000111011010000000"),
        01 => norf(ywl, "000000000000000000000000000111111000"),
        00 => norf(ywl, "000000000000000000000000100000000111"),
        others => '1');

      c2d3gate <= norf(xromdo(7 downto 0));

      yromdo <= not ny;
      --
      -------------------------------------------------------------------------

    end block;

  end block;



  -----------------------------------------------------------------------------
  -- COUNT DOWN
  --
  cntdown_block : block
    signal cntq : unsigned(7 downto 3) := (others => '0');
  begin
    process (clkcntdn)
    begin
      if rising_edge(clkcntdn) then
        if ncntdnload = '0' then
          cntq(7 downto 4) <= unsigned(dinalq(7 downto 4));
          cntq(3)          <= '1';

        else
          if ncntdn = '0' then
            cntq <= cntq - 1;
          end if;

        end if;
      end if;
    end process;

    cntdn0 <= '1' when cntq = 0 else '0';

  end block;



  -----------------------------------------------------------------------------
  -- DIN ALIGNMENT
  --
  din_block : block
    signal dinlat  : std_logic_vector(i_d'range) := (others => '0');
    signal ndincom : std_logic;

    signal latchhq, latchh : std_logic_vector(15 downto 8);

  begin
    process (osc.base)
    begin
      if rising_edge(osc.base) then
        if clkdin = '0' then
          dinlat <= i_d;
        end if;
      end if;
    end process;

    ndincom <= norf(dinlat and xromdo(7 downto 0));

    process (clrdinal, c2d3gated)
    begin
      if clrdinal = '1' then
        dinalq <= (others => '0');
      elsif rising_edge(c2d3gated) then
        dinalq <= not ndincom & dinalq(7 downto 1);
      end if;
    end process;

    process (osc.base)
    begin
      if rising_edge(osc.base) then
        if ealatchh = '1' then
          latchhq <= dinalq;
        end if;
      end if;
    end process;

    latchh(8) <= dinalq(0) when eavcu = '1' else
                 latchhq(8);
    latchh(15 downto 9) <= (others => '0') when eavcu = '1' else
                           latchhq(15 downto 9);

    process (clk2ena)
    begin
      if rising_edge(clk2ena) then
        if neaload = '0' then
          -- parallel load
          aq( 0)          <= eavcu nor (not dinalq(0));
          aq( 7 downto 1) <= dinalq(7 downto 1);
          aq(15 downto 8) <= latchh;
        else
          if eainc = '1' then
            -- toggle/ripple
            aq <= std_logic_vector(unsigned(aq) + 1);
          end if;
        end if;
      end if;
    end process;

  end block;



  -----------------------------------------------------------------------------
  -- Random data source
  --
  random_block : block
    signal lfsr     : std_logic_vector(9 downto 0) := (others => '0');
    signal all0     : std_logic;
    signal feedback : std_logic;
  begin

    all0 <= norf(lfsr(lfsr'high-1 downto 0));

    feedback <= (lfsr(9) xor lfsr(2)) nor all0;

    process (clk2ena)
    begin
      if rising_edge(clk2ena) then
        lfsr <= lfsr(8 downto 0) & (rstdel nor feedback);
      end if;
    end process;

    random <= lfsr(9);

  end block;



  -----------------------------------------------------------------------------
  -- START control logic
  --
  start_block : block
    alias  vcu  : std_logic is i_vcu;
    signal nvcu : std_logic;

    signal startq        : std_logic_vector(2 downto 0);
    alias  startq0       : std_logic is startq(0);
    alias  startq1       : std_logic is startq(1);
    alias  startq2       : std_logic is startq(2);
    signal startriseq    : std_logic;
    signal startriseqvcu : std_logic;

    signal ffsset         : std_logic;
    signal ffsloop        : std_logic;
    signal ffs1q, ffs2q,
           ffs3q, ffs4q   : std_logic := '1';
    signal ffs1nq, ffs2nq,
           ffs3nq, ffs4nq : std_logic;
    signal ffs5q          : std_logic := '0';
    signal ffs5nq         : std_logic;
    signal vcumode        : std_logic;
    alias  nvcumode       : std_logic is ffs1q;

    signal vcufinal    : std_logic;
    signal vcufinal1q,
           vcufinal2q  : std_logic;
    signal nvcufinal12 : std_logic;

    signal xromdo7nqdel : std_logic;

    signal msff1q, msff2q,
           pmsff3q  : std_logic := '0';
    signal msff1nq, msff2nq,
           pmsff3nq : std_logic;
    signal msffset  : std_logic;

    signal n001x, n002x, n003x, n004x, n005x, n006x,
           n007x, n008x, n009x, n014x, n015x,
           n016x, n017x : std_logic;
    signal n012x : r_clk;

    signal busy1q,
           busy2q   : std_logic;
    signal setbusy1 : std_logic;

  begin
    nvcu    <= not vcu;
    vcumode <= not nvcumode;

    vcufinal <= not nvcufinal;

    process (rstdel, clk2ena)
    begin
      if rstdel = '1' then
        startq     <= (others => '0');
        startriseq <= '1';

      elsif rising_edge(clk2ena) then
        startq     <= startq(1 downto 0) & starttst;
        startriseq <= startrise nand vcu;

      end if;
    end process;

    startrise <= (not startq1) nor startq2;
    startriseqvcu <= not startriseq;


    ffsset <= rstdel or n005x;
    process (ffsset, clk2ena)
    begin
      if ffsset = '1' then
        ffs1q <= '1';
        ffs1q <= '1';
        ffs4q <= '1';
        ffs5q <= '0';
      elsif rising_edge(clk2ena) then
        ffs1q <= (ffs1nq  and ffsloop)   nor (nvcu    and startrise);  -- async set
        ffs2q <= (ffs2nq  and ffsloop)   nor ( vcu    and startrise);  -- async set
        ffs4q <= (ffs4q   nor startrise) nor (startq1  nor ffs3nq);    -- async set
        ffs5q <= ffs5nq  nand ffsloop;  -- async clr

      end if;
    end process;
    process (clk2ena)
    begin
      if rising_edge(clk2ena) then
        ffs3q <= ffs1nq;               -- no async set/clr
      end if;
    end process;
    ffs1nq <= not ffs1q;
    ffs2nq <= not ffs2q;
    ffs3nq <= not ffs3q;
    ffs4nq <= not ffs4q;
    ffs5nq <= not ffs5q;
    --
    ffsloop <= ffs1q or (not ffs3q) or (not fsromdo(6));


    process (vcufinal, clk2enb)
    begin
      if vcufinal = '1' then
        vcufinal1q <= '1';
        vcufinal2q <= '1';

      elsif rising_edge(clk2enb) then
        vcufinal1q <= not vcufinal1q;

        if vcufinal1q = '1' then
          vcufinal2q <= not vcufinal2q;
        end if;
      end if;

    end process;

    vcufinal12  <= (not vcufinal1q) nor (not vcufinal2q);
    nvcufinal12 <= not vcufinal12;
    nvcufinal   <= rstdel nor (nvcumode nor (not ffs4q));

    eavcu <= ffs2nq xor (ffs3q nor nvcumode);

    neaload <= (nvcumode or (ffs3q and (ffs2nq or (not fsromdo(6)) or nvcufinal12)));

    xromdo7nqdel_b : entity work.vlm5030_delay_inv
      generic map (
        -- delay by ~558ns = 2 clocks
        g_numclks => 2
      )
      port map (
        i_clk => osc,
        i_in  => xromdo7q,
        o_out => xromdo7nqdel
      );

    clkdin   <= (xromdo7nq and xromdo7nqdel and (vcumode or startriseqvcu)) nor ffs4nq;

    ealatchh <= startriseqvcu or (fsromdo(7) and ffs3q and vcufinal12);


    msffset <= rstdel or startrise;
    --
    process (msffset, c2d0)
    begin
      if msffset = '1' then
        msff1q <= '1';
      elsif rising_edge(c2d0) then
        msff1q <= n008x nor (pmsff3nq and xromdo(7));
      end if;
    end process;
    msff1nq <= not msff1q;
    --
    process (msffset, c2d6)
    begin
      if msffset = '1' then
        msff2q <= '1';
      elsif rising_edge(c2d6) then
        msff2q <= msff2nq nor (xromdo(1) and n007x and pmsff3nq);
      end if;
    end process;
    msff2nq <= not msff2q;
    --
    process (c2d6)
    begin
      if rising_edge(c2d6) then
        pmsff3q <= (n007x and xromdo(0)) nor (pmsff3nq and msff1q);
      end if;
    end process;
    pmsff3nq <= not pmsff3q;


    n001x <= not( (not fsromdo(7)) or nvcufinal12 or ffs2nq or ffs3nq );
    n002x <= not( dq(0) or vcufinal2q or vcufinal1q );
    n003x <= not( n002x or dq(1) or n004x );
    n004x <= vcufinal1q nor (not dq(0));
    n005x <= not( msff2q or n006x or nc2d6 );
    n006x <= n009x nand cntdn0;
    n007x <= not( n003x or ffs5nq or (fsromdo(13) nand dinalq(7)) );
    n008x <= not( msff1q or rstdel or (not n006x) );
    n009x <= not( nvcufinal12 or (not fsromdo(6)) or (not xromdo(7)) );
    n012x <= not( nc2d10 or n003x or fsromevalout or ffs5nq );
    n014x <= n001x nor not( n003x or ffs5nq or fsromnorhigh );
    n015x <= not( dq(1) or n004x or (not ( vcufinal2q or (not vcufinal1q) or dq(0)) ) );
    n016x <= (not fsromdo(6)) nor (not( dq(1) or n017x or (dq(0) and vcufinal1q) ));
    n017x <= not( dq(0) or (not vcufinal12) or (not vcufinal1q) );

    ncntdn <= not( msff1nq and n009x and (not cntdn0) );

    ncntdnload <= xromdo(5) nand pmsff3nq;
    clkcntdn <= (c2d0 and msff1nq) or (c2d6 and (xromdo(5) and pmsff3nq));

    clrdinal <= msff1nq or n012x;

    eainc <= n014x nor msff1nq;


    setbusy1 <= startq1 nor startriseqvcu;
    process (setbusy1, clk2ctrl)
    begin
      if setbusy1 = '1' then
        busy1q <= '1';
      elsif rising_edge(clk2ctrl) then
        busy1q <= startriseqvcu nor (not busy1q);
      end if;
    end process;

    process (ffsset, clk2ctrl)
    begin
      if ffsset = '1' then
        busy2q <= '1';
      elsif rising_edge(clk2ctrl) then
        busy2q <= vcumode nor (not busy2q);
      end if;
    end process;

    nbsy <= (not busy1q) nor (not busy2q);

    me <= not( ffs4q or i_start or xromdo7nq );

    rflatchwen <= not( fsromevalout or n003x or nc2d6 );

    asshift2 <= not( vcufinal2q or dq(1) or dq(0) );

    updtpitch <= not( n015x or (not fsromdo(6)) or (not xromdo(11)) );

    enrf2ID <= (not( n016x and (tstend2ID nor ffs5nq) )) nor nc2d8;

    clkksa <= n012x;

    ensum2ID <= not( ffs5nq or tstend2ID or ((n016x nor nc2d8) nor c2d4) );

  end block;



  -----------------------------------------------------------------------------
  -- K-Factor ROM
  --
  krom_block : block
    signal ka : std_logic_vector(4 downto 0);
  begin

    agen_block : block
      signal nfsrdo6 : std_logic;
      signal ksaq    : std_logic_vector(3 downto 0);
    begin

      ka  <= dinalq(7 downto 3);

      process (fsromdo(6), clkksa)
        variable toggle : std_logic_vector(ksaq'high+1 downto 0);
      begin
        if fsromdo(6) = '1' then
          ksaq <= "1011";

        elsif rising_edge(clkksa) then
          toggle(0) := '1';
          for idx in 0 to ksaq'high loop
            if toggle(idx) = '1' then
              ksaq(idx) <= not ksaq(idx);
            end if;

            toggle(idx+1) := ksaq(idx) nor (not toggle(idx));
          end loop;

        end if;
      end process;

      nfsrdo6 <= not fsromdo(6);

      ksa(3 downto 0) <= (
        0 => ((not ksaq(0)) and nfsrdo6) nor (nfsrdo6 nor yromdo(3)),
        1 => ((not ksaq(1)) and nfsrdo6) nor (nfsrdo6 nor yromdo(2)),
        2 => ((not ksaq(2)) and nfsrdo6) nor (nfsrdo6 nor yromdo(1)),
        3 => ((not ksaq(3)) and nfsrdo6) nor (fsromdo(6) and (yromdo(0) xor (xromdo(10) nor xromdo(11))))
        );

    end block;

    rom_block : block
      signal kslice0, kslice1, kslice2, kslice3, kslice4, kslice5 : std_logic_vector(9 downto 0);

      -- NOTE: vectors for norf must be range 0 to N!
      signal wl       : std_logic_vector(0 to 31);
      signal wl_slice : std_logic_vector(0 to  5);

      alias   kaodd : std_logic is dinalq(2);
      signal nkaodd : std_logic;
      alias   ksa0  : std_logic is ksa(0);
      signal nksa0  : std_logic;
      signal range_s0s1s2, range_s3s4 : std_logic;

      signal kout : std_logic_vector(nkdo'range);
    begin

      process (ka)
      begin
        wl <= (others => '0');
        wl(to_integer(unsigned(ka))) <= '1';
      end process;

      ---------------------------------------------------------------------------
      -- KROM data out
      --
      -- The sequence of the word lines must match the ROM bitmaps below

      --              wl(0)                          wl(31)
      --                |                              |
      kslice0 <= (--    |                              |
        00 => orf(wl, "11110111110000101101000111000101"),
        01 => orf(wl, "00000001001010110101010111101111"),
        02 => orf(wl, "00110111010011001110111111000001"),
        03 => orf(wl, "10110010011100000001101111010010"),
        04 => orf(wl, "10011011100000000001011111001011"),
        05 => orf(wl, "11011100000000000100111111000110"),
        06 => orf(wl, "11100000000000000110101010010100"),
        07 => orf(wl, "00000000000000001000110011100111"),
        08 => orf(wl, "00000000000000001111000011111000"),
        09 => orf(wl, "11111111111111110000000011111111"),
        others => '0');

      kslice1 <= (
        00 => orf(wl, "00000100101110101100000110000011"),
        01 => orf(wl, "01011010100100111100111110001111"),
        02 => orf(wl, "11110001110111001101001110110111"),
        03 => orf(wl, "10001010111000000000011110011100"),
        04 => orf(wl, "01010011000000001110111110001000"),
        05 => orf(wl, "10011100000000001010000001111010"),
        06 => orf(wl, "11100000000000001100101010101100"),
        07 => orf(wl, "00000000000000000000110011001111"),
        08 => orf(wl, "00000000000000001111000011110000"),
        09 => orf(wl, "11111111111111110000000011111111"),
        others => '0');

      kslice2 <= (
        00 => orf(wl, "11101010111101111111011110101011"),
        01 => orf(wl, "10111111010110001000010100101010"),
        02 => orf(wl, "11000100101011001110000001000100"),
        03 => orf(wl, "11111101000100101101101110100000"),
        04 => orf(wl, "10101001111101001110100000110101"),
        05 => orf(wl, "10011011010110001111001010010011"),
        06 => orf(wl, "11010010011000001111110011011010"),
        07 => orf(wl, "11100011100000001111111100011100"),
        08 => orf(wl, "11111100000000001111111111100000"),
        09 => orf(wl, "11111111111111111000000000000000"),
        others => '0');

      kslice3 <= (
        00 => orf(wl, "10100100000001100111000110101010"),
        01 => orf(wl, "11001001010100100000001010010010"),
        02 => orf(wl, "11110001100110110101011001111100"),
        03 => orf(wl, "11111110000111000110010010101010"),
        04 => orf(wl, "11111111111000000111100011001100"),
        05 => orf(wl, "11111111111111111000000011110000"),
        06 => orf(wl, "11111111111111111111111100000000"),
        others => '0');

      kslice4 <= (
        03 => orf(wl, "10101010100000000000000000000000"),
        04 => orf(wl, "11001100110101010111111111111111"),
        05 => orf(wl, "10001111000110011010101010000000"),
        06 => orf(wl, "11110000000111100011001100101010"),
        07 => orf(wl, "10000000000111111100001111001100"),
        08 => orf(wl, "11111111111000000000001111110000"),
        09 => orf(wl, "11111111111111111111110000000000"),
        others => '0');

      kslice5 <= (
        06 => not ka(1),
        07 => not ka(2),
        08 => not ka(3),
        09 => not ka(4),
        others => '0');
      --
      ---------------------------------------------------------------------------

      -------------------------------------------------------------------------
      -- Output mux
      --
      range_s0s1s2 <= norf(ksa(3 downto 1));
      range_s3s4   <= ksa(2) nor (ksa(3) nand ksa(1));
      nkaodd       <= not kaodd;
      nksa0        <= not ksa0;

      -- NOTE: Each bitline forms a distributed complex gate: (enable) AND (OR'ed wl-transistors)
      --       Bitlines from each slice are then NORed together to the data lines.
      --
      --       1. Wordlines are OR'ed on each bitline by the transistors
      --       2. The enable lines gate the effect of the bitline:
      --          - enabled: can pull data line to 0 if result of OR is 1
      --          - not enabled: data line remains 1
      --       3. Bitline result is visible as a NOR on the data line if enabled
      --
      --       This model pre-calculates the bitlines above using OR and NORs
      --       their results (enabled by the slice wls) onto the data lines.
      --
      --       --> kout represents the data lines

      wl_slice <= (range_s0s1s2 and nksa0 and nkaodd) &  -- enable kslice0
                  (range_s0s1s2 and nksa0 and  kaodd) &  -- enable kslice1
                  (range_s0s1s2 and  ksa0           ) &  -- enable kslice2
                  (range_s3s4   and nksa0           ) &  -- enable kslice3
                  (range_s3s4   and  ksa0           ) &  -- enable kslice4
                  (range_s0s1s2 nor range_s3s4      ) ;  -- enable kslice5
      kout_gen : for idx in kout'range generate
        kout(idx) <= norf(wl_slice,
                          kslice0(idx) &
                          kslice1(idx) &
                          kslice2(idx) &
                          kslice3(idx) &
                          kslice4(idx) &
                          kslice5(idx));
      end generate;
      -- data lines are subsequently inverted by the INVBUFs
      nkdo <= not kout;

    end block;
  end block;



  -----------------------------------------------------------------------------
  -- Register file
  --
  -- Memory map
  --
  --             9 8 7 6 5 4 3 2 1 0
  -- ksa[3..0] +---------------------+
  --        11 |      Pitch   |XXXXXX|
  --           +---------------------+
  --        10 |XXXXXX|    Energy    |
  --           +---------------------+
  --         9 |  K10 |XXXXXXXXXXXXXX|
  --           +---------------------+
  --         8 |  K9  |XXXXXXXXXXXXXX|
  --           +---------------------+
  --         7 |  K8  |XXXXXXXXXXXXXX|
  --           +---------------------+
  --         6 |  K7  |XXXXXXXXXXXXXX|
  --           +---------------------+
  --         5 |  K6  |XXXXXXXXXXXXXX|
  --           +---------------------+
  --         4 |  K5  |XXXXXXXXXXXXXX|
  --           +---------------------+
  --         3 |   K4   |XXXXXXXXXXXX|
  --           +---------------------+
  --         2 |   K3   |XXXXXXXXXXXX|
  --           +---------------------+
  --         1 |          K2         |
  --           +---------------------+
  --         0 |          K1         |
  --           +---------------------+
  --
  --
  regfile_block : block
    signal rf0, rf1 : std_logic_vector(9 downto 0) := (others => '0');
    signal rf2, rf3 : std_logic_vector(9 downto 6) := (others => '0');
    signal rf4, rf5,
           rf6, rf7,
           rf8, rf9 : std_logic_vector(9 downto 7) := (others => '0');
    signal rf10     : std_logic_vector(6 downto 0) := (others => '0');
    signal rf11     : std_logic_vector(9 downto 3) := (others => '0');
    signal a        : natural;
    signal al       : std_logic_vector(0 to 11);
    signal nrfdo    : std_logic_vector(rfdo'range);

  begin
    a <= to_integer(unsigned(ksa));

    process (osc.base)
    begin
      if rising_edge(osc.base) then
        if rflatchwen = '1' then
          case a is
            when  0 => rf0  <= nkdo;
            when  1 => rf1  <= nkdo;
            when  2 => rf2  <= nkdo( rf2'range);
            when  3 => rf3  <= nkdo( rf3'range);
            when  4 => rf4  <= nkdo( rf4'range);
            when  5 => rf5  <= nkdo( rf5'range);
            when  6 => rf6  <= nkdo( rf6'range);
            when  7 => rf7  <= nkdo( rf7'range);
            when  8 => rf8  <= nkdo( rf8'range);
            when  9 => rf9  <= nkdo( rf9'range);
            when 10 => rf10 <= nkdo(rf10'range);
            when 11 => rf11 <= nkdo(rf11'range);
            when others => null;
          end case;
        end if;
      end if;
    end process;

    process (a)
    begin
      al <= (others => '0');
      if a <= al'high then
        al(a) <= '1';
      end if;
    end process;

    -- unimplemented bits read as '0'
    -- they're coded with '1' here because the norif function inverts the vec operand
    nrfdo(0) <= norif(al, rf0(0) & rf1(0) &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   & rf10(0) &   '1'   );
    nrfdo(1) <= norif(al, rf0(1) & rf1(1) &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   & rf10(1) &   '1'   );
    nrfdo(2) <= norif(al, rf0(2) & rf1(2) &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   & rf10(2) &   '1'   );
    nrfdo(3) <= norif(al, rf0(3) & rf1(3) &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   & rf10(3) & rf11(3) );
    nrfdo(4) <= norif(al, rf0(4) & rf1(4) &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   & rf10(4) & rf11(4) );
    nrfdo(5) <= norif(al, rf0(5) & rf1(5) &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   & rf10(5) & rf11(5) );
    nrfdo(6) <= norif(al, rf0(6) & rf1(6) & rf2(6) & rf3(6) &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   &  '1'   & rf10(6) & rf11(6) );
    nrfdo(7) <= norif(al, rf0(7) & rf1(7) & rf2(7) & rf3(7) & rf4(7) & rf5(7) & rf6(7) & rf7(7) & rf8(7) & rf9(7) &   '1'   & rf11(7) );
    nrfdo(8) <= norif(al, rf0(8) & rf1(8) & rf2(8) & rf3(8) & rf4(8) & rf5(8) & rf6(8) & rf7(8) & rf8(8) & rf9(8) &   '1'   & rf11(8) );
    nrfdo(9) <= norif(al, rf0(9) & rf1(9) & rf2(9) & rf3(9) & rf4(9) & rf5(9) & rf6(9) & rf7(9) & rf8(9) & rf9(9) &   '1'   & rf11(9) );
    -- register file ouputs are subsequently inverted by the INVBUFs
    rfdo <= not nrfdo;

    rfdo97zero <= norf(rfdo(9 downto 7));

  end block;



  -----------------------------------------------------------------------------
  -- ADDSHIFT
  --
  addshift_block : block
    signal nIDlat, port_nID,
           port_rf  : std_logic_vector(nID'range);
    signal sumlat   : std_logic_vector(nID'high+2 downto 0);
    signal cin, sum : std_logic_vector(nID'high+1 downto 0);
    signal idpos    : std_logic;
  begin

    idpos <= not( (dq(6) and updtpitch) nor c2d5fin);

    cin(0) <= not idpos;

    --
    -- Generate adder/shifter slices
    --
    addshift_slice : for idx in nID'high downto 0 generate

      -- nID bus latch cell
      process (osc.base)
      begin
        if rising_edge(osc.base) then
          if c2d1 = '1' then
            nIDlat(idx) <= nID(idx);
          end if;
        end if;
      end process;

      -- adder port: nID bus
      port_nID(idx) <= not nIDlat(idx) when idpos = '1' else nIDlat(idx);
      -- adder port: registerfile or shift
      port_rf(idx)  <= rfdo(idx)     when c2d5fin = '0'  else
                       sumlat(idx+1) when asshift2 = '0'     else
                       sumlat(idx+2);
      -- xor3 cell
      sum(idx)   <= port_nID(idx) xor port_rf(idx) xor cin(idx);
      -- carry cell
      cin(idx+1) <= (port_nID(idx) and port_rf(idx)) or ((port_nID(idx) or port_rf(idx)) and cin(idx));

      -- sum latch cell
      -- note: sumlat[0] is omitted on the die
      process (osc.base)
      begin
        if rising_edge(osc.base) then
          if c2d4 = '1' then
            sumlat(idx) <= sum(idx);
          end if;
        end if;
      end process;
    end generate;

    -- extra cells
    sum(nID'high+1) <= cin(nID'high+1) xor port_nID(nID'high) xor port_rf(nID'high);
    process (osc.base)
    begin
      if rising_edge(osc.base) then
        if c2d4 = '1' then
          sumlat(nID'high+1) <= sum(nID'high+1);
        end if;
      end if;
    end process;
    sumlat(nID'high+2) <= sumlat(nID'high+1);

    assum <= sum(nID'range);

  end block;



  -----------------------------------------------------------------------------
  -- Pitch incrementer
  --
  pitchinc_block : block
    signal pitchlat,
          pitchreg : std_logic_vector(9 downto 3) := (others => '0');
    signal toggle  : std_logic_vector(pitchreg'high+1 downto pitchreg'low);
  begin

    enpitchlat <= not (c2d4 nand updtpitch);

    process (osc.base)
    begin
      if rising_edge(osc.base) then
        if enpitchlat = '1' then
          pitchlat <= nID(pitchlat'range);
        end if;
      end if;
    end process;

    process (rstdel, clk2ena)
    begin
      if rstdel = '1' then
        pitchreg <= (others => '0');

      elsif rising_edge(clk2ena) then
        if pitchoverflow = '1' then
          -- load
          pitchreg <= pitchlat;
        else
          -- toggle reg
          for idx in pitchreg'range loop
            if toggle(idx) = '1' then
              pitchreg(idx) <= not pitchreg(idx);
            end if;
          end loop;
        end if;

      end if;
    end process;

    toggle(3) <= '1';
    gen_toggle: for idx in 4 to toggle'high generate
      toggle(idx) <= (not pitchreg(idx-1)) nor (not toggle(idx-1));
    end generate;

    pitchoverflow <= toggle(toggle'high);

  end block;


  pitchmod <= not( (not(dq(6) xor dq(7))) or tstend2ID or nc2d1 );



  -----------------------------------------------------------------------------
  -- nID bus
  --
  nID_block : block
    signal wl : std_logic_vector(0 to 5);
  begin

    wl     <=          tstend2ID & enrf2ID & ensum2ID & updtpitch & enIDlinv2ID  & enmem02ID  ;
    --               |           |         |          |           |              |            |
    nID(0) <= norf(wl,  i_d(0)   & rfdo(0) & assum(0) &    '0'    & not idlat(0) & nmem0do(0) );
    nID(1) <= norf(wl,  i_d(1)   & rfdo(1) & assum(1) &    '0'    & not idlat(1) & nmem0do(1) );
    nID(2) <= norf(wl,  i_d(2)   & rfdo(2) & assum(2) &    '0'    & not idlat(2) & nmem0do(2) );
    nID(3) <= norf(wl,  i_d(3)   & rfdo(3) & assum(3) &    '0'    & not idlat(3) & nmem0do(3) );
    nID(4) <= norf(wl,  i_d(4)   & rfdo(4) & assum(4) &    '0'    & not idlat(4) & nmem0do(4) );
    nID(5) <= norf(wl,  i_d(5)   & rfdo(5) & assum(5) &    '0'    & not idlat(5) & nmem0do(5) );
    nID(6) <= norf(wl,  i_d(6)   & rfdo(6) & assum(6) & pitchmod  & not idlat(6) & nmem0do(6) );
    nID(7) <= norf(wl,  i_d(7)   & rfdo(7) & assum(7) &    '0'    & not idlat(7) & nmem0do(7) );
    nID(8) <= norf(wl,  i_d(0)   & rfdo(8) & assum(8) &    '0'    &     '0'      & nmem0do(8) );
    nID(9) <= norf(wl,  i_d(1)   & rfdo(9) & assum(9) &    '0'    &     '0'      & nmem0do(9) );

  end block;



  -----------------------------------------------------------------------------
  -- ID Latch
  --
  idlat_block : block
    signal idlaten : std_logic;
    signal n       : std_logic;
  begin

    idlaten     <= not( (not fsromdo(6)) or (not xromdo(10)) or nc2d8 );
    enIDlinv2ID <= not( (not xromdo(10)) or tstend2ID or nc2d1 );

    n           <= xromdo7q or tstend2IE or c2d7fin;
    enIDlinv2IE <= (not( (maskdq53 and random) or ((not maskdq53) and pitchoverflow) )) nor n;
    enIDl2IE    <= not( n or random or idlatall1 or (not maskdq53) );

    process (osc.base)
    begin
      if rising_edge(osc.base) then
        if idlaten = '1' then
          idlat <= nID(7 downto 0);
        end if;
      end if;
    end process;

    idlatall1 <= norf(not idlat);

  end block;



  -----------------------------------------------------------------------------
  -- Memories
  --
  mem_block : block
    signal swapa : unsigned(3 downto 0);
    signal a     : natural;

    signal clkmem0, clkmem1, clkmem2 : std_logic;

    type t_mem0 is array(0 to 9) of std_logic_vector(nID'range);
    signal mem0 : t_mem0 := (others => (others => '0'));
    type t_mem1 is array(0 to 9) of std_logic_vector(nIE'range);
    signal mem1 : t_mem1 := (others => (others => '0'));
    type t_mem2 is array(0 to 8) of std_logic_vector(nIE'range);
    signal mem2 : t_mem2 := (others => (others => '0'));
  begin

    yswap_gen : for idx in 3 downto 0 generate
      swapa(idx) <= yromdo(3-idx);
    end generate;
    a <= to_integer(swapa);

    --
    -- MEM0
    --
    clkmem0   <= not( xromdo(7) or (not fsromdo(6)) or (xromdo7q and yromdo(4)) or nc2d8 );
    enmem02ID <= not( tstend2ID or (xromdo7q and yromdo(4)) or nc2d1 );

    mem0_p : process (osc.base)
    begin
      if rising_edge(osc.base) then
        if clkmem0 = '1' then
          if a <= t_mem0'high then
            mem0(a) <= nID;
          end if;
        end if;
      end if;
    end process;

    -- the latches provide inverted Q on output, enabled by a
    nmem0do <= not mem0(a) when a <= t_mem0'high else (others => '1');
    mem0do  <= not nmem0do;


    --
    -- MEM1
    --
    clkmem1   <= yromdo(4) nor nc2d10;
    enmem12IE <= not( yromdo(4) or tstend2IE or c2d7fin );

    mem1_p : process (osc.base)
    begin
      if rising_edge(osc.base) then
        if clkmem1 = '1' then
          if a <= t_mem1'high then
            mem1(a) <= nIE;
          end if;
        end if;
      end if;
    end process;

    -- the latches provide inverted Q on output, enabled by a
    mem1do2IE <= not mem1(a) when a <= t_mem1'high else (others => '1');


    --
    -- MEM2
    --
    clkmem2   <= yromdo(4) and c2d10;
    enmem22IE <= not( tstend2IE or (not xromdo(7)) or nc2d10 );

    mem2_p : process (osc.base)
    begin
      if rising_edge(osc.base) then
        if clkmem2 = '1' then
          if a <= t_mem2'high then
            mem2(a) <= nIE;
          end if;
        end if;
      end if;
    end process;

    -- the latches provide inverted Q on output, enabled by a
    mem2do2IE <= not mem2(a) when a <= t_mem2'high else (others => '1');


  end block;


  -----------------------------------------------------------------------------
  -- Arithmetic block
  --
  arithmetic_block : block
    signal memxdo       : std_logic_vector(nIE'range);
    signal wl           : std_logic_vector(0 to 1);
    constant num_slices : natural := nIE'high+2;  -- 13 slices

    type r_row is record
      line1   : std_logic;
      line2   : std_logic;
      line3   : std_logic;
      line4in : std_logic_vector(num_slices-1+1 downto 0);
      lxor    : std_logic_vector(num_slices-1 downto 0);
      sum     : std_logic_vector(num_slices-1 downto 0);
      carry   : std_logic_vector(num_slices-1 downto 0);
      rowcarry1, rowcarry2 : std_logic;
    end record;
    signal row0, row1, row2, row3, row4 : r_row;

    -- map the slice index to the memxdo index
    function map_slice_memxdo_f (slice : natural) return natural is
    begin
      if slice <= 9 then
        -- slices 0 to 9 are supplied by memxdo(9) downto (0)
        return 9-slice;
      elsif slice = 10 then
        -- slice 10 is supplied by memxdo(11)
        return 11;
      elsif slice = 11 then
        -- slice 11 is supplied by memxdo(10)
        return 10;
      else
        -- invalid slice
        return 100;
      end if;
    end;
    --
    function line1_f(odd_prev : std_logic;
                     even     : std_logic) return std_logic is
    begin
      return not (odd_prev xor (not even));
    end;
    --
    function line2_f(odd_prev : std_logic;
                     even     : std_logic;
                     odd      : std_logic) return std_logic is
    begin
      return (
                    (not odd_prev)
                and (not even)
                and      odd
              ) or (
                         odd_prev
                and      even
                and (not odd)
              );
    end;
    --
    function xor_f (line4_upper : std_logic;
                    line1       : std_logic;
                    line2       : std_logic;
                    line3       : std_logic;
                    line4_lower : std_logic) return std_logic is
    begin
      return line3 xor not((line4_upper and line2) or (line4_lower and line1));
    end;
    --
    function sum_f (porta : std_logic;
                    portb : std_logic;
                    carry : std_logic) return std_logic is
    begin
      return porta xor portb xor carry;
    end;
    --
    function carry_f (porta : std_logic;
                      portb : std_logic;
                      carry : std_logic) return std_logic is
    begin
      return (
                carry and (porta or portb)
              ) or (
                porta and portb
              );
    end;
    --
    function map_slice_ie_f (slice : natural) return natural is
    begin
      return map_slice_memxdo_f(slice);
    end;

    signal oeq, oenq : std_logic;
    signal memlatmuxq : std_logic_vector(nIE'range);
    type r_memlatmux is record
      sum   : std_logic_vector(nIE'range);
      carry : std_logic_vector(nIE'range);
    end record;
    signal memlatmux : r_memlatmux;

    type r_iereg is record
      q     : std_logic_vector(nIE'range);
      portb : std_logic_vector(nIE'range);
      cout  : std_logic_vector(nIE'high+1 downto 0);
      sum   : std_logic_vector(nIE'range);
    end record;
    signal iereg : r_iereg;

  begin

    wl <= yromdo(4) & not yromdo(4);
    memxdo_gen : for idx in memxdo'range generate
      memxdo(idx) <= norf(wl, mem1do2IE(idx) & mem2do2IE(idx));
    end generate;


    --
    -- Row 0
    --
    row0.line1 <= line1_f(odd_prev => '1',
                          even     => mem0do(0));
    row0.line2 <= (not mem0do(0)) nor mem0do(1);  -- line2_f with odd_prev='1'
    row0.line3 <= mem0do(1);
    row0.line4in(0) <= row0.line4in(1);  -- metal short between upper and lower line4
    -- preset with 0 to replicate unimplemented left half of MUXI
    row0.line4in(13) <= '0';
    --
    row0_gen_low : for slice in 0 to 11 generate
      row0.line4in(slice+1) <= not memxdo(map_slice_memxdo_f(slice));
      row0.lxor(slice)  <= xor_f(line4_upper => row0.line4in(slice+1),
                                 line1 => row0.line1,
                                 line2 => row0.line2,
                                 line3 => row0.line3,
                                 line4_lower => row0.line4in(slice));
      row0.sum(slice)   <= sum_f(porta => row0.lxor(slice),
                                 portb => '0',
                                 carry => '0');
      row0.carry(slice) <= '0';
    end generate;

    -- rowcarry is built based on slice 11
    row0.rowcarry1 <= not( (row0.line1 and row0.line4in(12)) or (row0.line3 or (not row0.sum(11))) );
    row0.rowcarry2 <= row0.rowcarry1;

    --
    -- Row 1
    --
    row1.line1 <= line1_f(odd_prev => mem0do(1),
                          even     => mem0do(2));
    row1.line2 <= line2_f(odd_prev => mem0do(1),
                          even     => mem0do(2),
                          odd      => mem0do(3));
    row1.line3 <= mem0do(3);
    row1.line4in <= (others => '0');
    -- slices 0 and 1
    row1_gen_0_1 : for slice in 0 to 1 generate
      row1.lxor(slice)  <= xor_f(line4_upper => row0.line4in(slice+1),
                                 line1 => row1.line1,
                                 line2 => row1.line2,
                                 line3 => row1.line3,
                                 line4_lower => row0.line4in(slice));
      row1.sum(slice)   <= not row1.lxor(slice);
      row1.carry(slice) <= '0' when slice = 0 else
                            row1.lxor(slice);
    end generate;
    -- slice 2 has special carry calculation
    row1.lxor(2)  <= xor_f(line4_upper => row0.line4in(2+1),
                           line1 => row1.line1,
                           line2 => row1.line2,
                           line3 => row1.line3,
                           line4_lower => row0.line4in(2));
    row1.sum(2)   <= sum_f(porta => row1.lxor(2),
                           portb => row0.sum(2-2),
                           carry => '0');  -- just XOR lxor with sum-2
    row1.carry(2) <= (not row1.lxor(2)) nand row0.sum(2-2);  -- special operator in slice 2
    -- slices 3 to 12
    row1_gen_upper : for slice in 3 to num_slices-1 generate
      row1.lxor(slice) <= xor_f(line4_upper => row0.line4in(slice+1), -- slice 12 expects 0
                                line1 => row1.line1,
                                line2 => row1.line2,
                                line3 => row1.line3,
                                line4_lower => row0.line4in(slice));
      row1.sum(slice)   <= sum_f(porta => row1.lxor(slice),
                                 portb => row0.sum(slice-2),
                                 carry => '0');  -- just XOR lxor with sum-2
      row1.carry(slice) <= carry_f(porta => row1.lxor(slice),
                                   portb => row0.sum(slice-2),
                                   carry => '0');  -- just AND lxor with sum-2
    end generate;

    -- rowcarry chain
    row1.rowcarry1 <= carry_f(porta => not row1.line3,
                              portb => row1.sum(12),
                              carry => row0.rowcarry2);
    row1.rowcarry2 <= carry_f(porta => row1.carry(12),
                              portb => row1.sum(11),
                              carry => row1.rowcarry1);
                                 

    --
    -- Row 2
    --
    row2.line1 <= line1_f(odd_prev => mem0do(3),
                          even     => mem0do(4));
    row2.line2 <= line2_f(odd_prev => mem0do(3),
                          even     => mem0do(4),
                          odd      => mem0do(5));
    row2.line3 <= mem0do(5);
    row2.line4in <= (others => '0');
    -- slices 0 and 1
    row2_gen_0_1 : for slice in 0 to 1 generate
      row2.lxor(slice)  <= xor_f(line4_upper => row0.line4in(slice+1),
                                 line1 => row2.line1,
                                 line2 => row2.line2,
                                 line3 => row2.line3,
                                 line4_lower => row0.line4in(slice));
      row2.sum(slice)   <= not row2.lxor(slice);
      row2.carry(slice) <= '0' when slice = 0 else
                            row2.lxor(slice);
    end generate;
    -- slices 2 to 12
    row2_gen_upper : for slice in 2 to num_slices-1 generate
      row2.lxor(slice) <= xor_f(line4_upper => row0.line4in(slice+1), -- slice 12 expects 0
                                line1 => row2.line1,
                                line2 => row2.line2,
                                line3 => row2.line3,
                                line4_lower => row0.line4in(slice));
      row2.sum(slice)   <= sum_f(porta => row2.lxor(slice),
                                 portb => row1.sum(slice-2),
                                 carry => row1.carry(slice-1));
      row2.carry(slice) <= carry_f(porta => row2.lxor(slice),
                                   portb => row1.sum(slice-2),
                                   carry => row1.carry(slice-1));
    end generate;

    -- rowcarry chain
    row2.rowcarry1 <= carry_f(porta => not row2.line3,
                              portb => row2.sum(12),
                              carry => row1.rowcarry2);
    row2.rowcarry2 <= carry_f(porta => row2.carry(12),
                              portb => row2.sum(11),
                              carry => row2.rowcarry1);

    --
    -- Row 3
    --
    row3.line1 <= line1_f(odd_prev => mem0do(5),
                          even     => mem0do(6));
    row3.line2 <= line2_f(odd_prev => mem0do(5),
                          even     => mem0do(6),
                          odd      => mem0do(7));
    row3.line3 <= mem0do(7);
    row3.line4in <= (others => '0');
    -- slices 0 and 1
    row3_gen_0_1 : for slice in 0 to 1 generate
      row3.lxor(slice)  <= xor_f(line4_upper => row0.line4in(slice+1),
                                 line1 => row3.line1,
                                 line2 => row3.line2,
                                 line3 => row3.line3,
                                 line4_lower => row0.line4in(slice));
      row3.sum(slice)   <= not row3.lxor(slice);
      row3.carry(slice) <= '0' when slice = 0 else
                            row3.lxor(slice);
    end generate;
    -- slices 2 to 12
    row3_gen_upper : for slice in 2 to num_slices-1 generate
      row3.lxor(slice) <= xor_f(line4_upper => row0.line4in(slice+1), -- slice 12 expects 0
                                line1 => row3.line1,
                                line2 => row3.line2,
                                line3 => row3.line3,
                                line4_lower => row0.line4in(slice));
      row3.sum(slice)   <= sum_f(porta => row3.lxor(slice),
                                 portb => row2.sum(slice-2),
                                 carry => row2.carry(slice-1));
      row3.carry(slice) <= carry_f(porta => row3.lxor(slice),
                                   portb => row2.sum(slice-2),
                                   carry => row2.carry(slice-1));
    end generate;

    -- rowcarry chain
    row3.rowcarry1 <= carry_f(porta => not row3.line3,
                              portb => row3.sum(12),
                              carry => row2.rowcarry2);
    row3.rowcarry2 <= carry_f(porta => row3.carry(12),
                              portb => row3.sum(11),
                              carry => row3.rowcarry1);

    --
    -- Row 4
    --
    row4.line1 <= line1_f(odd_prev => mem0do(7),
                          even     => mem0do(8));
    row4.line2 <= line2_f(odd_prev => mem0do(7),
                          even     => mem0do(8),
                          odd      => mem0do(9));
    row4.line3 <= mem0do(9);
    row4.line4in <= (others => '0');
    -- slices 0 and 1
    row4_gen_0_1 : for slice in 0 to 1 generate
      row4.lxor(slice)  <= xor_f(line4_upper => row0.line4in(slice+1),
                                 line1 => row4.line1,
                                 line2 => row4.line2,
                                 line3 => row4.line3,
                                 line4_lower => row0.line4in(slice));
      row4.sum(slice)   <= not row4.lxor(slice);
      row4.carry(slice) <= '0' when slice = 0 else
                            row4.lxor(slice);
    end generate;
    -- slices 2 to 12
    row4_gen_upper : for slice in 2 to num_slices-1 generate
      row4.lxor(slice) <= xor_f(line4_upper => row0.line4in(slice+1), -- slice 12 expects 0
                                line1 => row4.line1,
                                line2 => row4.line2,
                                line3 => row4.line3,
                                line4_lower => row0.line4in(slice));
      row4.sum(slice)   <= sum_f(porta => row4.lxor(slice),
                                 portb => row3.sum(slice-2),
                                 carry => row3.carry(slice-1));
      row4.carry(slice) <= carry_f(porta => row4.lxor(slice),
                                   portb => row3.sum(slice-2),
                                   carry => row3.carry(slice-1));
    end generate;

    -- rowcarry chain
    row4.rowcarry1 <= carry_f(porta => not row4.line3,
                              portb => row4.sum(12),
                              carry => row3.rowcarry2);
    row4.rowcarry2 <= row4.rowcarry1;


    --
    -- MEMLATMUX
    --
    memlatmux_p : process (c2d7fin)
    begin
      if rising_edge(c2d7fin) then
        memlatmuxq <= not ieregdrv;
      end if;
    end process;
    --
    gen_memlatmux : for slice in 0 to memlatmuxq'high generate
      memlatmux.sum(slice) <=     row4.carry(slice+1) when c2d7fin = '0' else
                                  memlatmuxq(slice)   when yromdo(4) = '0'   else  -- OEQ  /c2d7fin nor  yromdo(4) = '1'
                              not memlatmuxq(slice);                               -- OEnQ /c2d7fin nor /yromdo(4) = '1'
      memlatmux.carry(slice) <= row4.sum(slice);
    end generate;


    --
    -- IEREG level
    --

    -- represents MUX on output of ROWCARRY chain
    iereg.cout(iereg.cout'high) <= not yromdo(4) when c2d7fin = '1' else
                                   row4.rowcarry2;
    iereg_gen : for slice in 0 to nIE'high generate

      -- IEREG
      iereg_p : process (ieregload)
      begin
        if falling_edge(ieregload) then
          iereg.q(slice) <= nIE(map_slice_ie_f(slice));
        end if;
      end process;

      iereg.portb(slice) <= not iereg.q(slice) when c2d7fin = '1' else
                            memlatmux.carry(slice);
      iereg.cout(slice)  <= carry_f(porta => memlatmux.sum(slice),
                                    portb => iereg.portb(slice),
                                    carry => iereg.cout(slice+1));
      iereg.sum(slice)   <= sum_f(porta => memlatmux.sum(slice),
                                  portb => iereg.portb(slice),
                                  carry => iereg.cout(slice+1));
    end generate;

    ieregdrv <= iereg.sum       when   ( ((not iereg.cout(0)) xor (not iereg.cout(1))) nand  c2d7fin ) = '1' else
                (others => '1') when not(      iereg.cout(1)   or (not iereg.cout(0))    or nc2d7fin ) = '1' else
                (others => '0') when not( (not iereg.cout(1))  or      iereg.cout(0)     or nc2d7fin ) = '1' else
                (others => 'X');        -- invalid
    iregdrv_gen : for slice in 0 to nIE'high generate
      ieregdrv4IE(slice) <= ieregdrv(map_slice_ie_f(slice));
    end generate;

    ieregload <= ( (c2d10 and yromdo(4)) or (  c2d6 and (xromdo7q nand yromdo(4) ) ) );
    enieregfa2IE <= not( tstend2IE or xromdo(7) or nc2d10 );


    --
    -- nIE distribution to ABUS and DAC
    --
    c2d10xr9 <= c2d10 and xromdo(9);
    enIE2A <= i_tst1 nor xromdo7q;

    ieaddr_p : process (c2d10xr9)
    begin
      if falling_edge(c2d10xr9) then
        ieaddrreg <= not nIE;
      end if;
    end process;

  end block;


  -----------------------------------------------------------------------------
  -- nIE bus
  --
  nIE_block : block
    signal wl : std_logic_vector(0 to 5);
  begin

    wl      <=          enIDlinv2IE  & enIDl2IE &   enmem12IE   &   enmem22IE   &  enieregfa2IE   & tstend2IE;
    --                |              |          |               |               |                 |          |
    nIE( 0) <= norf(wl,     '0'      &   '1'    & mem1do2IE( 0) & mem2do2IE( 0) & ieregdrv4IE( 0) &  i_d(0)  );
    nIE( 1) <= norf(wl, not idlat(0) & idlat(0) & mem1do2IE( 1) & mem2do2IE( 1) & ieregdrv4IE( 1) &  i_d(1)  );
    nIE( 2) <= norf(wl, not idlat(1) & idlat(1) & mem1do2IE( 2) & mem2do2IE( 2) & ieregdrv4IE( 2) &  i_d(2)  );
    nIE( 3) <= norf(wl, not idlat(2) & idlat(2) & mem1do2IE( 3) & mem2do2IE( 3) & ieregdrv4IE( 3) &  i_d(3)  );
    nIE( 4) <= norf(wl, not idlat(3) & idlat(3) & mem1do2IE( 4) & mem2do2IE( 4) & ieregdrv4IE( 4) &  i_d(4)  );
    nIE( 5) <= norf(wl, not idlat(4) & idlat(4) & mem1do2IE( 5) & mem2do2IE( 5) & ieregdrv4IE( 5) &  i_d(5)  );
    nIE( 6) <= norf(wl, not idlat(5) & idlat(5) & mem1do2IE( 6) & mem2do2IE( 6) & ieregdrv4IE( 6) &  i_d(6)  );
    nIE( 7) <= norf(wl, not idlat(6) & idlat(6) & mem1do2IE( 7) & mem2do2IE( 7) & ieregdrv4IE( 7) &  i_d(7)  );
    nIE( 8) <= norf(wl, not idlat(7) & idlat(7) & mem1do2IE( 8) & mem2do2IE( 8) & ieregdrv4IE( 8) &  i_d(0)  );
    nIE( 9) <= norf(wl,     '0'      &   '1'    & mem1do2IE( 9) & mem2do2IE( 9) & ieregdrv4IE( 9) &  i_d(1)  );
    nIE(10) <= norf(wl,     '0'      &   '1'    & mem1do2IE(10) & mem2do2IE(10) & ieregdrv4IE(10) &  i_d(2)  );
    nIE(11) <= norf(wl,     '0'      &   '1'    & mem1do2IE(11) & mem2do2IE(11) & ieregdrv4IE(11) &  i_d(3)  );

  end block;



  -----------------------------------------------------------------------------
  -- DAC block
  --
  dac_block : block
    signal pwmsel : std_logic;
  begin

    pwm_block : block
      signal pwm0, pwm1, pwm2 : std_logic;

      signal nc2d10xr9del : std_logic;
      --signal nclk2gd5hi,
      --      nclk2gd5lo    : std_logic;

      signal pwmreg1toggle,
             pwmreg2toggle    : std_logic;
      signal pwmreg0, pwmreg1,
             pwmreg2          : std_logic;

      signal pwmcomm,
             pwmcommdel,
             pwmcommand : std_logic;
      signal pwmcomp    : std_logic;

    begin

      pwm2 <= ieaddrreg(4) when tstenIE2DAC = '0' else nIE(4);
      pwm1 <= ieaddrreg(3) when tstenIE2DAC = '0' else nIE(3);
      pwm0 <= ieaddrreg(2) when tstenIE2DAC = '0' else nIE(2);

      nc2d10xr9del_b : entity work.vlm5030_delay_inv
        generic map (
          -- delay by ~1.1us = 4 osc clocks
          g_numclks => 4
        )
        port map (
          i_clk => osc,
          i_in  => c2d10xr9.val,
          o_out => nc2d10xr9del
        );
      -- nclk2gd5hi <= nc2d10xr9del       nor clk2gd5;
      -- nclk2gd5lo <= (not nc2d10xr9del) nor clk2gd5;

      pwmreg1toggle <= pwmreg0;
      pwmreg2toggle <= (not pwmreg1) nor (not pwmreg0);

      process (clk2gd5)
      begin
        if rising_edge(clk2gd5) then
          if nc2d10xr9del = '0' then
            -- clear
            pwmreg0 <= '0';
            pwmreg1 <= '0';
            pwmreg2 <= '0';

          else
            pwmreg0 <= not pwmreg0;
            if pwmreg1toggle = '1' then
              pwmreg1 <= not pwmreg1;
            end if;
            if pwmreg2toggle = '1' then
              pwmreg2 <= not pwmreg2;
            end if;
          end if;
        end if;

      end process;

      pwmcomm <= norf(pwmreg0 & pwmreg1 & pwmreg2);
      pwmcommdel_b : entity work.vlm5030_delay
        generic map (
          -- delay by ~1.1us = 4 osc clocks
          g_numclks => 4
        )
        port map (
          i_clk => osc,
          i_in  => pwmcomm,
          o_out => pwmcommdel
        );
      pwmcommand <= pwmcomm and pwmcommdel;

      pwmcomp <= norif(pwm2 & pwm1 & pwm0, pwmreg2 & pwmreg1 & pwmreg0);

      pwmsr_b : entity work.vlm5030_srlatch
        port map (
          i_clk => osc.base,
          i_res => pwmcomp,
          i_set => pwmcommand,
          o_q   => pwmsr
        );
      pwmsel <= not i_vcu when tstenIE2DAC = '1' else
                not pwmsr;

    end block;

    dacrom_block : block
      signal ndac   : std_logic_vector(4 downto 0);
      signal dacval, dacpwm : signed(o_dao'high+1 downto 0);
    begin

      ndac <= ieaddrreg(9) & not ieaddrreg(8 downto 5) when tstenIE2DAC = '0' else
              nIE(9) & not nIE(8 downto 5);

      -- The R-ladder between GND and VREF has 34 segments.
      -- * the lowest setting (-16 & pwmsel=1) generates 33 over 1
      -- * the highest setting (15 & pwmsel=0) generates 1 over 33
      -- TODO: check absolute resistance value of segments

      -- dacval is range 1 to 32
      dacval <= RESIZE(signed(not ndac), dacval'length) + 16+1;
      -- dacpwm is range 1 to 33
      dacpwm <= dacval when pwmsel = '1' else dacval + 1;

      o_dao <= std_logic_vector(dacpwm(o_dao'range));
      -- TST2 not modelled
      o_tst2 <= '0';

    end block;

  end block;



  -----------------------------------------------------------------------------
  -- Address bus block
  --
  abus_block : block
    signal wl : std_logic_vector(0 to 5);
  begin

    wl      <=              eaoen  &     enIE2A       & tstenID2A  &  tstenIE2A  & tstenctrl2A & tstenIE2DAC;
    --                    |        |                  |            |             |             |            |
    o_a( 0) <= not norf(wl, aq( 0) &   ieaddrreg(0)   & not nID(0) & not nIE( 0) &    '0'      &     '0'    );
    o_a( 1) <= not norf(wl, aq( 1) &   ieaddrreg(1)   & not nID(1) & not nIE( 1) &    '0'      &     '0'    );
    o_a( 2) <= not norf(wl, aq( 2) &   ieaddrreg(2)   & not nID(2) & not nIE( 2) &    '0'      &     '0'    );
    o_a( 3) <= not norf(wl, aq( 3) &   ieaddrreg(3)   & not nID(3) & not nIE( 3) &    '0'      &     '0'    );
    o_a( 4) <= not norf(wl, aq( 4) &   ieaddrreg(4)   & not nID(4) & not nIE( 4) &    '0'      &     '0'    );
    o_a( 5) <= not norf(wl, aq( 5) &   ieaddrreg(5)   & not nID(5) & not nIE( 5) &    '0'      &     '0'    );
    o_a( 6) <= not norf(wl, aq( 6) &   ieaddrreg(6)   & not nID(6) & not nIE( 6) &    '0'      &     '0'    );
    o_a( 7) <= not norf(wl, aq( 7) &   ieaddrreg(7)   & not nID(7) & not nIE( 7) &    '0'      &     '0'    );
    o_a( 8) <= not norf(wl, aq( 8) &   ieaddrreg(8)   & not nID(8) & not nIE( 8) &    '0'      &     '0'    );
    o_a( 9) <= not norf(wl, aq( 9) & not ieaddrreg(9) & not nID(9) & not nIE( 9) &    '0'      &     '0'    );
    o_a(10) <= not norf(wl, aq(10) &   pitchoverflow  &     '0'    &     '0'     &  c2d0.val   &    pwmsr   );
    o_a(11) <= not norf(wl, aq(11) &      random      &     '0'    &     '0'     &  xromdo7nq  &     '0'    );
    o_a(12) <= not norf(wl, aq(12) &   ieaddrreg(10)  &     '0'    & not nIE(10) & fsromdo(13) &     '0'    );
    o_a(13) <= not norf(wl, aq(13) &   ieaddrreg(11)  &     '0'    & not nIE(11) & vcufinal12  &     '0'    );
    o_a(14) <= not norf(wl, aq(14) &        '0'       &     '0'    &     '0'     &   cntdn0    &     '0'    );
    o_a(15) <= not norf(wl, aq(15) &        '0'       &     '0'    &     '0'     &     '0'     &     '0'    );

  end block;


  o_audio <= ieaddrreg(9 downto 0) when tstenIE2DAC = '0' else
             not nIE(9 downto 0);

  o_bsy <= not nbsy;

  o_me_l <= not me;

  mte_delay_b : entity work.vlm5030_delay
    generic map (
      -- delay for ~6us ==> 21 osc clocks
      g_numclks => 21
    )
    port map (
      i_clk => osc,
      i_in  => xromdo(35),
      o_out => o_mte
    );

end;
