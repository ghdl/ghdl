--------------------------------------------------------------------------------
-- memac_rmii_rx_vhd                                                          --
-- Modular Ethernet MAC (MEMAC): receive RMII to UMI shim_                    --
--------------------------------------------------------------------------------
-- (C) Copyright 2025 Adam Barnes <ambarnes@gmail_com>                        --
-- This file is part of The Tyto Project_ The Tyto Project is free software:  --
-- you can redistribute it and/or modify it under the terms of the GNU Lesser --
-- General Public License as published by the Free Software Foundation,       --
-- either version 3 of the License, or (at your option) any later version_    --
-- The Tyto Project is distributed in the hope that it will be useful, but    --
-- WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE_ See the GNU Lesser General Public     --
-- License for more details_ You should have received a copy of the GNU       --
-- Lesser General Public License along with The Tyto Project_ If not, see     --
-- https://www_gnu_org/licenses/_                                             --
--------------------------------------------------------------------------------
-- TODO: ASYNC_REG constraints for crs_dv stages 1-3

library ieee;
  use ieee.std_logic_1164.all;

package memac_rmii_rx_pkg is

  component memac_rmii_rx is
    port (
      rst         : in    std_ulogic;
      clk         : in    std_ulogic;
      rmii_clken  : in    std_ulogic;
      rmii_crs_dv : in    std_ulogic;
      rmii_er     : in    std_ulogic;
      rmii_d      : in    std_ulogic_vector(1 downto 0);
      umii_clken  : in    std_ulogic;
      umii_dv     : out   std_ulogic;
      umii_er     : out   std_ulogic;
      umii_d      : out   std_ulogic_vector(7 downto 0)
    );
  end component memac_rmii_rx;

end package memac_rmii_rx_pkg;

--------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity memac_rmii_rx is
  port (
    rst         : in    std_ulogic;
    clk         : in    std_ulogic;
    rmii_clken  : in    std_ulogic;
    rmii_crs_dv : in    std_ulogic;
    rmii_er     : in    std_ulogic;
    rmii_d      : in    std_ulogic_vector(1 downto 0);
    umii_clken  : in    std_ulogic;                    -- coincident with every 4th rmii_clken
    umii_dv     : out   std_ulogic;
    umii_er     : out   std_ulogic;
    umii_d      : out   std_ulogic_vector(7 downto 0)
  );
end entity memac_rmii_rx;

architecture rtl of memac_rmii_rx is

  constant LATENCY : integer := 8;

  -- pipeline stages

  signal s1_crs_dv : std_ulogic;
  signal s1_er     : std_ulogic;
  signal s1_d      : std_ulogic_vector(1 downto 0);

  signal s2_crs_dv : std_ulogic;
  signal s2_er     : std_ulogic;
  signal s2_d      : std_ulogic_vector(1 downto 0);

  signal s3_crs_dv : std_ulogic;
  signal s3_er     : std_ulogic;
  signal s3_d      : std_ulogic_vector(1 downto 0);

  signal s4_crs_dv : std_ulogic;
  signal s4_er     : std_ulogic;
  signal s4_d      : std_ulogic_vector(1 downto 0);

  signal s5_act    : std_ulogic;
  signal s5_crs    : std_ulogic;
  signal s5_dv     : std_ulogic;
  signal s5_er     : std_ulogic;
  signal s5_d      : std_ulogic_vector(1 downto 0);
  signal s5_dibit  : std_ulogic_vector(1 downto 0);

  signal s6_stb    : std_ulogic;
  signal s6_crs    : std_ulogic_vector(3 downto 0);
  signal s6_dv     : std_ulogic_vector(3 downto 0);
  signal s6_er     : std_ulogic_vector(3 downto 0);
  signal s6_d      : std_ulogic_vector(7 downto 0);

  signal s7_crs    : std_ulogic_vector(3 downto 0);
  signal s7_dv     : std_ulogic_vector(3 downto 0);
  signal s7_er     : std_ulogic_vector(3 downto 0);
  signal s7_d      : std_ulogic_vector(7 downto 0);

begin

  P_MAIN: process(rst, clk)

    variable i : integer range 0 to 3;

  begin
    if rst then

      s1_crs_dv <= '0';
      s1_er     <= '0';
      s1_d      <= (others => '0');

      s2_crs_dv <= '0';
      s2_er     <= '0';
      s2_d      <= (others => '0');

      s3_crs_dv <= '0';
      s3_er     <= '0';
      s3_d      <= (others => '0');

      s4_crs_dv <= '0';
      s4_er     <= '0';
      s4_d      <= (others => '0');

      s5_act    <= '0';
      s5_crs    <= '0';
      s5_dv     <= '0';
      s5_er     <= '0';
      s5_d      <= (others => '0');
      s5_dibit  <= "00";

      s6_stb    <= '0';
      s6_crs    <= (others => '0');
      s6_dv     <= (others => '0');
      s6_er     <= (others => '0');
      s6_d      <= (others => '0');

      s7_crs    <= (others => '0');
      s7_dv     <= (others => '0');
      s7_er     <= (others => '0');
      s7_d      <= (others => '0');

      umii_dv <= '0';
      umii_er <= '0';
      umii_d  <= (others => '0');

    elsif rising_edge(clk) then

      s6_stb <= '0'; -- momentary

      if rmii_clken = '1' then

        s1_crs_dv <= rmii_crs_dv;
        s1_er     <= rmii_er;
        s1_d      <= rmii_d;

        s2_crs_dv <= s1_crs_dv;
        s2_er     <= s1_er;
        s2_d      <= s1_d;

        s3_crs_dv <= s2_crs_dv; -- crs_dv is non-metastable from here on
        s3_er     <= s2_er;
        s3_d      <= s2_d;

        s4_crs_dv <= s3_crs_dv;
        s4_er     <= s3_er;
        s4_d      <= s3_d;

        -- separate CRS and DV
        s5_act <=
          '0' when s3_crs_dv = '0' and s4_crs_dv = '0' else
          '1' when s4_crs_dv = '1' and s4_d /= "00";
        s5_crs <=
          '0' when s3_crs_dv = '0' and s4_crs_dv = '0' else
          '1' when s5_act = '0' and s4_crs_dv = '1' else
          '0' when s5_crs = '1' and s4_crs_dv = '0';
        s5_dv <=
          '0' when s3_crs_dv = '0' and s4_crs_dv = '0' else
          '1' when s5_act = '0' and s4_crs_dv = '1' and s4_d = "01";
        s5_er <= s4_er;
        s5_d  <= s4_d;
        s5_dibit <=
          "00" when s3_crs_dv = '0' and s4_crs_dv = '0' else
          std_ulogic_vector(unsigned(s5_dibit) + 1) when s5_dv = '1';

        -- assemble dibits into octets
        i := to_integer(unsigned(s5_dibit));
        s6_stb                   <= '1' when s5_dibit = "11" else '0';
        s6_crs(i)                <= s5_crs;
        s6_dv(i)                 <= s5_dv;
        s6_er(i)                 <= s5_er;
        s6_d(1+(i*2) downto i*2) <= s5_d;

      end if;

      -- stage 7: latch assembled octets
      if s6_stb then
        s7_crs    <= s6_crs;
        s7_dv     <= s6_dv;
        s7_er     <= s6_er;
        s7_d      <= s6_d;
      elsif umii_clken then
        s7_crs    <= (others => '0');
        s7_dv     <= (others => '0');
        s7_er     <= (others => '0');
        s7_d      <= (others => '0');
      end if;

      -- output signals
      if umii_clken then
        umii_dv <= '1' when s7_dv /= "0000" else '0';
        umii_er <= '1' when s7_er /= "0000" else '0';
        umii_d  <= s7_d;
      end if;

    end if;
  end process P_MAIN;

end architecture rtl;
