-------------------------------------------------------------------------------
--                                                                           --
--          X       X   XXXXXX    XXXXXX    XXXXXX    XXXXXX      X          --
--          XX     XX  X      X  X      X  X      X  X           XX          --
--          X X   X X  X         X      X  X      X  X          X X          --
--          X  X X  X  X         X      X  X      X  X         X  X          --
--          X   X   X  X          XXXXXX   X      X   XXXXXX      X          --
--          X       X  X         X      X  X      X         X     X          --
--          X       X  X         X      X  X      X         X     X          --
--          X       X  X      X  X      X  X      X         X     X          --
--          X       X   XXXXXX    XXXXXX    XXXXXX    XXXXXX      X          --
--                                                                           --
--                                                                           --
--                       O R E G A N O   S Y S T E M S                       --
--                                                                           --
--                            Design & Consulting                            --
--                                                                           --
-------------------------------------------------------------------------------
--                                                                           --
--         Web:           http://www.oregano.at/                             --
--                                                                           --
--         Contact:       mc8051@oregano.at                                  --
--                                                                           --
-------------------------------------------------------------------------------
--                                                                           --
--  MC8051 - VHDL 8051 Microcontroller IP Core                               --
--  Copyright (C) 2001 OREGANO SYSTEMS                                       --
--                                                                           --
--  This library is free software; you can redistribute it and/or            --
--  modify it under the terms of the GNU Lesser General Public               --
--  License as published by the Free Software Foundation; either             --
--  version 2.1 of the License, or (at your option) any later version.       --
--                                                                           --
--  This library is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        --
--  Lesser General Public License for more details.                          --
--                                                                           --
--  Full details of the license can be found in the file LGPL.TXT.           --
--                                                                           --
--  You should have received a copy of the GNU Lesser General Public         --
--  License along with this library; if not, write to the Free Software      --
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA  --
--                                                                           --
-------------------------------------------------------------------------------
--
--
--         Author:                 Roland Höller
--
--         Filename:               mc8051_alu_struc.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.9 $
--
--         Date of Latest Version: $Date: 2006-09-07 10:02:11 $
--
--
--         Description: Connects the units alumux, alucore, addsub_core,
--                      comb_mltplr, comb_divider, and dcml_adjust together.
--                      The whole design is made up of combinational logic.
--
--
--
--
-------------------------------------------------------------------------------
architecture struc of mc8051_alu is

  signal s_alu_result   : std_logic_vector(DWIDTH-1 downto 0);
  signal s_alu_new_cy   : std_logic_vector((DWIDTH-1)/4 downto 0);
  signal s_alu_op_a     : std_logic_vector(DWIDTH-1 downto 0);
  signal s_alu_op_b     : std_logic_vector(DWIDTH-1 downto 0);
  signal s_alu_cmd      : std_logic_vector(3 downto 0);
  signal s_dvdnd        : std_logic_vector(DWIDTH-1 downto 0);
  signal s_dvsor        : std_logic_vector(DWIDTH-1 downto 0);
  signal s_qutnt        : std_logic_vector(DWIDTH-1 downto 0);
  signal s_rmndr        : std_logic_vector(DWIDTH-1 downto 0);
  signal s_mltplcnd     : std_logic_vector(DWIDTH-1 downto 0);
  signal s_mltplctr     : std_logic_vector(DWIDTH-1 downto 0);
  signal s_product      : std_logic_vector((DWIDTH*2)-1 downto 0);
  signal s_dcml_data    : std_logic_vector(DWIDTH-1 downto 0);
  signal s_dcml_rslt    : std_logic_vector(DWIDTH-1 downto 0);
  signal s_dcml_cy      : std_logic;
  signal s_addsub_rslt  : std_logic_vector(DWIDTH-1 downto 0);
  signal s_addsub_newcy : std_logic_vector((DWIDTH-1)/4 downto 0);
  signal s_addsub_ov    : std_logic;
  signal s_addsub_cy    : std_logic;
  signal s_addsub       : std_logic;
  signal s_addsub_opa   : std_logic_vector(DWIDTH-1 downto 0);
  signal s_addsub_opb   : std_logic_vector(DWIDTH-1 downto 0);

begin                 -- architecture structural

  i_alumux : alumux
    generic map (
      DWIDTH => DWIDTH)
    port map (
      -- Primary I/Os of the ALU unit.
      rom_data_i    => rom_data_i,
      ram_data_i    => ram_data_i,
      acc_i         => acc_i,
      cmd_i         => cmd_i,
      cy_i          => cy_i,
      ov_i          => ov_i,
      cy_o          => new_cy_o,
      ov_o          => new_ov_o,
      result_a_o    => result_a_o,
      result_b_o    => result_b_o,
      -- I/Os connecting the submodules.
      result_i      => s_alu_result,
      new_cy_i      => s_alu_new_cy,
      addsub_rslt_i => s_addsub_rslt,
      addsub_cy_i   => s_addsub_newcy,
      addsub_ov_i   => s_addsub_ov,
      op_a_o        => s_alu_op_a,
      op_b_o        => s_alu_op_b,
      alu_cmd_o     => s_alu_cmd,
      opa_o         => s_addsub_opa,
      opb_o         => s_addsub_opb,
      addsub_o      => s_addsub,
      addsub_cy_o   => s_addsub_cy,
      dvdnd_o       => s_dvdnd,
      dvsor_o       => s_dvsor,
      qutnt_i       => s_qutnt,
      rmndr_i       => s_rmndr,
      mltplcnd_o    => s_mltplcnd,
      mltplctr_o    => s_mltplctr,
      product_i     => s_product,
      dcml_data_o   => s_dcml_data,
      dcml_data_i   => s_dcml_rslt,
      dcml_cy_i     => s_dcml_cy);

  i_alucore : alucore
    generic map (
      DWIDTH    => DWIDTH)
    port map (
      op_a_i    => s_alu_op_a,
      op_b_i    => s_alu_op_b,
      alu_cmd_i => s_alu_cmd,
      cy_i      => cy_i,
      cy_o      => s_alu_new_cy,
      result_o  => s_alu_result);

  i_addsub_core : addsub_core
    generic map (DWIDTH => DWIDTH)
    port map (opa_i    => s_addsub_opa,
              opb_i    => s_addsub_opb,
              addsub_i => s_addsub,
              cy_i     => s_addsub_cy,
              cy_o     => s_addsub_newcy,
              ov_o     => s_addsub_ov,
              rslt_o   => s_addsub_rslt);

  gen_multiplier1 : if C_IMPL_MUL = 1 generate
    i_comb_mltplr : comb_mltplr
      generic map (
        DWIDTH     => DWIDTH)
      port map (
        mltplcnd_i => s_mltplcnd,
        mltplctr_i => s_mltplctr,
        product_o  => s_product);
  end generate gen_multiplier1;
  gen_multiplier0 : if C_IMPL_MUL /= 1 generate
    s_product <= (others => '0');
  end generate gen_multiplier0;

  gen_divider1  : if C_IMPL_DIV = 1 generate
    i_comb_divider : comb_divider
      generic map (
        DWIDTH  => DWIDTH)
      port map (
        dvdnd_i => s_dvdnd,
        dvsor_i => s_dvsor,
        qutnt_o => s_qutnt,
        rmndr_o => s_rmndr);
  end generate gen_divider1;
  gen_divider0  : if C_IMPL_DIV /= 1 generate
    s_qutnt <= (others => '0');
    s_rmndr <= (others => '0');
  end generate gen_divider0;

  gen_dcml_adj1  : if C_IMPL_DA = 1 generate
    i_dcml_adjust : dcml_adjust
      generic map (
        DWIDTH => DWIDTH)
      port map (
        data_i => s_dcml_data,
        cy_i   => cy_i,
        data_o => s_dcml_rslt,
        cy_o   => s_dcml_cy);
  end generate gen_dcml_adj1;
  gen_dcml_adj0  : if C_IMPL_DA /= 1 generate
    s_dcml_rslt <= (others => '0');
    s_dcml_cy   <= '0';
  end generate gen_dcml_adj0;

end struc;
