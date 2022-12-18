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
--         Filename:               alumux_rtl.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.6 $
--
--         Date of Latest Version: $Date: 2002-01-07 12:17:44 $
--
--
--         Description: Select data path according to the actual command.
--
--
--
--
-------------------------------------------------------------------------------
architecture rtl of alumux is

  constant DA             : std_logic_vector(5 downto 0) := "100000";
  constant ADD_ACC_RAM    : std_logic_vector(5 downto 0) := "100001";
  constant ADD_ACC_ROM    : std_logic_vector(5 downto 0) := "100010";
  constant ADDC_ACC_RAM   : std_logic_vector(5 downto 0) := "100011";
  constant ADDC_ACC_ROM   : std_logic_vector(5 downto 0) := "100100";
  constant AND_ACC_RAM    : std_logic_vector(5 downto 0) := "100101";
  constant AND_ACC_ROM    : std_logic_vector(5 downto 0) := "100110";
  constant AND_RAM_ROM    : std_logic_vector(5 downto 0) := "100111";
  constant SUB_ACC_RAM    : std_logic_vector(5 downto 0) := "101000";
  constant SUB_ACC_ROM    : std_logic_vector(5 downto 0) := "101001";
  constant MUL_ACC_RAM    : std_logic_vector(5 downto 0) := "101010";
  constant DIV_ACC_RAM    : std_logic_vector(5 downto 0) := "101011";
  constant OR_RAM_ACC     : std_logic_vector(5 downto 0) := "101100";
  constant OR_ROM_ACC     : std_logic_vector(5 downto 0) := "101101";
  constant OR_ROM_RAM     : std_logic_vector(5 downto 0) := "101110";
  constant XOR_RAM_ACC    : std_logic_vector(5 downto 0) := "101111";
  constant XOR_ROM_ACC    : std_logic_vector(5 downto 0) := "110000";
  constant XOR_ROM_RAM    : std_logic_vector(5 downto 0) := "110001";
  constant RL_ACC         : std_logic_vector(5 downto 0) := "110010";
  constant RLC_ACC        : std_logic_vector(5 downto 0) := "110011";
  constant RR_ACC         : std_logic_vector(5 downto 0) := "110100";
  constant RRC_ACC        : std_logic_vector(5 downto 0) := "110101";
  constant INV_ACC        : std_logic_vector(5 downto 0) := "110110";
  constant INV_RAM        : std_logic_vector(5 downto 0) := "110111";
  constant DEC_ACC        : std_logic_vector(5 downto 0) := "111000";
  constant DEC_RAM        : std_logic_vector(5 downto 0) := "111001";
  constant COMP_RAM_ACC   : std_logic_vector(5 downto 0) := "111010";
  constant COMP_ROM_ACC   : std_logic_vector(5 downto 0) := "111011";
  constant COMP_ROM_RAM   : std_logic_vector(5 downto 0) := "111100";
  constant INC_ACC        : std_logic_vector(5 downto 0) := "111110";
  constant INC_RAM        : std_logic_vector(5 downto 0) := "111111";

  constant NOP  : std_logic_vector(3 downto 0) := "0000";
  constant LAND : std_logic_vector(3 downto 0) := "0011";
  constant LOR  : std_logic_vector(3 downto 0) := "0101";
  constant LXOR : std_logic_vector(3 downto 0) := "0110";
  constant RL   : std_logic_vector(3 downto 0) := "0111";
  constant RLC  : std_logic_vector(3 downto 0) := "1000";
  constant RR   : std_logic_vector(3 downto 0) := "1001";
  constant RRC  : std_logic_vector(3 downto 0) := "1010";
  constant COMP : std_logic_vector(3 downto 0) := "1011";
  constant INV  : std_logic_vector(3 downto 0) := "1100";

begin

  -- Multiplex the input data and generate the command for the alu core.
  p_alucore_mux : process (rom_data_i,
                           ram_data_i,
                           acc_i,     
                           cmd_i)
  begin
    case cmd_i is
       when AND_ACC_RAM   =>
         alu_cmd_o <= LAND;
         op_a_o    <= acc_i;
         op_b_o    <= ram_data_i;
       when AND_ACC_ROM   =>
         alu_cmd_o <= LAND;
         op_a_o    <= acc_i;
         op_b_o    <= rom_data_i;
       when AND_RAM_ROM   =>
         alu_cmd_o <= LAND;
         op_a_o    <= ram_data_i;
         op_b_o    <= rom_data_i;
       when OR_RAM_ACC    =>
         alu_cmd_o <= LOR;
         op_a_o    <= acc_i;
         op_b_o    <= ram_data_i;
       when OR_ROM_ACC    =>
         alu_cmd_o <= LOR;
         op_a_o    <= acc_i;
         op_b_o    <= rom_data_i;
       when OR_ROM_RAM    =>
         alu_cmd_o <= LOR;
         op_a_o    <= rom_data_i;
         op_b_o    <= ram_data_i;
       when XOR_RAM_ACC   =>
         alu_cmd_o <= LXOR;
         op_a_o    <= acc_i;
         op_b_o    <= ram_data_i;
       when XOR_ROM_ACC   =>
         alu_cmd_o <= LXOR;
         op_a_o    <= acc_i;
         op_b_o    <= rom_data_i;
       when XOR_ROM_RAM   =>
         alu_cmd_o <= LXOR;
         op_a_o    <= rom_data_i;
         op_b_o    <= ram_data_i;
       when RL_ACC        =>
         alu_cmd_o <= RL;
         op_a_o    <= acc_i;
         op_b_o    <= ( others => '0' );
       when RLC_ACC       =>
         alu_cmd_o <= RLC;
         op_a_o    <= acc_i;
         op_b_o    <= ( others => '0' );
       when RR_ACC        =>
         alu_cmd_o <= RR;
         op_a_o    <= acc_i;
         op_b_o    <= ( others => '0' );
       when RRC_ACC       =>          
         alu_cmd_o <= RRC;            
         op_a_o    <= acc_i;          
         op_b_o    <= ( others => '0' );
       when INV_ACC       =>          
         alu_cmd_o <= INV;            
         op_a_o    <= acc_i;          
         op_b_o    <= ( others => '0' );
       when INV_RAM       =>          
         alu_cmd_o <= INV;            
         op_a_o    <= ram_data_i;     
         op_b_o    <= ( others => '0' );
       when COMP_RAM_ACC  =>          
         alu_cmd_o <= COMP;           
         op_a_o    <= acc_i;          
         op_b_o    <= ram_data_i;     
       when COMP_ROM_ACC  =>          
         alu_cmd_o <= COMP;           
         op_a_o    <= acc_i;          
         op_b_o    <= rom_data_i;     
       when COMP_ROM_RAM  =>          
         alu_cmd_o <= COMP;           
         op_a_o    <= ram_data_i;     
         op_b_o    <= rom_data_i;     
       when others        =>          
         alu_cmd_o <= NOP;            
         op_a_o    <= ( others => '0' );
         op_b_o    <= ( others => '0' );
    end case;
  end process p_alucore_mux;

  -- Multiplex the input data for all the functions not included in the
  -- alu core.
  p_ext_mux : process (ram_data_i,
                       rom_data_i,
                       acc_i,
		       cy_i,
                       cmd_i)
  begin
    case cmd_i is
       when DA =>
         dcml_data_o <= acc_i;
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '0';
         addsub_cy_o <= '0';
         opa_o       <= ( others => '0' );
         opb_o       <= ( others => '0' );
       when DIV_ACC_RAM =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= acc_i;
         dvsor_o     <= ram_data_i;
         addsub_o    <= '0';
         addsub_cy_o <= '0';
         opa_o       <= ( others => '0' );
         opb_o       <= ( others => '0' );
       when MUL_ACC_RAM =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= acc_i;
         mltplctr_o  <= ram_data_i;
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '0';
         addsub_cy_o <= '0';
         opa_o       <= ( others => '0' );
         opb_o       <= ( others => '0' );
       when INC_ACC =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '1';
         addsub_cy_o <= '0';
         opa_o       <= acc_i;
         opb_o       <= std_logic_vector(conv_unsigned(1, DWIDTH));
       when INC_RAM =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '1';
         addsub_cy_o <= '0';
         opa_o       <= ram_data_i;
         opb_o       <= std_logic_vector(conv_unsigned(1, DWIDTH));
       when DEC_ACC =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '0';
         addsub_cy_o <= '0';
         opa_o       <= acc_i;
         opb_o       <= std_logic_vector(conv_unsigned(1, DWIDTH));
       when DEC_RAM =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '0';
         addsub_cy_o <= '0';
         opa_o       <= ram_data_i;
         opb_o       <= std_logic_vector(conv_unsigned(1, DWIDTH));
       when SUB_ACC_RAM =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '0';
         addsub_cy_o <= cy_i((DWIDTH-1)/4);
         opa_o       <= acc_i;
         opb_o       <= ram_data_i;
       when SUB_ACC_ROM =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '0';
         addsub_cy_o <= cy_i((DWIDTH-1)/4);
         opa_o       <= acc_i;
         opb_o       <= rom_data_i;
       when ADD_ACC_RAM =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '1';
         addsub_cy_o <= '0';
         opa_o       <= acc_i;
         opb_o       <= ram_data_i;
       when ADD_ACC_ROM =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '1';
         addsub_cy_o <= '0';
         opa_o       <= acc_i;
         opb_o       <= rom_data_i;
       when ADDC_ACC_RAM =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '1';
         addsub_cy_o <= cy_i((DWIDTH-1)/4);
         opa_o       <= acc_i;
         opb_o       <= ram_data_i;
       when ADDC_ACC_ROM =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '1';
         addsub_cy_o <= cy_i((DWIDTH-1)/4);
         opa_o       <= acc_i;
         opb_o       <= rom_data_i;
       when others =>
         dcml_data_o <= ( others => '0' );
         mltplcnd_o  <= ( others => '0' );
         mltplctr_o  <= ( others => '0' );
         dvdnd_o     <= ( others => '0' );
         dvsor_o     <= ( others => '0' );
         addsub_o    <= '0';
         addsub_cy_o <= '0';
         opa_o       <= ( others => '0' );
         opb_o       <= ( others => '0' );
    end case;
  end process p_ext_mux;

  -- Multiplex the results for all the units contributing to the ALU.
  p_rslt_mux : process (ram_data_i,
                        cy_i,
                        ov_i,
                        product_i,
                        qutnt_i,
                        rmndr_i,
                        result_i,
                        new_cy_i,
			addsub_rslt_i,
			addsub_cy_i,
			addsub_ov_i,
                        dcml_data_i,
                        dcml_cy_i,
                        cmd_i)
  begin
    case cmd_i is
       when DA          =>
	 if (C_IMPL_DA /= 0) then
           result_a_o         <= dcml_data_i;
           result_b_o         <= ( others => '0' );
           cy_o               <= cy_i;
           cy_o((DWIDTH-1)/4) <= dcml_cy_i;
           ov_o               <= ov_i;
	 else  
           result_a_o         <= ( others => '0' );
           result_b_o         <= ( others => '0' );
           cy_o               <= conv_std_logic_vector(0,(DWIDTH-1)/4+1);
           ov_o               <= '0';
	 end if;
       when DIV_ACC_RAM =>
	 if (C_IMPL_DIV /= 0) then
           result_a_o         <= qutnt_i;
           result_b_o         <= rmndr_i;
           cy_o               <= conv_std_logic_vector(0,(DWIDTH-1)/4+1);
           if ram_data_i = conv_std_logic_vector(0,DWIDTH) then
             ov_o             <= '1';
           else
             ov_o             <= '0';
           end if;
	 else
           result_a_o         <= ( others => '0' );
           result_b_o         <= ( others => '0' );
           cy_o               <= conv_std_logic_vector(0,(DWIDTH-1)/4+1);
	   ov_o               <= '0';
	 end if;
       when MUL_ACC_RAM =>
	 if (C_IMPL_MUL /= 0) then
           result_a_o         <= product_i(DWIDTH-1 downto 0);
           result_b_o         <= product_i(DWIDTH*2-1 downto DWIDTH);
           cy_o               <= conv_std_logic_vector(0,(DWIDTH-1)/4+1);
           if product_i(DWIDTH*2-1 downto DWIDTH)
               = conv_std_logic_vector(0, DWIDTH) then
             ov_o             <= '0';
           else
             ov_o             <= '1';
           end if;
	 else
           result_a_o         <= ( others => '0' );
           result_b_o         <= ( others => '0' );
           cy_o               <= conv_std_logic_vector(0,(DWIDTH-1)/4+1);
           ov_o               <= '0';
	 end if;
       when SUB_ACC_RAM | SUB_ACC_ROM | ADD_ACC_RAM | ADD_ACC_ROM |
	    ADDC_ACC_RAM | ADDC_ACC_ROM  =>
         result_a_o         <= addsub_rslt_i;
         result_b_o         <= ( others => '0' );
         cy_o               <= addsub_cy_i;
         ov_o               <= addsub_ov_i;
       when INC_ACC | INC_RAM | DEC_ACC | DEC_RAM =>
         result_a_o         <= addsub_rslt_i;
         result_b_o         <= ( others => '0' );
         cy_o               <= cy_i;
         ov_o               <= addsub_ov_i;	 
       when others      =>
         result_a_o         <= result_i;
         result_b_o         <= ( others => '0' );
         cy_o               <= new_cy_i;
         ov_o               <= ov_i;
    end case;
  end process p_rslt_mux;

end rtl;
