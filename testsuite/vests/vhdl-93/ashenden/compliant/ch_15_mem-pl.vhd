
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- ---------------------------------------------------------------------
--
-- $Id: ch_15_mem-pl.vhd,v 1.3 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library bv_utilities;

use bv_utilities.bv_arithmetic.all;

architecture preloaded of memory is

begin 

  mem_behavior : process is

                           constant high_address : natural := mem_size - 1;

                         type memory_array is
                           array (natural range 0 to high_address / 4) of dlx_bv_word;

                         variable mem : memory_array
                           := ( X"20020000",	 --           addi  r2, r0, 0
                                X"ac020018",  -- loop:     sw    counter(r0), r2
                                X"20420001",  --           addi  r2, r2, 1
                                X"6441000a",  --           snei  r1, r2, 10
                                X"1420fff0",  --           bnez  r1, loop
                                X"44000000",  --           trap  0
                                X"00000000",  -- counter:  .word 0
                                others => X"00000000" );

                         variable byte_address, word_address : natural;
                         variable write_access : boolean;


                         procedure do_write is
                           subtype ls_2_bits is bit_vector(1 downto 0);
                         begin
                           case width is
                             when dlx_mem_width_word =>
                               mem(word_address) := to_bitvector(d);
                             when dlx_mem_width_halfword =>
                               if To_bit(a(1)) = '0' then  -- ms half word
                                 mem(word_address)(0 to 15) := to_bitvector( d(0 to 15) );
                               else  -- ls half word
                                 mem(word_address)(16 to 31) := to_bitvector( d(16 to 31) );
                               end if;
                             when dlx_mem_width_byte =>
                               case ls_2_bits'(To_bitvector(a(1 downto 0))) is
                                 when b"00" =>
                                   mem(word_address)(0 to 7) := to_bitvector( d(0 to 7) );
                                 when b"01" =>
                                   mem(word_address)(8 to 15) := to_bitvector( d(8 to 15) );
                                 when b"10" =>
                                   mem(word_address)(16 to 23) := to_bitvector( d(16 to 23) );
                                 when b"11" =>
                                   mem(word_address)(24 to 31) := to_bitvector( d(24 to 31) );
                               end case;
                             when others =>
                               report "illegal width indicator in write" severity error;
                           end case;
                         end do_write;

                         procedure do_read is
                         begin
                           d <= To_X01( mem(word_address) );
                         end do_read;

  begin
    -- initialize outputs
    d <= disabled_dlx_word;
    ready <= '0';

    -- process memory cycles
    loop
      -- wait for a command, valid on leading edge of phi2
      wait on phi2 until rising_edge(phi2) and To_bit(mem_enable) = '1';

      -- decode address and perform command if selected
      byte_address := bv_to_natural(To_bitvector(a));
      write_access := To_bit(write_enable) = '1';
      if byte_address <= high_address then
        word_address := byte_address / 4;
        if write_access then -- write cycle
          do_write;
          wait for Tac_first;  -- write access time, 1st cycle
        else -- read cycle
          wait for Tac_first;  -- read access time, 1st cycle
          do_read;
        end if;
        -- ready synchronous with phi2
        wait until rising_edge(phi2);
        ready <= '1' after Tpd_clk_out;
        wait until falling_edge(phi2);
        ready <= '0' after Tpd_clk_out;
        --  do subsequent cycles in burst
        while To_bit(burst) = '1' loop
	  word_address := (word_address + 1) mod (mem_size / 4);
          wait until rising_edge(phi2);
          if write_access then -- write cycle
            do_write;
            wait for Tac_burst;  -- write access time, burst cycle
          else -- read cycle
            wait for Tac_burst;  -- read access time, burst cycle
            do_read;
          end if;
          -- ready synchronous with phi2
          wait until rising_edge(phi2);
          ready <= '1' after Tpd_clk_out;
          wait until falling_edge(phi2);
          ready <= '0' after Tpd_clk_out;
        end loop;
        if not write_access then  -- was read
          d <= disabled_dlx_word after Tpd_clk_out;
        end if;
      end if;
    end loop;
  end process mem_behavior;

end architecture preloaded;
