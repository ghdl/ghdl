
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
-- $Id: ch_15_mem-fl.vhd,v 1.3 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library bv_utilities;

use bv_utilities.bv_arithmetic.all,
  std.textio.all;

architecture file_loaded of memory is
begin

  mem_behavior : process is

                           constant high_address : natural := mem_size - 1;

                         type memory_array is
                           array (natural range 0 to high_address / 4) of dlx_bv_word;

                         variable mem : memory_array;

                         variable byte_address, word_address : natural;
                         variable write_access : boolean;

                         procedure load is

                           file binary_file : text open read_mode is load_file_name;
                           variable L : line;
                           variable ch : character;
                           variable line_number : natural := 0;
                           variable addr : natural;
                           variable word : dlx_bv_word;

                           procedure read_hex_natural ( L : inout line;  n : out natural ) is
                             variable result : natural := 0;
                           begin
                             for i in 1 to 8 loop
                               read(L, ch);
                               if '0' <= ch and ch <= '9' then
                                 result := result*16 + character'pos(ch) - character'pos('0');
                               elsif 'A' <= ch and ch <= 'F' then
                                 result := result*16 + character'pos(ch) - character'pos('A') + 10;
                               elsif 'a' <= ch and ch <= 'f' then
                                 result := result*16 + character'pos(ch) - character'pos('a') + 10;
                               else
                                 report "Format error in file " & load_file_name
                                   & " on line " & integer'image(line_number) severity error;
                               end if;
                             end loop;
                             n := result;
                           end read_hex_natural;

                           procedure read_hex_word ( L : inout line;  word : out dlx_bv_word ) is
                             variable digit : natural;
                             variable r : natural := 0;
                           begin
                             for i in 1 to 8 loop
                               read(L, ch);
                               if '0' <= ch and ch <= '9' then
                                 digit := character'pos(ch) - character'pos('0');
                               elsif 'A' <= ch and ch <= 'F' then
                                 digit := character'pos(ch) - character'pos('A') + 10;
                               elsif 'a' <= ch and ch <= 'f' then
                                 digit := character'pos(ch) - character'pos('a') + 10;
                               else
                                 report "Format error in file " & load_file_name
                                   & " on line " & integer'image(line_number)
                                   severity error;
                               end if;
                               word(r to r+3) := natural_to_bv(digit, 4);
                               r := r + 4;
                             end loop;
                           end read_hex_word;

                         begin
                           while not endfile(binary_file) loop
                             readline(binary_file, L);
                             line_number := line_number + 1;
                             read_hex_natural(L, addr);
                             read(L, ch);  -- the space between addr and data
                             read_hex_word(L, word);
                             mem(addr / 4) := word;
                           end loop;
                         end load;

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
    load;  -- read binary memory image into memory array
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

end architecture file_loaded;
