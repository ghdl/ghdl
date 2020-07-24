-- ======================================================================
-- AES encryption/decryption
-- Copyright (C) 2019 Torsten Meissner
-------------------------------------------------------------------------
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-- ======================================================================


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.aes_pkg.all;



entity aes_enc is
  generic (
    design_type : string  := "ITER"
  );
  port (
    reset_i     : in  std_logic;                   -- async reset
    clk_i       : in  std_logic;                   -- clock
    key_i       : in  std_logic_vector(0 to 127);  -- key input
    data_i      : in  std_logic_vector(0 to 127);  -- data input
    valid_i     : in  std_logic;                   -- input key/data valid flag
    accept_o    : out std_logic;
    data_o      : out std_logic_vector(0 to 127);  -- data output
    valid_o     : out std_logic;                   -- output data valid flag
    accept_i    : in  std_logic
  );
end entity aes_enc;



architecture rtl of aes_enc is


begin


  IterG : if design_type = "ITER" generate


    signal s_round : t_enc_rounds;


  begin


    CryptP : process (reset_i, clk_i) is
      variable v_state : t_datatable2d;
      variable v_key   : t_key;
    begin
      if (reset_i = '0') then
        v_state  := (others => (others => (others => '0')));
        v_key    := (others => (others => '0'));
        s_round  <= 0;
        accept_o <= '0';
        data_o   <= (others => '0');
        valid_o  <= '0';
      elsif (rising_edge(clk_i)) then
        case s_round is

          when 0 =>
            accept_o <= '1';
            if (accept_o = '1' and valid_i = '1') then
              accept_o <= '0';
              v_state  := set_state(data_i);
              v_key    := set_key(key_i);
              s_round  <= s_round + 1;
            end if;

          when 1 =>
            v_state := addroundkey(v_state, v_key);
            v_key   := key_round(v_key, s_round-1);
            s_round <= s_round + 1;

          when t_enc_rounds'high-1 =>
            v_state := subbytes(v_state);
            v_state := shiftrow(v_state);
            v_state := addroundkey(v_state, v_key);
            s_round <= s_round + 1;
            -- set data & valid to save one cycle
            valid_o <= '1';
            data_o  <= get_state(v_state);

          when t_enc_rounds'high =>
            if (valid_o = '1' and accept_i = '1') then
              valid_o <= '0';
              data_o  <= (others => '0');
              s_round <= 0;
              -- Set accept to save one cycle
              accept_o <= '1';
            end if;

          when others =>
            v_state := subbytes(v_state);
            v_state := shiftrow(v_state);
            v_state := mixcolumns(v_state);
            v_state := addroundkey(v_state, v_key);
            v_key   := key_round(v_key, s_round-1);
            s_round <= s_round + 1;

        end case;
      end if;
    end process CryptP;


    psl : block is

      signal s_key , s_din, s_dout : std_logic_vector(0 to 127) := (others => '0');

    begin

      process (clk_i) is
      begin
        if (rising_edge(clk_i)) then
          s_key  <= key_i;
          s_din  <= data_i;
          s_dout <= data_o;
        end if;
      end process;

      default clock is rising_edge(clk_i);

      -- initial reset
      restrict {not reset_i; reset_i[+]}[*1];

      -- constraints
      assume always (valid_i and not accept_o -> next valid_i);
      assume always (valid_i and not accept_o -> next key_i = s_key);
      assume always (valid_i and not accept_o -> next data_i = s_din);

      ACCEPTO_c : cover {accept_o};
      ACCEPT_IN_ROUND_0_ONLY_a : assert always (accept_o -> s_round = 0);

      VALIDI_AND_ACCEPTO_c : cover {valid_i and accept_o};
      ACCEPT_OFF_WHEN_VALID_a : assert always (valid_i and accept_o -> next not accept_o);

      VALIDO_c : cover {valid_o};
      VALID_IN_LAST_ROUND_ONLY_a : assert always (valid_o -> s_round = t_enc_rounds'high);

      VALIDO_AND_ACCEPTI_c : cover {valid_o and accept_i};
      VALID_OFF_WHEN_ACCEPTED_a : assert always (valid_o and accept_i -> next not valid_o);

      VALIDO_AND_NOT_ACCEPTI_c : cover {valid_o and not accept_i};
      VALID_STABLE_WHEN_NOT_ACCEPTED_a : assert always (valid_o and not accept_i -> next valid_o);
      DATA_STABLE_WHEN_NOT_ACCEPTED_a : assert always (valid_o and not accept_i -> next data_o = s_dout);

    end block psl;


  end generate IterG;



end architecture rtl;
