-- fft.vhd
-- This file is part of bladeRF-wiphy.
--
-- Copyright (C) 2021 Nuand, LLC.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    use ieee.math_real.all;

entity fft is
   generic(
      PARALLEL   :  in  natural := 4;
      N          :  in  natural := 8;
      BITS       :  in  natural := 16
   );
   port(
      clock      :  in  std_logic;
      reset      :  in  std_logic;

      inverse    :  in  std_logic;
      in_real    :  in  std_logic_vector(BITS-1 downto 0);
      in_imag    :  in  std_logic_vector(BITS-1 downto 0);
      in_valid   :  in  std_logic;
      in_sop     :  in  std_logic;
      in_eop     :  in  std_logic;

      out_real   :  out std_logic_vector(BITS-1 downto 0);
      out_imag   :  out std_logic_vector(BITS-1 downto 0);
      out_error  :  out std_logic;
      out_valid  :  out std_logic;
      out_sop    :  out std_logic;
      out_eop    :  out std_logic
   );
end entity;

architecture mult of fft is
   type fft_out_t is record
      out_real   :  std_logic_vector(BITS-1 downto 0);
      out_imag   :  std_logic_vector(BITS-1 downto 0);
      out_error  :  std_logic;
      out_valid  :  std_logic;
      out_sop    :  std_logic;
      out_eop    :  std_logic;
   end record;
   type fft_out_arr_t is array(natural range <>) of fft_out_t;

   signal  fft_out      : fft_out_arr_t(0 to PARALLEL-1);

   signal  in_idx       : natural range 0 to PARALLEL;
   signal  out_idx      : natural range 0 to PARALLEL;
   signal  in_mask      : std_logic_vector(PARALLEL-1 downto 0);

begin

   sync : process(clock, reset)
      variable tmp_idx : natural range 0 to PARALLEL;
   begin
      if (reset = '1') then
         in_idx  <= 0;
         out_idx <= 0;
         in_mask <= std_logic_vector(to_unsigned(1, PARALLEL));
      elsif (rising_edge(clock)) then
         if (in_eop = '1') then
            if (in_idx = PARALLEL-1) then
               tmp_idx := 0;
            else
               tmp_idx := tmp_idx + 1;
            end if;
            in_mask <= std_logic_vector(shift_left(to_unsigned(1, PARALLEL), tmp_idx));
            in_idx  <= tmp_idx;
         end if;
         if (out_eop = '1') then
            if (out_idx = PARALLEL-1) then
               out_idx <= 0;
            else
               out_idx <= out_idx + 1;
            end if;
         end if;
      end if;
   end process;

   U_fft_gen: for i in 0 to PARALLEL-1 generate
      U_fft_inst : entity work.fft(arch)
         generic map(
            N    => N,
            BITS => BITS
         ) port map(
            clock      => clock,
            reset      => reset,
            inverse    => inverse,
            in_real    => in_real,
            in_imag    => in_imag,
            in_valid   => in_mask(i) and in_valid,
            in_sop     => in_mask(i) and in_sop,
            in_eop     => in_mask(i) and in_eop,
            out_real   => fft_out(i).out_real,
            out_imag   => fft_out(i).out_imag,
            out_error  => fft_out(i).out_error,
            out_valid  => fft_out(i).out_valid,
            out_sop    => fft_out(i).out_sop,
            out_eop    => fft_out(i).out_eop
         );
   end generate;

   process(fft_out, out_idx)
   begin
      out_real   <= fft_out(out_idx).out_real;
      out_imag   <= fft_out(out_idx).out_imag;
      out_error  <= fft_out(out_idx).out_error;
      out_valid  <= fft_out(out_idx).out_valid;
      out_sop    <= fft_out(out_idx).out_sop;
      out_eop    <= fft_out(out_idx).out_eop;
   end process;

end architecture mult;

architecture arch of fft is
   constant ADDR_BITS   : integer := integer(ceil(log2(real(N))));
   constant NUM_STAGES  : integer := integer(ceil(log2(real(N))));
   constant POSTBITS    : integer := 0;
   function PIPELINE_BITS return integer is
   begin
      return BITS + NUM_STAGES;
   end function;

   constant DATA_BITS   : integer := PIPELINE_BITS*2;

   type complex_sample_t is record
      i           : signed(PIPELINE_BITS-1 downto 0);
      q           : signed(PIPELINE_BITS-1 downto 0);
   end record;

   type complex_sample_arr_t is array(natural range <>) of complex_sample_t;
   function NULL_COMPLEX_SAMPLE return complex_sample_t is
      variable ret : complex_sample_t;
   begin
      ret.i := ( others => '0' );
      ret.q := ( others => '0' );
      return(ret);
   end function;

   type mem_bank_ctrl_t is record
      acc        : std_logic;
      write      : std_logic;
      solo       : std_logic;

      addr_a     : std_logic_vector(ADDR_BITS-1 downto 0);
      in_a       : std_logic_vector(DATA_BITS-1 downto 0);
      data_a     : std_logic_vector(DATA_BITS-1 downto 0);
      addr_b     : std_logic_vector(ADDR_BITS-1 downto 0);
      in_b       : std_logic_vector(DATA_BITS-1 downto 0);
      data_b     : std_logic_vector(DATA_BITS-1 downto 0);
   end record;

   function slv_to_cst(x : std_logic_vector) return complex_sample_t is
      variable ret : complex_sample_t;
   begin
      ret.i := resize(signed(x(x'high-1 downto PIPELINE_BITS)), PIPELINE_BITS);
      ret.q := resize(signed(x(PIPELINE_BITS-1   downto 0)),    PIPELINE_BITS);
      return(ret);
   end function;

   function reverse_bit_order(x : unsigned) return std_logic_vector is
      variable ret : std_logic_vector(x'range);
   begin
      for i in x'range loop
         ret(i) := x(x'high - i);
      end loop;
      return(ret);
   end function;

   function NULL_MEM_BANK_CTRL return mem_bank_ctrl_t is
      variable ret : mem_bank_ctrl_t;
   begin
      ret.acc      := '0';
      ret.solo     := '0';
      ret.write    := '0';
      ret.addr_a   := ( others => '0' );
      ret.in_a     := ( others => '0' );
      ret.data_a   := ( others => '0' );
      ret.addr_b   := ( others => '0' );
      ret.in_b     := ( others => '0' );
      ret.data_b   := ( others => '0' );
      return(ret);
   end function;

   type fsm_t is (IDLE, LOAD, FIRST_STAGE, RUN_STAGE, WAIT_STAGE, READ_OUT, STOP, RESET_STAGE);
   type r_fsm_t is (IDLE, PASSTHROUGH, MEM_READ);

   type mem_bank_ctrl_arr_t is array(natural range <>) of mem_bank_ctrl_t;
   type state_t is record
      fsm          : fsm_t;
      rfsm         : r_fsm_t;
      count        : integer range 0 to N+1;
      bf_ready     : std_logic;
      iter         : integer range 0 to N+2;

      mbc          : mem_bank_ctrl_arr_t(1 downto 0);
      buffer_idx   : std_logic;
      write_idx    : unsigned(ADDR_BITS-1 downto 0);

      stage        : integer range 0 to N;
      twiddle_idx  : unsigned(ADDR_BITS-2 downto 0);
      tw           : complex_sample_t;

      sop          : std_logic;
      eop          : std_logic;
      N2_sample    : complex_sample_t;
      N2_sample_r  : complex_sample_t;
      out_sample   : complex_sample_t;
      valid        : std_logic;
   end record;

   type butter_fly_t is record
      A, B, TW     : complex_sample_t;
      addr_a       : std_logic_vector(ADDR_BITS-1 downto 0);
      addr_b       : std_logic_vector(ADDR_BITS-1 downto 0);
      valid        : std_logic;
   end record;

   type butter_fly_arr_t is array(natural range <>) of butter_fly_t;
   signal bf_pl    : butter_fly_arr_t(0 to 3);

   function NULL_BF_T return butter_fly_t is
      variable ret : butter_fly_t;
   begin
      ret.A      := NULL_COMPLEX_SAMPLE;
      ret.B      := NULL_COMPLEX_SAMPLE;
      ret.TW     := NULL_COMPLEX_SAMPLE;
      ret.addr_a := ( others => '0' );
      ret.addr_b := ( others => '0' );
      ret.valid  := '0';
      return(ret);
   end function;

   function shift_sample(x : complex_sample_t ; enable : std_logic) return complex_sample_t is
      variable ret : complex_sample_t;
   begin
      if (enable = '0') then
         ret.i := shift_right(x.i, POSTBITS*NUM_STAGES);
         ret.q := shift_right(x.q, POSTBITS*NUM_STAGES);
      else
         ret.i := shift_right(x.i, NUM_STAGES+POSTBITS*NUM_STAGES);
         ret.q := shift_right(x.q, NUM_STAGES+POSTBITS*NUM_STAGES);
      end if;
      return(ret);
   end function;

   function NULL_STATE_T return state_t is
      variable ret : state_t;
   begin
      ret.fsm    := IDLE;
      ret.rfsm   := IDLE;
      for i in ret.mbc'range loop
         ret.mbc(i) := NULL_MEM_BANK_CTRL;
      end loop;

      ret.count        := 0;

      ret.iter         := 0;
      ret.bf_ready     := '0';

      ret.buffer_idx   := '0';
      ret.write_idx    := ( others => '0' );

      ret.stage        := 0;

      ret.twiddle_idx  := ( others => '0' );

      ret.tw.i         := ( others => '0' );
      ret.tw.q         := ( others => '0' );

      ret.sop          := '0';
      ret.eop          := '0';
      ret.valid        := '0';
      ret.N2_sample    := NULL_COMPLEX_SAMPLE;
      ret.N2_sample_r  := NULL_COMPLEX_SAMPLE;
      ret.out_sample   := NULL_COMPLEX_SAMPLE;
      return(ret);
   end function;

   function rc_func(x : real) return real is
   begin
      if (x < 0.0) then
         return(ceil(x));
      else
         return(floor(x));
      end if;
   end function;

   function gen_roots_of_unity return complex_sample_arr_t is
      variable t_s, t_c : real := 0.0;
      variable ret : complex_sample_arr_t(((N/2)-1) downto 0);
   begin
      for i in 0 to (N/2)-1 loop
         t_c := rc_func(cos(real(MATH_2_PI * real(i) / real(N))) * real(2**(BITS-1) - 1));
         t_s := rc_func(sin(real(MATH_2_PI * real(i) / real(N))) * real(2**(BITS-1) - 1));
         ret(i).i := to_signed(integer(t_c), PIPELINE_BITS);
         ret(i).q := to_signed(integer(t_s), PIPELINE_BITS);
         --report integer'image(i) & " = " & integer'image(integer(t_c)) &
         --            " , " & integer'image(integer(t_s)) ;
      end loop;

      return(ret);
   end function;

   constant TLUT : complex_sample_arr_t(((N/2)-1) downto 0) := gen_roots_of_unity;

   signal current, future : state_t := NULL_STATE_T;

   signal muxed_mbc :  mem_bank_ctrl_arr_t(1 downto 0);

   signal data_mbc  :  mem_bank_ctrl_arr_t(1 downto 0);
   signal curr_data :  mem_bank_ctrl_t;

   signal mix       :  complex_sample_t;
   signal T_A, T_B  :  complex_sample_t;

   signal comp_mbc  :  mem_bank_ctrl_t;
begin
   U_mem_banks: for i in 0 to 1 generate
      U_mem_bank: entity work.dual_port_ram(synth)
         generic map(
            ADDR_BITS  => ADDR_BITS,
            DATA_BITS  => DATA_BITS
         )
         port map(
            clock      => clock,
            reset      => reset,

            acc        => muxed_mbc(i).acc,
            solo       => muxed_mbc(i).solo,
            write      => muxed_mbc(i).write,

            addr_a     => muxed_mbc(i).addr_a,
            in_a       => muxed_mbc(i).in_a,
            data_a     => data_mbc(i).data_a,

            addr_b     => muxed_mbc(i).addr_b,
            in_b       => muxed_mbc(i).in_b,
            data_b     => data_mbc(i).data_b
         );
   end generate;

   comp_mbc.addr_a <= bf_pl(3).addr_a;
   comp_mbc.in_a   <= std_logic_vector(T_A.i) & std_logic_vector(T_A.q);
   comp_mbc.addr_b <= bf_pl(3).addr_b;
   comp_mbc.in_b   <= std_logic_vector(T_B.i) & std_logic_vector(T_B.q);
   comp_mbc.acc    <= bf_pl(3).valid;
   comp_mbc.write  <= bf_pl(3).valid;
   comp_mbc.solo   <= '0';

   sync : process(clock, reset)
   begin
      if (reset = '1') then
         current <= NULL_STATE_T;
         bf_pl(1).addr_a <= ( others => '0' );
         bf_pl(1).addr_b <= ( others => '0' );
         bf_pl(2) <= NULL_BF_T;
         bf_pl(3) <= NULL_BF_T;
      elsif (rising_edge(clock)) then
         current <= future;

         bf_pl(1).valid  <= current.bf_ready;
         bf_pl(1).addr_a <= current.mbc(0).addr_a;
         bf_pl(1).addr_b <= current.mbc(0).addr_b;
         bf_pl(2)        <= bf_pl(1);
         bf_pl(3)        <= bf_pl(2);
      end if;
   end process;

   butterfly : process(clock, reset)
   begin
      if (rising_edge(clock)) then
         mix.i <= resize(shift_right(bf_pl(1).B.i * bf_pl(1).TW.i - bf_pl(1).B.q * bf_pl(1).TW.q, BITS-1-POSTBITS), PIPELINE_BITS);
         mix.q <= resize(shift_right(bf_pl(1).B.i * bf_pl(1).TW.q + bf_pl(1).B.q * bf_pl(1).TW.i, BITS-1-POSTBITS), PIPELINE_BITS);
         T_A.i <= shift_left(bf_pl(2).A.i, POSTBITS) + mix.i;
         T_A.q <= shift_left(bf_pl(2).A.q, POSTBITS) + mix.q;
         T_B.i <= shift_left(bf_pl(2).A.i, POSTBITS) - mix.i;
         T_B.q <= shift_left(bf_pl(2).A.q, POSTBITS) - mix.q;
      end if;
   end process;

   out_sop   <= current.sop;
   out_valid <= current.valid;
   out_eop   <= current.eop;
   out_error <= '1' when current.fsm = STOP else '0';

   out_real <= std_logic_vector(resize(current.out_sample.i, BITS));
   out_imag <= std_logic_vector(resize(current.out_sample.q, BITS));

   comb : process(all)
      variable tmp_addr_a, tmp_addr_b : unsigned(ADDR_BITS-1 downto 0);
      variable ones_reg : unsigned(ADDR_BITS-2 downto 0);
      variable tmp_tw   : complex_sample_t;
   begin
      tmp_tw := current.tw;
      if (inverse = '1' ) then
         bf_pl(1).TW     <= tmp_tw;
      else
         bf_pl(1).TW.i   <= tmp_tw.i;
         bf_pl(1).TW.q   <= -tmp_tw.q;
      end if;
      bf_pl(1).A      <= slv_to_cst(curr_data.data_a);
      if (current.fsm = FIRST_STAGE or (current.fsm = WAIT_STAGE and current.stage = 0)) then
         bf_pl(1).B      <= current.N2_sample_r;
      else
         bf_pl(1).B      <= slv_to_cst(curr_data.data_b);
      end if;
      if (current.buffer_idx = '0') then
         muxed_mbc(0) <= current.mbc(0); -- during RUN_STAGES: READ
         curr_data    <= data_mbc(0);

         muxed_mbc(1) <= comp_mbc; -- during RUN_STAGES: WRITE
      else
         muxed_mbc(0) <= comp_mbc; -- during RUN_STAGES: WRITE

         muxed_mbc(1) <= current.mbc(0); -- during RUN_STAGES: READ
         curr_data    <= data_mbc(1);
      end if;

      future <= current;

      for i in future.mbc'range loop
         future.mbc(i) <= NULL_MEM_BANK_CTRL;
      end loop;
      future.bf_ready    <= '0';
      future.sop         <= '0';
      future.eop         <= '0';
      future.valid       <= '0';

      ones_reg := ( others => '1' );

      -- note, this updates on the next cycle
      if (current.fsm = FIRST_STAGE or current.fsm = RUN_STAGE or current.fsm = WAIT_STAGE) then
         tmp_tw := TLUT(to_integer(current.twiddle_idx));
         future.tw <= tmp_tw;
         future.twiddle_idx <= to_unsigned(current.iter, ones_reg'high+1)
                                 and shift_left(ones_reg, NUM_STAGES-1-current.stage);
      end if;

      future.N2_sample_r <= current.N2_sample;

      case current.fsm is
         when IDLE =>
            if (in_sop = '1') then
               future.fsm <= LOAD;
               if (in_valid = '1') then
                  future.mbc(0).addr_b <= std_logic_vector(to_unsigned(1, ADDR_BITS));
                  future.mbc(0).addr_a <= reverse_bit_order(current.write_idx);
                  future.mbc(0).in_a   <= std_logic_vector(resize(signed(in_real), PIPELINE_BITS) & resize(signed(in_imag), PIPELINE_BITS));
                  future.mbc(0).acc    <= '1';
                  future.mbc(0).solo   <= '1';
                  future.mbc(0).write  <= '1';
                  future.write_idx <= current.write_idx + 1;
                  future.count <= 1;
               end if;
            end if;
         when LOAD =>
            if (in_valid = '1') then
               future.write_idx <= current.write_idx + 1;
               future.count <= current.count + 1;
               if (current.write_idx = (N/2)) then
                  future.mbc(0).addr_a <= reverse_bit_order(current.write_idx-32);
                  future.mbc(0).addr_b <= reverse_bit_order(current.write_idx);
                  future.N2_sample.i  <= resize(signed(in_real), PIPELINE_BITS);
                  future.N2_sample.q  <= resize(signed(in_imag), PIPELINE_BITS);
                  future.bf_ready    <= '1';
                  future.mbc(0).acc    <= '1';
                  future.fsm <= FIRST_STAGE;
               else
                  future.mbc(0).addr_b <= std_logic_vector(to_unsigned(1, ADDR_BITS));
                  future.mbc(0).addr_a <= reverse_bit_order(current.write_idx);
                  future.mbc(0).in_a   <= std_logic_vector(resize(signed(in_real), PIPELINE_BITS) & resize(signed(in_imag), PIPELINE_BITS));
                  future.mbc(0).acc    <= '1';
                  future.mbc(0).solo   <= '1';
                  future.mbc(0).write  <= '1';
               end if;
            end if;
            if (in_eop = '1') then
               future.fsm <= STOP;
            end if;
         when FIRST_STAGE =>
            if (in_valid = '1') then
               future.count <= current.count + 1;
               future.write_idx <= current.write_idx + 1;
               future.bf_ready    <= '1';
               future.mbc(0).addr_a <= reverse_bit_order(current.write_idx-32);
               future.mbc(0).addr_b <= reverse_bit_order(current.write_idx);
               future.mbc(0).acc    <= '1';
               future.N2_sample.i <= resize(signed(in_real), PIPELINE_BITS);
               future.N2_sample.q <= resize(signed(in_imag), PIPELINE_BITS);
               if (current.write_idx = N-1) then
                  future.iter <= 3;
                  future.fsm <= WAIT_STAGE;
                  if (in_eop = '0') then
                     future.fsm <= STOP;
                  end if;
               else
                  if (in_eop = '1') then
                     future.fsm <= STOP;
                  end if;
               end if;
            end if;
         when RUN_STAGE =>
            future.mbc(0).acc  <= '1';
            future.bf_ready    <= '1';
            tmp_addr_a := rotate_left(to_unsigned(current.iter*2, ADDR_BITS), current.stage);
            tmp_addr_b := rotate_left(to_unsigned(current.iter*2+1, ADDR_BITS), current.stage);

            future.mbc(0).addr_a <= std_logic_vector(tmp_addr_a);
            future.mbc(0).addr_b <= std_logic_vector(tmp_addr_b);
            if (current.iter = (N/2)-1) then
               future.iter <= 3;
               future.fsm  <= WAIT_STAGE;
            else
               future.iter <= current.iter + 1;
            end if;

         when WAIT_STAGE =>
            if (current.iter = 0) then
               future.buffer_idx <= not current.buffer_idx;
               if (current.stage < NUM_STAGES-1) then
                  future.stage <= current.stage + 1;
                  future.fsm   <= RUN_STAGE;
                  future.iter  <= 0;
               else
                  future.fsm <= READ_OUT;
                  future.iter  <= N/2 + 2;
                  future.mbc(0).addr_a <= std_logic_vector(to_unsigned(N/2+1, ADDR_BITS));
                  future.mbc(0).acc    <= '1';
                  future.mbc(0).solo   <= '1';
               end if;
            else
               future.iter <= current.iter - 1;
            end if;
         when READ_OUT =>
            if (current.iter = N+1) then
               future.fsm <= RESET_STAGE;
               future.eop <= '1';
            end if;
            if (current.iter < N) then
               future.mbc(0).addr_a <= std_logic_vector(to_unsigned(current.iter, ADDR_BITS));
            end if;
            future.iter <= current.iter + 1;
            future.mbc(0).acc    <= '1';
            future.mbc(0).solo   <= '1';

         when others =>
            future <= NULL_STATE_T;
      end case;

      case current.rfsm is
         when IDLE =>
            if (current.fsm = RUN_STAGE and current.stage = NUM_STAGES - 1) then
               future.rfsm <= PASSTHROUGH;
            end if;
         when PASSTHROUGH =>
            if (current.fsm = RUN_STAGE) then
               if (current.iter = 4) then
                  future.N2_sample <= T_B;
                  future.sop <= '1';
               end if;
            end if;

            if (current.iter > 3 or current.fsm = WAIT_STAGE) then
               if (current.iter = (N/2)+2) then
                  future.out_sample <= shift_sample(current.N2_sample, inverse);
               else
                  future.out_sample <= shift_sample(T_A, inverse);
               end if;
               future.valid <= '1';
            end if;

            if (current.fsm = READ_OUT) then
               future.rfsm <= MEM_READ;
            end if;
         when MEM_READ =>
            if (current.iter = N+1) then
               future.rfsm <= IDLE;
            end if;
            future.out_sample <= shift_sample(slv_to_cst(curr_data.data_a), inverse);
            future.valid <= '1';
         when others =>
            future <= NULL_STATE_T;
      end case;

   end process;


end architecture;
