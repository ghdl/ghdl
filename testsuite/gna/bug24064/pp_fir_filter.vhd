--------------------------------------------------------------------------------
--! @file
--! @brief pp_fir_filter.
--!        This implements a poly-phase fir filter that can be used for
--!        rational resampling or rational sample delay.
--!        The taps of the FIR filter are generated at compile time and start
--!        as a Hann-windowed sinc function.  0-phase offset is then normalized
--!        to be 0.98 amplitude.
--!        The generics determine the resolution of the fir-filter, as well as
--!        as the number of phases.
--------------------------------------------------------------------------------
library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    use ieee.math_real.all;
library work;
    use work.er_pack.all;

entity pp_fir_filter is
    generic (
        --! The width of each tap in bits
        taps_width_g    : natural :=  16;
        --! The number of lobes.  This is basically the number of taps per filter
        num_lobes_g     : natural :=   8;
        --! The number of parallel channels
        num_channels_g  : natural :=   1;
        --! The number of taps per lobe
        taps_per_lobe_g : natural := 512;
        --! The number of taps to skip to get to the next tap
        step_size_g     : natural := 512);
    port (
        -- standard ports
        clk_i : in  std_logic;
        rst_i : in  std_logic;

        -- input data ports
        --! Run the filter without taking another sample
        run_i     : in  std_logic;
        phase_i   : in  std_logic_vector(log2(taps_per_lobe_g) downto 0);
        data_en_i : in  std_logic;
        data_i    : in  std_logic_vector(num_channels_g*taps_width_g-1 downto 0);

        -- output data ports
        data_o    : out std_logic_vector(num_channels_g*taps_width_g-1 downto 0);
        data_en_o : out std_logic);
end entity pp_fir_filter;

architecture behavior of pp_fir_filter is
    ----------------------------------------------------------------------------
    -- Types, Subtypes, and Constants
    ----------------------------------------------------------------------------
    subtype word_t  is signed(1*taps_width_g-1 downto 0);
    subtype dword_t is signed(2*taps_width_g-1 downto 0);
    subtype save_range is natural range 2*taps_width_g-2 downto 1*taps_width_g-1;
    type word_vector_t  is array (integer range <>) of word_t;
    type dword_vector_t is array (integer range <>) of dword_t;
    type rom_t          is array (integer range <>) of signed(data_i'range);

    -- The state machine deals with the MACCs
    type state_type is (
        idle_state,  -- Waiting for input signal
        load_state,  -- Load the sample into the input ram
        mult_state,  -- First multiply does not accumulate product
        macc_state,  -- P += A*B
        save_state); -- Save the output
    type dsp_opcode_type is (
        clear,       -- P  = 0
        mult,        -- P  = A*B
        macc,        -- P += A*B
        hold);       -- P  = P
    constant round_val  : dword_t := shift_left(to_signed(1, dword_t'length), taps_width_g-2);

    -- We want the phase offset to be in relation to the middle of the center
    -- lobe.  For this reason, we will need to determine the offset of the first
    -- sample in relation to the step_size, taps_per_lobe, and the number of
    -- lobes
    constant phase_offset_c : natural :=
--      (num_lobes_g * (taps_per_lobe_g - step_size_g+1)) mod taps_per_lobe_g;
        (num_lobes_g/2 * (taps_per_lobe_g - step_size_g));
    constant num_regs_c : natural :=
--      (num_lobes_g * (taps_per_lobe_g / step_size_g));
        (num_lobes_g);

    ----------------------------------------------------------------------------
    -- functions
    ----------------------------------------------------------------------------
    function load_sinc_rom (
        taps_per_lobe : natural;
        num_lobes     : natural)
    return word_vector_t is
        -- The returned ram
        variable rom      : word_vector_t(0 to taps_per_lobe * num_lobes-1);

        -- Stuff for the actual sinc calculation
        variable real_rom : real_vector(rom'range);
        variable half     : real := real(rom'length/2);
        variable nm1      : real := real(rom'length-1);
        variable phase    : real;
        variable sinc     : real;
        variable hann     : real;

        -- for power calculation
        variable power : real;
    begin
        ------------------------------------------------------------------------
        -- Tap generation
        ------------------------------------------------------------------------
        for idx in real_rom'range loop
            -- Determine the phase, but multiply it by PI to get the correct
            -- phase shift
            phase := math_pi * (real(idx) - half) / real(taps_per_lobe);

            -- Don't divide by zero
            if phase = 0.0 then
                sinc := 1.0;
            else
                sinc := sin(phase) / phase;
            end if;

            -- Multiply it by a hann window
            hann := 0.5 * (1.0 - cos(2.0*math_pi*real(idx)/nm1));

            -- Put it in the rom
            real_rom(idx) := sinc*hann;
        end loop;

        ------------------------------------------------------------------------
        -- Energy measurement
        ------------------------------------------------------------------------
        -- Now that the ram is complete, we still need to make sure that we
        -- scale everything to be a power of one.  This is to make sure that we
        -- don't overflow during the actual addition.
        power := 0.0;
        for idx in 0 to num_regs_c-1 loop
            power := power + real_rom(phase_offset_c + idx*step_size_g);
        end loop;

        ------------------------------------------------------------------------
        -- Normalization
        ------------------------------------------------------------------------
        -- Now put it in the actual ram
        for idx in rom'range loop
            real_rom(idx) := real_rom(idx) * (0.98 / power);
            rom     (idx) := signed(to_slv(real_rom(idx), word_t'length));
        end loop;

        -- return it
        return rom;
    end function load_sinc_rom;

    -----------------------------------------------------------------------------
    constant taps_rom : word_vector_t := load_sinc_rom(taps_per_lobe_g, num_lobes_g);

    ----------------------------------------------------------------------------
    -- Signals
    ----------------------------------------------------------------------------
    signal phase_reg : natural;
    signal data_reg  : std_logic_vector(data_i'range);

    signal state      : state_type;
    signal dsp_opcode : dsp_opcode_type;

    -- DSP Signals
    signal a : word_vector_t (0 to num_channels_g-1);
    signal b : word_t;
    signal p : dword_vector_t(0 to num_channels_g-1);
    signal r : word_vector_t (0 to num_channels_g-1);

    -- RAM/ROM Signals
    signal taps_addr      : natural;
    signal next_taps_addr : natural;
    signal z_addr         : natural;
    signal z_ram          : rom_t(0 to num_regs_c-1);
    signal z_ram_en       : std_logic;

    -- Quantization signals
    signal q : dword_vector_t(0 to num_channels_g-1);

    -- for internal testing
    signal rom_data_test : word_t;
    signal rom_addr_test : natural;

--------------------------------------------------------------------------------
begin
--------------------------------------------------------------------------------
    -- The actual fir filter part
    -----------------------------------------------------------------------------
    -- Direct signal assignments
    -----------------------------------------------------------------------------
    a_gen : for idx in 0 to num_channels_g-1 generate
        -- Get the input for the multiplication
        a(idx) <= z_ram(z_addr)((idx+1)*taps_width_g-1 downto idx*taps_width_g);

        -- Since the rounding is combinational, we can sum it up here
        q(idx) <= p(idx) + round_val;

        -- Now the data out
        data_o((idx+1)*taps_width_g-1 downto idx*taps_width_g) <=
            std_logic_vector(r(idx));
    end generate a_gen;

    -- This one is easy
    b <= taps_rom(taps_addr);      -- Select MUX

    -----------------------------------------------------------------------------
    -- FIR process controls the main state machine behind the serial FIR
    -----------------------------------------------------------------------------
    fsm_proc : process(clk_i)
        variable idx_hi : natural;
        variable idx_lo : natural;
    begin
        if rising_edge(clk_i) then
            if rst_i = '1' then
                state          <= idle_state;
                dsp_opcode     <= clear;
                z_ram_en       <= '0';
                z_addr         <=  0 ;
                taps_addr      <=  0 ;
                next_taps_addr <=  0 ;
                data_en_o      <= '0';
--              data_o         <= (others => '0');
            else
                -- Default cases
                z_ram_en  <= '0';
                data_en_o <= '0';
                next_taps_addr <= next_taps_addr + step_size_g;

                -- Other cases
                case state is
                    -----------------------------------------------------------------
                    when idle_state =>
                        dsp_opcode <= clear;
                        z_addr     <=  0 ;
                        taps_addr  <=  0 ;
                        if data_en_i = '1' or run_i = '1' then
                            z_ram_en  <= data_en_i;
                            state     <= load_state;
                            phase_reg <= phase_offset_c + to_integer(unsigned(phase_i));
                            data_reg  <= data_i;
                        end if;
                    -----------------------------------------------------------------
                    when load_state =>
                        dsp_opcode     <= clear;
                        z_addr         <=  0 ;
                        taps_addr      <= phase_reg;
                        next_taps_addr <= phase_reg;
                        state          <= mult_state;
                    -----------------------------------------------------------------
                    when mult_state =>
                        dsp_opcode <= mult;
                        z_addr     <=  0 ;
                        taps_addr  <= phase_reg;
                        state      <= macc_state;
                    -----------------------------------------------------------------
                    when macc_state =>
                        dsp_opcode <= macc;

                        -- The delayed version of the incoming signal
--                      if next_taps_addr >= taps_rom'length then
                        if z_addr = z_ram'high then
                            state <= save_state;
                        else
                            z_addr    <= z_addr + 1;
                            taps_addr <= next_taps_addr;
                        end if;
                    -----------------------------------------------------------------
                    when save_state =>
                        dsp_opcode <= macc;
                        z_addr     <=  0 ;
                        data_en_o  <= '1';
                        state      <= idle_state;
                        for idx in q'range loop
                            r(idx) <= q(idx)(save_range);
                        end loop;
                    -----------------------------------------------------------------
                end case;
            end if;
        end if;
    end process fsm_proc;

    -----------------------------------------------------------------------------
    -- DSP48 process emulates a DSP48 (partially)
    -----------------------------------------------------------------------------
    alu_proc : process(clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i = '1' then
                p <= (others => (others => '0'));
            else
                case dsp_opcode is
                    ------------------------------------------------------------
                    when clear =>
                        p <= (others => (others => '0'));
                    ------------------------------------------------------------
                    when mult =>
                        for idx in p'range loop
                            p(idx) <= a(idx) * b;
                        end loop;
                    ------------------------------------------------------------
                    when macc =>
                        for idx in p'range loop
                            p(idx) <= p(idx) + a(idx) * b;
                        end loop;
                    ------------------------------------------------------------
                    when hold =>
                        null;
                    ------------------------------------------------------------
               end case;
           end if;
        end if;
    end process alu_proc;

    -----------------------------------------------------------------------------
    -- Shift RAM
    -----------------------------------------------------------------------------
    -- I'm calling it the z ram, since it is the z delay of the incoming signal
    shift_ram_proc : process(clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i = '1' then
                z_ram <= (others => (others => '0'));
            elsif z_ram_en = '1' then
                z_ram <= signed(data_reg) & z_ram(0 to z_ram'length-2);
            end if;
        end if;
    end process shift_ram_proc;

    ----------------------------------------------------------------------------
    -- tests
    ----------------------------------------------------------------------------
    -- synthesis off
    -- Test the rom by iterating through the rom
    rom_test_proc : process(clk_i)
    begin
        if rising_edge(clk_i) then
            if rst_i = '1' then
                rom_addr_test <= 0;
            else
                if rom_addr_test >= taps_rom'length-1 then
                    rom_addr_test <= 0;
                else
                    rom_addr_test <= rom_addr_test + 1;
                end if;
            end if;
        end if;
    end process rom_test_proc;

    -- combinational read
    rom_data_test <= taps_rom(rom_addr_test);
    -- synthesis on

end architecture behavior;
