LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.fixed_pkg.ALL;
USE ieee.numeric_std.ALL;

ENTITY repro1 IS
  GENERIC (
    -- Number of channels
    g_CHANNELS            : NATURAL := 4;

    -- Number of internal biquads
    -- The order is given by 2*g_NUM_BIQUADS
    g_NUM_BIQUADS         : NATURAL := 2;
    -- Signed fixed-point representation of biquads' coefficients
    g_COEFF_INT_WIDTH     : NATURAL := 4;
    g_COEFF_FRAC_WIDTH    : NATURAL := 3
  );
  PORT (
    -- Clock
    clk_i                 : IN  STD_LOGIC;
    -- Reset
    rst_n_i               : IN  STD_LOGIC
  );
END;

ARCHITECTURE behave OF repro1 is
  -- Type that wraps all biquad coefficients (a0 = 1)
  type t_biquad_coeffs is record
    b0 : sfixed;
    b1 : sfixed;
    b2 : sfixed;
    a1 : sfixed;
    a2 : sfixed;
  end record;

  -- Type that wraps all internal biquads' coefficients (a0 = 1)
  type t_iir_filt_coeffs is array (natural range <>) of t_biquad_coeffs;

  TYPE t_iir_filts_x_or_y IS ARRAY (NATURAL RANGE <>) of SFIXED;
  TYPE t_fofb_shaper_filt_coeffs IS
    ARRAY (NATURAL RANGE <>) OF t_iir_filt_coeffs;

  TYPE t_wb_fofb_shaper_filt_regs_coeffs_i_ifc IS RECORD
    data  : STD_LOGIC_VECTOR(31 DOWNTO 0);
  END RECORD;
  TYPE t_wb_fofb_shaper_filt_regs_coeffs_o_ifc IS RECORD
    addr  : STD_LOGIC_VECTOR(8 DOWNTO 2);
    data  : STD_LOGIC_VECTOR(31 DOWNTO 0);
    wr    : STD_LOGIC;
  END RECORD;

  TYPE t_wb_fofb_shaper_filt_regs_coeffs_i_ifc_arr IS
    ARRAY (NATURAL RANGE <>) OF t_wb_fofb_shaper_filt_regs_coeffs_i_ifc;
  TYPE t_wb_fofb_shaper_filt_regs_coeffs_o_ifc_arr IS
    ARRAY (NATURAL RANGE <>) OF t_wb_fofb_shaper_filt_regs_coeffs_o_ifc;

  -- Number of bits in Wishbone register interface
  -- +2 to account for BYTE addressing
  CONSTANT c_PERIPH_ADDR_SIZE : NATURAL := 2+2;

  CONSTANT c_MAX_CHANNELS : NATURAL := 12;
  CONSTANT c_MAX_ABI_BIQUADS : NATURAL := 10;

  CONSTANT c_WB_FOFB_SHAPER_FILT_REGS_COEFFS_I_IFC_0s :
    t_wb_fofb_shaper_filt_regs_coeffs_i_ifc := (data => (OTHERS => '0'));
  CONSTANT c_WB_FOFB_SHAPER_FILT_REGS_COEFFS_O_IFC_0s :
    t_wb_fofb_shaper_filt_regs_coeffs_o_ifc := (addr => (OTHERS => '0'),
                                                data => (OTHERS => '0'),
                                                wr => '0');

  -- The signed fixed-point representation of coefficients is aligned to the
  -- left in Wishbone registers
  PURE FUNCTION f_parse_wb_coeff(wb_coeff : STD_LOGIC_VECTOR)
  RETURN SFIXED IS
  BEGIN
    RETURN to_sfixed(wb_coeff(31 DOWNTO
             32-(g_COEFF_INT_WIDTH + g_COEFF_FRAC_WIDTH)), g_COEFF_INT_WIDTH-1,
             -g_COEFF_FRAC_WIDTH);
  END f_parse_wb_coeff;

  SIGNAL wb_fofb_shaper_filt_regs_coeffs_i_ifc_arr :
    t_wb_fofb_shaper_filt_regs_coeffs_i_ifc_arr(c_MAX_CHANNELS-1 DOWNTO 0) :=
      (OTHERS => c_WB_FOFB_SHAPER_FILT_REGS_COEFFS_I_IFC_0s);
  SIGNAL wb_fofb_shaper_filt_regs_coeffs_o_ifc_arr :
    t_wb_fofb_shaper_filt_regs_coeffs_o_ifc_arr(c_MAX_CHANNELS-1 DOWNTO 0) :=
      (OTHERS => c_WB_FOFB_SHAPER_FILT_REGS_COEFFS_O_IFC_0s);

  SIGNAL coeffs : t_fofb_shaper_filt_coeffs(g_CHANNELS-1 DOWNTO 0)(
                    c_MAX_ABI_BIQUADS-1 DOWNTO 0)(
                    b0(g_COEFF_INT_WIDTH-1 DOWNTO -g_COEFF_FRAC_WIDTH),
                    b1(g_COEFF_INT_WIDTH-1 DOWNTO -g_COEFF_FRAC_WIDTH),
                    b2(g_COEFF_INT_WIDTH-1 DOWNTO -g_COEFF_FRAC_WIDTH),
                    a1(g_COEFF_INT_WIDTH-1 DOWNTO -g_COEFF_FRAC_WIDTH),
                    a2(g_COEFF_INT_WIDTH-1 DOWNTO -g_COEFF_FRAC_WIDTH));

  SIGNAL biquad_idx : NATURAL RANGE 0 TO c_MAX_ABI_BIQUADS-1 := 0;
BEGIN
  ASSERT g_COEFF_INT_WIDTH > 1 and g_COEFF_FRAC_WIDTH > 1 and
         g_COEFF_INT_WIDTH + g_COEFF_FRAC_WIDTH <= 32
    REPORT "ABI supports at most 32-bits coefficients (g_COEFF_INT_WIDTH + " &
           "g_COEFF_FRAC_WIDTH). Also, the SFIXED type requires each of these" &
           "to be at least 1."
    SEVERITY FAILURE;

  -- NOTE: All wb_fofb_shaper_filt_regs RAM interfaces addresses are
  --       internally connected to same signals. So, pick just one of
  --       them to index the coefficients.
  biquad_idx <= to_integer(UNSIGNED(
    wb_fofb_shaper_filt_regs_coeffs_o_ifc_arr(0).addr(8 DOWNTO 5)));

  pp: PROCESS(clk_i) IS
  BEGIN
          wb_fofb_shaper_filt_regs_coeffs_i_ifc_arr(0).data <=
            (to_slv(coeffs(0)(biquad_idx).b0), OTHERS => '0');
  end process;
END ARCHITECTURE behave;
