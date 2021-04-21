library ieee;
context ieee.ieee_std_context;

entity axis_conv1d9x1 is
  generic (
    fifo_depth : integer := 0 -- ceiling of the log base 2 of the desired FIFO length
  );
  port (
    s_axis_clk   : in  std_logic;
    s_axis_rstn  : in  std_logic;
    s_axis_rdy   : out std_logic;
    s_axis_data  : in  std_logic_vector(31 downto 0);
    s_axis_valid : in  std_logic;
    s_axis_strb  : in  std_logic_vector(3 downto 0);
    s_axis_last  : in  std_logic;

    m_axis_clk   : in  std_logic;
    m_axis_rstn  : in  std_logic;
    m_axis_valid : out std_logic;
    m_axis_data  : out std_logic_vector(31 downto 0);
    m_axis_rdy   : in  std_logic;
    m_axis_strb  : out std_logic_vector(3 downto 0);
    m_axis_last  : out std_logic
  );
end axis_conv1d9x1;

architecture arch of axis_conv1d9x1 is

  constant data_width : integer := 32;

  signal r, e, f, wr, rd, valid : std_logic;
  signal d, q : std_logic_vector(31 downto 0);

  signal acca, accb, acca_n, accb_n : signed(31 downto 0);
  signal rlast : std_logic;

begin

  r <= (s_axis_rstn nand m_axis_rstn);

  fifo: entity work.fifo
    generic map (
      fifo_depth => fifo_depth,
      data_width => 32
    )
    port map (
      CLKW => s_axis_clk,
      CLKR => m_axis_clk,
      RST => r,
      WR => wr,
      RD => rd,
      E => e,
      F => f,
      D => d,
      Q => q
    );

  process(s_axis_clk) begin
    if rising_edge(s_axis_clk) then
      if not s_axis_rstn then
        acca <= (others=>'0');
        accb <= (others=>'0');
      elsif s_axis_valid and s_axis_rdy then
        acca <= acca_n;
        accb <= accb_n;
      end if;
    end if;
  end process;

  acca_n <=
    acca + signed(s_axis_data(15 downto 0)) when rlast='0' else
    resize(signed(s_axis_data(15 downto 0)), acca) when ??(s_axis_valid and s_axis_rdy) else
    (others=>'0');
  accb_n <=
    accb + signed(s_axis_data(31 downto 16)) when rlast='0' else
    resize(signed(s_axis_data(31 downto 16)), acca) when ??(s_axis_valid and s_axis_rdy) else
    (others=>'0');

  process(s_axis_clk) begin
    if rising_edge(s_axis_clk) then
      if not s_axis_rstn then
        rlast <= '0';
      else
        rlast <= s_axis_last and (s_axis_valid and s_axis_rdy);
      end if;
    end if;
  end process;

  wr <= rlast;
  d <= std_logic_vector(accb(18 downto 3)) & std_logic_vector(acca(18 downto 3));

-- AXI4 Stream Slave logic

  s_axis_rdy <= not f;

-- AXI4 Stream Master logic

  rd <= (not e) and (valid nand (not m_axis_rdy));

  process(m_axis_clk) begin
    if rising_edge(m_axis_clk) then
      if ((not m_axis_rstn) or ((valid and E) and m_axis_rdy))='1' then
        valid <= '0';
      elsif rd then
        valid <= '1';
      end if;
    end if;
  end process;

  m_axis_valid <= valid;
  m_axis_last <= q(d'left);
  m_axis_strb <= (others=>'1');
  m_axis_data <= q(data_width-1 downto 0);

end architecture;
