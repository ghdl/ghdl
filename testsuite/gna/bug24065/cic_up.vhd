library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

entity cic_up is
   generic(
      num_bits   : natural := 16;   -- How many bits in our incoming data?
      num_stages : natural :=  3;   -- How many stages in the CIC? (N)
      resamp     : natural := 32;   -- By what factor are we changing our data rate? (R)
      diff_dly   : natural :=  1;   -- The differential delay (M)
      out_rate   : natural :=  8);  -- Number of clocks per output sample
                                    --  Must be a power of two
   port(
      clk_i   : in  std_logic;
      rst_i   : in  std_logic;
      -- Input ports
      data_i  : in  std_logic_vector(num_bits-1 downto 0);
      valid_i : in  std_logic;
      -- Output samples
      data_o  : out std_logic_vector;
      valid_o : out std_logic);
end entity cic_up;

architecture behavior of cic_up is
   -----------------------------------------------------------------------------
   -- Functions
   -----------------------------------------------------------------------------
   function log(b,n: positive) return natural is
      variable temp   : natural := 1;
      variable answer : natural := 0;
   begin
      while temp < n loop
         answer := answer + 1;
         temp   := temp * b;
      end loop;
      return answer;
   end function log;

   -----------------------------------------------------------------------------
   function signed_add(l,r: std_logic_vector) return std_logic_vector is
      variable resized_r : signed(r'high+1 downto r'low);
   begin
      resized_r := resize(signed(r), r'length+1);
      return std_logic_vector(signed(l) + resized_r);
   end function signed_add;

   -----------------------------------------------------------------------------
   function signed_sub(l,r: std_logic_vector) return std_logic_vector is
      variable resized_r : signed(r'high+1 downto r'low);
   begin
      resized_r := resize(signed(r), r'length+1);
      return std_logic_vector(signed(l) - resized_r);
   end function signed_sub;

   -----------------------------------------------------------------------------
   impure function input_bits return natural is
      variable a, b, c : integer;
   begin
      a := (num_bits   + 1);
      b := (num_stages - 2)*(log(2,resamp));
      c := (num_stages - 1)*(log(2,diff_dly));
      return a + b + c;
   end function input_bits;

   -----------------------------------------------------------------------------
   impure function output_bits return natural is
      variable a, b, c : integer;
   begin
      a := (num_bits   + 0);
      b := (num_stages - 1)*(log(2,resamp));
      c := (num_stages - 0)*(log(2,diff_dly));
      return a + b + c;
   end function output_bits;

   -----------------------------------------------------------------------------
   -- Types, subtypes, and constants
   -----------------------------------------------------------------------------
   -- Bit growth constants
   constant direction     : string    := "up";
   subtype int_i_range    is natural range  input_bits-1     downto 0;
   subtype int_o_range    is natural range output_bits-1     downto 0;
   subtype comb_sum_range is natural range num_bits          downto 0;
   subtype count_range    is natural range log(2,out_rate)-1 downto 0;
   -- Array type for differential delay
   subtype word      is std_logic_vector(num_bits-1 downto 0);
   type comb_reg_type   is array (integer range <>) of word;
   -----------------------------------------------------------------------------
   -- Signals
   -----------------------------------------------------------------------------
   signal comb_reg : comb_reg_type(0 to diff_dly-1)  := (others => (others => '0'));
   signal comb_sum : std_logic_vector(comb_sum_range):= (others => '0');
   signal int_in   : std_logic_vector(int_i_range);
   signal int_sum  : signed          (int_o_range)   := (others => '0');
   signal int_en   : std_logic;
   signal count    : unsigned(count_range)           := (others => '0');
   signal c_dly    : std_logic;
begin
   -----------------------------------------------------------------------------
   -- Map outputs
   -----------------------------------------------------------------------------
   data_o <= std_logic_vector(int_sum);

   -----------------------------------------------------------------------------
   -- Comb (derivative) section
   -----------------------------------------------------------------------------
   comb_proc : process(clk_i)
   begin
      if rising_edge(clk_i) then
         -- Comb registers
         if rst_i = '1' then
            comb_reg <= (others => (others => '0'));
            comb_sum <= (others => '0');
         elsif valid_i = '1' then
            comb_reg <= data_i & comb_reg(0 to comb_reg'high-1);
            comb_sum <= signed_sub(data_i, comb_reg(comb_reg'high));
         end if;
      end if;
   end process comb_proc;

   -----------------------------------------------------------------------------
   -- Integrator section
   -----------------------------------------------------------------------------
   int_proc : process(clk_i)
   begin
      if rising_edge(clk_i) then
         -- Integrator registers
         if rst_i = '1' then
            int_sum  <= (others => '0');
         else
            if num_stages = 1 then
               -- The connection between the ints and the combs is determined
               --  by the generic out_rate.  This is indirectly controlled by
               --  the top bit in count, which counts to out_rate.
               if out_rate = 1 then
                  valid_o <= '1';
               else
                  valid_o <= count(count'left) and not c_dly;
               end if;

               -- The size of the first integrator register could be a different
               --  size from the last stage of the combs.  This should make up
               --  that difference.
               if valid_i = '1' then
                  int_sum <= resize(int_sum + signed(comb_sum), int_sum'length);
               end if;

            -- Every other stage of the integrators is controlled by the enable
            --  signal of the previous stage
            elsif int_en = '1' then
               int_sum <= int_sum + signed(int_in);
               valid_o <= '1';
            else
               valid_o <= '0';
            end if;
         end if;
      end if;
   end process int_proc;

   -----------------------------------------------------------------------------
   -- Recursive instantiation and termination
   -----------------------------------------------------------------------------
   cic_gen : if num_stages > 1 generate
   begin
      cic_inst : entity work.cic_up
         generic map(
            num_bits   => num_bits+1,
            num_stages => num_stages-1,
            diff_dly   => diff_dly,
            resamp     => resamp,
            out_rate   => out_rate)
         port map(
            clk_i   => clk_i,
            rst_i   => rst_i,
            -- Input ports
            data_i  => comb_sum,
            valid_i => valid_i,
            -- Output samples
            data_o  => int_in,
            valid_o => int_en);
   end generate cic_gen;

   valid_gen : if num_stages = 1 generate
   begin
      vary_valid_o : if out_rate /= 1 generate
      begin
         valid_o_proc : process(clk_i)
         begin
            if rising_edge(clk_i) then
               if rst_i = '1' then
                  count <= (others => '0');
                  c_dly <= '0';
               else
                  count <= count + 1;
                  c_dly <= count(count'left);
               end if;
            end if;
         end process valid_o_proc;
      end generate vary_valid_o;
   end generate valid_gen;

end architecture behavior;
