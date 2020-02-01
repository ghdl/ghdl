library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mult_pkg.all;

entity mult is port (
  clk : in std_logic;
  rst : in std_logic;
  slot : in std_logic;
  a : in mult_i_t;
  y : out mult_o_t);
end mult;

architecture stru of mult is
  signal this_c : mult_reg_t;
  signal this_r : mult_reg_t := MULT_RESET;

  begin
  mult : process(this_r, slot, a)
  variable this : mult_reg_t;
  variable aa : std_logic_vector(31 downto 0);
  variable ah : std_logic_vector(30 downto 0);
  variable bh : std_logic_vector(15 downto 0);
  variable abh2 : std_logic_vector(32 downto 0);
  variable p2 : std_logic_vector(31 downto 0);
  variable p3 : std_logic_vector(31 downto 0);
  variable sgn : std_logic_vector(31 downto 0);
  variable pm : std_logic_vector(47 downto 0);
  variable c : std_logic_vector(63 downto 0);
  variable acc : std_logic_vector(63 downto 0);
  variable region: std_logic_vector(2 downto 0);
  variable sat : std_logic;
  variable code : mult_codeline_t;
  begin
     this := this_r;

    code := MULT_CODE(this.state);
    y.busy <= code.busy; -- FIXME: warning : combinatorial output

  -- operand intermediates, multiplier and input mux, lower 31bits of A and upper/lower 16bits of B
    aa := this.m1;
    if code.sela = MB then aa := this.mb; end if;

    ah := aa(30 downto 0);
    if code.size = B16 then ah(30 downto 15) := (others => '0'); end if;

    bh := this.m2(15 downto 0);
    if code.size = B16 then bh(15) := '0';
    elsif code.shift = '1' then bh := '0' & this.m2(30 downto 16); end if;

  -- partial product adder input mux
    if code.size = B16 then abh2 := '0' & x"0000" & (aa(15) and this.m2(15)) & this.abh(29 downto 15);
    elsif this.shift = '0' then abh2 := '0' & this.abh(46 downto 15);
    else abh2 := '0' & (aa(31) and this.m2(31)) & this.abh(45 downto 15); end if;

  -- partial products adders
    p2 := (others => '0');
    if aa(31) = '1' and code.shift = '1' then p2 := '0' & this.m2(30 downto 0); end if;
    if aa(15) = '1' and code.size = B16 then p2 := x"0000" & '0' & this.m2(14 downto 0); end if;

    p3 := (others => '0');
    if this.m2(31) = '1' and code.shift = '1' then p3 := '0' & aa(30 downto 0); end if;
    if this.m2(15) = '1' and code.size = B16 then p3 := x"0000" & '0' & aa(14 downto 0); end if;

    if code.sign = 1 then sgn := (others => '1'); else sgn := (others => '0'); end if;
    pm := std_logic_vector(unsigned(abh2) + unsigned(sgn(0) & (this.p23 xor sgn)) + code.sign) & this.abh(14 downto 0);

    this.p23 := std_logic_vector(unsigned(p2) + unsigned(p3));

    if this.shift = '0' then
      if pm(47) = '1' and code.size = B16 then c := x"ffff" & pm;
      else c := x"0000" & pm; end if;
    else c := pm & x"0000";
    end if;

  -- accumulator
    acc := std_logic_vector(unsigned(c) + unsigned((this.mach and to_slv(code.use_h, 32)) & this.macl));

  -- saturate
    sat := '1'; region := (others => '0');
    case this.result_op is
      when IDENTITY => sat := '0';

      when SATURATE64 =>
        if acc(63) = '0' and acc(62 downto 47) /= x"0000" then region(0) := '1';
        elsif acc(63) = '1' and acc(62 downto 47) /= x"ffff" then region(0) := '1'; end if;

        region(2 downto 1) := this.mach(31) & acc(63);

        if c(63) = '0' then
          case region is
            when "001" | "010" | "011" | "101" => acc := P48MAX;
            when "111" => acc := N48MAX;
            when others => sat := '0';
          end case;
        else
          case region is
            when "001" => acc := P48MAX;
            when "011" | "100" | "101" | "111" => acc := N48MAX;
            when others => sat := '0';
          end case;
        end if;

      when SATURATE32 =>
        region := this.macl(31) & acc(31) & '0';
        if c(31) = '0' then
          case (region) is
            when "010" => acc := P32MAX;
            when others => sat := '0';
          end case;
        else
          case (region) is
            when "100" => acc := N32MAX;
            when others => sat := '0';
          end case;
        end if;
    end case;

  -- multiplier
    this.abh := std_logic_vector(unsigned(ah) * unsigned(bh));

  -- load the internal registers from the CPU
    if slot = '1' then
      if a.command /= NOP then
        this.m2 := a.in2;
        if a.command = MACL or a.command = MACW then this.mb := this.m1; end if;
      end if;

      if a.wr_m1 = '1' then this.m1 := a.in1; end if;
    end if;

    if slot = '1' and a.wr_mach = '1' then this.mach := a.in1;
    elsif slot = '1' and (a.command = DMULSL or a.command = DMULUL) then this.mach := x"00000000";
    elsif this.state = MACWS1 and sat = '1' then this.mach := this.mach or x"00000001";
    elsif code.mach_en = '1' then this.mach := acc(63 downto 32);
    end if;

    if slot = '1' and a.wr_macl = '1' then this.macl := a.in2;
    elsif slot = '1' and a.command /= NOP and a.command /= MACL and a.command /= MACW then this.macl := x"00000000";
    elsif code.macl_en = '1' then this.macl := acc(31 downto 0);
    end if;

  -- delayed versions of the control signals to delay for p23 pipeline register
    this.state := code.state;
    this.shift := code.shift;

  -- load the command from the CPU
    if code.busy = '0' and slot = '1' then
      this.result_op := IDENTITY;
      this.state := a.command;

      if a.command = MACL then
        if a.s = '1' then this.result_op := SATURATE64; end if;
      elsif a.command = MACW then -- override start state, MACWS and MACW set different busy and mach_en values
        if a.s = '1' then this.result_op := SATURATE32; this.state := MACWS; end if;
      end if;
    end if;

    this_c <= this;
  end process;

  mult_r0 : process(clk, rst)
  begin
     if rst='1' then
        this_r <= MULT_RESET;
     elsif clk='1' and clk'event then
        this_r <= this_c;
     end if;
  end process;

  -- drive the outputs
  y.mach <= this_r.mach;
  y.macl <= this_r.macl;
end stru;
