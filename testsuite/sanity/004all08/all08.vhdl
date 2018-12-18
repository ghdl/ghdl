    --  Files containing most (if not all) features of vhdl08.
    --  Like a comment.

    --  TODO: at specifications.

    library ieee;
    use ieee.std_logic_1164.all;

    package pkg is
      --  TODO: file param,
      procedure clear (v : out std_logic_vector);

      type my_enum is
        (lit_a, lit_b, lit_c, 'e');
      type my_short is range -2**15 to 2**15 - 1;
      type DISTANCE is range 0 to 1E16 units
        -- primary unit:
        angstrom;
        -- metric lengths:
        nm = 10 angstrom;
        um = 1000 nm;
        mm = 1000 um;
        cm = 10 mm;
      end units;

      type my_float is range 0.0 to 1.0e20;

      attribute user_attr : boolean;
      attribute user_attr of clear [std_logic_vector]: procedure is True;

      type cell;
      type cell_acc is access cell;
      type cell is record
        chain : cell_acc;
        val : natural;
      end record;

      procedure prepend (ch : inout cell_acc; val : natural);

      type text_file is file of string;

      type sharedcounter is protected
         procedure increment (n : natural);
         procedure decrement (n : natural);
      end protected;
    end pkg;

    package body pkg is
      procedure clear (v : out std_logic_vector) is
      begin
        v := (v'range => '0');
      end clear;

      procedure prepend (ch : inout cell_acc; val : natural)
      is
        variable res : cell_acc;
        variable len : natural;
      begin
        --  Check if already in the list.
        res := ch;
        while res /= null loop
          if res.val = val then
           return;
          end if;
          res := res.all.chain;
        end loop;

        len := 0;
        res := ch;
        loop
           exit when res = null;
           len := len + 1;
           res := res.chain;
           next when res.val = val;
        end loop;

        res := new cell'(chain => ch, val => val);
        ch := res;
      end prepend;

      type sharedcounter is protected body
        variable val : natural := 0;
        procedure increment (n : natural) is
        begin
          val := val + n;
        end increment;

        procedure decrement (n : natural) is
        begin
          val := val - n;
        end decrement;
      end protected body;

    end pkg;

    library ieee;
    use ieee.std_logic_1164.all;

    entity reg is
      generic (width : natural);
      port (clk : std_logic;
            rst_n : std_logic;
            d : in std_logic_vector (width - 1 downto 0);
            q : out std_logic_vector (width - 1 downto 0));
      subtype bus_type is std_logic_vector (width - 1 downto 0);
    begin
      assert width < 128 report "large width" severity warning;
    end reg;

    architecture behav of reg is
    begin
      process (clk, rst_n)
      begin
        if rising_edge(clk) then
          if rst_n = '0' then
            q <= (others => '0');
          else
            q <= d;
          end if;
        end if;
      end process;
    end behav;

    entity reg_tb is
    end reg_tb;

    library ieee;
    use ieee.std_logic_1164.all;
    use work.pkg.all;

    architecture behav of reg_tb is
      component reg is
        generic (width : natural);
        port (clk : std_logic;
              rst_n : std_logic;
              d : in std_logic_vector (width - 1 downto 0);
              q : out std_logic_vector (width - 1 downto 0));
      end component reg;

      subtype data_type is std_logic_vector (31 downto 0);

      function get_vector (n : natural) return data_type is
      begin
        case n is
          when 0 =>
            return x"0000_0000";
          when 1 =>
            return x"0000_0001";
          when 2 =>
            return x"1111_1111";
          when 3 | 4 =>
            return data_type'(x"3333_4444");
          when 5 to 7 =>
             return (0 to 5 => '1', 6 | 7 => '0', others => '1');
          when others =>
            return x"ffff_ffff";
        end case;
      end get_vector;

      signal clk : std_logic bus;
      signal rst_n : std_logic := '0';
      signal din, dout : data_type;

      alias my_clk : std_logic is clk;

      group syn is (signal <>);
      group sig_syn : syn (clk, rst_n);

      type data_array_type is array (natural range <>) of data_type;

      file input_file : text_file;

      procedure disp_msg (msg : string) is
      begin
        report msg
          severity note;
      end disp_msg;

    begin
      process
      begin
        clk <= '0', '1' after 10 ns;
        wait for 20 ns;
      end process;

      rst_n <= '0', '1' after 25 ns;

      disp_msg ("start of design");

      process
      begin
        disp_msg (msg => "test is starting");

        for i in 1 to 10 loop
          din <= get_vector(i);
          wait until rising_edge(my_clk);
        end loop;
        wait;
      end process;

      compute: process
        variable v : integer;
        variable b1, b2, b3 : boolean;
        variable bv1, bv2 : bit_vector (0 to 7);
      begin
        b2 := true;
        b1 := (b2 and b3) or b1;
        b3 := (b1 xor b2) nand b3;
        b2 := (b1 nor b2) xnor b3;

        bv1 := bv2 sll v;
        bv2 := bv1 rol v;
        bv1 := not(bv2 sra (v rem 3));

        v := -2;
        v := ((3 * v) / 4) ** 2;
        v := (v + 4) - 1;
        v := natural (v mod 128);
        b1 := v >= 3;
        b2 := v /= 4;
        b3 := b2 or (v = 5);

        report "v = " & integer'image (v) severity note;
        wait;
      end process compute;

      cmp_reg : reg
        generic map (width => 32)
        port map (clk => clk,
          rst_n => rst_n,
          d => din,
          q => dout);

      blk1: block (clk)
        signal dout2 : data_type register;
        disconnect dout2 : data_type after 1 ns;

        signal dout3 : data_type;
        signal dout4 : data_type;
      begin
        assert dout (7 downto 0) = din (7 downto 0);
        assert dout'right = 0;

        dout2 <= guarded din;

        with dout(0) select
          dout4 <= din when '0',
                  (others => '1') when others;

        g1: for i in 0 to 40 generate
           g2: if i <= data_type'left generate
             cmp: entity work.reg
                generic map (width => 1)
                port map (clk => clk,
                           rst_n => rst_n,
                           d(0) => din (i),
                           q(0) => dout3 (i));
          end generate g2;
        end generate g1;
      end block;
    end behav;

    configuration cfg of reg_tb is
      for behav
        --  component configuration.
        for cmp_reg : reg
          use entity work.reg;
        end for;
        --  TODO: blocks, generate
      end for;
    end cfg;
