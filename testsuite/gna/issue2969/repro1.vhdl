entity repro1 is
end entity;

architecture rtl of repro1 is
  signal sig  : bit_vector(21 downto 0) := (others => '0');
begin
end architecture;

entity repro is
end entity;

architecture rtl of repro is
begin
  i_repro1 : entity work.repro1;

  p_proc : process
    function get_coded_width(data_width : natural) return natural is
    begin
      return 22;
    end function get_coded_width;
    
    constant C_DATA_WIDTH  : natural := 16;
--    alias sig1 is <<signal i_repro1.sig : bit_vector(21 downto 0)>>;
    alias sig2 is <<signal i_repro1.sig : bit_vector(get_coded_width(C_DATA_WIDTH) downto 1)>>;
    
    procedure force_edac_error(signal arg : inout bit_vector) is
    begin
      report "left: " & integer'image(arg'left);
      report "right: " & integer'image(arg'right);
    end procedure;
  begin
--      force_edac_error(sig1);
--      report "OK 1";
      force_edac_error(sig2);
      assert sig2'left = 22 severity failure;
      assert sig2'right = 1 severity failure;     
      report "OK 2";
    wait;
  end process;
end architecture;
