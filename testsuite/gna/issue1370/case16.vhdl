entity case16 is
end entity case16;

architecture rtl of case16 is

begin

  process
    variable decode : bit_vector(2 downto 0);
    variable cnt : integer := 0;
    
  begin
    
    case decode(1 downto 0) is
      when "00" =>
        decode(1 downto 0)  := "01";
        report "Startup ...";
        wait for 1 ps;
      when "01" =>
        decode(1 downto 0)  := "11";
        report "Go Green ...";
        wait for 10 ps;
      when "10" =>
        decode(1 downto 0)  := "01";
        report "Go Red ...";
        wait for 10 ps;
      when "011" =>   ----<<<<   line 44
        decode(1 downto 0)  := "10";
        report "Go Yellow ...";
        wait for 2 ps;
    end case;
    
    cnt := cnt + 1;
    if cnt = 25 then
      wait;
    end if;
    
  end process;
end rtl;
