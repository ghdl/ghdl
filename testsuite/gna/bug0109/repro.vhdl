package my_pkg_gen is
  generic (type el_type);
  
  type sfixed is array (integer range <>) of el_type;
  function to_string (inp: sfixed) return string;
end my_pkg_gen;

package body my_pkg_gen is
  function to_string (inp: sfixed) return string is
  begin
    return "image-pkg";
  end to_string;
end my_pkg_gen;

package my_pkg is new work.my_pkg_gen generic map (el_type => bit);



use work.my_pkg.all;
entity repro is
end;

architecture behavioral of repro is
    function to_string (inp: sfixed) return string is
    begin
      return "image-ent";
    end function;
begin
    process
        variable z: sfixed (3 downto -3);
    begin
        z := "1111000";
        report "z = " & to_string (z);
        wait;
    end process;
end architecture behavioral;
