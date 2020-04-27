use work.mwe_pkg.all;

entity ent is
end;

architecture behav of ent is
  function f1 (a, b : boolean) return integer is
  begin
    return 1;
  end f1;

  procedure p2 (v : bit_vector (3 downto 0)) is
  begin
    null;
  end p2;

  subtype byte is bit_vector (7 downto 0);
  procedure p3 (a, b : byte) is
  begin
    null;
  end p3;

  procedure p4 (t : std.standard.natural range 0 to 4) is
  begin
    null;
  end p4;
  
  shared variable v : mwe_t;
begin
  process
  begin
    v.send_random_data;
    report f1'path_name;
    assert f1'path_name = ":ent:f1[boolean,boolean return integer]:"
      severity failure;
    report p2'path_name;
    assert p2'path_name = ":ent:p2[bit_vector]:"
      severity failure;
    report p3'path_name;
    assert p3'path_name = ":ent:p3[byte,byte]:"
      severity failure;
    report p4'path_name;
    assert p4'path_name = ":ent:p4[natural]:"
      severity failure;
    wait;
  end process;
end behav;
