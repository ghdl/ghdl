entity test_dict is
end test_dict;

use work.datastructures.all;

architecture behav of test_dict is
begin
  process
    variable dict : String_String_Dict;
  begin
    dict.set ("entity", "module");
    dict.set ("process", "always");

    assert dict.get ("entity") = "module" severity failure;
    wait;
  end process;
end behav;
