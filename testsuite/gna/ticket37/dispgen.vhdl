entity dispgen is
  generic (str : string := "init");
end dispgen;

architecture behav of dispgen is
begin
  assert false report "str: " & str severity note;
end behav;
