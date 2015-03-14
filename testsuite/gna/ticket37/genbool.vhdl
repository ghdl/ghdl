entity genbool is
  generic (val : boolean := False);
end genbool;

architecture behav of genbool is
begin
  assert val severity failure;
end behav;
