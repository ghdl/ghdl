entity assert01 is
  generic (
    ginv : boolean := false
  );
  port (i : bit;
        o : out bit);
end assert01;

architecture behav of assert01 is
begin
  assert (not ginv) report "ginv is true" severity failure;
  o <= i;
end behav;
