package g1 is
  generic(c : natural);
  function t return l;
end;


package g2 is
  generic(package g is new g1 generic map(<>));
end;
