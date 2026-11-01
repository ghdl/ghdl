package samelinedecl is
  type t is (a, b);  --  Comment for :t:

  function f (x : bit) return bit;  --  Comment for :f:

  function g (
    y : bit  --  Comment for :y:
  ) return bit;  --  Comment for :g:

  procedure p (u : in bit;   --  Comment for :u:
               w : in bit);  --  Comment for :w: (on the line of w)
end package;
