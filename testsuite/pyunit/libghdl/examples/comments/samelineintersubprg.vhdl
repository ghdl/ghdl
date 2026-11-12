package samelineintersubprg is
  generic (
    type T;
    function f (x : T) return boolean is <>;  --  Comment for :f:
    procedure p (y : T) is <>;  --  Comment for :p:
    function g (
      z : T  --  Comment for :z:
    ) return boolean is <>;  --  Comment for :g:
    function h (u : T) return boolean is <>  --  Comment for :h:
  );
end package;
