entity e2c is
  generic (gen1 : natural := 5);
end;

architecture a of e2c is
    function outer(arg : integer) return integer is
        function inner1(arg : integer) return integer is
        begin
            return arg + gen1;
        end;
    begin
        return inner1(arg + 3);
    end;
begin
end;
