-- IEEE Std 1076-1993 5.2.1 Ginding Indication (example)

package global_signals is   -- THIS PACKAGE MISSING IN THE EXAMPLE
   signal Tied_High:   bit := '1';
end package;

entity AND_GATE is
   generic (I1toO, I2toO: DELAY_LENGTH := 4 ns);
   port        (I1, I2: in BIT;   O: out BIT);
end entity AND_GATE;

architecture Behavior of AND_GATE is  -- ADDED
   signal In1, In2:    BIT;
begin
   In1 <= I1 after I1toO;
   In2 <= I2 after I2toO;

   O <= In1 and In2;

   process
   begin
       report
           LF & HT & "I1to0 = " & time'image(I1toO) &
           LF & HT & "I2to0 = " & time'image(I2toO);
       wait;
   end process;
end architecture Behavior;

entity XOR_GATE is
   generic (I1toO, I2toO : DELAY_LENGTH := 4 ns);
   port        (I1, I2: in BIT;    O : out BIT);
end entity XOR_GATE;

architecture Behavior of XOR_GATE is  -- ADDED
   signal In1, In2:    BIT;
begin
   In1 <= I1 after I1toO;
   In2 <= I2 after I2toO;

   O <= In1 xor In2;

   process
   begin
       report
           LF & HT & "I1to0 = " & time'image(I1toO) &
           LF & HT & "I2to0 = " & time'image(I2toO);
       wait;
   end process;
end architecture Behavior;

package MY_GATES is
  component AND_GATE is
     generic  (I1toO, I2toO: DELAY_LENGTH := 4 ns);
     port       (I1, I2: in BIT;   O: out BIT);
  end component AND_GATE;

  component XOR_GATE is
     generic  (I1toO, I2toO: DELAY_LENGTH := 4 ns);
     port       (I1, I2: in BIT;   O : out BIT);
  end component XOR_GATE;
end package MY_GATES;

entity Half_Adder is
   port    (X, Y: in BIT;
              Sum, Carry: out BIT);
end entity Half_Adder;

use WORK.MY_GATES.all;
architecture Structure of Half_Adder is
   signal O:   bit;            -- Added
   for L1: XOR_GATE use
       entity WORK.XOR_GATE(Behavior)      --  The primary binding indication
            generic map (3 ns, 3 ns)       --  for instance L1.
            port map (I1 => I1, I2 => I2, O => O);

   for L2: AND_GATE use
       entity WORK.AND_GATE(Behavior)      --  The primary binding indication
            -- generic map (3 ns, 4 ns)       --  for instance L2.
            port map (I1, open, O);

begin
   L1: XOR_GATE    port map (X, Y, Sum);
   L2: AND_GATE    port map (X, Y, Carry);
end architecture Structure;

use WORK.GLOBAL_SIGNALS.all;

configuration Different of Half_Adder is
   for Structure
       for L1: XOR_GATE
           generic map (2.9 ns, 3.6 ns); --  The  incremental binding
       end for;               --  indication of L1; rebinds its generics.

       for L2: AND_GATE
           -- generic map (2.8 ns, 3.25 ns)  -- The incremental binding
           port map (I2 => Tied_High); -- indication L2; rebinds its generics
       end for;                          -- and binds its open port.
   end for;
end configuration Different;

