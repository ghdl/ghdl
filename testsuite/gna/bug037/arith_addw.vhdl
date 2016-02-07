-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- =============================================================================
-- Authors:					Thomas B. Preusser
--
-- Entity:					arith_addw
--
-- Description:
-- ------------------------------------
--	Implements wide addition providing several options all based
--	on an adaptation of a carry-select approach.
--	
--	References:
--		* Hong Diep Nguyen and Bogdan Pasca and Thomas B. Preusser:
--			FPGA-Specific Arithmetic Optimizations of Short-Latency Adders,
--			FPL 2011.
--			-> ARCH:     AAM, CAI, CCA
--			-> SKIPPING: CCC
--		
--		* Marcin Rogawski, Kris Gaj and Ekawat Homsirikamol:
--			A Novel Modular Adder for One Thousand Bits and More
--			Using Fast Carry Chains of Modern FPGAs, FPL 2014.
--			-> ARCH:		 PAI
--			-> SKIPPING: PPN_KS, PPN_BK
--	
-- License:
-- =============================================================================
-- Copyright 2007-2015 Technische Universitaet Dresden - Germany
--										 Chair for VLSI-Design, Diagnostics and Architecture
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--		http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- =============================================================================

library	IEEE;
use			IEEE.std_logic_1164.all;

library	PoC;
use			PoC.utils.all;
use			PoC.arith.all;


entity arith_addw is
  generic (
    N : positive;                    -- Operand Width
    K : positive;                    -- Block Count

    ARCH        : tArch     := AAM;        -- Architecture
    BLOCKING    : tBlocking := DFLT;       -- Blocking Scheme
    SKIPPING    : tSkipping := CCC;        -- Carry Skip Scheme
		P_INCLUSIVE : boolean   := false       -- Use Inclusive Propagate, i.e. c^1
  );
  port (
    a, b : in std_logic_vector(N-1 downto 0);
    cin  : in std_logic;

    s    : out std_logic_vector(N-1 downto 0);
    cout : out std_logic
  );
end entity;

use std.textio.all;

library	IEEE;
use			IEEE.numeric_std.all;


architecture rtl of arith_addw is

  -- Determine Block Boundaries
  type tBlocking_vector is array(tArch) of tBlocking;
  constant DEFAULT_BLOCKING : tBlocking_vector := (AAM => ASC, CAI => DESC, PAI => DESC, CCA => DESC);

  type integer_vector is array(natural range<>) of integer;
  impure function compute_blocks return integer_vector is
    variable  bs  : tBlocking := BLOCKING;
    variable  res : integer_vector(K-1 downto 0);

    variable l : line;
  begin
    if bs = DFLT then
      bs := DEFAULT_BLOCKING(ARCH);
    end if;
    case bs is
      when FIX =>
        assert N >= K
          report "Cannot have more blocks than input bits."
          severity failure;
        for i in res'range loop
          res(i) := ((i+1)*N+K/2)/K;
        end loop;

      when ASC =>
        assert N-K*(K-1)/2 >= K
          report "Too few input bits to implement growing block sizes."
          severity failure;
        for i in res'range loop
          res(i) := ((i+1)*(N-K*(K-1)/2)+K/2)/K + (i+1)*i/2;
        end loop;

      when DESC =>
        assert N-K*(K-1)/2 >= K
          report "Too few input bits to implement growing block sizes."
          severity failure;
        for i in res'range loop
          res(i) := ((i+1)*(N+K*(K-1)/2)+K/2)/K - (i+1)*i/2;
        end loop;

      when others =>
        report "Unknown blocking scheme: "&tBlocking'image(bs) severity failure;

    end case;
    --synthesis translate_off
    write(l, "Implementing "&integer'image(N)&"-bit wide adder: ARCH="&tArch'image(ARCH)&
             ", BLOCKING="&tBlocking'image(bs)&'[');
    for i in K-1 downto 1 loop
      write(l, res(i)-res(i-1));
      write(l, ',');
    end loop;
    write(l, res(0));
    write(l, "], SKIPPING="&tSkipping'image(SKIPPING));
    writeline(output, l);
    --synthesis translate_on
    return  res;
  end compute_blocks;
  constant BLOCKS : integer_vector(K-1 downto 0) := compute_blocks;

  signal g : std_logic_vector(K-1 downto 1);  -- Block Generate
  signal p : std_logic_vector(K-1 downto 1);  -- Block Propagate
  signal c : std_logic_vector(K-1 downto 1);  -- Block Carry-in
begin

  -----------------------------------------------------------------------------
  -- Rightmost Block + Carry Computation Core
  blkCore: block
    constant M : positive := BLOCKS(0);  -- Rightmost Block Width
  begin

    -- Carry Computation with Carry Chain
    genCCC: if SKIPPING = CCC generate
      signal x, y : unsigned(K+M-2 downto 0);
      signal z    : unsigned(K+M-1 downto 0);
    begin
      x <= unsigned(g & a(M-1 downto 0));
			genExcl: if not P_INCLUSIVE generate
				y <= unsigned((g or p) & b(M-1 downto 0));
				-- carry recovery for other blocks
				c <= std_logic_vector(z(K+M-2 downto M)) xor p;
			end generate genExcl;
			genIncl: if P_INCLUSIVE generate
				y <= unsigned(p & b(M-1 downto 0));
				-- carry recovery for other blocks
				c <= std_logic_vector(z(K+M-2 downto M)) xor (p xor g);
			end generate genIncl;
      z <= ('0' & x) + y + (0 to 0 => cin);

      -- output of rightmost block
      s(M-1 downto 0) <= std_logic_vector(z(M-1 downto 0));

      -- carry output
      cout <= z(z'left);
    end generate genCCC;

    -- LUT-based Carry Computations
    genLUT: if SKIPPING /= CCC generate
      signal z : unsigned(M downto 0);
    begin
      -- rightmost block
      z <= unsigned('0' & a(M-1 downto 0)) + unsigned(b(M-1 downto 0)) + (0 to 0 => cin);
      s(M-1 downto 0) <= std_logic_vector(z(M-1 downto 0));

      -- Plain linear LUT-based Carry Forwarding
      genPlain: if SKIPPING = PLAIN generate
        signal t : std_logic_vector(K downto 1);
      begin
        -- carry forwarding
        t(1)            <= z(M);
        t(K downto 2)   <= g or (p and c);
        c    <= t(K-1 downto 1);
        cout <= t(K);
      end generate genPlain;

			-- Kogge-Stone Parallel Prefix Network
			genPPN_KS: if SKIPPING = PPN_KS generate
				subtype tLevel is std_logic_vector(K-1 downto 0);
				type tLevels is array(natural range<>) of tLevel;
				constant LEVELS : positive := log2ceil(K);
				signal   pp, gg : tLevels(0 to LEVELS);
			begin
				-- carry forwarding
				pp(0) <= p & 'X';
				gg(0) <= g & z(M);
				genLevels: for i in 1 to LEVELS generate
					constant D : positive := 2**(i-1);
				begin
					pp(i) <= (pp(i-1)(K-1 downto D) and pp(i-1)(K-D-1 downto 0)) & pp(i-1)(D-1 downto 0);
					gg(i) <= (gg(i-1)(K-1 downto D) or (pp(i-1)(K-1 downto D) and gg(i-1)(K-D-1 downto 0))) & gg(i-1)(D-1 downto 0);
				end generate genLevels;
        c    <= gg(LEVELS)(K-2 downto 0);
        cout <= gg(LEVELS)(K-1);
      end generate genPPN_KS;

			-- Brent-Kung Parallel Prefix Network
			genPPN_BK: if SKIPPING = PPN_BK generate
				subtype tLevel is std_logic_vector(K-1 downto 0);
				type tLevels is array(natural range<>) of tLevel;
				constant LEVELS : positive := log2ceil(K);
				signal   pp, gg : tLevels(0 to 2*LEVELS-1);
			begin
				-- carry forwarding
				pp(0) <= p & 'X';
				gg(0) <= g & z(M);
				genMerge: for i in 1 to LEVELS generate
					constant D : positive := 2**(i-1);
				begin
					genBits: for j in 0 to K-1 generate
						genOp: if j mod (2*D) = 2*D-1 generate
							gg(i)(j) <= (pp(i-1)(j) and gg(i-1)(j-D)) or gg(i-1)(j);
							pp(i)(j) <=  pp(i-1)(j) and pp(i-1)(j-D);
						end generate;
						genCp: if j mod (2*D) /= 2*D-1 generate
							gg(i)(j) <= gg(i-1)(j);
							pp(i)(j) <= pp(i-1)(j);
						end generate;
					end generate;
				end generate genMerge;
				genSpread: for i in LEVELS+1 to 2*LEVELS-1 generate
					constant D : positive := 2**(2*LEVELS-i-1);
				begin
					genBits: for j in 0 to K-1 generate
						genOp: if j > D and (j+1) mod (2*D) = D generate
							gg(i)(j) <= (pp(i-1)(j) and gg(i-1)(j-D)) or gg(i-1)(j);
							pp(i)(j) <=  pp(i-1)(j) and pp(i-1)(j-D);
						end generate;
						genCp: if j <= D or (j+1) mod (2*D) /= D generate
							gg(i)(j) <= gg(i-1)(j);
							pp(i)(j) <= pp(i-1)(j);
						end generate;
					end generate;
				end generate genSpread;
        c    <= gg(gg'high)(K-2 downto 0);
        cout <= gg(gg'high)(K-1);
      end generate genPPN_BK;

    end generate genLUT;

  end block blkCore;

  -----------------------------------------------------------------------------
  -- Implement Carry-Select Variant
  --
  -- all but rightmost block, implementation architecture selected by ARCH
  genBlocks: for i in 1 to K-1 generate
    -- Covered Index Range
    constant LO : positive := BLOCKS(i-1);  -- Low  Bit Index
    constant HI : positive := BLOCKS(i)-1;  -- High Bit Index

    -- Internal Block Interface
    signal aa : unsigned(HI downto LO);
    signal bb : unsigned(HI downto LO);
    signal ss : unsigned(HI downto LO);
  begin

    -- Connect common block interface
    aa <= unsigned(a(HI downto LO));
    bb <= unsigned(b(HI downto LO));
    s(HI downto LO) <= std_logic_vector(ss);

    -- ARCH-specific Implementations

    --Add-Add-Multiplex
    genAAM: if ARCH = AAM generate
      signal s0 : unsigned(HI+1 downto LO);     -- Block Sum (cin=0)
      signal s1 : unsigned(HI+1 downto LO);     -- Block Sum (cin=1)
    begin
      s0 <= ('0' & aa) + bb;
      s1 <= ('0' & aa) + bb + 1;
      g(i) <= s0(HI+1);
			genExcl: if not P_INCLUSIVE generate
				p(i) <= s1(HI+1) xor s0(HI+1);
			end generate genExcl;
			genIncl: if P_INCLUSIVE generate
				p(i) <= s1(HI+1);
			end generate genIncl;

      ss <= s0(HI downto LO) when c(i) = '0' else s1(HI downto LO);
    end generate genAAM;

    -- Compare-Add-Increment
    genCAI: if ARCH = CAI generate
      signal s0 : unsigned(HI+1 downto LO);     -- Block Sum (cin=0)
    begin
      s0 <= ('0' & aa) + bb;
      g(i) <= s0(HI+1);
			genExcl: if not P_INCLUSIVE generate
				p(i) <= 'X' when Is_X(std_logic_vector(aa&bb)) else
								'1' when (aa xor bb) = (aa'range => '1') else '0';
			end generate genExcl;
			genIncl: if P_INCLUSIVE generate
				p(i) <= 'X' when Is_X(std_logic_vector(aa&bb)) else
								'1' when aa >= not bb else '0';
			end generate genIncl;
      ss <= s0(HI downto LO) when c(i) = '0' else s0(HI downto LO)+1;
    end generate genCAI;

    -- Propagate-Add-Increment
    genPAI: if ARCH = PAI generate
      signal s0 : unsigned(HI+1 downto LO);     -- Block Sum (cin=0)
    begin
      s0 <= ('0' & aa) + bb;
      g(i) <= s0(HI+1);
			genExcl: if not P_INCLUSIVE generate
				p(i) <= 'X' when Is_X(std_logic_vector(s0)) else
								'1' when s0(HI downto LO) = (HI downto LO => '1') else '0';
			end generate genExcl;
			genIncl: if P_INCLUSIVE generate
				p(i) <= 'X' when Is_X(std_logic_vector(s0)) else
								'1' when s0(HI downto LO) = (HI downto LO => '1') else g(i);
			end generate genIncl;
      ss <= s0(HI downto LO) when c(i) = '0' else s0(HI downto LO)+1;
    end generate genPAI;

    -- Compare-Compare-Add
    genCCA: if ARCH = CCA generate
      g(i) <= 'X' when Is_X(std_logic_vector(aa&bb)) else
              '1' when aa >  not bb else '0';
			genExcl: if not P_INCLUSIVE generate
				p(i) <= 'X' when Is_X(std_logic_vector(aa&bb)) else
								'1' when (aa xor bb) = (aa'range => '1') else '0';
			end generate genExcl;
			genIncl: if P_INCLUSIVE generate
				p(i) <= 'X' when Is_X(std_logic_vector(aa&bb)) else
								'1' when aa >= not bb else '0';
			end generate genIncl;
      ss <= aa + bb + (0 to 0 => c(i));
    end generate genCCA;

  end generate genBlocks;

end architecture;
