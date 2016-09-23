-- EMACS settings: -*-  tab-width: 2;indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2;replace-tabs off;indent-width 2;
-- =============================================================================
-- Authors:					Patrick Lehmann
--
-- Package:     		Protected type implementations.
--
-- Description:
-- -------------------------------------
-- .. TODO:: No documentation available.
--
-- License:
-- =============================================================================
-- Copyright 2007-2016 Technische Universitaet Dresden - Germany,
--  					 				 Chair for VLSI-Design, Diagnostics and Architecture
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


package corelib_List is
	generic (
		type ELEMENT_TYPE;
		InitialMasterListSize	: POSITIVE	:= 4;
		InitialChunkListSize	: POSITIVE	:= 8;
		MasterListResize			: POSITIVE	:= 8;
		ChunkListResize				: POSITIVE	:= 8
	);

	type ELEMENT_ARRAY is array(NATURAL range <>) of ELEMENT_TYPE;
	
	-- protected list implementation
	type PT_LIST is protected
		procedure				Init;
		-- procedure				Clear;
		procedure				Append(Value : ELEMENT_TYPE);
		impure function	Append(Value : ELEMENT_TYPE) return NATURAL;
		procedure				Append(Values : ELEMENT_ARRAY);
		impure function	Append(Values : ELEMENT_ARRAY) return NATURAL;
		-- procedure				Prepend(Value : ELEMENT_TYPE);
		-- impure function	Prepend(Value : ELEMENT_TYPE) return NATURAL;
		-- procedure				Prepend(Values : ELEMENT_ARRAY);
		-- impure function	Prepend(Values : ELEMENT_ARRAY) return NATURAL;
		-- procedure				Insert(Index : NATURAL; Value : ELEMENT_TYPE);
		-- procedure				Insert(Index : NATURAL; Values : ELEMENT_ARRAY);
		impure function	IndexOf(Value : ELEMENT_TYPE) return INTEGER;
		procedure				Set(Index : NATURAL; Value : ELEMENT_TYPE);
		impure function	Set(Index : NATURAL; Value : ELEMENT_TYPE) return ELEMENT_TYPE;
		impure function	Get(Index : NATURAL) return ELEMENT_TYPE;
		procedure				RemoveAt(Index : NATURAL);
		impure function	RemoveAt(Index : NATURAL) return ELEMENT_TYPE;
		procedure				Remove(Value : ELEMENT_TYPE);
		impure function	Remove(Value : ELEMENT_TYPE) return NATURAL;
		-- procedure				Remove(Values : ELEMENT_ARRAY);
		impure function	ToArray(Start : INTEGER := 0; Stop : INTEGER := -1; COUNT : NATURAL := 0) return ELEMENT_ARRAY;
		impure function	Count return natural;
		impure function	Size return positive;
		-- procedure				Resize(Size : positive);
	end protected;
end package;


package body corelib_List is
	-- protected list implementation
	type PT_LIST is protected body
		subtype T_Chunk is ELEMENT_ARRAY;
		type P_Chunk is access T_Chunk;
	
		type T_MasterListItem is record
			Count			: NATURAL;
			Pointer		: P_Chunk;
		end record;
		type T_MasterList is array(NATURAL range <>) of T_MasterListItem;
		type P_MasterList is access T_MasterList;
		
		type T_AddressTuple is record
			MasterIndex	: INTEGER;
			ChunkIndex	: INTEGER;
			ListIndex		: INTEGER;
		end record;
		
		variable I_Count						: NATURAL				:= 0;
		variable I_MasterList_Size	: POSITIVE			:= InitialMasterListSize;
		variable I_MasterList_Count	: NATURAL				:= 0;
		variable I_MasterList_Last	: NATURAL				:= 0;
		variable I_MasterList				: P_MasterList	:= null;
		
		procedure Init is
		begin
			I_Count									:= 0;
			I_MasterList_Size				:= InitialMasterListSize;
			I_MasterList_Count			:= 1;
			I_MasterList_Last				:= 0;
			I_MasterList						:= new T_MasterList(0 to InitialMasterListSize - 1);
			I_MasterList(0).Count		:= 0;
			I_MasterList(0).Pointer	:= new T_Chunk(0 to InitialChunkListSize - 1);
		end procedure;
				
		procedure CheckResize(Size : positive) is
			variable i										: NATURAL;
			variable j										: NATURAL;
			variable Remaining						: INTEGER;
			variable New_Chunks						: NATURAL;
			variable New_MasterList_Size	: NATURAL;
			variable New_MasterList				: P_MasterList;
		begin
			Remaining := Size;
			
			i := I_MasterList_Last;
			Remaining		:= Remaining - (InitialChunkListSize - I_MasterList(i).Count);
			New_Chunks	:= (Remaining + ChunkListResize - 1) / ChunkListResize;
			if ((I_MasterList_Size - I_MasterList_Count) < New_Chunks) then
				New_MasterList_Size := I_MasterList_Size + ((New_Chunks + MasterListResize - 1) / MasterListResize) * MasterListResize;
				New_MasterList			:= new T_MasterList(0 to New_MasterList_Size - 1);
				for j in 0 to I_MasterList_Count - 1 loop
					New_MasterList(j).Count 	:= I_MasterList(j).Count;
					New_MasterList(j).Pointer := I_MasterList(j).Pointer;
				end loop;
				deallocate(I_MasterList);
				I_MasterList			:= New_MasterList;
				I_MasterList_Size	:= New_MasterList_Size;
			end if;
			for j in I_MasterList_Count to I_MasterList_Count + New_Chunks - 1 loop
				I_MasterList(j).Count 	:= 0;
				I_MasterList(j).Pointer := new T_Chunk(0 to InitialChunkListSize - 1);
			end loop;
			I_MasterList_Count	:= I_MasterList_Count + New_Chunks;
		end procedure;
		
		-- procedure Clear is
		-- begin
			
		-- end procedure;
		
		procedure Append(Value : ELEMENT_TYPE) is
			variable i : NATURAL;
			variable j : NATURAL;
		begin
			CheckResize(1);
			
			i := I_MasterList_Last;
			if (I_MasterList(i).Count >= InitialChunkListSize) then
				i									:= i + 1;
				I_MasterList_Last	:= i;
			end if;
			
			j															:= I_MasterList(i).Count;
			I_MasterList(i).Pointer(j)		:= Value;
			I_MasterList(i).Count					:= j + 1;
			I_Count												:= I_Count + 1;
		end procedure;
		
		impure function	Append(Value : ELEMENT_TYPE) return NATURAL is
		begin
			Append(Value);
			return I_Count - 1;
		end function;
		
		procedure Append(Values : ELEMENT_ARRAY) is
		begin
			
		end procedure;
		
		impure function	Append(Values : ELEMENT_ARRAY) return NATURAL is
		begin
			Append(Values);
			return I_Count - Values'length;
		end function;
		
		-- procedure Prepend(Value : ELEMENT_TYPE) is
		-- begin
			
		-- end procedure;
		
		-- impure function	Prepend(Value : ELEMENT_TYPE) return NATURAL is
		-- begin
			
		-- end function;
		
		-- procedure Prepend(Values : ELEMENT_ARRAY) is
		-- begin
			
		-- end procedure;
		
		-- impure function	Prepend(Values : ELEMENT_ARRAY) return NATURAL is
		-- begin
			
		-- end function;
		
		-- procedure Insert(Index : NATURAL; Value : ELEMENT_TYPE) is
		-- begin
			
		-- end procedure;
		
		-- procedure Insert(Index : NATURAL; Values : ELEMENT_ARRAY) is
		-- begin
			
		-- end procedure;
		
		impure function	AddressOf(Value : ELEMENT_TYPE) return T_AddressTuple is
			variable k	: NATURAL;
		begin
			k := 0;
			for i in 0 to I_MasterList_Count - 1 loop
				for j in 0 to I_MasterList(i).Count - 1 loop
					if (I_MasterList(i).Pointer(j) = Value) then
						return (i, j, k);
					end if;
					k := k + 1;
				end loop;
			end loop;
			return (-1, -1, -1);
		end function;
		
		impure function	AddressOf(Index : NATURAL) return T_AddressTuple is
			variable j	: NATURAL;
			variable k	: NATURAL;
		begin
			if (Index >= I_Count) then
				report "Index is out of range." severity ERROR;
				return (-1, -1, -1);
			end if;
			k := Index;
			for i in 0 to I_MasterList_Count - 1 loop
				j := I_MasterList(i).Count;
				if (k < j) then
					return (i, k, Index);
				else
					k := k - j;
				end if;
			end loop;
			return (-1, -1, -1);
		end function;
		
		impure function	IndexOf(Value : ELEMENT_TYPE) return INTEGER is
			constant idx		: T_AddressTuple	:= AddressOf(Value);
		begin
			return idx.ListIndex;
		end function;
		
		procedure Set(Index : NATURAL; Value : ELEMENT_TYPE) is
			constant idx		: T_AddressTuple	:= AddressOf(Index);
		begin
			if (idx.ListIndex /= -1) then
				I_MasterList(idx.MasterIndex).Pointer(idx.ChunkIndex) := Value;
			end if;
		end procedure;
		
		impure function Set(Index : NATURAL; Value : ELEMENT_TYPE) return ELEMENT_TYPE is
			constant idx		: T_AddressTuple	:= AddressOf(Index);
			variable old		: ELEMENT_TYPE;
		begin
			if (idx.ListIndex /= -1) then
				old := I_MasterList(idx.MasterIndex).Pointer(idx.ChunkIndex);
				I_MasterList(idx.MasterIndex).Pointer(idx.ChunkIndex) := Value;
			end if;
			return old;
		end function;
		
		impure function	Get(Index : NATURAL) return ELEMENT_TYPE is
			constant idx		: T_AddressTuple	:= AddressOf(Index);
			variable Empty	: ELEMENT_TYPE;
		begin
			if (idx.ListIndex /= -1) then
				return I_MasterList(idx.MasterIndex).Pointer(idx.ChunkIndex);
			end if;
			return Empty;
		end function;
		
		procedure RemoveChunk(ChunkIndex : NATURAL) is
		begin
			deallocate(I_MasterList(ChunkIndex).Pointer);
			for i in ChunkIndex to I_MasterList_Count - 2 loop
				I_MasterList(i).Count			:= I_MasterList(i + 1).Count;
				I_MasterList(i).Pointer		:= I_MasterList(i + 1).Pointer;
			end loop;
			I_MasterList_Count := I_MasterList_Count - 1;
		end procedure;
		
		procedure Remove(Idx : T_AddressTuple) is
			constant i			: INTEGER					:= idx.MasterIndex;
		begin
			if ((Idx.ChunkIndex = 0) and (I_MasterList(i).Count = 1)) then
				RemoveChunk(i);
			else
				for j in Idx.ChunkIndex to InitialChunkListSize - 2 loop
					I_MasterList(i).Pointer(j)	:= I_MasterList(i).Pointer(j + 1);
				end loop;
				I_MasterList(i).Count := I_MasterList(i).Count - 1;
			end if;
			I_Count := I_Count - 1;
		end procedure;
		
		procedure RemoveAt(Index : NATURAL) is
		begin
			Remove(AddressOf(Index));
		end procedure;
		
		impure function	RemoveAt(Index : NATURAL) return ELEMENT_TYPE is
			constant idx		: T_AddressTuple	:= AddressOf(Index);
			constant i			: INTEGER					:= idx.MasterIndex;
			constant j			: INTEGER					:= idx.ChunkIndex;
			constant Value	: ELEMENT_TYPE		:= I_MasterList(i).Pointer(j);
		begin
			Remove(idx);
			return Value;
		end function;
		
		procedure Remove(Value : ELEMENT_TYPE) is
		begin
			Remove(AddressOf(Value));
		end procedure;
		
		impure function	Remove(Value : ELEMENT_TYPE) return NATURAL is
			constant idx		: T_AddressTuple	:= AddressOf(Value);
		begin
			Remove(idx);
			return idx.ListIndex;
		end function;
		
		-- procedure Remove(Values : ELEMENT_ARRAY) is
		-- begin
			
		-- end procedure;
		
		impure function	ToArray(Start : INTEGER := 0; Stop : INTEGER := -1; COUNT : NATURAL := 0) return ELEMENT_ARRAY is
			variable Result : ELEMENT_ARRAY(0 to I_Count - 1);
			variable k			: NATURAL;
		begin
			k := 0;
			for i in 0 to I_MasterList_Count - 1 loop
				for j in 0 to I_MasterList(i).Count - 1 loop
					Result(k)	:= I_MasterList(i).Pointer(j);
					k					:= k + 1;
				end loop;
			end loop;
			return Result;
		end function;
		
		impure function	Count return natural is
		begin
			return I_Count;
		end function;
		
		impure function	Size return positive is
		begin
			return I_MasterList_Size * InitialChunkListSize;
		end function;
		
		-- procedure Resize(Size : positive) is
		-- begin
			
		-- end procedure;
	end protected body;
end package body;
