/*  GRT stack implementation for Win32
    Copyright (C) 2004, 2005 Felix Bertram.

    GHDL is free software; you can redistribute it and/or modify it under
    the terms of the GNU General Public License as published by the Free
    Software Foundation; either version 2, or (at your option) any later
    version.

    GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with GCC; see the file COPYING.  If not, write to the Free
    Software Foundation, 59 Temple Place - Suite 330, Boston, MA
    02111-1307, USA.
*/
//-----------------------------------------------------------------------------
// Project:     GHDL - VHDL Simulator
// Description: Win32 port of stacks package
// Note:        Tristan's original i386/Linux used assembly-code 
//              to manually switch stacks for performance reasons.
// History:     2004feb09, FB, created.
//-----------------------------------------------------------------------------

#include <windows.h>
//#include <pthread.h>
//#include <stdlib.h>
//#include <stdio.h>


//#define INFO printf
#define INFO (void)

// GHDL names an endless loop calling FUNC with ARG a 'stack'
// at a given time, only one stack may be 'executed'
typedef struct 
{	HANDLE              thread;         // stack's thread
	HANDLE              mutex;          // mutex to suspend/resume thread
	void                (*Func)(void*); // stack's FUNC
	void*               Arg;            // ARG passed to FUNC
} Stack_Type_t, *Stack_Type;


static Stack_Type_t      main_stack_context;
extern void grt_set_main_stack (Stack_Type_t *stack);

//------------------------------------------------------------------------------
void grt_stack_init(void)
// Initialize the stacks package.
// This may adjust stack sizes.
// Must be called after grt.options.decode.
// => procedure Stack_Init;
{	INFO("grt_stack_init\n");
	INFO("  main_stack_context=0x%08x\n", &main_stack_context);

	// create event. reset event, as we are currently running
	main_stack_context.mutex = CreateEvent(NULL,  // lpsa
	                                       FALSE, // fManualReset
	                                       FALSE, // fInitialState
	                                       NULL); // lpszEventName

	grt_set_main_stack (&main_stack_context);
}

//------------------------------------------------------------------------------
static unsigned long __stdcall grt_stack_loop(void* pv_myStack)
{
	Stack_Type myStack= (Stack_Type)pv_myStack;

	INFO("grt_stack_loop\n");
	
	INFO("  myStack=0x%08x\n", myStack);

	// block until event becomes set again.
	// this happens when this stack is enabled for the first time
	WaitForSingleObject(myStack->mutex, INFINITE);
	
	// run stack's function in endless loop
	while(1)
	{	INFO("  call 0x%08x with 0x%08x\n", myStack->Func, myStack->Arg);
		myStack->Func(myStack->Arg);
	}
	
	// we never get here...
	return 0;
}

//------------------------------------------------------------------------------
Stack_Type grt_stack_create(void* Func, void* Arg) 
// Create a new stack, which on first execution will call FUNC with
// an argument ARG.
// => function Stack_Create (Func : Address; Arg : Address) return Stack_Type;
{  	Stack_Type newStack;
	DWORD      m_IDThread; // Thread's ID (dummy)

	INFO("grt_stack_create\n");
	INFO("  call 0x%08x with 0x%08x\n", Func, Arg);
			
	newStack= malloc(sizeof(Stack_Type_t));
	
	// init function and argument
	newStack->Func= Func;
	newStack->Arg=  Arg;
	
	// create event. reset event, so that thread will blocked in grt_stack_loop
	newStack->mutex= CreateEvent(NULL,  // lpsa
	                             FALSE, // fManualReset
	                             FALSE, // fInitialState
	                             NULL); // lpszEventName
	
	INFO("  newStack=0x%08x\n", newStack);
	
	// create thread, which executes grt_stack_loop
	newStack->thread= CreateThread(NULL,           // lpsa
	                               0,              // cbStack
	                               grt_stack_loop, // lpStartAddr
	                               newStack,       // lpvThreadParm
	                               0,              // fdwCreate
	                               &m_IDThread);   // lpIDThread
	
	return newStack;
}

//------------------------------------------------------------------------------
void grt_stack_switch(Stack_Type To, Stack_Type From)
// Resume stack TO and save the current context to the stack pointed by
// CUR.
// => procedure Stack_Switch (To : Stack_Type; From : Stack_Type);
{	INFO("grt_stack_switch\n");
	INFO("  from 0x%08x to 0x%08x\n", From, To);
	
	// set 'To' event. this will make the other thread either
	// - start for first time in grt_stack_loop
	// - resume at WaitForSingleObject below
	SetEvent(To->mutex);
		
	// block until 'From' event becomes set again
	// as we are running, our event is reset and we block here
	// when stacks are switched, with above SetEvent, we may proceed
	WaitForSingleObject(From->mutex, INFINITE);
}

//------------------------------------------------------------------------------
void grt_stack_delete(Stack_Type Stack)
// Delete stack STACK, which must not be currently executed.
// => procedure Stack_Delete (Stack : Stack_Type);
{	INFO("grt_stack_delete\n");
}

//----------------------------------------------------------------------------
#ifndef WITH_GNAT_RUN_TIME
void __gnat_raise_storage_error(void)
{
   abort ();
}

void __gnat_raise_program_error(void)
{
   abort ();
}
#endif

//----------------------------------------------------------------------------
// end of file

