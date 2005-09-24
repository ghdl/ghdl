/*  GRT stack implementation based on pthreads.
    Copyright (C) 2003, 2004, 2005 Felix Bertram & Tristan Gingold.

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
// Description: pthread port of stacks package, for use with MacOSX
// Note:        Tristan's original i386/Linux used assembly-code 
//              to manually switch stacks for performance reasons.
// History:     2003may22, FB, created.
//-----------------------------------------------------------------------------

#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>


//#define INFO printf
#define INFO (void)

// GHDL names an endless loop calling FUNC with ARG a 'stack'
// at a given time, only one stack may be 'executed'
typedef struct 
{	pthread_t           thread;         // stack's thread
	pthread_mutex_t     mutex;          // mutex to suspend/resume thread
	void                (*Func)(void*); // stack's FUNC
	void*               Arg;            // ARG passed to FUNC
} Stack_Type_t, *Stack_Type;

Stack_Type_t      main_stack_context;
extern Stack_Type grt_stack_main_stack;

//------------------------------------------------------------------------------
void grt_stack_init(void)
// Initialize the stacks package.
// This may adjust stack sizes.
// Must be called after grt.options.decode.
// => procedure Stack_Init;
{	INFO("grt_stack_init\n");
	INFO("  main_stack_context=0x%08x\n", &main_stack_context);
	
	pthread_mutex_init(&(main_stack_context.mutex), NULL);

	// lock the mutex, as we are currently running
	pthread_mutex_lock(&(main_stack_context.mutex));
	
	grt_stack_main_stack= &main_stack_context;
}

//------------------------------------------------------------------------------
static void* grt_stack_loop(void* pv_myStack)
{
	Stack_Type myStack= (Stack_Type)pv_myStack;

	INFO("grt_stack_loop\n");
	
	INFO("  myStack=0x%08x\n", myStack);

	// block until mutex becomes available again.
	// this happens when this stack is enabled for the first time
	pthread_mutex_lock(&(myStack->mutex));
	
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
{
  	Stack_Type newStack;

	INFO("grt_stack_create\n");
	INFO("  call 0x%08x with 0x%08x\n", Func, Arg);
			
	newStack= malloc(sizeof(Stack_Type_t));
	
	// init function and argument
	newStack->Func= Func;
	newStack->Arg=  Arg;
	
	// create mutex
	pthread_mutex_init(&(newStack->mutex), NULL);
	
	// block the mutex, so that thread will blocked in grt_stack_loop
	pthread_mutex_lock(&(newStack->mutex));
	
	INFO("  newStack=0x%08x\n", newStack);
	
	// create thread, which executes grt_stack_loop
	pthread_create(&(newStack->thread), NULL, grt_stack_loop, newStack); 
	
	return newStack;
}

//------------------------------------------------------------------------------
void grt_stack_switch(Stack_Type To, Stack_Type From)
// Resume stack TO and save the current context to the stack pointed by
// CUR.
// => procedure Stack_Switch (To : Stack_Type; From : Stack_Type);
{	INFO("grt_stack_switch\n");
	INFO("  from 0x%08x to 0x%08x\n", From, To);
	
	// unlock 'To' mutex. this will make the other thread either
	// - starts for first time in grt_stack_loop
	// - resumes at lock below
	pthread_mutex_unlock(&(To->mutex));
		
	// block until 'From' mutex becomes available again
	// as we are running, our mutex is locked and we block here
	// when stacks are switched, with above unlock, we may proceed
	pthread_mutex_lock(&(From->mutex));
}

//------------------------------------------------------------------------------
void grt_stack_delete(Stack_Type Stack)
// Delete stack STACK, which must not be currently executed.
// => procedure Stack_Delete (Stack : Stack_Type);
{	INFO("grt_stack_delete\n");
}

//------------------------------------------------------------------------------
void __gnat_raise_storage_error(void)
{
   abort ();
}

void __gnat_raise_program_error(void)
{
   abort ();
}

//------------------------------------------------------------------------------
// end of file

