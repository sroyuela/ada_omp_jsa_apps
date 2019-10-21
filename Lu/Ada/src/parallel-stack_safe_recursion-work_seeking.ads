------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                           P A R A L L E L .
--                          S T A C K _ S A F E _                           --
--                          R E C U R S I O N .
--                        W O R K _ S E E K I N G                         --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                  Copyright (C) 2011, Bradley J. Moore                    --
--                                                                          --
--  Paraffin is free software;  you can  redistribute it  and/or modify it  --
--  under  terms of the  GNU General Public License  as  published  by the  --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later  version.  Paraffin is  distributed in the hope that it  will be  --
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  --
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU  --
--  General Public License for  more details.  You should have  received a  --
--  copy of the GNU General Public License distributed with Paraffin;  see  --
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,   --
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.         --
--                                                                          --
--  As a  special exception, if other files  instantiate generics from      --
--  this unit,  or you link this  unit with other files  to produce an      --
--  executable,  this unit  does  not by  itself  cause the  resulting      --
--  executable to be covered by  the GNU General Public License.  This      --
--  exception does  not however invalidate  any other reasons  why the      --
--  executable file might be covered by the GNU Public License.             --
------------------------------------------------------------------------------

--  This procedure provides the capability to recurse in parallel
--  using a work seeking approach without producing an overall result.
--  In addition, the recursion can be limited such that when attempting to
--  recurse deeper than the specified limit, the recursion item is "saved"
--  for later processing which will then be processed with a fresh stack,
--  thus ensuring that overflow below the limit does not occur.
--  NOTE: Also, this stack safe feature may be applied to a single worker
--  so it may be beneficial to use this generic even in cases where parallel
--  execution is not needed.
--  This version of the code is considered "safer" than the stack_limited
--  version because this version lets you specify the limit in terms of
--  stack size or as a percentage of the current stack size, intead of just
--  the recursive call depth. It is difficult to know if a call depth limit
--  would be contained on the stack, but for instance specifying the limit as
--  80% of the stack size should ensure that the recursion does not overflow.

with System.Storage_Elements;
with Parallel.Dispatching_Domains;

generic
package Parallel.Stack_Safe_Recursion.Work_Seeking is

   type Recursion_Dispatcher is limited interface;

   procedure Recurse
     (Dispatcher : Recursion_Dispatcher;
      Item : Work_Type;
      Stack_Limit : System.Address)
   is abstract;

   type Recursion_Dispatcher_Access is access all Recursion_Dispatcher'Class;

   type Work_Seeking_Manager
     (Other_Workers : access Work_Seeking_State := null;
      Ceiling_Priority : System.Priority := System.Default_Priority;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size;
      Affinity : access Dispatching_Domains.CPU_Set := null;
      Allow_Migration : Boolean := True;
      Max_Depth : Parallel.Stack_Percentage
      := Parallel.Default_Maximum_Stack_Depth
      --  Maximum depth of the recursion expressed as a percentage of
      --  the current stack size.
      --  Attempts to recurse deeper than this limit result in
      --  "deferring" the work item to be processed later with a fresh
      --  stack
      )
   is limited new Parallelism_Manager with
      record
         Dispatcher : aliased Recursion_Dispatcher_Access
           := null;
      end record;

   not overriding
   function Create
     (Others_Seeking_Work : aliased in out Work_Seeking_State;
      Ceiling_Priority : System.Priority := System.Default_Priority;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size;
      Affinity : access Dispatching_Domains.CPU_Set := null;
      Allow_Migration : Boolean := True;
      Max_Depth : Parallel.Stack_Percentage :=
        Parallel.Default_Maximum_Stack_Depth)
   return Work_Seeking_Manager;

private

   pragma Warnings (Off, "*postcondition does not mention result*");

   overriding
   procedure Execute_Parallel_Subprogram
     (Manager : in out Work_Seeking_Manager;
      Item : Work_Type;
      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      --  Top level item to process recursively
      Process : not null access procedure
        (Item : Work_Type;
         Stack_Limit : System.Address);
      Stack_Deferrals : out Natural)
   with Pre'Class => Manager.Dispatcher = null,
        Post'Class => Manager.Dispatcher = null;

   pragma Warnings (On, "*postcondition does not mention result*");

end Parallel.Stack_Safe_Recursion.Work_Seeking;
