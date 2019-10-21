------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                           P A R A L L E L .
--                F U N C T I O N A L _ R E D U C T I O N .
--                S T A C K _ S A F E _ R E C U R S I O N .
--                 P O O L E D _ W O R K _ S E E K I N G
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

--  This package provides the capability to recurse in parallel
--  using a work seeking approach and produce a result for an elementary type.

with Parallel.Task_Pools;
with Parallel.Dispatching_Domains;

generic
package Parallel.
  Functional_Reducing_Stack_Safe_Recursion.Pooled_Work_Seeking is

   type Recursion_Dispatcher is limited interface;

   function Recurse
     (Dispatcher : Recursion_Dispatcher;
      Item : Work_Type;
      Stack_Limit : System.Address) return Result_Type is abstract;

   type Recursion_Dispatcher_Access is access all Recursion_Dispatcher'Class;

   type Work_Seeking_Manager
     (Workers : not null access Task_Pools.Task_Pool_Interface'Class
      := Parallel.Task_Pools.Default_Task_Pool;
      Other_Workers : access Work_Seeking_State := null;
      Ceiling_Priority : System.Priority := System.Default_Priority;
      Affinity : access Dispatching_Domains.CPU_Set := null;
      Allow_Migration : Boolean := True;
      Max_Depth : Parallel.Stack_Percentage :=
        Parallel.Default_Maximum_Stack_Depth
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
      Workers : not null access Task_Pools.Task_Pool_Interface'Class
      := Parallel.Task_Pools.Default_Task_Pool;
      Ceiling_Priority : System.Priority := System.Default_Priority;
      Affinity : access Dispatching_Domains.CPU_Set := null;
      Allow_Migration : Boolean := True;
      Max_Depth : Parallel.Stack_Percentage :=
        Parallel.Default_Maximum_Stack_Depth)
   return Work_Seeking_Manager;

private

   pragma Warnings (Off, "*postcondition does not mention result*");

   overriding
   function Execute_Parallel_Subprogram
     (Manager : in out Work_Seeking_Manager;
      Item : Work_Type;
      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      Process : not null access
        function (Item : Work_Type;
                  Stack_Limit : System.Address) return Result_Type;
      Stack_Deferrals : aliased out Natural)
   return Result_Type
   with Pre'Class => Manager.Dispatcher = null and then
     Worker_Count <= Manager.Workers.Available_Workers,
        Post'Class => Manager.Dispatcher = null;

   pragma Warnings (On, "*postcondition does not mention result*");

end Parallel.Functional_Reducing_Stack_Safe_Recursion.Pooled_Work_Seeking;
