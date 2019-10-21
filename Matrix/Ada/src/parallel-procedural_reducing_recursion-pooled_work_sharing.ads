------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                            P A R A L L E L .
--              P R O C E D U R A L _ R E D U C T I O N .
--                           R E C U R S I O N .
--                P O O L E D _ W O R K _ S H A R I N G
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
--  using a work sharing approach and produce a result for an elementary type.

with Parallel.Task_Pools;
private with Parallel.Procedural_Reducing_Linked_List;
private with Ada.Task_Attributes;

generic
package Parallel.Procedural_Reducing_Recursion.Pooled_Work_Sharing is

   type Recursion_Branch_Count is range 1 .. 2**8 - 1;

   type Recursion_Dispatcher is limited interface;

   procedure Recurse
     (Dispatcher : Recursion_Dispatcher;
      Item : Work_Type;
      Split : Recursion_Branch_Count;
      Of_Splits : Recursion_Branch_Count;
      Result : out Result_Type) is abstract;

   type Recursion_Dispatcher_Access is access all Recursion_Dispatcher'Class;

   type Work_Sharing_Manager
     (Workers : not null access Task_Pools.Task_Pool_Interface'Class
      := Parallel.Task_Pools.Default_Task_Pool;
      Ceiling_Priority : System.Priority := System.Default_Priority)
   is limited new Parallelism_Manager with
      record
         Dispatcher : aliased Recursion_Dispatcher_Access
           := null;
      end record;

   not overriding
   function Create
     (Workers : not null access Task_Pools.Task_Pool_Interface'Class
      := Parallel.Task_Pools.Default_Task_Pool;
      Ceiling_Priority : System.Priority := System.Default_Priority)
      return Work_Sharing_Manager;

   not overriding
   function Sequential
     (Dispatcher : Work_Sharing_Manager)
     return Boolean;

private

   pragma Warnings (Off, "*postcondition does not mention result*");

   overriding
   procedure Execute_Parallel_Subprogram
     (Manager : in out Work_Sharing_Manager;
      Item : Work_Type;
      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      Process : not null access procedure
        (Item : Work_Type;
         Result : out Result_Type);
      Result : out Result_Type)
   with Pre'Class => Manager.Dispatcher = null and then
     Worker_Count <= Manager.Workers.Available_Workers,
        Post'Class => Manager.Dispatcher = null;

   pragma Warnings (On, "*postcondition does not mention result*");

   package Reducing_List is new Procedural_Reducing_Linked_List
     (Result_Type,
      Reducer,
      Identity_Value);

   package Parallel_Task_Attributes is new Ada.Task_Attributes
     (Attribute     => Worker_Count_Type,
      Initial_Value => Worker_Count_Type'First);

   function Sequential
     (Dispatcher : Work_Sharing_Manager)
      return Boolean
   is (Parallel_Task_Attributes.Value <= 1);

   pragma Inline (Sequential);

end Parallel.Procedural_Reducing_Recursion.Pooled_Work_Sharing;
