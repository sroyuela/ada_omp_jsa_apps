------------------------------------------------------------------------------
--                                                                          --
--                 Paraffin - Parallelism Generics for Ada                  --
--                                                                          --
--                             P A R A L L E L .
--                P R O C E D U R A L _ R E D U C T I O N .
--                            R E C U R S I O N .
--                  P O O L E D _ W O R K _ S H A R I N G
--                                                                          --
--                                B o d y                                   --
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
with System;
with Ada.Exceptions; use Ada;

package body Parallel.Procedural_Reducing_Recursion.Pooled_Work_Sharing is

   function Create
     (Workers : not null access Task_Pools.Task_Pool_Interface'Class
      := Parallel.Task_Pools.Default_Task_Pool;
      Ceiling_Priority : System.Priority := System.Default_Priority)
      return Work_Sharing_Manager is
   begin
      return Manager : Work_Sharing_Manager
        (Workers,
         Ceiling_Priority)
      do
         null;
      end return;

   end Create;

   procedure Execute_Parallel_Subprogram
     (Manager : in out Work_Sharing_Manager;
      Item : Work_Type;
      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      Process : not null access procedure
        (Item : Work_Type;
         Result : out Result_Type);
      Result : out Result_Type)
   is

      type Work_Sharing_Plan is
        new Parallel.Task_Pools.Work_Plan with null record;

      overriding procedure Engage (Plan : Work_Sharing_Plan);

      protected type Internal_Work_Manager is

         pragma Priority (Manager.Ceiling_Priority);

         entry Wait_For_Work
           (Item : out Work_Type;
            Subs : out Worker_Count_Type;
            Done : out Boolean);

         procedure Offer_Work
           (Item          : Work_Type;
            Subs          : Worker_Count_Type;
            Work_Accepted : out Boolean);

         entry Wait_For_Worker_Initialization
           (Item          : Work_Type;
            Work_Accepted : out Boolean);

         entry Wait_For_Completion
           (Final_Result : out Result_Type);

         procedure Reduce (Value : in out Result_Type);

         procedure Save_Exception (E : Exceptions.Exception_Occurrence);

      private
         All_Done           : Boolean           := False;
         Work_Offered       : Boolean           := False;
         Assignment_Value   : Work_Type;
         Assignment_Subs    : Worker_Count_Type := 0;
         Outstanding_Workers : Worker_Count_Type := 0;
         Result : Result_Type := Identity_Value;
         Exception_Raised    : Boolean := False;
         Saved_Exception     : Ada.Exceptions.Exception_Occurrence;
      end Internal_Work_Manager;

      protected body Internal_Work_Manager is

         procedure Offer_Work
           (Item          : Work_Type;
            Subs          : Worker_Count_Type;
            Work_Accepted : out Boolean) is
         begin
            --  Others looking for work
            if Wait_For_Work'Count > 0 then

               Work_Offered := True;
               Assignment_Value := Item;
               Assignment_Subs := Subs;
               Outstanding_Workers := Outstanding_Workers + 1;

               Work_Accepted := True;
            else
               --  Nobody looking for work or no work to offer
               Work_Accepted := False;
            end if;
         end Offer_Work;

         procedure Reduce (Value : in out Result_Type) is
         begin
            Reducer (Result, Value);
            Outstanding_Workers := Outstanding_Workers - 1;
            if Wait_For_Work'Count = Outstanding_Workers then
               All_Done := True;
            end if;
         end Reduce;

         procedure Save_Exception (E : Exceptions.Exception_Occurrence) is
         begin
            if not Exception_Raised then
               Ada.Exceptions.Save_Occurrence (Target => Saved_Exception,
                                               Source => E);
               Exception_Raised := True;
            end if;

            Outstanding_Workers := Outstanding_Workers - 1;
            if Wait_For_Work'Count = Outstanding_Workers then
               All_Done := True;
            end if;
         end Save_Exception;

         entry Wait_For_Completion
           (Final_Result : out Result_Type) when Outstanding_Workers = 0 is
         begin
            Final_Result := Result;

            if Exception_Raised then
               Ada.Exceptions.Reraise_Occurrence (X => Saved_Exception);
            end if;
         end Wait_For_Completion;

         entry Wait_For_Work
           (Item : out Work_Type;
            Subs : out Worker_Count_Type;
            Done : out Boolean) when Work_Offered or else All_Done is
         begin
            Item := Assignment_Value;
            Subs := Assignment_Subs;

            Work_Offered := False;
            Done := All_Done;
         end Wait_For_Work;

         entry Wait_For_Worker_Initialization
           (Item          : Work_Type;
            Work_Accepted : out Boolean)
           when Wait_For_Work'Count = Worker_Count is
         begin
            Offer_Work (Item, Worker_Count, Work_Accepted);
         end Wait_For_Worker_Initialization;

      end Internal_Work_Manager;

      --  Allow client to call recursion routine
      Internal_Manager : Internal_Work_Manager;

      procedure Engage
        (Plan : Work_Sharing_Plan)
      is
         pragma Unreferenced (Plan);

         Value          : Work_Type;
         Temp           : Result_Type;
         Subcontractors : Worker_Count_Type;
         Done           : Boolean;

      begin -- Engage

         --  Execution completed, look for work
         --  from other tasks if possible
         Internal_Manager.Wait_For_Work
           (Item => Value,
            Subs => Subcontractors,
            Done => Done);

         if not Done then
            Parallel_Task_Attributes.Set_Value (Val => Subcontractors);

            Process (Value, Temp);
            --  Worker switching over to sequential processing
            Internal_Manager.Reduce (Temp);
         end if;

      exception
         when E : others =>
            Internal_Manager.Save_Exception (E);
      end Engage;

      procedure Recurse
      type Internal_Dispatcher_Type is new
        Recursion_Dispatcher with null record;

      overriding
      procedure Recurse
        (Dispatcher : Internal_Dispatcher_Type;
         Item       : Work_Type;
         Split      : Recursion_Branch_Count;
         Of_Splits  : Recursion_Branch_Count;
         Result     : out Result_Type)
      is
         pragma Unreferenced (Dispatcher);

         Work_Accepted : Boolean := False;

         Subcontractors : constant Worker_Count_Type
           := Parallel_Task_Attributes.Value;

         --  Check to see if we can further divide the work
         Subcontractor_Count : Worker_Count_Type :=
           Subcontractors / Worker_Count_Type (Of_Splits);
      begin

         --  Assign any leftover subcontractors
         if Worker_Count_Type (Split) <=
           Subcontractors rem Worker_Count_Type (Of_Splits) then
            Subcontractor_Count := Subcontractor_Count + 1;
         end if;

         if Subcontractors > 0 then

            --  The last split is run under the current task, rather than
            --  attempt to offer to a new worker
            if Split = Of_Splits then

               Parallel_Task_Attributes.Set_Value (Val => Subcontractor_Count);

               --  If there is only one subcontractor left, this worker takes
               --  the work, and proceeds sequentially. Otherwise, there is
               --  still potential for farming out work to subcontractors,
               --  so we proceed in parallel.
               Process (Item, Result);
               return;

            else
               Internal_Manager.Offer_Work
                 (Item,
                  Subcontractor_Count,
                  Work_Accepted);

               if Work_Accepted then
                  Result := Identity_Value;
                  return;
               end if;  --  Work_Accepted
            end if; --  Split = Of_Splits
         end if; --  Workers_Are_Idle ...

         --  If we get here, it's likely because the only worker available is
         --  the one looking for subcontractors, in this case, we consider
         --  that the work is assigned to the calling worker, and we no longer
         --  need to worry about assigning work to subcontractors
         Process (Item, Result);
      end Recurse;

      Plan : aliased Work_Sharing_Plan;
      --  Allow client to call recursion routine
      Internal_Dispatcher : aliased Internal_Dispatcher_Type;
      Work_Accepted : Boolean;

   begin -- Execute_Parallel_Subprogram

      --  Allow client to call recursion routine
      Manager.Dispatcher := Internal_Dispatcher'Unchecked_Access;

      --  Get the workers needed to do the work from the task pool
      Manager.Workers.Offer_Work
        (Item          => Plan,
         Worker_Count  => Worker_Count);

      Internal_Manager.Wait_For_Worker_Initialization
        (Item,
         Work_Accepted);

      pragma Assert (Work_Accepted);

      Internal_Manager.Wait_For_Completion (Result);

      Manager.Dispatcher := null;

   end Execute_Parallel_Subprogram;

end Parallel.Procedural_Reducing_Recursion.Pooled_Work_Sharing;
