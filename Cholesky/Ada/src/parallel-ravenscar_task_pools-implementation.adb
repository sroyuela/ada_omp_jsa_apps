package body Parallel.Ravenscar_Task_Pools.Implementation is
   protected body Worker_Manager is

      procedure Offer_Work
        (Worker : Pool_Index;
         Plan   : aliased in out Work_Plan'Class;
         Item   : Plan_Index) is
      begin
         --  Cant make another offer until the previous one has been accepted
         pragma Assert (not Offered_Work);

         Worker_Index := Worker;
         Assignment_Id   := Item;
         Assignment_Plan := Plan'Unchecked_Access;
         Offered_Work := True;
      end Offer_Work;

      entry Wait_For_Work
        (Worker : out Pool_Index;
         Plan   : out Work_Plan_Access;
         Item   : out Plan_Index) when Offered_Work is
      begin
         Worker := Worker_Index;
         Plan   := Assignment_Plan;
         Item   := Assignment_Id;
         Assignment_Id := 0;
         Offered_Work := False;
      end Wait_For_Work;

   end Worker_Manager;

   protected body Task_Manager is

      function Active_Workers return Pool_Worker_Count is
      begin
         return Outstanding_Workers;
      end Active_Workers;

      procedure Next_Worker_Id
        (Plan : aliased in out Work_Plan'Class;
         Requester : Plan_Index;
         Item : out Plan_Index) is
      begin
         --  Let the plan safely initialize its state as this is
         --  called from within a protected operation.
         Plan.Starting (Requester, Item);

      end Next_Worker_Id;

      procedure Offer_Work
        (Plan          : aliased in out Work_Plan'Class;
         Item          : Plan_Index)
      is
         Worker_Index : Pool_Index;
      begin

         Worker_Index := Idle_Workers
              (Number_Of_Workers - Outstanding_Workers);
         Outstanding_Workers := Outstanding_Workers + 1;

         Pool.Workers (Worker_Index).Manager.Offer_Work
           (Worker_Index,
            Plan,
            Item);

      end Offer_Work;

      procedure Offer_Work_To_Group
       (Plan : aliased in out Work_Plan'Class;
        Worker_Count : Positive_Worker_Count)
      is
         Worker_Index : Pool_Index;
      begin
         for I in 1 .. Worker_Count loop

            Worker_Index := Idle_Workers
                 (Number_Of_Workers - Outstanding_Workers);
            Outstanding_Workers := Outstanding_Workers + 1;

            Pool.Workers (Worker_Index).Manager.Offer_Work
              (Worker_Index,
               Plan,
               Plan_Index (I));

         end loop;
      end Offer_Work_To_Group;

      procedure Set_Worker_Idle
        (Worker : Pool_Index;
         Plan : aliased in out Work_Plan'Class;
         Item : Plan_Index) is
      begin
         Outstanding_Workers := Outstanding_Workers - 1;
         Idle_Workers (Number_Of_Workers - Outstanding_Workers) := Worker;
         Plan.Completing (Item);
      end Set_Worker_Idle;

   end Task_Manager;

   task body Worker_Task is
      Worker_Index : Pool_Index;
      Plan   : Work_Plan_Access;
      Item   : Plan_Index;
   begin -- Worker

      Work_Loop : loop

         --  Execution completed, look for work
         --  from other tasks if possible
         Static_Worker.Manager.Wait_For_Work
           (Worker_Index,
            Plan,
            Item);

         Plan.Engage (Worker_Index, Item);

      end loop Work_Loop;

   end Worker_Task;

   --------------------------------------------------

   function Create_Idle_List return Idle_List is
   begin
      return Result : Idle_List do
         for I in Result'Range loop
            Result (I) := I;
         end loop;
      end return;
   end Create_Idle_List;

   --------------------------------------------------

   procedure Finished_Work
     (Pool : in out Task_Pool;
      Worker : Pool_Index;
      Plan : aliased in out Work_Plan'Class;
      Item : Plan_Index) is
   begin
      Pool.Manager.Set_Worker_Idle (Worker,
                                    Plan,
                                    Item);
   end Finished_Work;

   procedure Next_Worker_Id
     (Pool : in out Task_Pool;
      Plan : aliased in out Work_Plan'Class;
      Requester : Plan_Index;
      Item : out Plan_Index) is
   begin
      Pool.Manager.Next_Worker_Id (Plan, Requester, Item);
   end Next_Worker_Id;

   procedure Offer_Work
     (Pool : in out Task_Pool;
      Plan : aliased in out Work_Plan'Class;
      Item : Plan_Index) is
   begin
      Pool.Manager.Offer_Work (Plan, Item);
   end Offer_Work;

   procedure Offer_Work_To_Group
     (Pool : in out Task_Pool;
      Plan : aliased in out Work_Plan'Class;
      Worker_Count : Positive_Worker_Count) is
   begin
      Pool.Manager.Offer_Work_To_Group (Plan, Worker_Count);
   end Offer_Work_To_Group;

end Parallel.Ravenscar_Task_Pools.Implementation;
