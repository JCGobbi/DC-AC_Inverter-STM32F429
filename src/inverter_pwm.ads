with STM32.GPIO;   use STM32.GPIO;
with STM32.Timers; use STM32.Timers;
with STM32.PWM;    use STM32.PWM;

with STM_Board;    use STM_Board;

package Inverter_PWM is

   -----------------
   -- Definitions --
   -----------------

   type PWM_Phase is (A, B);
   --  Each phase of a full bridge circuit.

   type PWM_Alignment is
      (Edge, --  Positive edge
       Center); --  Center of positive part

   --  Describes where on the PWM waveform the signals shall be aligned.

   --  The final maximum amplitude for the sine voltage is defined by the
   --  maximum sine table value, that is 1.00000.
   --  Considering that the battery nominal voltage is 12 Volts, this will
   --  be the peak AC value, which corresponds to a primary AC RMS voltage
   --  of 12 V / sqrt(2) = 8.485 V.
   --  With a minimum battery voltage of 10 V, the minimum AC RMS voltage
   --  will be 10 V / sqrt(2) = 7.07 V.
   --  The transformer voltage ratio between the primary and secondary, for
   --  a maximum output voltage of 230 V RMS, will be 230 V / 7.07 V = 32.5,
   --  so the turns ratio of the transformer will be (Ns / Np) = 33.

   subtype Table_Amplitude is Float range 0.0 .. 1.0;

   subtype Duty_Cycle is Float range 0.0 .. 100.0;

   --  The upload frequency of the duty cycle is defined by the number of points
   --  for each semi-sinusoid.
   --  For 50 Hz we have 2 half senoids * 50 Hz * 250 points = 25000 Hz.
   --  For 60 Hz we have 2 half senoids * 60 Hz * 250 points = 30000 Hz.

   PWM_Frequency_Hz : Frequency_Hz := 30_000.0; -- for 60 Hz

   --  STM32F429 operates at 168 MHz with 84 MHz into Prescaler.
   --  For 50 Hz we have 84 MHz / 25 kHz = 3360 ticks by each 25 kHz period,
   --  so the minimum duty cycle is 100 / 3360 = 0.0298 %.
   --  For 60 Hz we have 84 MHz / 30 kHz = 2800 ticks by each 30 kHz period,
   --  so the minimum duty cycle is 100 / 2800 = 0.0357 %.

   subtype Deadtime_Range is Float range 0.0 .. 400.0e-9;
   --  Maximum deadtime permissible is 126 us.
   --  Maximum deadtime chosen is 1% of the PWM_Frequency_Hz = 0.01/25_000.
   PWM_Deadtime : constant Deadtime_Range := 166.7e-9;
   --  The delay exists in the rising edges.
   --  It depends on the electronic circuit rise and fall times.
   --  166.7e-9 * 30 kHz * 100 = 0.5% of the total period,
   --  0.5% of 2800 ticks = 14 ticks.

   -----------------------------
   -- Procedures and function --
   -----------------------------

   procedure Initialize_PWM
      (Frequency : Frequency_Hz;
       Deadtime  : Deadtime_Range;
       Alignment : PWM_Alignment);
   --  Initialize the timer peripheral for PWM.
   --  Each phase needs to be enabled manually after this.

   procedure Enable_Phase (Phase : PWM_Phase)
     with Inline;
   --  Enable PWM generation for the specified phase.

   procedure Disable_Phase (Phase : PWM_Phase)
     with Inline;
   --  Disable PWM generation for the specified phase.

   procedure Start_PWM
     with Pre => Is_Initialized;
   --  Start the generation of sinusoid wave by enabling interrupt.

   procedure Stop_PWM
     with Pre => Is_Initialized;
   --  Stop the generation of sinusoid wave by disabling interrupt.

   function Get_Duty_Resolution return Duty_Cycle;
   --  Return the minimum step that the duty can be changed, in percent.

   procedure Set_Duty_Cycle
      (Phase : PWM_Phase;
       Value : Duty_Cycle);
   --  Sets the duty cycle in percent for the specified phase.

   subtype Gain_Range is Float range 0.0 .. 1.0;
   --  For correcting battery voltage and AC output variation.

   procedure Set_Sine_Gain (Value : Gain_Range);
   --  Securelly sets the value of the sine wave gain.

   procedure Set_Duty_Cycle
      (Phase     : PWM_Phase;
       Amplitude : Table_Amplitude;
       Gain      : Gain_Range);
   --  Sets the duty cycle for the specified phase.

   procedure Set_PWM_Gate_Power (Enabled : in Boolean)
     with Pre => (if Enabled = False then STM_Board.Is_Initialized
                  else Is_Initialized and STM_Board.Is_Initialized);
   --  Enable or disable the output of the gate drivers.

   procedure Reset_Sine_Step;
   --  Set the sine table step counter to the last position of the
   --  table, or 250, whose amplitude value is 0.

   procedure Safe_State;
   --  Forces the inverter into a state that is considered safe.
   --  Typically this disables the PWM generation (all switches off), and
   --  turns off the power to the gate drivers.

   function Is_Initialized return Boolean;
   --  Returns True if the board specifics are initialized.

   function Has_APB2_Frequency (This : Timer) return Boolean;
   --  Timers 1, 8, 9, 10, 11

   function Has_APB1_Frequency (This : Timer) return Boolean;
   --  Timers 3, 4, 6, 7, 12, 13, 14

private

   Initialized : Boolean := False;

   subtype Sine_Step_Range is Natural range 1 .. 250;
   --  Number of steps for the half sine table.

   Sine_Step : Sine_Step_Range := Sine_Step_Range'Last;
   --  The table last step value is 0.

   --  The table for sine generation is produced knowing the number of points
   --  to complete 1/4 sine period. The semi-sinusoid, or 1/2 sine period is
   --  completed with these same points in reverse order.
   --  The equation which defines each point is:
   --
   --  D = A * sin(pi/2 * x/N)
   --  D = Duty cycle at a given discrete point;
   --  A = Signal amplitude of the maximum duty cycle. We adopt 1.0.
   --  pi/2 = 1/4 of the sine period
   --  x = Step number
   --  N = Number of points = 125

   --  Sine table with 125 points from 0 to 1.00000, in direct and reverse order
   --  to create a semi-sinusoid with 250 points.
   Sine_Table : constant array (Sine_Step_Range) of Table_Amplitude :=
     (1.25660E-02, 2.51301E-02, 3.76902E-02, 5.02443E-02, 6.27905E-02,
      7.53268E-02, 8.78512E-02, 1.00362E-01, 1.12856E-01, 1.25333E-01,
      1.37790E-01, 1.50226E-01, 1.62637E-01, 1.75023E-01, 1.87381E-01,
      1.99710E-01, 2.12007E-01, 2.24271E-01, 2.36499E-01, 2.48690E-01,
      2.60842E-01, 2.72952E-01, 2.85019E-01, 2.97042E-01, 3.09017E-01,
      3.20944E-01, 3.32820E-01, 3.44643E-01, 3.56412E-01, 3.68125E-01,
      3.79779E-01, 3.91374E-01, 4.02906E-01, 4.14376E-01, 4.25779E-01,
      4.37116E-01, 4.48383E-01, 4.59580E-01, 4.70704E-01, 4.81754E-01,
      4.92727E-01, 5.03623E-01, 5.14440E-01, 5.25175E-01, 5.35827E-01,
      5.46394E-01, 5.56876E-01, 5.67269E-01, 5.77573E-01, 5.87785E-01,
      5.97905E-01, 6.07930E-01, 6.17860E-01, 6.27691E-01, 6.37424E-01,
      6.47056E-01, 6.56586E-01, 6.66012E-01, 6.75333E-01, 6.84547E-01,
      6.93653E-01, 7.02650E-01, 7.11536E-01, 7.20309E-01, 7.28969E-01,
      7.37513E-01, 7.45941E-01, 7.54251E-01, 7.62443E-01, 7.70513E-01,
      7.78462E-01, 7.86288E-01, 7.93990E-01, 8.01567E-01, 8.09017E-01,
      8.16339E-01, 8.23533E-01, 8.30596E-01, 8.37528E-01, 8.44328E-01,
      8.50994E-01, 8.57527E-01, 8.63923E-01, 8.70184E-01, 8.76307E-01,
      8.82291E-01, 8.88136E-01, 8.93841E-01, 8.99405E-01, 9.04827E-01,
      9.10106E-01, 9.15241E-01, 9.20232E-01, 9.25077E-01, 9.29777E-01,
      9.34329E-01, 9.38734E-01, 9.42991E-01, 9.47098E-01, 9.51057E-01,
      9.54865E-01, 9.58522E-01, 9.62028E-01, 9.65382E-01, 9.68583E-01,
      9.71632E-01, 9.74527E-01, 9.77268E-01, 9.79855E-01, 9.82287E-01,
      9.84564E-01, 9.86686E-01, 9.88652E-01, 9.90461E-01, 9.92115E-01,
      9.93611E-01, 9.94951E-01, 9.96134E-01, 9.97159E-01, 9.98027E-01,
      9.98737E-01, 9.99289E-01, 9.99684E-01, 9.99921E-01, 1.00000E+00,
      9.99921E-01, 9.99684E-01, 9.99289E-01, 9.98737E-01, 9.98027E-01,
      9.97159E-01, 9.96134E-01, 9.94951E-01, 9.93611E-01, 9.92115E-01,
      9.90461E-01, 9.88652E-01, 9.86686E-01, 9.84564E-01, 9.82287E-01,
      9.79855E-01, 9.77268E-01, 9.74527E-01, 9.71632E-01, 9.68583E-01,
      9.65382E-01, 9.62028E-01, 9.58522E-01, 9.54865E-01, 9.51057E-01,
      9.47098E-01, 9.42991E-01, 9.38734E-01, 9.34329E-01, 9.29777E-01,
      9.25077E-01, 9.20232E-01, 9.15241E-01, 9.10106E-01, 9.04827E-01,
      8.99405E-01, 8.93841E-01, 8.88136E-01, 8.82291E-01, 8.76307E-01,
      8.70184E-01, 8.63923E-01, 8.57527E-01, 8.50994E-01, 8.44328E-01,
      8.37528E-01, 8.30596E-01, 8.23533E-01, 8.16339E-01, 8.09017E-01,
      8.01567E-01, 7.93990E-01, 7.86288E-01, 7.78462E-01, 7.70513E-01,
      7.62443E-01, 7.54251E-01, 7.45941E-01, 7.37513E-01, 7.28969E-01,
      7.20309E-01, 7.11536E-01, 7.02650E-01, 6.93653E-01, 6.84547E-01,
      6.75333E-01, 6.66012E-01, 6.56586E-01, 6.47056E-01, 6.37424E-01,
      6.27691E-01, 6.17860E-01, 6.07930E-01, 5.97905E-01, 5.87785E-01,
      5.77573E-01, 5.67269E-01, 5.56876E-01, 5.46394E-01, 5.35827E-01,
      5.25175E-01, 5.14440E-01, 5.03623E-01, 4.92727E-01, 4.81754E-01,
      4.70704E-01, 4.59580E-01, 4.48383E-01, 4.37116E-01, 4.25779E-01,
      4.14376E-01, 4.02906E-01, 3.91374E-01, 3.79779E-01, 3.68125E-01,
      3.56412E-01, 3.44643E-01, 3.32820E-01, 3.20944E-01, 3.09017E-01,
      2.97042E-01, 2.85019E-01, 2.72952E-01, 2.60842E-01, 2.48690E-01,
      2.36499E-01, 2.24271E-01, 2.12007E-01, 1.99710E-01, 1.87381E-01,
      1.75023E-01, 1.62637E-01, 1.50226E-01, 1.37790E-01, 1.25333E-01,
      1.12856E-01, 1.00362E-01, 8.78512E-02, 7.53268E-02, 6.27905E-02,
      5.02443E-02, 3.76902E-02, 2.51301E-02, 1.25660E-02, 0.00000E+00);

   PWM_Timer_Ref : access Timer := PWM_Timer'Access;

   Modulators : array (PWM_Phase'Range) of PWM_Modulator;

   type Gate_Setting is record
      Channel   : Timer_Channel;
      Pin_H     : GPIO_Point;
      Pin_L     : GPIO_Point;
      Pin_AF    : STM32.GPIO_Alternate_Function;
   end record;

   type Gate_Settings is array (PWM_Phase'Range) of Gate_Setting;

   Gate_Phase_Settings : constant Gate_Settings :=
      (A => Gate_Setting'(Channel => PWM_A_Channel,
                          Pin_H   => PWM_A_H_Pin,
                          Pin_L   => PWM_A_L_Pin,
                          Pin_AF  => PWM_A_GPIO_AF),
       B => Gate_Setting'(Channel => PWM_B_Channel,
                          Pin_H   => PWM_B_H_Pin,
                          Pin_L   => PWM_B_L_Pin,
                          Pin_AF  => PWM_B_GPIO_AF));

   Sine_Gain : Gain_Range := 0.0;
   --  Defines the gain of the sinusoid according to the battery voltage.

   protected PWM_Handler is
      pragma Interrupt_Priority (PWM_ISR_Priority);

   private

      Counter : Integer := 0;
      --  For testing the output.

      Semi_Senoid : Boolean := False;
      --  Defines False = 1'st half sinusoid, True = 2'nd half sinusoid.

      procedure PWM_ISR_Handler with
        Attach_Handler => PWM_Interrupt;

   end PWM_Handler;

end Inverter_PWM;
