;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               mechanic.lisp
;;;;
;;;;   Started:            Fri May 14 19:44:29 2021
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;   Touretzky ch. 12 (374 页)
;;;;   Slightly reformatted to fit Graham's model.
;;;;
(defnode 'diagnose-engine "Does the engine turn over?" 'engine-turns-over 'engine-wont-turn-over)
(defnode 'engine-turns-over "Will the engine run for any period of time?" 'engine-will-run-briefly 'engine-wont-run)
(defnode 'engine-wont-run "Is there gas in the tank?" 'gas-in-tank 'no-gas)
(defnode 'no-gas "Fill the tank and try starting the engine again.")
(defnode 'engine-wont-turn-over "Do you hear any sound when you turn the key?" 'sound-when-turn-key 'no-sound-when-turn-key)
(defnode 'no-sound-when-turn-key "Is the battery voltage low?" 'battery-voltage-low 'battery-voltage-ok)
(defnode 'battery-voltage-low "Replace the battery")
(defnode 'battery-voltage-ok "Are the battery cables dirty or loose?" 'loose-cables 'battery-cables-good)
(defnode 'loose-cables "Clean the cables and tighten the connections.")
(defnode 'engine-will-run-briefly "Does the engine stall when cold but not when warm?" 'check-idle-speed 'unknown-problem)
(defnode 'check-idle-speed "Is the cold idle speed at least 700 RPM?" 'unknown-problem 'adjust-idle-speed)
(defnode 'adjust-idle-speed "Adjust the idle speed.")
(defnode 'unknown-problem "Sorry. I have no clue.")
