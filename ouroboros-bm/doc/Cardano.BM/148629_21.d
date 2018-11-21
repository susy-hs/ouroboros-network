format 221

statecanvas 128277 state_ref 128149 // setup
  
  xyzwh 233 243 2000 103 33
end
statecanvas 128533 state_ref 128277 // messaging
  
  xyzwh 447 244 2000 93 33
end
pseudostatecanvas 128789 pseudostate_ref 128277 // final
   xyz 614 388 2000
end
statecanvas 128917 state_ref 128405 // takedown
  
  xyzwh 454 384 2000 85 33
end
pseudostatecanvas 129557 pseudostate_ref 128405 // initial
   xyz 130 250 2000
end
note 131221 "The Backend is first setup.
Then, it receives messages that it forwards to its output, maybe processed.
At the end, it is taken down."
  xyzwh 125 110 2000 279 105
transitioncanvas 128661 transition_ref 128149 // <transition>
  
  from ref 128277 z 2001 to ref 128533
  write_horizontally default show_definition default drawing_language default
end
transitioncanvas 129045 transition_ref 128277 // <transition>
  
  from ref 128917 z 2001 to ref 128789
  write_horizontally default show_definition default drawing_language default
end
transitioncanvas 129685 transition_ref 128533 // <transition>
  
  from ref 129557 z 2001 to ref 128277
  write_horizontally default show_definition default drawing_language default
end
transitioncanvas 130581 transition_ref 128661 // <transition>
  
  from ref 128533 z 2001 to point 565 254
  line 130709 z 2001 to point 563 197
  line 130965 z 2001 to point 488 197
  line 130837 z 2001 to ref 128533
  write_horizontally default show_definition default drawing_language default
end
transitioncanvas 131093 transition_ref 128917 // <transition>
  
  from ref 128533 z 2001 to ref 128917
  write_horizontally default show_definition default drawing_language default
end
end
