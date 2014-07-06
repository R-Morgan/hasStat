import Exponential

propExpPDF x lambda =
    exponentialPDF x lambda >= 0 

propExpEquivPDF x lambda =
    exponentialPDF x lambda == exponentialAltPDF x (1 / lambda) 

