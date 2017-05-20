This directory contains Haskell programs for counting and generating closed affine and closed linear lambda terms.
The method is described in paper.

     Quantitative aspects of linear and affine closed lambda terms by Pierre Lescanne,
     arXiv:1702.03085  https://arxiv.org/abs/1702.03085 (see file counting.pdf in this directory)

and in a beamer presentation Lescanne_CLA_2017.pdf as well.



This directory contains the following files:

    Affine.hs:
        on counting affine terms (including closed affine terms)  with natural size.
        
    AffineGeneration.hs:
        on generating affine terms sorted by natural size and generating random affine terms.
        
    AffineNormalForm.hs:
        on counting and generating affine normal forms w.r.t. natural size.
        
    AffineSize0or1.hs:
        on counting and generating affine terms (including closed affine terms)  with variable size 0 or 1
        
    Constants.hs:
        some constants needed by other programs

    Lescanne_CLA_2017.pdf:
        a beamer presentation
    
    Linear.hs:
        on counting linear terms (including closed linear terms)  with natural size
    
    LinearClosedNormalForm.hs:
        on counting linear normal forms (including closed linear terms)  with natural size
    
    LinearGeneration.hs:
        on generating linear terms sorted by natural size and generating random linear terms
    
    LinearGenerationSize0or1.hs:
        on generating linear terms (including closed linear terms)  with variable size 0 or 1
    
    LinearNormalForm.hs:
        a module testing the accuracy of counting closed affine and linear normal forms
        (using a brute force method: genrating and filtering) and the method developed in this module)
    
    LinearNormalFormSize0or1.hs: 
        on counting normal forms sorted by variable size 0 or 1

    LinearSize0or1.hs:
        on counting normal forms sorted by natural size
    
    NaturalSize.hs:
        a utilitary module
    
    NormalForm.hs:
        a module sumarizing counting normal forms (affine and linear) w.r.t. natural size
    
    README.md:
        this file
    
    SwissCheese.hs:
        the specific date structure SwissCheese
    
    counting_affine.pdf:
        the associated paper