cfa <- '

# Teacher Quality
Math_Teacher_Understandable =~ BSBM17A + BSBM17B + BSBM17C + BSBM17D + BSBM17E + BSBM17F + BSBM17G;
Math_Teacher_Oderliness =~ BSBM18A + BSBM18B + BSBM18C + BSBM18D + BSBM18E + BSBM18F;

Math_Teacher_Follow =~ BTBM15B + BTBM15A + BTBM15C + BTBM15F;
Math_Teacher_Practice_Individually =~ BTBM15D + BTBM15E;
Science_Teacher_Experiments =~ BTBS15F + BTBS15D + BTBS15E +  BTBS15G + BTBS15H;
General_Teacher_Discussions =~ BTBG12G + BTBG12F + BTBG12D;

! low loadings General_Teacher_Explain =~  BTBG12E + BTBG12B;
! low loadings Science_Teacher_Memorizing =~ BTBS15K + BTBS15I + BTBS15J;
! low loadings Science_Teacher_Field_Work =~ BTBS15L + BTBS15B;

# Student_Attitudes
Math_Student_Attitudes =~ BSBM20A + BSBM20B + BSBM20C + BSBM20D + BSBM20E + BSBM20F + BSBM20G + BSBM20H + BSBM20I;
Science_Student_Attitudes =~ BSBS25A + BSBS25B + BSBS25C + BSBS25D + BSBS25E + BSBS25F + BSBS25G + BSBS25H + BSBS25I;

# School Environment
Lights =~ n15_0 + n15_1 + n15_2_rev;
Visibility =~ n16_0 + n16_1 + n16_2;
Comfort_furniture =~ n17_0 + n17_1 + n17_2;
Safety =~ n24_0 + n24_1 + n24_2 + n24_3 + n24_4;

# Outcomes
MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05;
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05;
WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A

Teacher_Quality <~ Math_Teacher_Understandable + Math_Teacher_Oderliness;
Student_Attitudes <~ Math_Student_Attitudes + Science_Student_Attitudes;
School_Environment <~ Lights + Visibility + Comfort_furniture + Safety + n_spaces + n_outside_spaces + n_tech + n11_2 + n12_2 + n14_2;
'

'# Regressions:
Student_Attitudes ~ a1*Teacher_Quality + a2*School_Environment
MATH ~ b1*Student_Attitudes + b2*Teacher_Quality + b3*School_Environment
SCIENCE ~ c1*Student_Attitudes + c2*Teacher_Quality + c3*School_Environment
WELLNESS ~ d1*Student_Attitudes + d2*Teacher_Quality + d3*School_Environment


# Indirect effects:
Teacher_Math := a1*b1
Teacher_Sciene := a1*c1
Teacher_Wellness := a1*d1

Environment_Math := a2*b1
Environment_Sciene := a2*c1
Environment_Wellness := a2*d1
'


sem <- '

# Teacher_Quality
Math_Teacher_Understandable =~ BSBM17B + BSBM17C + BSBM17D + BSBM17E + BSBM17F + BSBM17G
Math_Teacher_Oderliness =~ BSBM18A + BSBM18B + BSBM18C + BSBM18D + BSBM18E + BSBM18F

# Student_Attitudes
Math_Student_Attitudes =~ BSBM20A + BSBM20B + BSBM20C + BSBM20D + BSBM20E + BSBM20F + BSBM20G + BSBM20I
Science_Student_Attitudes =~ BSBS25A + BSBS25B + BSBS25C + BSBS25D + BSBS25E + BSBS25F + BSBS25G + BSBS25H + BSBS25I

Student_Attitudes =~ Math_Student_Attitudes + Science_Student_Attitudes

# School_Environment
Audibility =~ n15_0 + n15_1
Visibility =~ n16_0 + n16_1 + n16_2 + n14_2
Temperature =~ n11_2 + n12_2 
Safety =~ n24_1 + n24_2 + n24_4
!Comfort_furniture =~ n17_1 + n17_2

Space =~ Audibility + Visibility + Temperature


# Formative Factors
Teacher_Quality =~ 0
Teacher_Quality =~ 1*Math_Teacher_Understandable + Math_Teacher_Oderliness
School_Environment =~ 0
School_Environment ~ 1*Space + Safety + n_spaces


# Outcomes
MATH =~ BSMMAT01 + BSMMAT02 + BSMMAT03 + BSMMAT04 + BSMMAT05
SCIENCE =~ BSSSCI02 + BSSSCI01 + BSSSCI03 + BSSSCI04 + BSSSCI05
WELLNESS =~ BSBG13E + BSBG13B + BSBG13C + BSBG13D + BSBG13A

# Regressions:
Student_Attitudes ~ Teacher_Quality + School_Environment + BSBGHER + ITSEX
MATH ~ Student_Attitudes + Teacher_Quality + School_Environment + BSBGHER + ITSEX
SCIENCE ~ Student_Attitudes + Teacher_Quality + School_Environment + BSBGHER + ITSEX
WELLNESS ~ Student_Attitudes + Teacher_Quality + School_Environment + BSBGHER + ITSEX


'
