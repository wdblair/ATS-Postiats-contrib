<?php
/*
**
** The PHP code is generated by atscc2php
** The starting compilation time is: 2014-9-30: 14h:11m
**
*/

function
fact($arg0)
{
/*
  // $tmpret0
  // $tmp1
  // $tmp2
  // $tmp3
*/
  __patsflab_fact:
  $tmp1 = ats2phppre_gt_int0_int0($arg0, 0);
  if($tmp1) {
    $tmp3 = ats2phppre_sub_int0_int0($arg0, 1);
    $tmp2 = fact($tmp3);
    $tmpret0 = ats2phppre_mul_int0_int0($arg0, $tmp2);
  } else {
    $tmpret0 = 1;
  } // endif
  return $tmpret0;
} // end-of-function

/* ****** ****** */

/* end-of-compilation-unit */
?>